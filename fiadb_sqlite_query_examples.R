# query fiadb
# updated 5/7/2024 by Chris Mihiar
# load packages
library(RSQLite)
library(tidyverse)

# connect to FIA database----

fiadb <- dbConnect(SQLite(), 'D:/FIADB SQLite/SQLite_FIADB_ENTIRE.db')

# recover list of most recent evaluations by state----

max_evalgrp <- dbGetQuery(fiadb, "select statecd, Max(eval_grp) as maxeval
                                  from pop_eval_grp
                                  group by statecd
                                  order by statecd")

# timberland area by age class, forest type, and state----
# most recent evaluations for the US South

# -- Computing tree per acre by forest-type group and stand age (20-year classes)
# -- NOTE ** I add one acre (+1) to the calculation of forest area, to prevent the divide by zero error that may occur.  
# -- The results match evalidator
# -- Adapted from
# -- Andrew Hartsell April 30, 2024

area <- dbGetQuery(fiadb, "select peg.eval_grp, c.fortypcd, peg.statecd,
       sum(((ifnull(c.condprop_unadj * 
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns))+1) areatot,
       sum(((ifnull(c.condprop_unadj *
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns * CASE WHEN (c.stdage >= 0 AND c.stdage < 20)
                                        THEN 1 ELSE 0 END))+1) area20,
       sum(((ifnull(c.condprop_unadj *
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns * CASE WHEN (c.stdage >= 20 AND c.stdage < 40)
                                        THEN 1 ELSE 0 END))+1) area40,
       sum(((ifnull(c.condprop_unadj *
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns * CASE WHEN (c.stdage >= 40 AND c.stdage < 60)
                                        THEN 1 ELSE 0 END))+1) area60,
       sum(((ifnull(c.condprop_unadj *
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns * CASE WHEN (c.stdage >= 60 AND c.stdage < 80)
                                        THEN 1 ELSE 0 END))+1) area80,
       sum(((ifnull(c.condprop_unadj *
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns * CASE WHEN (c.stdage >= 80 AND c.stdage < 100)
                                        THEN 1 ELSE 0 END))+1) area100,
       sum(((ifnull(c.condprop_unadj *
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns * CASE WHEN (c.stdage >= 100)
                                        THEN 1 ELSE 0 END))+1) area100plus
  from POP_EVAL_GRP           peg,
       POP_EVAL_TYP           pet,
       POP_EVAL               pe,
       POP_ESTN_UNIT          eus,
       POP_STRATUM            ps,
       POP_PLOT_STRATUM_ASSGN ppsa,
       PLOT                   p,
       COND                   c
 where peg.cn = pet.eval_grp_cn
   and pet.eval_typ = 'EXPCURR'
   and pet.eval_cn = pe.cn
   and pe.cn = eus.eval_cn
   and eus.cn = ps.estn_unit_cn
   and ps.cn = ppsa.stratum_cn
   and ppsa.plt_cn = p.cn
   and p.cn = c.plt_cn
   and c.cond_status_cd = 1
   and c.reservcd = 0
   and c.siteclcd < 7
   and peg.eval_grp in (12022,52022,122020,132021,222020,
                          282022,372002,452021,472019,482022,512021)
 group by peg.eval_grp, c.fortypcd, peg.statecd
 order by c.fortypcd")


# number of trees by age class, forest type, and state----
# most recent evaluations for the US South
# timberland only for trees 1"dbh or larger

trees <- dbGetQuery(fiadb, "select peg.eval_grp, c.fortypcd, peg.statecd,
                    sum((ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns)) treestot,
                    sum(ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns * CASE WHEN (c.stdage >= 0 AND c.stdage < 20)
                                        THEN 1 ELSE 0 END) trees20,
                    sum(ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns * CASE WHEN (c.stdage >= 20 AND c.stdage < 40)
                                        THEN 1 ELSE 0 END) trees40,
                    sum(ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns * CASE WHEN (c.stdage >= 40 AND c.stdage < 60)
                                        THEN 1 ELSE 0 END) trees60,
                    sum(ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns * CASE WHEN (c.stdage >= 60 AND c.stdage < 80)
                                        THEN 1 ELSE 0 END) trees80,
                    sum(ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns * CASE WHEN (c.stdage >= 80 AND c.stdage < 100)
                                        THEN 1 ELSE 0 END) trees100,
                    sum(ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns * CASE WHEN (c.stdage >= 100)
                                        THEN 1 ELSE 0 END) trees100plus

from POP_EVAL_GRP            peg,
    POP_EVAL_TYP             pet,
    POP_EVAL                 pe,
    POP_ESTN_UNIT            eus,
    POP_STRATUM              ps,
    POP_PLOT_STRATUM_ASSGN   ppsa,
    PLOT                     p,
    COND                     c,
    TREE                     t
where peg.cn = pet.eval_grp_cn 
    and pet.eval_typ = 'EXPVOL' 
    and pet.eval_cn = pe.cn
    and pe.cn = eus.eval_cn
    and eus.cn = ps.estn_unit_cn
    and ps.cn = ppsa.stratum_cn
    and ppsa.plt_cn = p.cn
    and p.cn = c.plt_cn
    and c.plt_cn = t.plt_cn
    and c.condid = t.condid
    and c.cond_status_cd = 1
    and t.statuscd = 1 
    and t.dia >= 1.0
    and c.reservcd = 0
    and c.siteclcd < 7
    and peg.eval_grp in (12022,52022,122020,132021,222020,
                          282022,372002,452021,472019,482022,512021)
group by peg.eval_grp, c.fortypcd, peg.statecd
order by c.fortypcd")

# calculate trees per acre by forest type and state----

tpa <- inner_join(area, trees, by = c("EVAL_GRP", "FORTYPCD", "STATECD")) %>%
  mutate(tpatot = treestot / areatot,
         tpa20 = trees20 / area20,
         tpa40 = trees40 / area40,
         tpa60 = trees60 / area60,
         tpa80 = trees80 / area80,
         tpa100 = trees100 / area100,
         tpa100plus = trees100plus / area100plus)

saveRDS(tpa, "C:/Users/cmihiar/OneDrive - USDA/(RPA) Assessment/Development Value in Forestland/trees_per_acre.rds")

# trees per acre by age class---
# trees per acre conus states by fortypcd----

area <- dbGetQuery(fiadb, "select peg.eval_grp, c.fortypcd, peg.statecd,
       sum(((ifnull(c.condprop_unadj * 
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns))+1) areatot
  from POP_EVAL_GRP           peg,
       POP_EVAL_TYP           pet,
       POP_EVAL               pe,
       POP_ESTN_UNIT          eus,
       POP_STRATUM            ps,
       POP_PLOT_STRATUM_ASSGN ppsa,
       PLOT                   p,
       COND                   c
 where peg.cn = pet.eval_grp_cn
   and pet.eval_typ = 'EXPCURR'
   and pet.eval_cn = pe.cn
   and pe.cn = eus.eval_cn
   and eus.cn = ps.estn_unit_cn
   and ps.cn = ppsa.stratum_cn
   and ppsa.plt_cn = p.cn
   and p.cn = c.plt_cn
   and c.cond_status_cd = 1
   and c.reservcd = 0
   and c.siteclcd < 7
 group by peg.eval_grp, c.fortypcd, peg.statecd
 order by c.fortypcd")

trees <- dbGetQuery(fiadb, "select peg.eval_grp, c.fortypcd, peg.statecd,
                    sum((ifnull(t.tpa_unadj * 
                          CASE WHEN t.dia IS null THEN ps.adj_factor_subp
                                ELSE (CASE WHEN t.dia <= (5-0.001) THEN ps.adj_factor_micr
                                        ELSE (CASE WHEN p.macro_breakpoint_dia IS null
                                                    THEN ps.adj_factor_subp 
                                                    ELSE ps.adj_factor_macr END)
                                        END) 
                                END, 0) * ps.expns)) treestot

from POP_EVAL_GRP            peg,
    POP_EVAL_TYP             pet,
    POP_EVAL                 pe,
    POP_ESTN_UNIT            eus,
    POP_STRATUM              ps,
    POP_PLOT_STRATUM_ASSGN   ppsa,
    PLOT                     p,
    COND                     c,
    TREE                     t
where peg.cn = pet.eval_grp_cn 
    and pet.eval_typ = 'EXPVOL' 
    and pet.eval_cn = pe.cn
    and pe.cn = eus.eval_cn
    and eus.cn = ps.estn_unit_cn
    and ps.cn = ppsa.stratum_cn
    and ppsa.plt_cn = p.cn
    and p.cn = c.plt_cn
    and c.plt_cn = t.plt_cn
    and c.condid = t.condid
    and c.cond_status_cd = 1
    and t.statuscd = 1 
    and t.dia >= 1.0
    and c.reservcd = 0
    and c.siteclcd < 7
group by peg.eval_grp, c.fortypcd, peg.statecd
order by c.fortypcd")

tpa <- inner_join(area, trees, by = c("EVAL_GRP", "FORTYPCD", "STATECD")) %>%
  mutate(tpatot = treestot / areatot)

saveRDS(tpa, "C:/Users/cmihiar/OneDrive - USDA/Data/FIA Data/trees_per_acre.rds")

# age of current stock of timber----

timber_age <- dbGetQuery(fiadb, "SELECT PS.EVAL_GRP,
                                    T.SPGRPCD,
                                    C.STDAGE,
                                    PEG.STATECD,
                                    SUM(PS.EXPVOL*T.VOLCFNET*T.TPA_UNADJ*
                                      CASE WHEN T.DIA IS NULL
                                        THEN PS.ADJ_EXPVOL_SUBP
                                      WHEN T.DIA < 5
                                        THEN PS.ADJ_EXPVOL_MICR
                                      WHEN PS.MACRO_BREAKPOINT_DIA IS NULL
                                        THEN PS.ADJ_EXPVOL_SUBP
                                      WHEN T.DIA < PS.MACRO_BREAKPOINT_DIA
                                        THEN PS.ADJ_EXPVOL_SUBP
                                        ELSE PS.ADJ_EXPVOL_MACR
                                      END) 
                                      AS VOLCFNET
                                  FROM POP_EVAL_GRP PEG,
                                    PLOTSNAP PS,
                                    COND C,
                                    TREE T
                                  WHERE PS.CN = C.PLT_CN
                                    AND C.PLT_CN = T.PLT_CN
                                    AND C.CONDID = T.CONDID
                                    AND PEG.CN = PS.EVAL_GRP_CN
                                    AND C.COND_STATUS_CD = 1
                                    AND C.RESERVCD = 0
                                    AND SITECLCD < 7
                                    AND T.TREECLCD = 2
                                  GROUP BY PS.EVAL_GRP, PEG.STATECD,
                                    T.SPGRPCD, C.STDAGE")

# saveRDS(timber_age, "D:/FIADB SQLite/stdage_spgrp_state_conus.rds")

# average harvest age by forest type and state----

harvest_age <- dbGetQuery(fiadb, "SELECT PS.EVAL_GRP,
                                    C.FORTYPCD,
                                    C.STDAGE,
                                    PEG.STATECD,
                                    SUM(PS.EXPVOL*T.VOLCFNET*T.TPA_UNADJ*
                                      CASE WHEN T.DIA IS NULL
                                        THEN PS.ADJ_EXPVOL_SUBP
                                      WHEN T.DIA < 5
                                        THEN PS.ADJ_EXPVOL_MICR
                                      WHEN PS.MACRO_BREAKPOINT_DIA IS NULL
                                        THEN PS.ADJ_EXPVOL_SUBP
                                      WHEN T.DIA < PS.MACRO_BREAKPOINT_DIA
                                        THEN PS.ADJ_EXPVOL_SUBP
                                        ELSE PS.ADJ_EXPVOL_MACR
                                      END) 
                                      AS VOLCFNET
                                  FROM POP_EVAL_GRP PEG,
                                    PLOTSNAP PS,
                                    COND C,
                                    TREE T
                                  WHERE PS.CN = C.PLT_CN
                                    AND C.PLT_CN = T.PLT_CN
                                    AND C.CONDID = T.CONDID
                                    AND PEG.CN = PS.EVAL_GRP_CN
                                    AND C.COND_STATUS_CD = 1
                                    AND C.RESERVCD = 0
                                    AND SITECLCD < 7
                                    AND C.TRTCD1 = 10
                                    AND T.TREECLCD = 2
                                  GROUP BY PS.EVAL_GRP, PEG.STATECD,
                                    C.FORTYPCD, C.STDAGE")

# saveRDS(harvest_age, "D:/FIADB SQLite/average_harvest_age_state_conus.rds")

# average harvest volume by forest type and county----

harvest_vol <- dbGetQuery(fiadb, "SELECT PS.EVAL_GRP,
                                    C.FORTYPCD,
                                    C.COUNTYCD,
                                    SUM(PS.EXPVOL*T.VOLCFNET*T.TPA_UNADJ*
                                      CASE WHEN T.DIA IS NULL
                                        THEN PS.ADJ_EXPVOL_SUBP
                                      WHEN T.DIA < 5
                                        THEN PS.ADJ_EXPVOL_MICR
                                      WHEN PS.MACRO_BREAKPOINT_DIA IS NULL
                                        THEN PS.ADJ_EXPVOL_SUBP
                                      WHEN T.DIA < PS.MACRO_BREAKPOINT_DIA
                                        THEN PS.ADJ_EXPVOL_SUBP
                                        ELSE PS.ADJ_EXPVOL_MACR
                                      END) 
                                      AS VOLCFNET
                                  FROM POP_EVAL_GRP PEG,
                                    PLOTSNAP PS,
                                    COND C,
                                    TREE T
                                  WHERE PS.CN = C.PLT_CN
                                    AND C.PLT_CN = T.PLT_CN
                                    AND C.CONDID = T.CONDID
                                    AND PEG.CN = PS.EVAL_GRP_CN
                                    AND C.COND_STATUS_CD = 1
                                    AND C.RESERVCD = 0
                                    AND SITECLCD < 7
                                    AND C.TRTCD1 = 10
                                    AND T.TREECLCD = 2
                                  GROUP BY PS.EVAL_GRP, C.COUNTYCD,
                                    C.FORTYPCD")
# timber_acres -- timberland acres by forest type----

timber_acres <- dbGetQuery(fiadb, "SELECT ps.EVAL_GRP, c.fortypcd,
                              Sum([EXPCURR]*[CONDPROP_UNADJ]*[ADJ_EXPCURR]) 
                                AS [total_timberland_acres], 
                              peg.STATECD
                            FROM pop_eval_grp peg,
                              plotsnap ps,
                              cond c
                            WHERE ps.cn = c.plt_cn
                              AND peg.cn = ps.eval_grp_cn
                              AND c.cond_status_cd = 1
                              AND (c.siteclcd=1 or c.siteclcd=2 or c.siteclcd=3 or
                                c.siteclcd=4 or c.siteclcd=5 or c.siteclcd=6) 
                              AND c.reservcd=0
                              GROUP BY ps.eval_grp,
                                peg.STATECD,
                                c.fortypcd")

# redcedar_acres ----

redcedar_acres <- area <- dbGetQuery(fiadb, "select peg.eval_grp, c.fortypcd, peg.statecd,
       sum(((ifnull(c.condprop_unadj * 
                      CASE WHEN c.prop_basis == 'MACR' THEN ps.adj_factor_macr
                                                       ELSE ps.adj_factor_subp
                                                       END,
                      0) * ps.expns))+1) areatot
  from POP_EVAL_GRP peg,
       POP_EVAL_TYP pet,
       POP_EVAL pe,
       POP_ESTN_UNIT          eus,
       POP_STRATUM            ps,
       POP_PLOT_STRATUM_ASSGN ppsa,
       PLOT                   p,
       COND                   c,
       datamart_most_recent_inv mri
 where peg.cn = pet.eval_grp_cn
   and pet.eval_typ = 'EXPCURR'
   and pet.eval_cn = pe.cn
   and pe.cn = eus.eval_cn
   and eus.cn = ps.estn_unit_cn
   and ps.cn = ppsa.stratum_cn
   and ppsa.plt_cn = p.cn
   and p.cn = c.plt_cn
   and mri.eval_grps = peg.eval_grp
   and c.cond_status_cd = 1
   and c.reservcd = 0
   and c.siteclcd < 7
   and c.fortypcd = 171
 group by peg.eval_grp, c.fortypcd, peg.statecd
 order by c.fortypcd")

# planted_acres -- planted acres by species----

planted_acres <- dbGetQuery(fiadb, "SELECT ps.EVAL_GRP, c.stdorgsp, c.FORTYPCD,
                              Sum([EXPCURR]*[CONDPROP_UNADJ]*[ADJ_EXPCURR]) 
                                AS [total_timberland_acres], 
                              peg.STATECD
                            FROM pop_eval_grp peg,
                              plotsnap ps,
                              cond c
                            WHERE ps.cn = c.plt_cn
                              AND peg.cn = ps.eval_grp_cn
                              AND c.cond_status_cd = 1
                              AND c.stdorgcd = 1
                              AND (c.siteclcd=1 Or c.siteclcd=2 
                                or c.siteclcd=3 or
                                c.siteclcd=4 or c.siteclcd=5 or c.siteclcd=6) 
                              AND c.reservcd=0
                              GROUP BY ps.eval_grp,
                                peg.STATECD,
                                c.stdorgsp,
                                c.FORTYPCD")

# volume_timber -- volume of biomass on timberland by forest type----

volume_timber <- dbGetQuery(fiadb,
                              "SELECT peg.eval_grp,
                                Sum(ps.expvol*t.volcfnet*t.tpa_unadj*
                                  CASE WHEN t.dia IS NULL
                                        THEN ps.adj_expvol_subp
                                       WHEN t.dia < 5
                                        THEN ps.adj_expvol_micr
                                       WHEN ps.macro_breakpoint_dia IS NULL
                                        THEN ps.adj_expvol_subp
                                       WHEN t.dia < ps.macro_breakpoint_dia
                                        THEN ps.adj_expvol_subp
                                        ELSE ps.adj_expvol_macr
                                  END) AS volume,
                                peg.statecd,
                                c.fortypcd
                              FROM pop_eval_grp peg,
                                plotsnap ps,
                                cond c,
                                tree t
                              WHERE ps.cn = c.plt_cn
                                AND c.plt_cn = t.plt_cn
                                AND c.condid = t.condid
                                AND peg.cn = ps.eval_grp_cn
                                AND t.treeclcd = 2
                                AND c.cond_status_cd = 1
                                AND t.statuscd = 1
                                AND c.reservcd = 0
                                AND (c.siteclcd=1 Or c.siteclcd=2 
                                      or c.siteclcd=3 or c.siteclcd=4 
                                      or c.siteclcd=5 or c.siteclcd=6)
                              GROUP BY peg.eval_grp,
                                peg.statecd,
                                c.fortypcd")

# volume_timber -- volume of current stock by species, age, and size----

volume_timber <- dbGetQuery(fiadb,
                            "SELECT peg.eval_grp,
                                    peg.statecd,
                                    t.spcd,
                                    t.spgrpcd,
                                    c.fortypcd,
                                    t.dia,
                                    c.stdage,
                                    c.stdorgcd,
                                    c.trtcd1,
                                Sum(ps.expvol*t.volcfnet*t.tpa_unadj*
                                  CASE WHEN t.dia IS NULL
                                        THEN ps.adj_expvol_subp
                                       WHEN t.dia < 5
                                        THEN ps.adj_expvol_micr
                                       WHEN ps.macro_breakpoint_dia IS NULL
                                        THEN ps.adj_expvol_subp
                                       WHEN t.dia < ps.macro_breakpoint_dia
                                        THEN ps.adj_expvol_subp
                                        ELSE ps.adj_expvol_macr
                                  END) AS volcfnet
                              FROM pop_eval_grp peg,
                                plotsnap ps,
                                cond c,
                                tree t
                              WHERE ps.cn = c.plt_cn
                                AND c.plt_cn = t.plt_cn
                                AND c.condid = t.condid
                                AND peg.cn = ps.eval_grp_cn
                                AND t.treeclcd = 2
                                AND c.cond_status_cd = 1
                                AND t.statuscd = 1
                                AND c.reservcd = 0
                                AND c.owngrpcd = 40
                                AND c.siteclcd < 7
                                AND peg.eval_grp IN (12022,52022,122020,
                                                    132021,222020,282022,
                                                    372002,452021,472019,
                                                    482022,512021)
                              GROUP BY peg.statecd,
                                    t.spcd,
                                    t.spgrpcd,
                                    c.fortypcd,
                                    c.stdorgcd,
                                    c.trtcd1")

# volume_timber_distr -- volume of current stock by species, age, and size----

# volume_timber_mort <- dbGetQuery(fiadb,
#                             "SELECT peg.eval_grp,
#                                     peg.statecd,
#                                     t.spcd,
#                                     t.spgrpcd,
#                                     t.dia,
#                                     t.agentcd,
#                                     c.dstrbcd1,
#                                     c.dstrbcd2,
#                                     c.dstrbcd3,
#                                 Sum(ps.expvol*t.volcfnet*t.tpa_unadj) AS volcfnet
#                               FROM pop_eval_grp peg,
#                                 plotsnap ps,
#                                 cond c,
#                                 tree t
#                               WHERE ps.cn = c.plt_cn
#                                 AND c.plt_cn = t.plt_cn
#                                 AND c.condid = t.condid
#                                 AND peg.cn = ps.eval_grp_cn
#                                 AND t.treeclcd = 2
#                                 AND c.cond_status_cd = 1
#                                 AND t.statuscd = 2
#                                 AND c.reservcd = 0
#                                 AND c.owngrpcd = 40
#                                 AND c.siteclcd < 7
#                                 AND peg.eval_grp IN (12022)
#                               GROUP BY peg.statecd,
#                                     t.spcd,
#                                     t.spgrpcd,
#                                     t.agentcd,
#                                     c.dstrbcd1,
#                                     c.dstrbcd2,
#                                     c.dstrbcd3")

# volume_planted -- volume of biomass on planted timberland by species----

volume_planted <- dbGetQuery(fiadb,
                             "SELECT peg.eval_grp,
                                Sum(ps.expvol*t.volcfnet*t.tpa_unadj*
                                  CASE WHEN t.dia IS NULL
                                        THEN ps.adj_expvol_subp
                                       WHEN t.dia < 5
                                        THEN ps.adj_expvol_micr
                                       WHEN ps.macro_breakpoint_dia IS NULL
                                        THEN ps.adj_expvol_subp
                                       WHEN t.dia < ps.macro_breakpoint_dia
                                        THEN ps.adj_expvol_subp
                                        ELSE ps.adj_expvol_macr
                                  END) AS volume,
                                peg.statecd,
                                c.stdorgsp,
                                c.FORTYPCD
                              FROM pop_eval_grp peg,
                                plotsnap ps,
                                cond c,
                                tree t
                              WHERE ps.cn = c.plt_cn
                                AND c.plt_cn = t.plt_cn
                                AND c.condid = t.condid
                                AND peg.cn = ps.eval_grp_cn
                                AND t.treeclcd = 2
                                AND c.cond_status_cd = 1
                                AND t.statuscd = 1
                                AND c.reservcd = 0
                                AND (c.siteclcd=1 Or c.siteclcd=2 
                                      or c.siteclcd=3 or c.siteclcd=4 
                                      or c.siteclcd=5 or c.siteclcd=6)
                                AND c.stdorgcd = 1
                              GROUP BY peg.eval_grp,
                                peg.statecd,
                                c.stdorgsp,
                             c.FORTYPCD")

# clean up tables for area (by planted/non-planted)----

# load in region-state crosswalk table
georef <- distinct(read.csv("D:/georef.csv"),
                   state_fips, .keep_all = T) %>%
  select(state_abbr, state_fips, region, subregion) %>%
  mutate(state = str_pad(state_fips, 2, side = "left", "0")) %>%
  filter(!state_abbr == "DC")
  

ta <- timber_acres %>%
  filter(STATECD != 2, STATECD != 15, !STATECD > 56) %>%
  mutate(evalgrp = str_pad(as.character(EVAL_GRP), 6, side = 'left', "0"),
         state = substr(evalgrp, 1, 2),
         year = substr(evalgrp, 3, 6),
         type = ifelse(FORTYPCD %in% c(100:399),
                       "conif",
                       "nonconif")) %>%
  select(evalgrp, year, state, type, total_timberland_acres) %>%
  left_join(georef, by = "state") %>%
  group_by(year, type, state_abbr, region, subregion) %>%
  summarize(timberland_acres = sum(total_timberland_acres, na.rm = T),
            .groups = 'drop') %>%
  arrange(type, state_abbr, region, subregion, year)

write.csv(ta, "D:/state_level_timberland_acres_all_years.csv", row.names = F)

%>%
  mutate(region = ifelse(subregion %in% c("PNW", "PSW"), "PC",
                         ifelse(subregion %in% c("RMN", "RMS"), "RM", 
                                ifelse(subregion %in% c("NLS","NPS"), "NC",
                                       subregion)))) %>%
  group_by(year, type, region) %>%
  summarize(timberland_acres = sum(total_timberland_acres, na.rm = T),
            .groups = 'drop') %>%
  arrange(type, region, year)

pa_species <- planted_acres %>%
  filter(STATECD != 2, STATECD != 15, !STATECD > 56) %>%
  mutate(evalgrp = str_pad(as.character(EVAL_GRP), 6, side = 'left', "0"),
         state = substr(evalgrp, 1, 2),
         year = substr(evalgrp, 3, 6),
         planted_species = ifelse(is.na(STDORGSP), 0, STDORGSP), 
         type = ifelse(FORTYPCD %in% c(100:399),
                       "conif",
                       "nonconif")) %>%
  select(year, state, type, planted_species, total_timberland_acres) %>%
  left_join(georef, by = "state") %>%
  group_by(year, type, planted_species, region, subregion) %>%
  summarize(planted_acres = sum(total_timberland_acres, na.rm = T),
            .groups = 'drop') %>%
  arrange(type, planted_species, region, subregion, year)

# what types of trees get planted? top 90% in 2019 are...
# 54.4% of acres are loblloly pine
# 16% are dougfir
# 8% are slash pine
# 4% are red pine
# 3.4% are ponderosa pine
# 3.4% are longleaf pine
# 1.3% is eastern white pine
# 
# pa_species <- pa %>%
#   filter(year == 2019) %>%
#   group_by(planted_species) %>%
#   summarize(planted_acres = sum(planted_acres, na.rm = T), .groups = 'drop')
# pa_species$prop <- (pa_species$planted_acres /
#                       sum(pa_species$planted_acres)) * 100

pa <- select(pa_species, year, type, subregion, planted_acres) %>%
  mutate(region = ifelse(subregion %in% c("PNW", "PSW"), "PC",
                         ifelse(subregion %in% c("RMN", "RMS"), "RM", 
                                ifelse(subregion %in% c("NLS","NPS"), "NC",
                                       subregion)))) %>%
  group_by(year, type, region) %>%
  summarize(planted_acres = sum(planted_acres, na.rm = T), .groups = 'drop') %>%
  arrange(type, region, year)

area <- left_join(ta, pa, by = c('year', 'type' , 'region')) %>%
  pivot_wider(id_cols = c('year', 'region'), names_from = 'type',
              values_from = c('timberland_acres',
                              'planted_acres')) %>%
  mutate(planted_acres_nonconif = ifelse(is.na(planted_acres_nonconif), 0,
                                       planted_acres_nonconif),
         conif_planted_area_share = planted_acres_conif / 
           (planted_acres_conif + planted_acres_nonconif),
         conif_natural_area_share = timberland_acres_conif /
           (timberland_acres_conif + timberland_acres_nonconif))

# clean up tables for volume----

# total timber volume
tv <- volume_timber %>%
  filter(STATECD != 2, STATECD != 15, !STATECD > 56) %>%
  mutate(evalgrp = str_pad(as.character(EVAL_GRP), 6, side = 'left', "0"),
         state = substr(evalgrp, 1, 2),
         year = substr(evalgrp, 3, 6),
         type = ifelse(FORTYPCD %in% c(100:399),
                       "conif",
                       "nonconif")) %>%
  select(year, state, type, volume) %>%
  left_join(georef, by = "state") %>%
  mutate(region = ifelse(subregion %in% c("PNW", "PSW"), "PC",
                         ifelse(subregion %in% c("RMN", "RMS"), "RM", 
                                ifelse(subregion %in% c("NLS","NPS"), "NC",
                                       subregion)))) %>%
  group_by(year, type, region) %>%
  summarize(timber_vol = sum(volume, na.rm = T),
            .groups = 'drop') %>%
  arrange(type, region, year)

# planted timber volume
pv <- volume_planted %>%
  filter(STATECD != 2, STATECD != 15, !STATECD > 56) %>%
  mutate(evalgrp = str_pad(as.character(EVAL_GRP), 6, side = 'left', "0"),
         state = substr(evalgrp, 1, 2),
         year = substr(evalgrp, 3, 6),
         type = ifelse(FORTYPCD %in% c(100:399),
                       "conif",
                       "nonconif")) %>%
  select(year, state, type, volume) %>%
  left_join(georef, by = "state") %>%
  mutate(region = ifelse(subregion %in% c("PNW", "PSW"), "PC",
                         ifelse(subregion %in% c("RMN", "RMS"), "RM", 
                                ifelse(subregion %in% c("NLS","NPS"), "NC",
                                       subregion)))) %>%
  group_by(year, type, region) %>%
  summarize(planted_vol = sum(volume, na.rm = T),
            .groups = 'drop') %>%
  arrange(type, region, year)

volume <- left_join(tv, pv, by = c('year', 'type' , 'region')) %>%
  pivot_wider(id_cols = c('year', 'region'), names_from = 'type',
              values_from = c('timber_vol',
                              'planted_vol')) %>%
  mutate(planted_vol_nonconif = ifelse(is.na(planted_vol_nonconif), 0,
                                       planted_vol_nonconif),
         conif_planted_vol_share = planted_vol_conif / 
           (planted_vol_conif + planted_vol_nonconif),
         conif_natural_vol_share = timber_vol_conif /
           (timber_vol_conif + timber_vol_nonconif))

# save table----

df <- full_join(area, volume, by = c("year", "region"))

write.csv(df, "D:/conus_forest_data.csv", row.names = F)

# Plot----
df %>%
  ggplot( aes(x=year, y=timber_vol_conif,
              group=region, color=region)) +
  geom_line()

# Plot
df %>%
  ggplot( aes(x=year, y=timberland_acres_conif,
              group=region, color=region)) +
  geom_line()

# Plot
df %>%
  ggplot(aes(x=year, y=planted_acres_conif,
              group=region, color=region)) +
  geom_line()
