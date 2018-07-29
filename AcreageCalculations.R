# volume per acre from FIA

# load packages
library(RSQLite)

# set working directory
setwd("D:/GroupWork/FIA Data")

# connect to FIA database and query for trees, acres, and volume -----
# by forest type and county

fia <- dbConnect(SQLite(), 'D:/GroupWork/FIA DataFIADB.db')

#Two ways to find acres per plot
#In Restricted, two additions are pulled left for viewing ease: they blow up the plot acreage numbers
TimberlandPlotAcresRestricted <- dbGetQuery(fia, "SELECT peg.eval_grp EvalGroup, peg.eval_grp_descr EvalGroupDescription, 
                                      c.STATECD State, c.countycd County, 
                                      ps.cn PlotID,  
                                      SUM(ps.expcurr*c.condprop_unadj*ps.adj_expcurr) PlotAcres, 
                                      c.invyr InvYr, c.siteclcd SiteClass, c.stdage StandAge
                                      FROM pop_eval_grp peg,
                                      plotsnap ps,
                                      cond c,
                                      tree t
                                      WHERE ps.cn = c.plt_cn
                                      AND peg.cn = ps.eval_grp_cn
                                      AND c.plt_cn = t.plt_cn
                                      AND c.condid = t.condid
                        AND t.treeclcd = 2
                                      AND c.cond_status_cd = 1
                        AND t.statuscd = 1
                                      AND c.reservcd = 0
                                      AND (c.fortypcd=160 OR c.fortypcd=161 OR c.fortypcd=162 OR
                                           c.fortypcd=163 OR c.fortypcd=164 OR c.fortypcd=165 OR c.fortypcd=166 OR 
                                           c.fortypcd=167 OR c.fortypcd=168)
                                      AND (c.siteclcd=1 Or c.siteclcd=2 or c.siteclcd=3 or
                                            c.siteclcd=4 or c.siteclcd=5 or c.siteclcd=6)
                                      GROUP BY peg.eval_grp, peg.eval_grp_descr, c.STATECD, County, ps.cn
                                      ORDER BY peg.eval_grp DESC, County DESC, ps.cn DESC, PlotAcres DESC")

TimberlandPlotAcres <- dbGetQuery(fia, "SELECT peg.eval_grp EvalGroup, peg.eval_grp_descr EvalGroupDescription, 
                                      c.STATECD State, c.countycd County, 
                                  ps.cn PlotID,  
                                  SUM(ps.expcurr*c.condprop_unadj*ps.adj_expcurr) PlotAcres, 
                                  c.invyr InvYr, c.siteclcd SiteClass, c.stdage StandAge
                                  FROM pop_eval_grp peg,
                                  plotsnap ps,
                                  cond c
                                  WHERE ps.cn = c.plt_cn
                                  AND peg.cn = ps.eval_grp_cn
                                  AND c.cond_status_cd = 1
                                  AND c.reservcd = 0
                                  AND (c.fortypcd=160 OR c.fortypcd=161 OR c.fortypcd=162 OR
                                  c.fortypcd=163 OR c.fortypcd=164 OR c.fortypcd=165 OR c.fortypcd=166 OR 
                                  c.fortypcd=167 OR c.fortypcd=168)
                                  AND (c.siteclcd=1 Or c.siteclcd=2 or c.siteclcd=3 or
                                  c.siteclcd=4 or c.siteclcd=5 or c.siteclcd=6) 
                                  GROUP BY peg.eval_grp, peg.eval_grp_descr, c.STATECD, County, ps.cn
                                  ORDER BY peg.eval_grp DESC, County DESC, ps.cn DESC, PlotAcres DESC")


trees_fortyp <- dbGetQuery(fia, "SELECT peg.eval_grp,
                                Sum([EXPVOL]*[TPA_UNADJ]*
                    CASE WHEN t.dia IS NULL
                    THEN ps.adj_expvol_subp
                    WHEN t.dia < 5
                    THEN ps.adj_expvol_micr
                    WHEN ps.macro_breakpoint_dia IS NULL
                    THEN ps.adj_expvol_subp
                    WHEN t.dia < ps.macro_breakpoint_dia
                    THEN ps.adj_expvol_subp
                    ELSE ps.adj_expvol_macr
                    END) trees,
                    peg.statecd,
                    ps.countycd,
                    c.fortypcd,
                    c.owngrpcd
                    FROM pop_eval_grp peg,
                    plotsnap ps,
                    cond c,
                    tree t
                    WHERE ps.cn = c.plt_cn
                    AND c.plt_cn = t.plt_cn
                    AND c.condid = t.condid
                    AND peg.cn = ps.eval_grp_cn
                    AND c.cond_status_cd = 1
                    AND t.treeclcd = 2
                    AND t.statuscd = 1
                    AND t.dia >= 1
                    AND (c.siteclcd=1 Or c.siteclcd=2 or c.siteclcd=3 or
                    c.siteclcd=4 or c.siteclcd=5 or c.siteclcd=6) 
                    AND c.reservcd=0
                    GROUP BY peg.eval_grp,
                    peg.statecd,
                    ps.countycd,
                    c.fortypcd,
                    c.owngrpcd")

acres_fortyp <- dbGetQuery(fia, "SELECT ps.EVAL_GRP,
                                    Sum([EXPCURR]*[CONDPROP_UNADJ]*[ADJ_EXPCURR]) 
                                     AS [total_timberland_acres], 
                                     peg.STATECD, 
                                     ps.COUNTYCD, 
                                     c.OWNGRPCD,
                                     c.FORTYPCD
                                     FROM pop_eval_grp peg,
                                     plotsnap ps,
                                     cond c
                                     WHERE ps.cn = c.plt_cn
                                     AND peg.cn = ps.eval_grp_cn
                                     AND c.cond_status_cd = 1
                                     AND (c.siteclcd=1 Or c.siteclcd=2 or c.siteclcd=3 or
                                     c.siteclcd=4 or c.siteclcd=5 or c.siteclcd=6) 
                                     AND c.reservcd=0
                                     GROUP BY ps.eval_grp,
                                     peg.STATECD, 
                                     ps.COUNTYCD, 
                                     c.OWNGRPCD,
                                     c.FORTYPCD")

volume_fortyp <- dbGetQuery(fia, "SELECT peg.eval_grp, Sum(ps.expvol*t.volcfnet*t.tpa_unadj*
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
                     ps.countycd,
                     c.fortypcd,
                     c.owngrpcd
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
                     AND (c.siteclcd=1 Or c.siteclcd=2 or c.siteclcd=3 or
                     c.siteclcd=4 or c.siteclcd=5 or c.siteclcd=6)
                     GROUP BY peg.eval_grp,
                     peg.statecd,
                     ps.countycd,
                     c.fortypcd,
                     c.owngrpcd")

dbDisconnect(fia)

# join acres, trees, and volume-------------

d <- volume_fortyp %>%
  inner_join(acres_fortyp, 
             by = c('EVAL_GRP', 'STATECD', 'COUNTYCD', 'OWNGRPCD', 'FORTYPCD'))
