#author credit to: Shivan GC, Michigan State University
#install packages
install.packages("RSQLite")
# load packages
library(RSQLite)
library(tidyverse)
library(DBI)

# set working directory
setwd("C:/Users/shivangc/Box/FIA Data/SQLite_FIADB_ENTIRE")

# connect to FIA database and query for trees, acres, and volume -----
# by forest type and county

fiadb <- dbConnect(SQLite(), 'SQLite_FIADB_ENTIRE.db')

AL_Mortality_GS <- dbGetQuery(fiadb, "SELECT stfips, evalcd, owngrp, spgrpcd,spcd, dstb1,dstb2,dstb3,dia_class,agentcd,unitcd,
sum(ESTIMATED_VALUE* EXPNS)ESTIMATE
FROM (SELECT pop_stratum.estn_unit_cn,
 pop_stratum.cn STRATACN,
  plot.cn plot_cn,
   plot.prev_plt_cn,
    cond.cn cond_cn,
     plot.lat,
      plot.lon,
       pop_stratum.expns EXPNS,
       plot.statecd stfips,
       pop_stratum.evalid evalcd,
       cond.owngrpcd owngrp,
       tree.spgrpcd spgrpcd,
       tree.spcd spcd,
       tree.agentcd agentcd,
       cond.dstrbcd1 dstb1,
       cond.dstrbcd2 dstb2,
       cond.dstrbcd3 dstb3,
       cond.unitcd unitcd,
   
       CASE 
         WHEN TREE.DIA <6 THEN '<6 inches'
         WHEN TREE.DIA >= 6 AND TREE.DIA < 8 THEN '6-8 inches'
         WHEN TREE.DIA >= 8 AND TREE.DIA < 11 THEN '8-11 inches'
         WHEN TREE.DIA >= 11 AND TREE.DIA < 20 THEN '11-20 inches'
         ELSE '>=20 inches'
         END AS DIA_CLASS,
       SUM(GRM.TPAMORT_UNADJ * (CASE 
            WHEN COALESCE(GRM.SUBPTYP_GRM, 0) = 0 THEN (0) 
            WHEN GRM.SUBPTYP_GRM = 1 THEN POP_STRATUM.ADJ_FACTOR_SUBP 
            WHEN GRM.SUBPTYP_GRM = 2 THEN POP_STRATUM.ADJ_FACTOR_MICR 
            WHEN GRM.SUBPTYP_GRM = 3 THEN POP_STRATUM.ADJ_FACTOR_MACR 
            ELSE (0) END) * (CASE WHEN GRM.COMPONENT LIKE 'MORTALITY%' THEN TRE_MIDPT.VOLCFNET ELSE (0) END)) AS ESTIMATED_VALUE 
FROM POP_STRATUM POP_STRATUM 
    JOIN POP_PLOT_STRATUM_ASSGN POP_PLOT_STRATUM_ASSGN 
        ON (POP_STRATUM.CN = POP_PLOT_STRATUM_ASSGN.STRATUM_CN) 
    JOIN PLOT PLOT 
        ON (POP_PLOT_STRATUM_ASSGN.PLT_CN = PLOT.CN) 
    JOIN PLOTGEOM PLOTGEOM 
        ON (PLOT.CN = PLOTGEOM.CN) 
    JOIN COND COND 
        ON (PLOT.CN = COND.PLT_CN) 
    JOIN (SELECT P.PREV_PLT_CN, T.* 
        FROM PLOT P 
            JOIN TREE T 
                ON (P.CN = T.PLT_CN)) TREE 
        ON ((TREE.CONDID = COND.CONDID) AND (TREE.PLT_CN = COND.PLT_CN)) 
    LEFT OUTER JOIN PLOT PPLOT 
        ON (PLOT.PREV_PLT_CN = PPLOT.CN) 
    LEFT OUTER JOIN COND PCOND 
        ON ((TREE.PREVCOND = PCOND.CONDID) AND (TREE.PREV_PLT_CN = PCOND.PLT_CN)) 
    LEFT OUTER JOIN TREE PTREE 
        ON (TREE.PREV_TRE_CN = PTREE.CN) 
    LEFT OUTER JOIN TREE_GRM_BEGIN TRE_BEGIN 
        ON (TREE.CN = TRE_BEGIN.TRE_CN) 
    LEFT OUTER JOIN TREE_GRM_MIDPT TRE_MIDPT 
        ON (TREE.CN = TRE_MIDPT.TRE_CN) 
    LEFT OUTER JOIN (SELECT TRE_CN, DIA_BEGIN, DIA_MIDPT, DIA_END,
                             SUBP_COMPONENT_GS_FOREST AS COMPONENT,
                             SUBP_SUBPTYP_GRM_GS_FOREST AS SUBPTYP_GRM,
                             SUBP_TPAMORT_UNADJ_GS_FOREST AS TPAMORT_UNADJ 
                                FROM TREE_GRM_COMPONENT) GRM 
                                    ON (TREE.CN = GRM.TRE_CN) 
                                JOIN REF_SPECIES 
                                    ON (TREE.SPCD = REF_SPECIES.SPCD) 
WHERE 1=1 
    AND REF_SPECIES.WOODLAND = 'N' 
    AND ((pop_stratum.rscd=33 and pop_stratum.evalid=12303)) 
   
GROUP BY pop_stratum.estn_unit_cn,
            pop_stratum.cn,
            plot.cn,
            plot.statecd,
            plot.prev_plt_cn,           
            cond.cn,
            plot.lat,
            plot.lon,
            pop_stratum.expns,
            pop_stratum.evalid,
            cond.owngrpcd,
            tree.spgrpcd,
            tree.spcd,
            tree.agentcd,
            cond.dstrbcd1,
            cond.dstrbcd2,
            cond.dstrbcd3,
            cond.unitcd,
            CASE
WHEN TREE.DIA <6 THEN 'Less than 6 inches'
WHEN TREE.DIA >= 6 AND TREE.DIA < 8 THEN'6 to 8 inches'
WHEN TREE.DIA >= 8 AND TREE.DIA < 11 THEN'8 to 11 inches'
WHEN TREE.DIA >= 11 AND TREE.DIA < 20 THEN '11 to 20 inches'
ELSE 'More than 20 inches'
END)
GROUP BY stfips, evalcd, owngrp, spgrpcd,spcd, agentcd,dstb1,dstb2,dstb3, unitcd, dia_class
ORDER BY stfips, evalcd, owngrp
")

write.csv(AL_Mortality_GS, "C:/Users/shivangc/Box/FIA Data/AL_Mortality_GS_Nov4.csv", row.names = F)