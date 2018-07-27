library(RSQLite)
fia <- dbConnect(SQLite(), 'D:/GroupWork/FIA Data/FIADB.db')

Harvest.AL <- dbGetQuery(fia, "SELECT ps.cn, peg.eval_grp,ps.statecd, ps.countycd, 
                         t.spgrpcd, ref.common_name, c.fortypcd,
                         t.volcfnet, c.stdage,
                         ref.sftwd_hrdwd, c.siteclcd, c.stdszcd, t.invyr, c.owngrpcd,
                         c.dstrbcd1, c.dstrbyr1, c.trtcd1, c.trtyr1,
                         c.dstrbcd2, c.dstrbyr2, c.trtcd2, c.trtyr2,
                         c.dstrbcd3, c.dstrbyr3, c.trtcd3, c.trtyr3
                         FROM pop_eval_grp peg, plotsnap ps, cond c, tree t, ref_species ref
                         WHERE ps.cn = c.plt_cn
                         AND c.condid = t.condid
                         AND c.plt_cn = t.plt_cn
                         AND peg.cn = ps.eval_grp_cn
                         AND t.spcd = ref.spcd
                         AND t.treeclcd = 2 AND c.cond_status_cd = 1 AND t.statuscd=1
                         AND c.reservcd = 0
                         AND ps.statecd=1
                         AND ps.countycd=1
                         GROUP BY peg.eval_grp, ps.statecd, ps.countycd, 
                         t.spgrpcd, ref.common_name, c.fortypcd,
                         t.volcfnet, c.stdage,
                         ref.sftwd_hrdwd, c.siteclcd, c.stdszcd, t.invyr, c.owngrpcd,
                         c.dstrbcd1, c.dstrbyr1, c.trtcd1, c.trtyr1,
                         c.dstrbcd2, c.dstrbyr2, c.trtcd2, c.trtyr2,
                         c.dstrbcd3, c.dstrbyr3, c.trtcd3, c.trtyr3")

#pull all cns from all tables
#pull all inventory years and treatment years

practice <- dbGetQuery(fia, "SELECT c.plt_cn, t.plt_cn as 'tree plt cn', t.cn as 'tree cn', c.cn, peg.eval_grp, peg.eval_grp_descr, ps.statecd, ps.countycd, 
                       t.spgrpcd, ref.common_name, c.fortypcd,
                       c.stdage, t.invyr, c.trtcd1, c.trtyr1,
                       c.trtcd2, c.trtyr2,c.trtcd3, c.trtyr3
                       FROM pop_eval_grp peg, plotsnap ps, cond c, tree t, ref_species ref
                       WHERE plot.cn=t.plt_cn
                       AND plot.cn=c.plt_cn
                       AND ps.cn = c.plt_cn
                       AND c.condid = t.condid
                       AND c.plt_cn = t.plt_cn
                       AND peg.cn = ps.eval_grp_cn
                       AND t.spcd = ref.spcd
                       AND t.treeclcd = 2 AND c.cond_status_cd = 1 AND t.statuscd=1
                       AND c.reservcd = 0
                       AND ps.statecd=1
                       AND ps.countycd=1
                       GROUP BY peg.eval_grp, ps.statecd, ps.countycd, 
                       t.spgrpcd, ref.common_name, c.fortypcd,
                       t.volcfnet, c.stdage,
                       ref.sftwd_hrdwd, c.siteclcd, c.stdszcd, t.invyr, c.owngrpcd,
                       c.dstrbcd1, c.dstrbyr1, c.trtcd1, c.trtyr1,
                       c.dstrbcd2, c.dstrbyr2, c.trtcd2, c.trtyr2,
                       c.dstrbcd3, c.dstrbyr3, c.trtcd3, c.trtyr3")

practice2 <- dbGetQuery(fia, "SELECT p.cn, c.plt_cn, t.plt_cn as 'tree plt cn', t.cn as 'tree cn', c.cn, peg.eval_grp, 
                        peg.eval_grp_descr, ps.statecd, ps.countycd, t.spgrpcd, ref.common_name, c.trtcd1, c.trtyr1,
                        c.trtcd2, c.trtyr2,c.trtcd3, c.trtyr3
                        FROM plot p, pop_eval_grp peg, plotsnap ps, cond c, tree t, ref_species ref
                        WHERE p.cn=t.plt_cn
                        AND p.cn=c.plt_cn 
                        AND ps.cn = c.plt_cn
                        AND c.condid = t.condid
                        AND c.plt_cn = t.plt_cn
                        AND peg.cn = ps.eval_grp_cn
                        AND t.spcd = ref.spcd
                        AND t.treeclcd = 2 AND c.cond_status_cd = 1 AND t.statuscd=1
                        AND c.reservcd = 0
                        AND ps.statecd=1
                        AND ps.countycd=1")

practice3 <- dbGetQuery(fia, "SELECT p.cn, p.plot, peg.eval_grp, peg.eval_grp_descr, ps.statecd, ps.countycd, 
                        t.spgrpcd, ref.common_name, c.trtcd1, c.trtyr1, c.trtcd2, c.trtyr2,c.trtcd3, c.trtyr3
                        FROM plot p, plotsnap ps, cond c, tree t, ref_species ref, pop_eval_grp peg
                        WHERE p.cn=t.plt_cn
                        AND p.cn=c.plt_cn 
                        AND ps.cn = c.plt_cn
                        AND c.condid = t.condid
                        AND c.plt_cn = t.plt_cn
                        AND t.spcd = ref.spcd
                        AND t.treeclcd = 2 AND c.cond_status_cd = 1 AND t.statuscd=1
                        AND c.reservcd = 0
                        AND ps.statecd=1
                        AND ps.countycd=1")

singleton <- dbGetQuery(fia, "SELECT * 
                        FROM pop_eval_grp peg, plotsnap ps, cond c, tree t, ref_species ref
                        WHERE ps.cn = c.plt_cn
                        AND c.condid = t.condid
                        AND c.plt_cn = t.plt_cn
                        AND peg.cn = ps.eval_grp_cn
                        AND t.spcd = ref.spcd
                        AND t.treeclcd = 2 AND c.cond_status_cd = 1 AND t.statuscd=1
                        AND c.reservcd = 0
                        AND ps.statecd=1
                        AND ps.countycd=1") 

pop_eval <- dbGetQuery(fia, "SELECT * 
                       FROM pop_eval
                       WHERE statecd=1") 
plot_table <- dbGetQuery(fia, "SELECT *
                         FROM plot
                         WHERE statecd=1")

                        FROM pop_eval_grp peg, plotsnap ps, tree t, cond c
                        WHERE ps.cn = c.plt_cn
                        AND c.condid = t.condid
                        AND c.plt_cn = t.plt_cn
                        AND peg.cn = ps.eval_grp_cn
                        AND ps.statecd=1
                        AND ps.countycd=1
                        GROUP BY peg.eval_grp, peg.eval_grp_descr, t.cn, ps.cn")

# plotsnap_cn is the same as tree_cn (as it should be), so I drop it from the next query
# now I want to check how the eval group year compares to the inventory year

plots.al2 <- dbGetQuery(fia, 
                        "SELECT t.cn AS tree_cn, peg.eval_grp, 
                        peg.eval_grp_descr, t.invyr
                        FROM pop_eval_grp peg, plotsnap ps, tree t, cond c
                        WHERE ps.cn = c.plt_cn
                        AND c.condid = t.condid
                        AND c.plt_cn = t.plt_cn
                        AND peg.cn = ps.eval_grp_cn
                        AND ps.statecd=1
                        AND ps.countycd=1
                        GROUP BY peg.eval_grp, peg.eval_grp_descr, 
                        t.cn, t.invyr")

# the inventory do not match up with the eval group year
# try to find a pattern
# now I am going to look at the volume variable to see how it varies over eval groups and years

plots.al3 <- dbGetQuery(fia, 
                        "SELECT t.cn AS tree_cn, peg.eval_grp, 
                        peg.eval_grp_descr, t.invyr, t.volcfnet
                        FROM pop_eval_grp peg, plotsnap ps, tree t, cond c
                        WHERE ps.cn = c.plt_cn
                        AND c.condid = t.condid
                        AND c.plt_cn = t.plt_cn
                        AND peg.cn = ps.eval_grp_cn
                        AND ps.statecd=1
                        AND ps.countycd=1
                        GROUP BY peg.eval_grp, peg.eval_grp_descr, 
                        t.cn")

# continue to build up the code in different ways to see if you can find what you are looking for.


library(tidyverse)
subset_removal_only <- filter(Harvest.AL, TRTCD1==10)
subset_removal_replant <- filter(practice3, TRTCD1==10 & TRTCD2==30)
