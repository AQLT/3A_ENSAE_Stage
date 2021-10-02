tab <- lapply(readRDS("data/comparaison_fst/fst_h6.RDS"), `[[`, "final_weights")
tab <- tab[c("lc", "lc_min", "lc_med", "rkhs_timeliness")]
x <- tab[[1]]
tab_format <- do.call(rbind, lapply(tab, function(x){
    t(x[,c("smoothness.weight", "timeliness.weight", "fidelity.weight")])
}))
colnames(tab_format) <- sprintf("q=%i",0:5)
rownames(tab_format) <- gsub(".weight", "", tab_format, fixed = TRUE)

dput(colnames(x))
reshape2::dcast(x,"q~.")

dput(names(tab))
format.args = list(decimal.mark = ",",
                   nsmall = 3)
kbl(tab_format, caption = "Group Rows", booktabs = T,
    format.args = format.args)%>%
    kable_styling()%>%
    pack_rows(index=c("LC min. timeliness" = 3,
                      "LC max. timeliness" = 3,
                      "LC timeliness médiane" =3,
                      "RKHS min. timeliness" = 3))

library(kableExtra)
?pack_rows
