LOAD <- function(X){
assign(
		substr(X, 1, (nchar(X)-4)),
		as.character(read.csv(X, header=FALSE, fileEncoding="UTF-8-BOM")[,1]),
		envir = rlang::env_parent()
)
}

LOAD("conj_1_all.csv")
LOAD("conj_1_not_dep.csv")
LOAD("conj_1_deponent.csv")
LOAD("conj_2_all.csv")
LOAD("conj_2_not_dep.csv")
LOAD("conj_2_deponent.csv")
LOAD("conj_3_all.csv")
LOAD("conj_3_not_dep.csv")
LOAD("conj_3_deponent.csv")
LOAD("conj_4_all.csv")
LOAD("conj_4_not_dep.csv")
LOAD("conj_4_deponent.csv")
LOAD("irregular.csv")

sapply(conj_2_not_dep, function(X){substr(X, 2, )})
write.table(out, file="present_stem_2nd_conj_not_dep.tsv", quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)