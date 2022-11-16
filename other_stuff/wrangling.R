
second_all <- dictpage[grep(" V \\(2nd) ", dictpage)]
secondDep <- second_all[grep(" V \\(2nd) DEP", second_all)]
SCND <- second_all[-which(second_all %in% secondDep)]
out <- matrix(nrow=length(SCND), ncol=2, data=NA)
for (i in 1:length(SCND))
	{
	X <- SCND[i]
	out[i,] <- c(
		substr(	X, 
				2 , 
				unlist(gregexpr("V \\(2nd)", X))-3
		      ),
		substr(	X, 
				unlist(gregexpr(":", X))[2]+2, 
				nchar(X)
		      )
		      )
	}
write.table(out, file="2nd_conj_not_dep.tsv", quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)