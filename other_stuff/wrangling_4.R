setwd("C:\\Users\\spfeiffe\\OneDrive - Environmental Protection Agency (EPA)\\_mine\\Shiny_apps\\_not_active_at_the_moment\\demo_(Latin)\\other_stuff") 
r <- as.character(read.csv("conj_4_deponent.csv", header=FALSE, fileEncoding="UTF-8-BOM")[,1])



Mout <- matrix(nrow=0, ncol=4)
for (X in r)
	{
	Mout <- rbind	(
					Mout,
					c	(
						"4th_deponent",
						substr(X, 2, unlist(gregexpr(",",X))[1]-3),  # drop the -or to form the present stem.
						substr(X, unlist(gregexpr(" :: ", X))+4, nchar(X)),
						"present_stem"
						),
					c	(
						"4th_deponent",
						substr(X, unlist(gregexpr(",", X))[1]+2, unlist(gregexpr(",", X))[2]-2),  # drop the -i to form the perfect stem. 
						substr(X, unlist(gregexpr(" :: ", X))+4, nchar(X)),
						"perfect_stem"
						),
					c	(
						"4th_deponent",
						substr(X, unlist(gregexpr(",", X))[2]+2, unlist(gregexpr(" sum ", X))-3),  # drop the -us to form the PPP. 
						substr(X, unlist(gregexpr(" :: ", X))+4, nchar(X)),
						"PPP"
						)
					)
	}
write.csv(Mout, "Mout.csv")


