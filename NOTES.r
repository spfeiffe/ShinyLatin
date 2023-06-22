testdict <- matrix(nrow=3, ncol=3, data=c(
"3rd",	"auctumnesc",	"autumn is approaching, autumn is coming on;",
"2nd",	"aud",		"intend, be prepared; dare/have courage (to go/do), act boldly, venture, risk;",
"4th",	"aud",		"hear, listen, accept, agree with; obey; harken, pay attention; be able to hear;"), byrow=TRUE)







			hits <- dict[which(dict[,2] == r$thisRoot), ]
			if (length(unique(hits[,1])) == 1)
				{
				# same conjugation
				r$thisConjugation <- unique(hits[,1])
				} else	{
						# different conjugation
						r$thisConjugation <- "MoreThanOneConjugation"
						if (length(which(casePoss[,1] == r$thisEnding)) > 1)
							{
							# use r$thisEnding to tell them apart
							casePossFor1stConj <- casePoss[which(sapply(casePoss[,2], function(X){substr(X,1,3) == "1st"})), ] 
							casePossFor2ndConj <- casePoss[which(sapply(casePoss[,2], function(X){substr(X,1,3) == "2nd"})), ] 
							casePossFor3rdConj <- casePoss[which(sapply(casePoss[,2], function(X){substr(X,1,3) == "3rd"})), ] 
							casePossFor4thConj <- casePoss[which(sapply(casePoss[,2], function(X){substr(X,1,3) == "4th"})), ] 
							
							} else	{
									# the meaning is so close together they are basically the same word.
									}
						}
				
				
				
				
				
				



for (i in 1:nrow(dict))
	{
	x <- dict[i,2]
	if (length(which(dict[,2] == x)) > 1)
		{
		hits <- dict[which(dict[,2] == x), ]
		if (length(unique(hits[,1])) == 1)
			{
			# same conjugation
			} else	{
					print(as.character(x))
					}
		}
	}


abalienat
audeo
audio
laudas
laudes
mittunt