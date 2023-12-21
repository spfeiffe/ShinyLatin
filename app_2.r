# setwd("C:\\Users\\spfeiffe\\OneDrive - Environmental Protection Agency (EPA)\\_mine\\Shiny_apps\\_not_active_at_the_moment\\demo_(Latin)") 
#
#testdict <- c('M', 'b', 'c', 'Mi', 'Mic', 'f')
#allPossibleLatinRoots <- testdict[which(sapply(testdict, function(X){startsWith("Mickey", X)}))]
#thisLatinRoot <- allPossibleLatinRoots[max(sapply(allPossibleLatinRoots, nchar))]
#
ui <- fluidPage	(
				textInput	(
							"userEnteredString",
							"Enter exact Latin verb(-form) here:",
							value=""
							),
				uiOutput("translationGuide")
				)

server <- function(input, output)
	{
	##################################################################################
	# Load constants
	##################################################################################
	output$translationGuide <- renderText("Loading constants...")
	require(assertthat)
	require(stringi)
	if (!exists("dict"))
		{
		dict <- as.matrix(read.table("dict/all_regular.tsv", header=FALSE, fileEncoding="UTF-8-BOM", sep='\t', quote='"')[,1:4]) # conj, root, meaning, present_stem/perfect_stem/PPP 
		}
	if (!exists("casePoss"))
		{
		casePoss <- as.matrix(read.csv("case_poss/all_conj_not_dep.csv", header=FALSE))
		}
	allEndings <- unique(casePoss[,1]) # all possible endings
	##################################################################################
	# Begin reactive stuff
	##################################################################################
	r <- reactiveValues()
	observe	({
			if (nchar(trimws(input$userEnteredString)) == 0)
				{
				output$translationGuide <- renderText('Waiting.....')
				} else	{
						#################################################################################################
						#  Get the Latin root and ending #
						#################################################################################################
						# the next line takes a couple of seconds to execute 
						if (length(which(sapply(dict[,2], function(X){startsWith(input$userEnteredString, X)}))) != 0) 
							{
							r$allPossibleLatinRoots <- unique(dict[which(sapply(dict[,2], function(X){startsWith(input$userEnteredString, X)})), 2]) 
							if (length(which(sapply(r$allPossibleLatinRoots,nchar) == nchar(input$userEnteredString))) > 0)
								{
								r$APLRWANTWI <- # All Possible Latin Roots Which Are Not The Word Itself 
								  r$allPossibleLatinRoots[-which(sapply(r$allPossibleLatinRoots,nchar) == nchar(input$userEnteredString))]
								} else	{
										r$APLRWANTWI <- r$allPossibleLatinRoots
										}
							r$thisLatinRoot <- r$APLRWANTWI[which(sapply(r$APLRWANTWI,nchar) == max(sapply(r$APLRWANTWI,nchar)))]
							r$thisLatinEnding <- substr(input$userEnteredString, nchar(r$thisLatinRoot)+1, nchar(input$userEnteredString))
							r$dictHits <- dict[which(dict[,2] == r$thisLatinRoot), ]
							if ("matrix" %in% class(r$dictHits)) # 2 or more rows returned
								{
								r$conjAndPP <- character(0)
								for (i in 1:nrow(r$dictHits))
									{
									r$conjAndPP <- c	(
														r$conjAndPP,
														stringi::stri_flatten(c(r$dictHits[i,1], r$dictHits[i,4]), collapse=" ") 
														)
									}
								} else	{ # 1 row returned
										r$conjAndPP <- stringi::stri_flatten(c(r$dictHits[1], r$dictHits[4]), collapse=" ")
										}
							r$outMat <- matrix(nrow=2, ncol=1, data=c(paste0(r$thisLatinRoot, "-"), paste0("-", r$thisLatinEnding))) 
							for (i in 1:length(r$conjAndPP))
								{
								r$outMat <- rbind(r$outMat, r$conjAndPP[i])
								}
							# print(r$outMat)
							# print("#")
							# print("#")
							# print("#")
							output$translationGuide <- renderTable	(
																	matrix(nrow=3,ncol=3,data=letters[1:9]),
																	#r$outMat,
																	#rownames=TRUE,
																	colnames=FALSE
																	)
							print("Rendered.")
							} else	{
									output$translationGuide <- renderText("This word does not appear to be in this app's dictionary - make sure it is spelled correctly, is a regular verb, and if in a form derived from the PPP, does not include a space or linking verb.") 
									}
						}
			})
	}
	
shinyApp(ui=ui, server=server)


