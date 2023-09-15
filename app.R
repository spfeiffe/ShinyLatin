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
	require(assertthat)
	require(stringi)
	if (!exists("dict"))
		{
		dict <- as.matrix(read.table("dict/all_regular_non_dep.tsv", header=FALSE, fileEncoding="UTF-8-BOM", sep='\t', quote='"')[,1:4]) # conj, root, meaning, present_stem/perfect_stem 
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
	observe({
	##################################################################################
	# Get ending and thus case possibilities
	##################################################################################
						if (nchar(trimws(input$userEnteredString)) == 0)
							{
							output$translationGuide <- renderText('Waiting.....')
							} else 	{
											# output$translationGuide <- renderText('Oh, boy! ;)')
											r$matchingEndings <- allEndings[
											                              	which(
											                                    	sapply(
											                                          	allEndings,
											                                          	function(X)
											                                            	{
											                                            	endsWith(input$userEnteredString, X)
											                                            	}
											                                          	)
											                                   	 )
											                               ]
											#print(paste0("r$matchingEndings = ", r$matchingEndings))
											if (length(r$matchingEndings) == 0)
											  {
											  output$translationGuide <- renderText('This ending of this word does match any inflections which this app knows - make sure it is spelled correctly, is a regular verb, and is in a form using the present stem.')
											  } else {
																if (length(r$matchingEndings) == 1)
																		{
																		r$thisEnding <- r$matchingEndings
																		} else 	{
																				r$ncharOfMatchingEndings <- sapply(r$matchingEndings, nchar)
																				#print(paste0("r$ncharOfMatchingEndings = ", r$ncharOfMatchingEndings))
																				r$longestMatchingEnding <- r$matchingEndings[which(r$ncharOfMatchingEndings == max(r$ncharOfMatchingEndings))]
																				#print(paste0("r$longestMatchingEnding = ", r$longestMatchingEnding))
																				r$thisEnding <- r$longestMatchingEnding
																				}
																#print(paste0("r$thisEnding = ", r$thisEnding))
	##################################################################################
	# Get Latin root
	##################################################################################
																r$thisRoot <- substr	(
																						input$userEnteredString,
																						1,
																						max	(
																							unlist	(
																									gregexpr(
																											r$thisEnding,
																											input$userEnteredString
																											)
																									)-1
																							)
																						)
																#print(paste0("r$thisRoot = ", r$thisRoot))
																if (!(r$thisRoot %in% dict[,2]))
																  {
																  output$translationGuide <- renderText("This word does not appear to be in this app's dictionary - make sure it is spelled correctly, is a regular verb, and is in a form using the present stem.")
																  } else  {
	##################################################################################
	# Give translation-output
	##################################################################################
																		
																		   prepend <- function(a, b)
																				{
																				assert_that(class(a) == "character")
																				assert_that(class(b) == "character")
																				assert_that(length(a) == length(b))
																				theOutputRAW <- character(0)
																				for (i in 1:length(a))
																					{
																					theOutputRAW <- c(theOutputRAW, paste0(a[i], b[i]))
																					}
																				theOutput <- na.omit(theOutputRAW)
																				if (length(theOutput) != length(a))
																					{
																					stop("something went wrong with the prepend() function")
																					}
																				return(theOutput)
																				}
																			# prepend(letters[1:3], as.character(1:3))
																				
																			r$englishRoot <- stri_flatten	(
																											prepend	(
																													paste0(dict[which(dict[,2] == r$thisRoot), 1], "Conj: "),
																													dict[which(dict[,2] == r$thisRoot), 3]
																													),
																											collapse=" ---OR--- "
																											)
																		
																		
																		if (length(which(dict[,2] == r$thisRoot)) == 1)
																			{
																			r$thisConjugation <- dict[which(dict[,2] == r$thisRoot), 1]
																			r$casePossForThisConj <- casePoss[which(sapply(casePoss[,2], function(X){substr(X,1,3) == r$thisConjugation})), ]
																			r$case_possibilities <- r$casePossForThisConj[which(r$casePossForThisConj[,1] == r$thisEnding), 2]
																			r$englishRoot_2 <- r$englishRoot
																			} else	{
																					hits <- dict[which(dict[,2] == r$thisRoot), ]
																					if (length(unique(hits[,1])) == 1)
																						{
																						r$thisConjugation <- unique(hits[,1])
																						r$casePossForThisConj <- casePoss[which(sapply(casePoss[,2], function(X){substr(X,1,3) == r$thisConjugation})), ]
																						r$case_possibilities <- r$casePossForThisConj[which(r$casePossForThisConj[,1] == r$thisEnding), 2]
																						r$englishRoot_2 <- r$englishRoot
																						} else	{
																								r$thisConjugation <- "MoreThanOneConjugation"
																								xx <- sapply(unlist(strsplit(r$englishRoot, split=" ---OR--- ")), function(X){substr(X,1,3)})
																								r$case_possibilities <- casePoss[which(casePoss[,1]==r$thisEnding),2][which(sapply(casePoss[which(casePoss[,1]==r$thisEnding),2], function(X){substr(X,1,3)}) %in% xx)]
																								r$englishRoot_2 <- unlist(strsplit(r$englishRoot, split=" ---OR--- "))[which(xx %in% sapply(r$case_possibilities,function(X){substr(X,1,3)}))] 
																								}
																					}
																		
																		if (length(r$case_possibilities) == 0)
																			{
																			output$translationGuide <- renderText(paste0("The Latin root `", r$thisRoot, "` is in the dictionary, but does not have the ending `", r$thisEnding, "`"))  
																			} else	{
																					dataOut <- c	(
																									paste0("Input: '", input$userEnteredString, "'"),
																									paste0("Latin Root: '", r$thisRoot, "-'"),
																									paste0("English meaning: '", substr(r$englishRoot_2, 10, nchar(r$englishRoot_2)), "'"),
																									paste0("Latin Inflection: '-", r$thisEnding, "'"),
																									"Case Possibilities: "
																									)
																									
																					for (i in 1:length(r$case_possibilities))
																						{
																						dataOut <- c(dataOut, rep("", 4), r$case_possibilities[i])
																						}
																						
																					output$translationGuide <- renderTable	(matrix	(
																																	nrow = 5,
																																	ncol = length(r$case_possibilities)+1,
																																	data = dataOut,
																																	byrow = FALSE
																																	)
																															)
																					}
																			}
													}
									}
	##################################################################################
	# End reactive stuff
	##################################################################################
	       })
}

shinyApp(ui=ui, server=server)
