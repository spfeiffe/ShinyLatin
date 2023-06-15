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
	require(stringi)
	if (!exists("dict"))
		{
		dict <- as.matrix(read.table("dict/all_regular_non_dep_present_stem.tsv", header=FALSE, fileEncoding="UTF-8-BOM", sep='\t', quote='"')[,1:3]) # conj, root, meaning 
		}
	if (!exists("casePoss"))
		{
		casePoss <- as.matrix(read.csv("case_poss/all_conj_not_dep_present_stem.csv", header=FALSE))
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
											  output$translationGuide <- renderText('This ending of this word does match any inflections which this app knows - make sure it is spelled correctly, is a first conjugation verb, and is in a form using the present stem.')
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
																  output$translationGuide <- renderText("This word does not appear to be in this app's dictionary - make sure it is spelled correctly, is a first conjugation verb, and is in a form using the present stem.")
																  } else  {
	##################################################################################
	# Give translation-output
	##################################################################################
																           r$latinRoot 			<- paste0(r$thisRoot, "-")
																		   
																		   r$latinInflection <- paste0("-", r$thisEnding)
																		   
																		   r$latinConjugationRAW <- dict[which(dict[,2] == r$thisRoot), 1]
																		   if (length(unique(r$latinConjugationRAW)) > 1)
																				{
																				# try to tell the difference based on conjugation including r$thisEnding (e.g. audeo vs audio) 
																				r$latinConjugation 	<- stringi::stri_flatten(r$latinConjugationRAW , collapse=" ---OR--- ")
																				} else	{
																						r$latinConjugation <- unique(r$latinConjugationRAW)
																						}
																		   
																		   r$case_possibilities <- casePoss[which(casePoss[,1] == r$thisEnding), 2]
																		
																		   r$whatCase <- stringi::stri_flatten(r$case_possibilities	[
																																	which	(
																																			sapply	(
																																					r$case_possibilities,
																																					function(X)
																																						{
																																						substr(X, 1, 3)
																																						}
																																					) == r$latinConjugation
																																			)
																																	]
																												, collapse=" ---OR--- ")
																		   
																		   r$EnglishRootRAW <- dict[which(dict[,2] == r$thisRoot), 3]
																		   if (length(r$EnglishRootRAW) > 1)
																				{
																				r$EnglishRoot <- stringi::stri_flatten(r$EnglishRootRAW, collapse=" ---OR--- ")
																				} else	{
																						r$EnglishRoot <- r$EnglishRootRAW
																						}
																			
																			output$translationGuide <- renderTable	(matrix	(
																															nrow=6,
																															ncol=1,
																															data=c	(
																																	paste0("Input: '", input$userEnteredString, "'"),
																																	paste0("Latin Root: '", r$latinRoot, "'"),
																																	paste0("Latin Inflection: '", r$latinInflection, "'"),
																																	paste0("Latin Conjugation: '", r$latinConjugation, "'"),
																																	paste0("What Case: '", r$whatCase, "'"),
																																	paste0("English Root: '", r$EnglishRoot, "'")#,
																																	#paste0("Suggested English Phrasing: (prototype app not yet returning this field)")
																																	),
																															byrow=FALSE
																															)
																													)










                                        }
															}
												}
	##################################################################################
	# End reactive stuff
	##################################################################################
	       })
}

shinyApp(ui=ui, server=server)
