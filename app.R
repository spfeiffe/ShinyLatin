ui <- fluidPage(
			   			 textInput(
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
	if (!exists("dict"))
		{
		dict <- as.matrix(read.csv("dict/1st_conj_not_dep_present_stem.tsv", sep="\t", header=FALSE))
		}
	if (!exists("casePoss"))
		{
		casePoss <- as.matrix(read.csv("case_poss/1st_conj_not_dep_present_stem.csv", header=FALSE))
		}
	allEndings <- casePoss[,1] # all possible endings
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
											if (length(r$matchingEndings) == 0)
											  {
											  output$translationGuide <- renderText('This ending of this word does match any inflections which this app knows - make sure it is spelled correctly, is a first conjugation verb, and is in a form using the present stem.')
											  } else {
																if (length(r$matchingEndings) == 1)
																		{
																		r$thisEnding <- r$matchingEndings
																		} else 	{
																						r$ncharOfMatchingEndings <- sapply(r$matchingEndings, nchar)
																						r$longestMatchingEnding <- which(r$ncharOfMatchingEndings == max(r$ncharOfMatchingEndings))
																						r$thisEnding <- allEndings[r$longestMatchingEnding]
																						}
	##################################################################################
	# Get Latin root
	##################################################################################
																r$thisRoot <- substr(
																                     input$userEnteredString,
																                     1,
																										 max(
																                          unlist(
																                                 gregexpr(
																                                          r$thisEnding,
																                                          input$userEnteredString
																                                         )
																                                )-1
																                        )
																                    )
																if (!(r$thisRoot %in% dict[,1]))
																  {
																  output$translationGuide <- renderText("This word does not appear to be in this app's dictionary - make sure it is spelled correctly, is a first conjugation verb, and is in a form using the present stem.")
																  } else  {
	##################################################################################
	# Give translation-output
	##################################################################################
																           r$latinRoot <- paste0(r$thisRoot, "-")
																           r$latinInflection <- paste0("-", r$matchingEndings)
																           r$whatCasePoss <- casePoss[which(casePoss[,1] == r$thisEnding), 2]
																           englishVerbPhrasing <- "(prototype app not yet returning this field)"
																           r$englishRoot <- dict[which(dict[,1] == r$thisRoot), 2]
																           translation <- "(prototype app not yet returning this field)"
																           output$translationGuide <-
																							renderTable(matrix(
		 																														nrow=6,
		 																														ncol=2,
		 																														data=c(
																																				paste0("Input: '", input$userEnteredString, "'"),					"",
																																				paste0("Latin Root: '", r$latinRoot, "'"), 								paste0("Latin Inflection: '", r$latinInflection, "'"),
																																				"", 																											paste0("What Case Possibilities: '", r$whatCasePoss, "'"),
																																				"", 																											paste0("Suggested English verb-phrasing: '", r$englishVerbPhrasing, "'"),
																																				paste0("English Root: '", r$englishRoot, "'"), 						"",
																																				paste0("Suggested Translation: '", r$translation, "'"),		""
			 																																),
																																byrow=TRUE
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
