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
						if (length(which(sapply(dict[,2], function(X){startsWith(input$userEnteredString, X)}))) != 0) 
							{
							r$allPossibleLatinRoots <- dict[which(sapply(dict, function(X){startsWith(input$userEnteredString, X)}))]
							r$thisLatinRoot <- r$allPossibleLatinRoots[which(r$allPossibleLatinRoots == r$allPossibleLatinRoots[max(sapply(r$allPossibleLatinRoots, nchar))])] 
							output$translationGuide <- renderText(paste0("r$thisLatinRoot = ", r$thisLatinRoot))
							#if (length(r$thisLatinRoot) == 1)
							#	{
							#	output$translationGuide <- renderText(paste0(" r$thisLatinRoot = ", r$thisLatinRoot))
							#	} else	{
							#			output$translationGuide <- renderText(paste0("Something strange this way went... r$thisLatinRoot = ", r$thisLatinRoot))
							#			}
							} else	{
									output$translationGuide <- renderText("This word does not appear to be in this app's dictionary - make sure it is spelled correctly, is a regular verb, and if in a form derived from the PPP, does include a space or linking verb.") 
									}
						}
			})
	}
	
shinyApp(ui=ui, server=server)


