# setwd("C:\\Users\\spfeiffe\\OneDrive - Environmental Protection Agency (EPA)\\_mine\\Shiny_apps\\_not_active_at_the_moment\\demo_(Latin)") 
#
#testdict <- c('M', 'b', 'c', 'Mi', 'Mic', 'f')
#allPossibleLatinRoots <- testdict[which(sapply(testdict, function(X){startsWith("Mickey", X)}))]
#thisLatinRoot <- allPossibleLatinRoots[max(sapply(allPossibleLatinRoots, nchar))]
#
if (length(which(sapply(testdict, function(X){startsWith(input$userEnteredString, X)}))) != 0) 
	{
	r$allPossibleLatinRoots <- testdict[which(sapply(testdict, function(X){startsWith(input$userEnteredString, X)}))]
	r$thisLatinRoot <- r$allPossibleLatinRoots[max(sapply(r$allPossibleLatinRoots, nchar))]
	if (length(r$thisLatinRoot) > 1)
		{
		output$translationGuide <- renderText("Something strange this way went... ")
		}
	}
