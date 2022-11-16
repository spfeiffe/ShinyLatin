x <- c('abc', 'axc', 'abc', 'axc', 'abc')
x[grep('xc', x)]
allVerbs <- dictpage[grep(' V ', dictpage)]
length(allVerbs)

allFirstConjVerbs <- allVerbs[grep(' V \\(1st) ', allVerbs)]
length(allFirstConjVerbs) # 3740
firstConjVerbsDeponent <- allFirstConjVerbs[grep(' V \\(1st) DEP', allFirstConjVerbs)]
length(firstConjVerbsDeponent) # 481
firstConjNotDep <- allFirstConjVerbs[-grep(' V \\(1st) DEP', allFirstConjVerbs)]
length(firstConjNotDep)
write.csv(firstConjNotDep, file='firstConjNotDep.csv')
write.csv(allFirstConjVerbs, file='allFirstConjVerbs.csv')
write.csv(firstConjVerbsDeponent, file='firstConjVerbsDeponent.csv')

allSecondConjVerbs <- allVerbs[grep(' V \\(2nd) ', allVerbs)]
length(allSecondConjVerbs) # 606
secondConjVerbsDeponent <- allSecondConjVerbs[grep(' V \\(2nd) DEP', allSecondConjVerbs)]
length(secondConjVerbsDeponent) # 29
secondConjNotDep <- allSecondConjVerbs[-grep(' V \\(2nd) DEP', allSecondConjVerbs)]
length(secondConjNotDep)
write.csv(secondConjNotDep, file='secondConjNotDep.csv')
write.csv(allSecondConjVerbs, file='allSecondConjVerbs.csv')
write.csv(secondConjVerbsDeponent, file='secondConjVerbsDeponent.csv')

allThirdConjVerbs <- allVerbs[grep(' V \\(3rd) ', allVerbs)]
length(allThirdConjVerbs) # 2756
thirdConjVerbsDeponent <- allThirdConjVerbs[grep(' V \\(3rd) DEP', allThirdConjVerbs)]
length(thirdConjVerbsDeponent) # 202
thirdConjNotDep <- allThirdConjVerbs[-grep(' V \\(3rd) DEP', allThirdConjVerbs)]
length(thirdConjNotDep)
write.csv(thirdConjNotDep, file='thirdConjNotDep.csv')
write.csv(allThirdConjVerbs, file='allThirdConjVerbs.csv')
write.csv(thirdConjVerbsDeponent, file='thirdConjVerbsDeponent.csv')

allFourthConjVerbs <- allVerbs[grep(' V \\(4th) ', allVerbs)]
length(allFourthConjVerbs) # 462
fourthConjVerbsDeponent <- allFourthConjVerbs[grep(' V \\(4th) DEP', allFourthConjVerbs)]
length(fourthConjVerbsDeponent) # 65
fourthConjNotDep <- allFourthConjVerbs[-grep(' V \\(4th) DEP', allFourthConjVerbs)]
length(fourthConjNotDep)
write.csv(fourthConjNotDep, file='fourthConjNotDep.csv')
write.csv(allFourthConjVerbs, file='allFourthConjVerbs.csv')
write.csv(fourthConjVerbsDeponent, file='fourthConjVerbsDeponent.csv')

length(allVerbs) # 7812
length(allFirstConjVerbs) + length(allSecondConjVerbs) + length(allThirdConjVerbs) + length(allFourthConjVerbs) # 7564

allVerbsNotInTheFourConj <- allVerbs[-grep(' V \\(', allVerbs)]
length(allVerbsNotInTheFourConj) == (7812 - 7564) # TRUE
verbsNotInTheFourConjDeponent <- allVerbsNotInTheFourConj[grep(' DEP', allVerbsNotInTheFourConj)]
length(verbsNotInTheFourConjDeponent) # 0
write.csv(allVerbsNotInTheFourConj, file='irregularVerbs.csv')

















