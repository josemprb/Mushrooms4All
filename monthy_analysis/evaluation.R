## evaluation.R --> Also one tab per day of the week.

# setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/markdown/weekly_analysis")

# Load libraries
library(arules)
library(dplyr)

## October
# Try to find high confidence rules with one of the least sold products either in the
# antecedent or in the consequent. 
rulesOctA <- apriori (transOct, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Pick and Mix Bowls"), control = list (verbose=F)) 
rules_conf_OctA <- sort (rulesOctA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_OctA))

## November

rulesNovA <- apriori (transNov, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Pick and Mix Bowls"), control = list (verbose=F)) 
rules_conf_NovA <- sort (rulesNovA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_NovA))

rulesNovB <- apriori (transNov, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Bread Pudding"), control = list (verbose=F)) 
rules_conf_NovB <- sort (rulesNovB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_NovB))

## December

rulesDecA <- apriori (transDec, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Bare Popcorn"), control = list (verbose=F)) 
rules_conf_DecA <- sort (rulesDecA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_DecA))

rulesDecB <- apriori (transDec, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Dulce de Leche"), control = list (verbose=F)) 
rules_conf_DecB <- sort (rulesDecB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_DecB))

## January

rulesJanA <- apriori (transJan, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Muesli"), control = list (verbose=F)) 
rules_conf_JanA <- sort (rulesJanA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_JanA))

rulesJanB <- apriori (transJan, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Crisps"), control = list (verbose=F)) 
rules_conf_JanB <- sort (rulesJanB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_JanB))

## February

rulesFebA <- apriori (transFeb, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Duck egg"), control = list (verbose=F)) 
rules_conf_FebA <- sort (rulesFebA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_FebA))

## March

rulesMarA <- apriori (transMar, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Cherry me Dried fruit"), control = list (verbose=F)) 
rules_conf_MarA <- sort (rulesMarA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_MarA))

rulesMarB <- apriori (transMar, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Half slice Monster "), control = list (verbose=F)) 
rules_conf_MarB <- sort (rulesMarB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_MarB))

## April

rulesAprA <- apriori (transApr, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Cherry me Dried fruit"), control = list (verbose=F)) 
rules_conf_AprA <- sort (rulesAprA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_AprA))

rulesAprB <- apriori (transApr, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Half slice Monster "), control = list (verbose=F)) 
rules_conf_AprB <- sort (rulesAprB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_AprB))

rulesAprC <- apriori (transApr, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Brioche and salami"), control = list (verbose=F)) 
rules_conf_AprC <- sort (rulesAprC, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_AprC))

