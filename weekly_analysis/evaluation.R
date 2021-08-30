## evaluation.R --> Also one tab per day of the week.

# setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/markdown/weekly_analysis")

# Load libraries
library(arules)
library(dplyr)

## Monday
# Try to find high confidence rules with one of the least sold products either in the
# antecedent or in the consequent. For Monday, there are not less.sold products in highest
# confidence rules, but there are two items which are not very trendy: Art Tray and My-5 Fruit Shoot.
rulesMondayA <- apriori (transMonday, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="My-5 Fruit Shoot"), control = list (verbose=F)) 
rules_conf_MondayA <- sort (rulesMondayA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_MondayA))

rulesMondayB <- apriori (transMonday, parameter=list (supp = 0.001,conf = 0.01,minlen=2), appearance = list(default="rhs",lhs="Art Tray"), control = list (verbose=F))
rules_conf_MondayB <- sort (rulesMondayB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_MondayB))

## Tuesday

rulesTuesdayA <- apriori (transTuesday, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Extra Salami or Feta"), control = list (verbose=F)) 
rules_conf_TuesdayA <- sort (rulesTuesdayA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_TuesdayA))

## Wednesday

rulesWednesdayA <- apriori (transWednesday, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Christmas common"), control = list (verbose=F)) 
rules_conf_WednesdayA <- sort (rulesWednesdayA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_WednesdayA))

rulesWednesdayB <- apriori (transWednesday, parameter=list (supp = 0.001,conf = 0.01,minlen=2), appearance = list(default="rhs",lhs="Drinking chocolate spoons "), control = list (verbose=F))
rules_conf_WednesdayB <- sort (rulesWednesdayB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_WednesdayB))

## Thursday

rulesThursdayA <- apriori (transThursday, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Hack the stack"), control = list (verbose=F)) 
rules_conf_ThursdayA <- sort (rulesThursdayA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_ThursdayA))

rulesThursdayB <- apriori (transThursday, parameter=list (supp = 0.001,conf = 0.01,minlen=2), appearance = list(default="rhs",lhs="Bare Popcorn"), control = list (verbose=F))
rules_conf_ThursdayB <- sort (rulesThursdayB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_ThursdayB))

## Friday

rulesFridayA <- apriori (transFriday, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Duck egg"), control = list (verbose=F)) 
rules_conf_FridayA <- sort (rulesFridayA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_FridayA))

## Saturday

rulesSaturdayA <- apriori (transSaturday, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Mighty Protein"), control = list (verbose=F)) 
rules_conf_SaturdayA <- sort (rulesSaturdayA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_SaturdayA))

## Sunday

rulesSundayA <- apriori (transSunday, parameter=list (supp = 0.001,conf = 0.01), appearance = list (default="rhs",lhs="Dulce de Leche"), control = list (verbose=F)) 
rules_conf_SundayA <- sort (rulesSundayA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_SundayA))

rulesSundayB <- apriori (transSunday, parameter=list (supp = 0.001,conf = 0.01,minlen=2), appearance = list(default="rhs",lhs="Muesli"), control = list (verbose=F))
rules_conf_SundayB <- sort (rulesSundayB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_SundayB))

rulesSundayC <- apriori (transSunday, parameter=list (supp = 0.001,conf = 0.01,minlen=2), appearance = list(default="rhs",lhs="Eggs"), control = list (verbose=F))
rules_conf_SundayC <- sort (rulesSundayC, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf_SundayC))
