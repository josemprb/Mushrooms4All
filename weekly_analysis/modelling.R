## modelling.R --> Create one tab per weekday.

# setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/markdown/weekly_analysis")

# Load libraries
library(arules)
library(dplyr)

## Monday
# Read the csv file, there is one data set per day of the week.
transMonday <- read.transactions("data/week/monday.csv", format = "single", 
                                 cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesMonday <- apriori(transMonday, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesMonday)

# Select the best rules according to confidence parameter.
rules_conf_Monday <- sort (rulesMonday, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Monday))

## Tuesday
# Read the csv file.
transTuesday <- read.transactions("data/week/tuesday.csv", format = "single", 
                                  cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesTuesday <- apriori(transTuesday, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesTuesday)

# Select the best rules according to confidence parameter.
rules_conf_Tuesday <- sort (rulesTuesday, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Tuesday))

## Wednesday
# Read the csv file.
transWednesday <- read.transactions("data/week/wednesday.csv", format = "single", 
                                    cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesWednesday <- apriori(transWednesday, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesWednesday)

# Select the best rules according to confidence parameter.
rules_conf_Wednesday <- sort (rulesWednesday, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Wednesday))

## Thursday
# Read the csv file.
transThursday <- read.transactions("data/week/thursday.csv", format = "single", 
                                   cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesThursday <- apriori(transThursday, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesThursday)

# Select the best rules according to confidence parameter.
rules_conf_Thursday <- sort (rulesThursday, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Thursday))

## Friday
# Read the csv file.
transFriday <- read.transactions("data/week/friday.csv", format = "single", 
                                 cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesFriday <- apriori(transFriday, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesFriday)

# Select the best rules according to confidence parameter.
rules_conf_Friday <- sort (rulesFriday, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Friday))

## Saturday
# Read the csv file.
transSaturday <- read.transactions("data/week/saturday.csv", format = "single", 
                                   cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesSaturday <- apriori(transSaturday, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesSaturday)

# Select the best rules according to confidence parameter.
rules_conf_Saturday <- sort (rulesSaturday, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Saturday))

## Sunday
# Read the csv file.
transSunday <- read.transactions("data/week/sunday.csv", format = "single", 
                                 cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesSunday <- apriori(transSunday, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesSunday)

# Select the best rules according to confidence parameter.
rules_conf_Sunday <- sort (rulesSunday, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Sunday))
