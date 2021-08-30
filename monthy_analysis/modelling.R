## modelling.R --> Create one tab per weekday.

# setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/markdown/weekly_analysis")

# Load libraries
library(arules)
library(dplyr)

## October
# Read the csv file, there is one data set per day of the week.
transOct <- read.transactions("data/month/october.csv", format = "single", 
                                 cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesOct <- apriori(transOct, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesOct)

# Select the best rules according to confidence parameter.
rules_conf_Oct <- sort (rulesOct, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Oct))

## November
# Read the csv file.
transNov <- read.transactions("data/month/november.csv", format = "single", 
                                  cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesNov <- apriori(transNov, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesNov)

# Select the best rules according to confidence parameter.
rules_conf_Nov <- sort (rulesNov, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Nov))

## December
# Read the csv file.
transDec <- read.transactions("data/month/december.csv", format = "single", 
                                    cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesDec <- apriori(transDec, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesDec)

# Select the best rules according to confidence parameter.
rules_conf_Dec <- sort (rulesDec, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Dec))

## January
# Read the csv file.
transJan <- read.transactions("data/month/january.csv", format = "single", 
                                   cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesJan <- apriori(transJan, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesJan)

# Select the best rules according to confidence parameter.
rules_conf_Jan <- sort (rulesJan, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Jan))

## February
# Read the csv file.
transFeb <- read.transactions("data/month/february.csv", format = "single", 
                                 cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesFeb <- apriori(transFeb, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesFeb)

# Select the best rules according to confidence parameter.
rules_conf_Feb <- sort (rulesFeb, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Feb))

## March
# Read the csv file.
transMar <- read.transactions("data/month/march.csv", format = "single", 
                                   cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesMar <- apriori(transMar, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesMar)

# Select the best rules according to confidence parameter.
rules_conf_Mar <- sort (rulesMar, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Mar))

## April
# Read the csv file.
transApr <- read.transactions("data/month/april.csv", format = "single", 
                                 cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

# Find all association rules using apriori algorithm, and print a summary of the output.
rulesApr <- apriori(transApr, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rulesApr)

# Select the best rules according to confidence parameter.
rules_conf_Apr <- sort (rulesApr, by="confidence", decreasing=TRUE)
inspect(head(rules_conf_Apr))
