#setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/apriori_analysis")

# Load libraries.
library(arules)
library(dplyr)

# Read the prepared data set and apply a priori.
trans <- read.transactions("dataset.csv", format = "single", 
                           cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')
rules <- apriori(trans, parameter=list(sup=0.001,conf=0.001, target="rules"))
summary(rules)

# Select the best rules according to confidence and lift parameters.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))

rules_lift <- sort (rules, by="lift", decreasing=TRUE)
inspect(head(rules_lift))




