#setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/")

# Load libraries.
library(arules)
library(dplyr)

## Find a product and decide depending on the parameters given and the knowledge of the data scientist.

rulesA <- apriori (trans, parameter=list (supp=0.001,conf = 0.01), appearance = list (default="lhs",rhs="Afternoon with the baker"), control = list (verbose=F)) 
rules_confA <- sort (rulesA, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_confA))

rulesB <- apriori (trans, parameter=list (supp=0.001,conf = 0.1,minlen=2), appearance = list(default="rhs",lhs="Hack the stack"), control = list (verbose=F))
rules_confB <- sort (rulesB, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_confB))
