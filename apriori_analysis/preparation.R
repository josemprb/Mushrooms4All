
# setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/")

# Load libraries
library(arules)
library(dplyr)

# Read and create a data set just with Transaction and Item, to do a priori.
bread <- read.csv("source/BreadBasket_DMS.csv")
breadSelection <- select(bread,Transaction,Item)

# Find less sold products and print them.
tt <- as.data.frame(table(bread$Item))
colnames(tt) <- c("Item","Freq")
sortedDF <- tt[order(tt$Freq),]
numrows <- round(nrow(sortedDF)*0.25)
less.sold <- sortedDF[1:numrows,]
less.sold

# Assign a frequency of each item as a new parameter.
breadSelection$Freq <- 0
for (j in 1:length(breadSelection$Item)) {
  for (i in 1:length(tt$Freq)) {
    if (breadSelection$Item[j] == tt$Item[i]) {
      breadSelection$Freq[j] <- tt$Freq[i]
    }
  }
}

# Select the least frequent data to model it.
sortedBread <- breadSelection[order(breadSelection$Freq),]
numrows <- round(nrow(sortedBread)*0.1)
dataset <- sortedBread[1:numrows,]

# Write it to csv file.
write.csv(dataset, file = "dataset.csv", quote = FALSE, row.names = FALSE)

