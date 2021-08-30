## preparation.R

# setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Bakery/markdown/weekly_analysis")

# Load libraries
library(arules)
library(dplyr)

# Read the data set.
bread <- read.csv("source/BreadBasket_DMS.csv")

# Generate a new attribute with the days of the week.
bread$Weekday <- weekdays(as.Date(bread$Date))

# Create one data set per day of the week with only two attributes: transaction and item.
breadMonday <- select(filter(bread, Weekday== "lunes"), Transaction, Item)
breadTuesday <- select(filter(bread, Weekday== "martes"), Transaction, Item)
breadWednesday <- select(filter(bread, Weekday== "miércoles"), Transaction, Item)
breadThursday <- select(filter(bread, Weekday== "jueves"), Transaction, Item)
breadFriday <- select(filter(bread, Weekday== "viernes"), Transaction, Item)
breadSaturday <- select(filter(bread, Weekday== "sábado"), Transaction, Item)
breadSunday <- select(filter(bread, Weekday== "domingo"), Transaction, Item)

# Write to 'data' folder the 7 data sets, in a new folder called week.
write.csv(breadMonday, file = "data/week/monday.csv", quote = FALSE, row.names = FALSE)
write.csv(breadTuesday, file = "data/week/tuesday.csv", quote = FALSE, row.names = FALSE)
write.csv(breadWednesday, file = "data/week/wednesday.csv", quote = FALSE, row.names = FALSE)
write.csv(breadThursday, file = "data/week/thursday.csv", quote = FALSE, row.names = FALSE)
write.csv(breadFriday, file = "data/week/friday.csv", quote = FALSE, row.names = FALSE)
write.csv(breadSaturday, file = "data/week/saturday.csv", quote = FALSE, row.names = FALSE)
write.csv(breadSunday, file = "data/week/sunday.csv", quote = FALSE, row.names = FALSE)