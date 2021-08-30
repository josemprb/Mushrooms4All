#Create one data set per month with only two attributes: transaction and item.
breadOct<-select(subset(bread, format.Date(Date, "%m")=="10"),Transaction,Item)
breadNov<-select(subset(bread, format.Date(Date, "%m")=="11"),Transaction,Item)
breadDec<-select(subset(bread, format.Date(Date, "%m")=="12"),Transaction,Item)
breadJan<-select(subset(bread, format.Date(Date, "%m")=="01"),Transaction,Item)
breadFeb<-select(subset(bread, format.Date(Date, "%m")=="02"),Transaction,Item)
breadMar<-select(subset(bread, format.Date(Date, "%m")=="03"),Transaction,Item)
breadApr<-select(subset(bread, format.Date(Date, "%m")=="04"),Transaction,Item)

# Write to 'data' folder the 7 data sets, in a new folder called week.
write.csv(breadOct, file = "data/month/october.csv", quote = FALSE, row.names = FALSE)
write.csv(breadNov, file = "data/month/november.csv", quote = FALSE, row.names = FALSE)
write.csv(breadDec, file = "data/month/december.csv", quote = FALSE, row.names = FALSE)
write.csv(breadJan, file = "data/month/january.csv", quote = FALSE, row.names = FALSE)
write.csv(breadFeb, file = "data/month/february.csv", quote = FALSE, row.names = FALSE)
write.csv(breadMar, file = "data/month/march.csv", quote = FALSE, row.names = FALSE)
write.csv(breadApr, file = "data/month/april.csv", quote = FALSE, row.names = FALSE)

