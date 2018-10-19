data1112 <- read.csv("HMA_Basic_Indicators_2011_2012.csv")
data1316 <- read.csv("HMA_Basic_Indicators_2013_2016_v3.csv")
data1316 <- data1316[-44]
names(data1112)[35:39] <- names(data1316)[39:43]

levels(data1316$Year)[levels(data1316$Year)=="Current"] <- "2016"


data1316$Year <- as.character(data1316$Year)
data1112$Year <- as.character(data1112$Year)
data1316$Medicare.Provider.ID <- as.character(data1316$Medicare.Provider.ID)
data1112$Medicare.Provider.ID <- as.character(data1112$Medicare.Provider.ID)


names(data1112) == names(data1316)

mydata <- bind_rows(data1112,data1316)
warnings()
sapply(mydata, class)

write.csv(mydata,'../aha_data_hma_main_indicators_2011_2016.csv', row.names = FALSE)
