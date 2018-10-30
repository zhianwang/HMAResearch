library(magrittr)
library(dplyr)
setwd("D:\\Specialty_RX\\Registration_Survey")

dat <- read.csv("Specialty_Pharmacy_RegSurvey_1023.csv",header=TRUE, skip = 3, na.strings=c("","NA"), fileEncoding="UTF-16LE")

dat <- dat[c(5:6,13,14,29:36)]
names(dat) <- c(
  "Status",
  "Full Name",
  "Company",
  "Title",
  "Additional comments or anything you would like us to know about your organization related to specialty pharmacy",
  "Does your health system offer specialty pharmacy services?",
  "involved_degree",
  "How would the pharmacy department best be described in your health system?",
  "What is your current position?",
  "What is your knowledge level regarding Specialty Pharmacy within your organization?",
  "What is your primary objective for attending the Specialty Pharmacy Collaborative?",
  "Why haven't you considered starting a hospital-based specialty pharmacy?"
)

dat <- dat %>% filter(Status == "Accepted") %>% mutate(id = row_number())
df <- dat[c(13,2,7)]
df$involved_degree <- stringr::str_replace_all(df$involved_degree, "N/A", " : ")

newdf <- tidyr::separate(data = df, col = involved_degree, into = c('a','b','c','d','e','f','g','h','i','j'), sep = ", ")

newdf$j[newdf$`Full Name` == "Bowers, Donna"] <- "Training and education: Moderate involvement"
newdf$f[newdf$`Full Name` == "Blank, Gregory"] <- "Contracting with manufacturers: Limited involvement"
newdf$g[newdf$`Full Name` == "Blank, Gregory"] <- "Marketing to internal providers: Highly involved"
newdf$h[newdf$`Full Name` == "Blank, Gregory"] <- "Marketing to affiliate or external providers: Moderate involvement"
newdf$i[newdf$`Full Name` == "Blank, Gregory"] <- "Recruiting staff: Highly involved"
newdf$j[newdf$`Full Name` == "Blank, Gregory"] <- "Training and education: Highly involved"

newdf <- newdf[-2]

newdf <- newdf %>% tidyr::drop_na()

# Function to extract column name ----------------------------------------------------------
extract_colname <- function(df){
  numcol <- 2
  numvar <- length(names(df))-1
  print(numvar)
  for (i in 1:numvar){
    print(i)
    df <- tidyr::separate(data = df, col = names(df)[numcol], into = c("value","answer"),sep = ": ")
    names(df)[numcol+1] <- unique(df$value)
    df <- df[-numcol]
    numcol <- numcol+1
  }
  return(df)
}

newdf <- extract_colname(newdf)
mydf <- plyr::join(dat, newdf, type = "left", by = "id")
mydf <- mydf[c(13,2:4,6,8:10,7,14:23,11:12,5)]
names(mydf)[9] <- "How involved are you in the following areas related specifically to specialty pharmacy?"
#mydf <- mydf[!(mydf$id %in% c(1,56,57)),]
mydf <- mydf %>% mutate(id = row_number())

# save file to csv
openxlsx::write.xlsx(mydf, 'Specialty_Pharmacy_RegSurvey_1023_cleaned.xlsx')

######################################################################################################

names(mydf)[2:11] <- c("fullname","company","title","comments",
                 "offer_spx","pxdept","position","knowledgt","obj","involvedegree")



test <- plyr::join(dat, new1, type = "left", by = "id")

table(mydf$`What is your current position?`)
table(mydf$`What is your knowledge level regarding Specialty Pharmacy within your organization?`)

pos_freq <- plyr::count(mydf, '`What is your current position?`')
barplot(prop.table(table(mydf$`What is your current position?`)))
