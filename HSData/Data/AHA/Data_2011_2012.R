library(openxlsx)
library(dplyr)
###########################################################################################################
# 2011
# Read data
general2011 <- read.xlsx("2011/AHA Data- General Hospital Info - 2011.xlsx", sheet = 1)
beds2011 <- read.xlsx("2011/AHA Data- Beds and Utilization - 2011.xlsx", sheet = 1)
staffing2011 <- read.xlsx("2011/AHA Data- Staffing - 2011.xlsx", sheet = 1)
fin2011 <- read.xlsx("2011/AHA Data- Financials - 2011.xlsx", sheet = 1)
medi2011 <- read.xlsx("2011/AHA Data- Medicare and Medicaid Utilization - 2011.xlsx", sheet = 1)
# Subset
general2011 <- general2011[,c(1:8,71:74,77:79,81:83)]
beds2011 <- beds2011[,c(2,38,55,59,62,68:71)]
medi2011 <- medi2011[,c(2,39:42)]
fin2011 <- fin2011[,c(2,19:20)]
staffing2011 <- staffing2011[,c(2,38,44,112,119,126,133,140)]
# Join
data2011 <- general2011 %>% 
  left_join(beds2011) %>% 
  left_join(medi2011) %>%
  left_join(fin2011) %>%
  left_join(staffing2011)

###########################################################################################################
# 2012
# Read data
general2012 <- read.xlsx("2012/AHA Data- General Hospital Info - 2012.xlsx", sheet = 1)
beds2012 <- read.xlsx("2012/AHA Data- Beds and Utilization - 2012.xlsx", sheet = 1)
staffing2012 <- read.xlsx("2012/AHA Data- Staffing Info - 2012.xlsx", sheet = 1)
fin2012 <- read.xlsx("2012/AHA Data- Financials - 2012.xlsx", sheet = 1)
medi2012 <- read.xlsx("2012/AHA Data- Medicare and Medicaid Utilization - 2012.xlsx", sheet = 1)
# Subset
general2012 <- general2012[,c(1:8,71:74,77:79,81:83)]
beds2012 <- beds2012[,c(2,39,56,60,63,69:72)]
medi2012 <- medi2012[,c(2,37:40)]
fin2012 <- fin2012[,c(2,17:18)]
staffing2012 <- staffing2012[,c(2,36,42,110,117,124,131,138)]
# Join
data2012 <- general2012 %>% 
  left_join(beds2012) %>% 
  left_join(medi2012) %>%
  left_join(fin2012) %>%
  left_join(staffing2012)


names(data2011) == names(data2012)

mydata <- bind_rows(data2011,data2012)

nrow(mydata) == nrow(data2011)+nrow(data2012)

write.csv(mydata,"HMA_Basic_Indicators_2011_2012.csv",row.names = FALSE)
