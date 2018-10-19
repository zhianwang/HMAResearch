library(dplyr)
source("Code/GM11.R")

rev1117 <- read.csv("Result/revenue_1017.csv",header = TRUE)
rev1819 <- read.csv("Result/revenue_1819forecast_1017.csv")
varev <- read.xlsx("Data/Financial/VA_FS.xlsx", sheet = 1)
varev <- varev[-7]

# Part 0: filter out data already have 2018 data & predict VA 2018 -------------------------------------------------------------
actual18 <- rev1117 %>% filter(Year == "2018")
index <- which(rev1819$a_Name %in% actual18$a_Name)
rev18 <- rev1819[-index,c(1,2,4)]
names(rev18)[2:3] <- c("Total_Revenue", "NPR")
sysmappinglist <- read.xlsx("Data/HMA_Mapping_List.xlsx", sheet = "System")
rev18 <- left_join(rev18,sysmappinglist[,c(1,2,5)], by = "a_Name")
# addd actual
rev18 <- bind_rows(rev18,actual18)
rev18$Year <- "2018"

# VA
va18revmodel <- GM11(log(varev$Total_Revenue),1)
va18totalrevenue <- exp(va18revmodel$predict[1,2])
va18npr <- 0.96*va18totalrevenue
va18 <- data.frame(a_Name = "Veterans Health Administration", 
                   Total_Revenue = va18totalrevenue, 
                   NPR = va18npr,
                   HMA_System_Name = "VHA",
                   HMA_Member = "N")

# Part 1: 2017 Average ---------------------------------------------------------------------------------------
rev17 <- rev1117 %>% filter(Year == 2017)
va2017 <- varev %>% filter(Year == 2017)

top100 <- rev17 %>% filter(a_Name != "MEDNAX Services, Inc. (FL)") %>% # remove MEDNAX as a physician service association
  mutate(rank = min_rank(desc(Total_Revenue))) %>%
  top_n(100,Total_Revenue) %>%
  arrange(rank)
top100va <- bind_rows(top100,va2017)

rev17_avg <- rev17 %>% summarise_if(is.numeric,mean)
rev17_avg$Scope <- "All HS"
hma <- rev17 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean)
hma$Scope <- "HMA"
top100_avg <- top100 %>% summarise_if(is.numeric,mean) %>% select(-4)
top100_avg$Scope <- "Top100"
top100va_avg <- top100va %>% summarise_if(is.numeric,mean) %>% select(-4)
top100va_avg$Scope <- "Top100VA"
hmatop100 <- top100 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean) %>% select(-4)
hmatop100$Scope <- "HMAinTop100"

mydf2017avg <- bind_rows(rev17_avg,hma,top100_avg,top100va_avg,hmatop100)

write.csv(mydf2017avg,"Result/revenue17avg.csv",row.names = FALSE)
# Part 2: 2017 Sum --------------------------------------------------------------------------------------------
rev17_sum <- rev17 %>% summarise_if(is.numeric,sum)
rev17_sum$Scope <- "All HS"
hma_sum <- rev17 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum)
hma_sum$Scope <- "HMA"
top100_sum <- top100 %>% summarise_if(is.numeric,sum) %>% select(-4)
top100_sum$Scope <- "Top100"
top100va_sum <- top100va %>% summarise_if(is.numeric,sum) %>% select(-4)
top100va_sum$Scope <- "Top100VA"
hmatop100_sum <- top100 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum) %>% select(-4)
hmatop100_sum$Scope <- "HMAinTop100"

mydf2017sum <- bind_rows(rev17_sum,hma_sum,top100_sum,top100va_sum,hmatop100_sum)
mydf2017sum$Year <- "2017"

write.csv(mydf2017sum,"Result/revenue17sum.csv",row.names = FALSE)
# Part 3: 2018 Average -----------------------------------------------------------------------------------------
top100_18 <- left_join(top100[,c(1,5:7)], rev18[,c(1:3)], by = "a_Name")

top100va18 <- bind_rows(top100_18,va18)


rev18_avg <- rev18 %>% summarise_if(is.numeric,mean)
rev18_avg$Scope <- "All HS"
hma18 <- rev18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean)
hma18$Scope <- "HMA"
top100_avg18 <- top100_18 %>% summarise_if(is.numeric,mean) %>% select(-rank)
top100_avg18$Scope <- "Top100"
top100va_avg18 <- top100va18 %>% summarise_if(is.numeric,mean) %>% select(-rank)
top100va_avg18$Scope <- "Top100VA"
hmatop100_18 <- top100_18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean) %>% select(-rank)
hmatop100_18$Scope <- "HMAinTop100"

mydf2018avg <- bind_rows(rev18_avg,hma18,top100_avg18,top100va_avg18,hmatop100_18)
mydf2018avg$Year <- "2018"
#write.csv(mydf2017avg[-1],"Result/revenue18avg.csv",row.names = FALSE)

# Part 4: 2018 Sum --------------------------------------------------------------------------------------------
rev18_sum <- rev18 %>% summarise_if(is.numeric,sum)
rev18_sum$Scope <- "All HS"
hma18_sum <- rev18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum)
hma18_sum$Scope <- "HMA"
top100_sum18 <- top100_18 %>% summarise_if(is.numeric,sum) %>% select(-rank)
top100_sum18$Scope <- "Top100"
top100va_sum18 <- top100va18 %>% summarise_if(is.numeric,sum) %>% select(-rank)
top100va_sum18$Scope <- "Top100VA"
hmatop100_18_sum <- top100_18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum) %>% select(-rank)
hmatop100_18_sum$Scope <- "HMAinTop100"

mydf2018sum <- bind_rows(rev18_sum,hma18_sum,top100_sum18,top100va_sum18,hmatop100_18_sum)
mydf2018sum$Year <- "2018"
#write.csv(mydf2017sum[-1],"Result/revenue18sum.csv",row.names = FALSE)

avg_revenue_tbl <- rbind(mydf2017avg,mydf2018avg)
sum_revenue_tbl <- rbind(mydf2017sum,mydf2018sum)

write.csv(avg_revenue_tbl,"Result/1718_average_revenue.csv",row.names = FALSE)
write.csv(sum_revenue_tbl,"Result/1718_sum_revenue.csv",row.names = FALSE)
