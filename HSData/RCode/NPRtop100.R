library(dplyr)
library(purrr)
library(magrittr)

source("RCode/GM11.R")

# NPR Top 100 been split into 3 part: by System, by Hospital, and UCLA -----------------------------------------------
allhs <- read.csv("Result/allhs1116_1017.csv")
hospital_data <- read.csv("Result/hospital_data_1017.csv")
# Remove location info and ACO
hospital_data <- hospital_data[-c(5:12,16:18,40:43)]
# Top 100 HS ----------------------------------------------------------------------------------------------------
nprtop100mapping <- openxlsx::read.xlsx("Data/2017NPRtop100HS_1023_mapping.xlsx")

# Split by AHA_System_ID and AHA.ID
nprtop100_sys <- nprtop100mapping[,c(1,2,4:6)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(allhs, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  select(-c(3:5))

nprtop100_hos <- nprtop100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(hospital_data[c(1,2,4,8:19,22:28)], by = c("AHA.ID" = "AHA_ID")) %>%
  select(-c(3:8,10)) %>% 
  mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
  mutate(All_Physicians = (Physicians_and_dentists_FT +
                             Total_Physicians_NE +
                             Total_Physicians_TE +
                             Total_Physicians_TG +
                             Total_Physicians_TC +
                             Total_Physicians_TP),
         Hospitals = 1)

# Add UCLA
ucla <- read.csv("Result/hmamember1116_1017.csv") %>% 
  filter( HMA_System_Name == "UCLA Health") %>% 
  mutate(NPR_Rank = 89, a_Name = "University of California - Los Angeles") %>%
  select(-1)

names(nprtop100_hos) == names(nprtop100_sys)


nprtop100 <- bind_rows(nprtop100_sys,nprtop100_hos,ucla) %>% arrange(NPR_Rank, Year)

write.csv(nprtop100,"Result/nprtop1001116_1023.csv",row.names = FALSE)

# Top 100 + VA ---------------------------------------------------------------------------------------------------
VA <- allhs %>% filter(System_ID == "5999295") %>%
  mutate(NPR_Rank = 101, a_Name = "Department of Veterans Affairs") %>% select(24,25,1:23)

VA <- VA[-3]

names(nprtop100) == names(VA)

nprtop100VA <- bind_rows(nprtop100,VA)

write.csv(nprtop100VA,"Result/nprtop100VA1116_1023.csv",row.names = FALSE)

# HMA in Top 100 -------------------------------------------------------------------------------------------------
hmatop100 <- nprtop100 %>% left_join(nprtop100mapping[,c(2,4)],by = 'a_Name') %>%
  filter(!is.na(HMA_System_Name))

write.csv(hmatop100,"Result/nprhmatop1001116_1023.csv",row.names = FALSE)

###################################################################################################################
# Forecasting Indicator
###################################################################################################################
allhs <- read.csv("Result/allhs1116_1017.csv") %>% select(-c(5,6,9,11:14,17:21))
hma <- read.csv("Result/hmamember1116_1017.csv") %>% select(-c(5,6,9,11:14,17:21))
nprtop100 <- read.csv("Result/nprtop1001116_1023.csv") %>% select(-c(1,6,7,10,12:15,18:22))
nprhmatop100 <- read.csv("Result/nprhmatop1001116_1023.csv") %>% select(-c(1,6,7,10,12:15,18:22))
nprtop100VA <- read.csv("Result/nprtop100VA1116_1023.csv") %>% select(-c(1,6,7,10,12:15,18:22))

# Part 0: Functions -----------------------------------------------------------------------------------------------
transposedf <- function(df){
  n <- df$Year
  new_df <- as.data.frame(t(df[,-1]))
  colnames(new_df) <- n
  new_df$indicator <- row.names(new_df)
  return(new_df)
}

indicatorGM11 <- function(df,descale = "N"){
  df_t <- transposedf(df)
  
  df_long <- reshape2::melt(df_t,
                            id.vars="indicator",
                            variable.name="Year",
                            value.name="value")
  
  if(descale == "Y"){
    df_nest <- df_long %>%
      mutate(devalue = value/1000) %>%
      group_by(indicator) %>%
      tidyr::nest(.key = "data.tbl") %>%
      mutate(value = map(data.tbl,"devalue"))
  } else {
    df_nest <- df_long %>%
      group_by(indicator) %>%
      tidyr::nest(.key = "data.tbl") %>%
      mutate(value = map(data.tbl,"value"))
  }
  
  df_fit <- df_nest %>%
    mutate(model = map(.x=value, .f=GM11,2),
           predict = map(model,"predict")
    )
  if (descale == "Y"){
    df_predicted <- df_fit %>%
      tidyr::unnest(predict,.drop = TRUE) %>%
      mutate(predicted = preval*1000) %>%
      reshape2::dcast(indicator ~ period, value.var="predicted") %>%
      rename(Y2017 = "1", Y2018 = "2")
  } else{
    df_predicted <- df_fit %>%
      tidyr::unnest(predict,.drop = TRUE) %>%
      reshape2::dcast(indicator ~ period, value.var="preval") %>%
      rename(Y2017 = "1", Y2018 = "2")
  }
  
  return(df_predicted)
}

# Part 1: Forecast all basic indicators (Average) except # of Hospitals ------------------------------------------------------------------------------
allhs_summary <- allhs %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

hma_summary <- hma %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

nprtop100_summary <- nprtop100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

nprhmatop100_summary <- nprhmatop100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

nprtop100va_summary <- nprtop100VA %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

allhs_predicted <- indicatorGM11(allhs_summary)
allhs_predicted$Scope <- "All HS"
hma_predicted <- indicatorGM11(hma_summary)
hma_predicted$Scope <- "HMA"
nprtop100_predicted <- indicatorGM11(nprtop100_summary)
nprtop100_predicted$Scope <- "NPR Top 100"
nprhmatop100_predicted <- indicatorGM11(nprhmatop100_summary)
nprhmatop100_predicted$Scope <- "HMA in NPR Top100"
nprtop100va_predicted <- indicatorGM11(nprtop100va_summary)
nprtop100va_predicted$Scope <- "NPR Top 100 + VA"

mydf <- bind_rows(allhs_predicted,hma_predicted,nprtop100_predicted,nprtop100va_predicted,nprhmatop100_predicted)

write.csv(mydf, "Result/indicatior_1718_npr_1023.csv",row.names = FALSE)

# Part 2: Forecast the sum ----------------------------------------------------------------------------------------
allhs_sum <- allhs %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

hma_sum <- hma %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

nprtop100_sum <- nprtop100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

nprhmatop100_sum <- nprhmatop100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

nprtop100va_sum <- nprtop100VA %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

allhs_predicted <- indicatorGM11(allhs_sum,descale="Y")
allhs_predicted$Scope <- "All HS"
hma_predicted <- indicatorGM11(hma_sum,"Y")
hma_predicted$Scope <- "HMA"
nprtop100_predicted <- indicatorGM11(nprtop100_sum,"Y")
nprtop100_predicted$Scope <- "NPR Top 100"
nprhmatop100_predicted <- indicatorGM11(nprhmatop100_sum,"Y")
nprhmatop100_predicted$Scope <- "HMA in NPR Top 100"
nprtop100va_predicted <- indicatorGM11(nprtop100va_sum,"Y")
nprtop100va_predicted$Scope <- "NPR Top 100 + VA"

mysum <- bind_rows(allhs_predicted,hma_predicted,nprtop100_predicted,nprtop100va_predicted,nprhmatop100_predicted)

write.csv(mysum, "Result/indicatiorsum_1718_NPR_1023.csv",row.names = FALSE)

###################################################################################################################
# Revenue Part
###################################################################################################################
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

nprtop100 <- rev17 %>%
  filter(!a_Name %in% c("MEDNAX Services, Inc. (FL)", #remove MEDNAX as a physician service association
                        "Presence Health (IL)")) %>% # and Presence Health (IL) mergerd be part of Ascension
  mutate(npr_rank = min_rank(desc(NPR))) %>%
  top_n(100,NPR) %>%
  arrange(npr_rank)
nprtop100va <- bind_rows(nprtop100,va2017)

rev17_avg <- rev17 %>% summarise_if(is.numeric,mean)
rev17_avg$Scope <- "All HS"
hma <- rev17 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean)
hma$Scope <- "HMA"
nprtop100_avg <- nprtop100 %>% summarise_if(is.numeric,mean) %>% select(-4)
nprtop100_avg$Scope <- "NPR Top 100"
nprtop100va_avg <- nprtop100va %>% summarise_if(is.numeric,mean) %>% select(-4)
nprtop100va_avg$Scope <- "NPR Top 100 VA"
nprhmatop100 <- nprtop100 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean) %>% select(-4)
nprhmatop100$Scope <- "HMA in NPR Top 100"

mydf2017avg <- bind_rows(rev17_avg,hma,nprtop100_avg,nprtop100va_avg,nprhmatop100)

#write.csv(mydf2017avg,"Result/revenue17avg_npr.csv",row.names = FALSE)
# Part 2: 2017 Sum --------------------------------------------------------------------------------------------
rev17_sum <- rev17 %>% summarise_if(is.numeric,sum)
rev17_sum$Scope <- "All HS"
hma_sum <- rev17 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum)
hma_sum$Scope <- "HMA"
nprtop100_sum <- nprtop100 %>% summarise_if(is.numeric,sum) %>% select(-4)
nprtop100_sum$Scope <- "NPR Top 100"
nprtop100va_sum <- nprtop100va %>% summarise_if(is.numeric,sum) %>% select(-4)
nprtop100va_sum$Scope <- "NPR Top 100 VA"
nprhmatop100_sum <- nprtop100 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum) %>% select(-4)
nprhmatop100_sum$Scope <- "HMA in NPR Top 100"

mydf2017sum <- bind_rows(rev17_sum,hma_sum,nprtop100_sum,nprtop100va_sum,nprhmatop100_sum)
mydf2017sum$Year <- "2017"

#write.csv(mydf2017sum,"Result/revenue17sum_npr.csv",row.names = FALSE)
# Part 3: 2018 Average -----------------------------------------------------------------------------------------
nprtop100_18 <- left_join(nprtop100[,c(1,5:7)], rev18[,c(1:3)], by = "a_Name")

nprtop100va18 <- bind_rows(nprtop100_18,va18)


rev18_avg <- rev18 %>% summarise_if(is.numeric,mean)
rev18_avg$Scope <- "All HS"
hma18 <- rev18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean)
hma18$Scope <- "HMA"
nprtop100_avg18 <- nprtop100_18 %>% summarise_if(is.numeric,mean) %>% select(-npr_rank)
nprtop100_avg18$Scope <- "NPR Top 100"
nprtop100va_avg18 <- nprtop100va18 %>% summarise_if(is.numeric,mean) %>% select(-npr_rank)
nprtop100va_avg18$Scope <- "NPR Top 100 VA"
nprhmatop100_18 <- nprtop100_18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,mean) %>% select(-npr_rank)
nprhmatop100_18$Scope <- "HMA in NPR Top 100"

mydf2018avg <- bind_rows(rev18_avg,hma18,nprtop100_avg18,nprtop100va_avg18,nprhmatop100_18)
mydf2018avg$Year <- "2018"
#write.csv(mydf2017avg[-1],"Result/revenue18avg.csv",row.names = FALSE)

# Part 4: 2018 Sum --------------------------------------------------------------------------------------------
rev18_sum <- rev18 %>% summarise_if(is.numeric,sum)
rev18_sum$Scope <- "All HS"
hma18_sum <- rev18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum)
hma18_sum$Scope <- "HMA"
nprtop100_sum18 <- nprtop100_18 %>% summarise_if(is.numeric,sum) %>% select(-npr_rank)
nprtop100_sum18$Scope <- "NPR Top 100"
nprtop100va_sum18 <- nprtop100va18 %>% summarise_if(is.numeric,sum) %>% select(-npr_rank)
nprtop100va_sum18$Scope <- "NPR Top 100 VA"
nprhmatop100_18_sum <- nprtop100_18 %>% filter(HMA_Member == "Y") %>% summarise_if(is.numeric,sum) %>% select(-npr_rank)
nprhmatop100_18_sum$Scope <- "HMA in NPR Top 100"

mydf2018sum <- bind_rows(rev18_sum,hma18_sum,nprtop100_sum18,nprtop100va_sum18,nprhmatop100_18_sum)
mydf2018sum$Year <- "2018"
#write.csv(mydf2017sum[-1],"Result/revenue18sum.csv",row.names = FALSE)

avg_revenue_tbl <- rbind(mydf2017avg,mydf2018avg)
sum_revenue_tbl <- rbind(mydf2017sum,mydf2018sum)

write.csv(avg_revenue_tbl,"Result/1718_average_revenue_npr.csv",row.names = FALSE)
write.csv(sum_revenue_tbl,"Result/1718_sum_revenue_npr.csv",row.names = FALSE)

