library(dplyr)
library(purrr)
library(magrittr)

#source("Code/HS_demo_help_functions.R")

# Part 1: Data Cleaning ------------------------------------------------------------------------------------------
# Read Data
mappinglist <- openxlsx::read.xlsx("Data/HMA_Mapping_List.xlsx", sheet = "Hospital")
ahadat <- read.csv("Data/aha_data_hma_main_indicators_2011_2016.csv", header=TRUE)
caregroup <- openxlsx::read.xlsx("Data/CareGroup.xlsx")
baycare <- openxlsx::read.xlsx("Data/BayCare.xlsx")

# Add CareGroup as system into AHA table
ahadat$System.Name <- as.character(ahadat$System.Name)
ind <- which(ahadat$AHA.ID %in% caregroup$AHA.ID)
ahadat[ind,13:17] <- caregroup[1,8:12]
# Amend BayCare AHA System ID and Name
ind <- which(ahadat$AHA.ID %in% baycare$AHA.ID)
ahadat[ind,14:15] <- baycare[1,5:6]

# Update column names
makeColNamesUserFriendly <- function(df) {
  # Convert any number of consecutive dots to a single space.
  names(df) <- gsub(x = names(df),
                    pattern = "(\\.)+",
                    replacement = "_")
  
  # Drop the trailing spaces.
  names(df) <- gsub(x = names(df),
                    pattern = "_$",
                    replacement = "")
  df
}
ahadat <- makeColNamesUserFriendly(ahadat)

# Here's the part cleaning the less than one year data
# Split into 3 parts by 12 full month period
df <- split(ahadat,ahadat$Open_12_full_months_before_end_of_reporting_period)

hospital_NA <- df[[1]]
hospital_incomplete <- df[[2]]
hospital_fullyear <- df[[3]]

nrow(ahadat) == nrow(hospital_fullyear)+nrow(hospital_incomplete)+nrow(hospital_NA)

# Make an annulized projection for the incomplete period data
hospital_incomplete_est <- hospital_incomplete %>%
  mutate(Total_hospital_beds = round(365*Total_hospital_beds/Days_open_during_reporting_period,0),
         Admissions = round(365*Admissions/Days_open_during_reporting_period,0),
         Emergency_room_visits = round(365*Emergency_room_visits/Days_open_during_reporting_period,0),
         Other_outpatient_visits_non_ER = round(365*Other_outpatient_visits_non_ER/Days_open_during_reporting_period,0),
         Total_outpatient_visits = round(365*Total_outpatient_visits/Days_open_during_reporting_period,0),
         Physicians_and_dentists_FT = round(365*Physicians_and_dentists_FT/Days_open_during_reporting_period,0),
         Total_personnel_FT = round(365*Total_personnel_FT/Days_open_during_reporting_period,0))

# combine three parts together
hospital_data <- bind_rows(hospital_fullyear,hospital_incomplete_est,hospital_NA) %>%
  left_join(mappinglist[,c("AHA_ID","HMA_Member","HMA_System_Name")], by = "AHA_ID")

write.csv(hospital_data, "Result/hospital_data_1017.csv",row.names = FALSE)

# Part 2: Summary -------------------------------------------------------------------------------------------------
hospital_data <- read.csv("Result/hospital_data_1017.csv")
# Remove location info and ACO
hospital_data <- hospital_data[-c(5:12,16:18,40:43)]

#hospital_data$System_ID <- as.factor(hospital_data$System_ID)

# All Health System -----------------------------------------------------------------------------------------------
hsnumber <- hospital_data[-c(20:21)] %>% filter(System_member == "Y") %>%
  group_by(System_ID,Year) %>%
  mutate(Hospitals = n()) %>%
  select(System_ID,Year,Hospitals) %>%
  unique()

allhs <- hospital_data[-c(20:21)] %>% filter(System_member == "Y") %>%
  group_by(System_ID,Year) %>%
  summarise_if(is.numeric,sum,na.rm = TRUE) %>%
  mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
  mutate(All_Physicians = (Physicians_and_dentists_FT +
                           Total_Physicians_NE +
                           Total_Physicians_TE +
                           Total_Physicians_TG +
                           Total_Physicians_TC +
                           Total_Physicians_TP)) %>%
  left_join(hsnumber,by = c("System_ID","Year")) %>%
  tidyr::drop_na(System_ID)

write.csv(allhs,"Result/allhs1116_1017.csv",row.names = FALSE)

# HMA Member ---------------------------------------------------------------------------------------------------------
hma_number <- hospital_data[-c(20:21)] %>% filter(HMA_Member == "Y") %>%
  group_by(HMA_System_Name,Year) %>%
  mutate(Hospitals = n()) %>%
  select(HMA_System_Name,Year,Hospitals) %>%
  unique()

hmamember <- hospital_data[-c(20:21)] %>% filter(HMA_Member == "Y") %>%
  group_by(HMA_System_Name,Year) %>%
  summarise_if(is.numeric,sum,na.rm = TRUE) %>%
  mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
  mutate(All_Physicians = (Physicians_and_dentists_FT +
                           Total_Physicians_NE +
                           Total_Physicians_TE +
                           Total_Physicians_TG +
                           Total_Physicians_TC +
                           Total_Physicians_TP)) %>%
  left_join(hma_number,by = c("HMA_System_Name","Year"))

write.csv(hmamember,"Result/hmamember1116_1017.csv",row.names = FALSE)

# Top 100 HS ----------------------------------------------------------------------------------------------------
top100mapping <- openxlsx::read.xlsx("Data/2017top100HS_1014_mapping.xlsx")

# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:5)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(allhs, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  select(-c(3:5))
  
top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(hospital_data[c(1,2,4,8:19,22:28)], by = c("AHA.ID" = "AHA_ID")) %>%
  select(-c(3:7,9)) %>% 
  mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
  mutate(All_Physicians = (Physicians_and_dentists_FT +
                           Total_Physicians_NE +
                           Total_Physicians_TE +
                           Total_Physicians_TG +
                           Total_Physicians_TC +
                           Total_Physicians_TP),
         Hospitals = 1)

names(top100_hos) == names(top100_sys)

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(Rank, Year)

write.csv(top100,"Result/top1001116_1017.csv",row.names = FALSE)

# Top 100 + VA ---------------------------------------------------------------------------------------------------
VA <- allhs %>% filter(System_ID == "5999295") %>%
  mutate(Rank = 101, a_Name = "Department of Veterans Affairs") %>% select(24,25,1:23)

VA <- VA[-3]

names(top100) == names(VA)

top100VA <- bind_rows(top100,VA)

write.csv(top100VA,"Result/top100VA1116_1017.csv",row.names = FALSE)

# HMA in Top 100 -------------------------------------------------------------------------------------------------
hmatop100 <- top100 %>% left_join(top100mapping[,c(2,3)],by = 'a_Name') %>%
  filter(!is.na(HMA_System_Name))

write.csv(hmatop100,"Result/hmatop1001116_1017.csv",row.names = FALSE)
