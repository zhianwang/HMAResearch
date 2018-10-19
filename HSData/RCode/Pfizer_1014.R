library(dplyr)
library(purrr)
library(magrittr)

# Part 1: Baisc Indicators ----------------------------------------------------------------------------------------
pfe_allhs <- allhs %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% mutate(Scope = "All HS")

pfe_hma <- hmamember %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% mutate(Scope = "HMA")

pfe_top100 <- top100 %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% select(-2) %>% mutate(Scope = "Top100")

pfe_hmatop100 <- hmatop100 %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% select(-2) %>% mutate(Scope = "HMAinTop100")


pfe_stat <- bind_rows(pfe_allhs,pfe_hma,pfe_top100,pfe_hmatop100)

write.csv(pfe_stat,"Pfizer/raw_summary_1014.csv",row.names = FALSE)


sumHospitalNum <- function(df){
  df %>% filter(Year == 2016) %>% group_by(Year) %>% summarise(TotalHospital = sum(Hospitals))
}

sumHospitalNum(allhs)
sumHospitalNum(hmamember)
sumHospitalNum(top100)
sumHospitalNum(hmatop100)



pfe_allhs <- allhs %>% filter(Year == 2016) #%>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% mutate(Scope = "All HS")

pfe_hma <- hmamember %>% filter(Year == 2016) #%>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% mutate(Scope = "HMA")

pfe_top100 <- top100 %>% filter(Year == 2016) #%>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% select(-2) %>% mutate(Scope = "Top100")

pfe_hmatop100 <- hmatop100 %>% filter(Year == 2016) #%>% group_by(Year) %>% summarise_if(is.numeric,mean) %>% select(-2) %>% mutate(Scope = "HMAinTop100")






pfe_allhs_sum <- allhs %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,sum) %>% mutate(Scope = "All HS")

pfe_hma_sum <- hmamember %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,sum) %>% mutate(Scope = "HMA")

pfe_top100_sum <- top100 %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,sum) %>% select(-2) %>% mutate(Scope = "Top100")

pfe_hmatop100_sum <- hmatop100 %>% filter(Year == 2016) %>% group_by(Year) %>% summarise_if(is.numeric,sum) %>% select(-2) %>% mutate(Scope = "HMAinTop100")


pfe_stat_share <- bind_rows(pfe_allhs_sum,pfe_hma_sum,pfe_top100_sum,pfe_hmatop100_sum)
write.csv(pfe_stat_share,"Pfizer/sum_1014.csv",row.names = FALSE)

# Part 2: Health Plan related ------------------------------------------------------------------------------------
hospital_data <- read.csv("Result/hospital_data_1014.csv") %>% filter(Year == 2016) %>%
  select(c(2,4,13:15,31:32,40:45))

allhs <- hospital_data %>% filter(System_member == "Y") 
write.csv(allhs,"Pfizer/hshealthplan.csv",row.names = FALSE)

hmanumber <- hospital_data %>% filter(HMA_Member == "Y")


hmanumber %>% summarise_if(is.numeric,mean,na.rm = TRUE)

write.csv(hmanumber,"Pfizer/hmahealthplan.csv",row.names = FALSE)



top100mapping <- openxlsx::read.xlsx("Data/2017top100HS_1014_mapping.xlsx")
top100_sys <- top100mapping[,c(1:5)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(allhs, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  select(1,7,10:15)

top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(hospital_data, by = c("AHA.ID" = "AHA_ID")) %>%
  select(1:2,12:17) %>%
  rename(Hospital_Name = a_Name)

names(top100_hos) == names(top100_sys)

top100 <- bind_rows(top100_sys,top100_hos)
write.csv(top100,"Pfizer/top100healthplan.csv",row.names = FALSE)

