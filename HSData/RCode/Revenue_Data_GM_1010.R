library(openxlsx)
library(dplyr)
library(purrr)
library(magrittr)
source("Code/HS_demo_help_functions.R")

# Read data
# - Mappling list: the Modern Health HS name and HMA name
sysmappinglist <- read.xlsx("Data/HMA_Mapping_List.xlsx", sheet = "System") 
# - Modern Health data
mhdat <- read.csv("Data/Financial/ModerHealth_Financial_Annual_20180226.csv", header=TRUE)
# - Remaining  Health System Revenue data (mainly from financial statement, 2 HS from AHD)
fsdat <- read.xlsx("Data/Financial/Health_system_FS.xlsx", sheet = "Revenue")
# - Health System Rename and Merge
renametbl <- read.xlsx("Data/Merge&Rename.xlsx", sheet = "Rename")

# Part 1: Data Cleaning ----------------------------------------------------------------------------------------
# Handle the rename issue in MH data
for (i in 1:nrow(renametbl)){
  mhdat$a_Name[mhdat$a_Name == renametbl$Old_Name[i]] <- renametbl$New_Name[i]
  i =+ 1
}
# Handle the Merge Case
hsmerged <- mhdat %>% 
  filter(a_Name %in% c("Providence Health & Services (WA)","St. Joseph Health (CA)") & Year %in% c(2011:2015)) %>%
  mutate(a_Name = "Providence St. Joseph Health (WA)") %>%
  group_by(Year, a_Name) %>%
  summarise_if(is.numeric, sum)
mhdat <-mhdat %>% bind_rows(.,hsmerged) %>% 
  subset(., !(a_Name %in% c("Providence Health & Services (WA)","St. Joseph Health (CA)")))

hsmerged <- mhdat %>% 
  filter(a_Name %in% c("Advocate Health Care (IL)","Aurora Health Care (WI)") & Year %in% c(2011:2017)) %>%
  mutate(a_Name = "Advocate Aurora Health") %>%
  group_by(Year, a_Name) %>%
  summarise_if(is.numeric, sum)
mhdat <-mhdat %>% bind_rows(.,hsmerged) %>% 
  subset(., !(a_Name %in% c("Advocate Health Care (IL)","Aurora Health Care (WI)")))


# Combine Modern Health data and additional FS revenue data
revenue <- bind_rows(mhdat[,c("a_Name","Year","Total_Revenue","Net_Patient_Revenue")], fsdat[,c(1:4)])

# check the deplicate rows
nrow(revenue[duplicated(revenue), ])
# Sort the dataframe, and remove the duplicated rows based on HS Name and Year
revenue <- distinct(revenue[with(revenue,order(a_Name,Year,-Total_Revenue,-Net_Patient_Revenue)),], a_Name,Year, .keep_all = TRUE)

# Part 2: Calculate total revenue or NPR based on their ratio ----------------------------------------------------
rev <- revenue
nprtototalratio <- mean(rev$Net_Patient_Revenue/rev$Total_Revenue, na.rm = TRUE)
rev <- rev %>% 
  mutate(NPR_est = ifelse((!is.na(Total_Revenue))&(is.na(Net_Patient_Revenue)), 
                          Total_Revenue*nprtototalratio,Net_Patient_Revenue),
         Total_revenue_est = ifelse((is.na(rev$Total_Revenue))&!(is.na(rev$Net_Patient_Revenue)),
                                    Net_Patient_Revenue/nprtototalratio, Total_Revenue))
rev <- rev[c(-3,-4)]

# reshape the dataframe from long to wide for total revenue and NPR
totalrevenue <- reshape2::dcast(rev, a_Name ~ Year, value.var="Total_revenue_est")
NPR <- reshape2::dcast(rev, a_Name ~ Year, value.var="NPR_est")

# Part 3: Impute the 2017 NA based on previous year revenue ------------------------------------------------------
# Target: Only for targets with 2016 data
# Methods: 
#   - for target with only one year data, using Y2016 data as naive forecast/ramdom walk
#   - for target with 2-3 year data, using average growth rate
#   - for target with at least 4 years data, using Grey Forecasting Model GM(1,1)

nrow(totalrevenue[!(is.na(totalrevenue$`2016`)) & (is.na(totalrevenue$`2017`)),])
nrow(NPR[!(is.na(NPR$`2016`)) & (is.na(NPR$`2017`)),])

impute2017missingdata <- function(df){
  # Select records with 2016 data but miss 2017 data
  missingdata <- df[!(is.na(df$`2016`)) & (is.na(df$`2017`)),]
  # convert from wide to long
  missingdata_long <- reshape2::melt(missingdata,
                             id.vars="a_Name",
                             variable.name="Year",
                             value.name="value") %>% tidyr::drop_na()
  
  # consolidate data by HS, do log transformation on the revenue
  data_nest <- missingdata_long %>% 
    mutate(logvalue = log(value)) %>%
    group_by(a_Name) %>%
    tidyr::nest(.key = "data.tbl") %>%
    mutate(logvalue = map(data.tbl,"logvalue"),
           datalen = map(logvalue,length))
  
  # Build model
  data_fit <- data_nest %>%
    mutate(model = map(.x=logvalue, .f=forecasemodel,1),
           predict = map(model,"predict")
          )
  
  # extract the predicted 2017 value
  data_predicted <- data_fit %>%
    tidyr::unnest(predict,.drop = TRUE) %>%
    mutate(predicted = map(preval, exp),
           `2017` = round(as.numeric(predicted),0)) %>%
    select(a_Name, `2017`)
  
  return(data_predicted)
}

totalrevenue_missing_imputed <- impute2017missingdata(totalrevenue)
NPR_missing_imputed <- impute2017missingdata(NPR)

# Combine the forecastng value with the old misisng value tables
totalrevenue$`2017`[match(totalrevenue_missing_imputed$a_Name,totalrevenue$a_Name)] <- totalrevenue_missing_imputed$`2017`
NPR$`2017`[match(NPR_missing_imputed$a_Name,NPR$a_Name)] <- NPR_missing_imputed$`2017`

# Combine all tables
mappingtbl <- distinct(sysmappinglist[,c(1:2,5)]) ##1-HMA name; 2-HMA Member; 5-a_Name
revenuetbl <- combine_tables(mappingtbl,totalrevenue, NPR)

openxlsx::write.xlsx(revenuetbl,"Result/Revenue_1017.xlsx")
write.csv(revenuetbl,"Result/revenue_1017.csv",row.names = FALSE)


# Top 100 HS based on 2017 revenue -------------------------------------------------------------------------------

top100HS2017 <- totalrevenue %>% 
  select(a_Name,`2017`) %>% filter(a_Name != "MEDNAX Services, Inc. (FL)") %>% # remove MEDNAX as a physician service association
  #dplyr::rename(`2017` = total_revenue_2017)
  mutate(rank = min_rank(desc(`2017`))) %>%
  top_n(100,`2017`) %>%
  arrange(rank)

openxlsx::write.xlsx(top100HS2017,"Result/2017top100HS_1017.xlsx")

