library(openxlsx)
library(dplyr)
library(purrr)
library(magrittr)

source("Code/HS_demo_help_functions.R")

# Read Data
# - Mappling list: the Modern Health HS name and HMA name
#sysmappinglist <- read.xlsx("Data/HMA_Mapping_List.xlsx", sheet = "System")
# - 2011-2017 revenue table
revenue1117 <- read.csv("Result/revenue_1017.csv",header = TRUE)
# Remove 2018 data
revenue1117 <- revenue1117 %>% filter(Year != "2018")

# reshape the dataframe from long to wide for total revenue and NPR
totalrevenue <- reshape2::dcast(revenue1117, a_Name ~ Year, value.var="Total_Revenue")
NPR <- reshape2::dcast(revenue1117, a_Name ~ Year, value.var="NPR")

# make projection for the targets with 2017 figure
totalrevenue_wide <- totalrevenue[!(is.na(totalrevenue$`2017`)),]
npr_wide <- NPR[!(is.na(NPR$`2017`)),]

# convert from wide to long
totalrevenue_long <- reshape2::melt(totalrevenue_wide,
                                   id.vars="a_Name",
                                   variable.name="Year",
                                   value.name="revenue") %>% tidyr::drop_na()
npr_long <- reshape2::melt(npr_wide,
                          id.vars="a_Name",
                          variable.name="Year",
                          value.name="npr") %>% tidyr::drop_na()

# consolidate data by group, make log transformation on the revenue/npr
totalrevenue_nest <- totalrevenue_long %>% 
  mutate(logrevenue = log(revenue)) %>%
  group_by(a_Name) %>%
  tidyr::nest(.key = "data.tbl") %>%
  mutate(logvalue = map(data.tbl,"logrevenue"),
         datalen = map(logvalue,length))

npr_nest <- npr_long %>% 
  mutate(lognpr = log(npr)) %>%
  group_by(a_Name) %>%
  tidyr::nest(.key = "data.tbl") %>%
  mutate(logvalue = map(data.tbl,"lognpr"),
         datalen = map(logvalue,length))

# Build model - forecast 2 periods ahead
totalrevenue_fit <- totalrevenue_nest %>%
  mutate(model = map(.x=logvalue, .f=forecasemodel,2),
         class = map(model, "method"),
         predict = map(model,"predict"),
         error = map(model, "residual"),
         fit = map(model,"fit"),
         actual = map(model,"actual"),
         rmse = map(model,"rmse"),
         precision = map(model,"precision")
  )

npr_fit <- npr_nest %>%
  mutate(model = map(.x=logvalue, .f=forecasemodel,2),
         class = map(model, "method"),
         predict = map(model,"predict"),
         error = map(model, "residual"),
         fit = map(model,"fit"),
         actual = map(model,"actual"),
         rmse = map(model,"rmse"),
         precision = map(model,"precision")
  )

# Extract the predicted value
totalrevenue_predicted <- totalrevenue_fit %>%
  tidyr::unnest(predict,.drop = TRUE) %>%
  mutate(predicted = map(preval, exp)) %>%
  select(a_Name,period,predicted) %>% 
  reshape2::dcast(a_Name ~ period, value.var="predicted") %>%
  rename(Y2018 = "1", Y2019 = "2")

npr_predicted <- npr_fit %>%
  tidyr::unnest(predict,.drop = TRUE) %>%
  mutate(predicted = map(preval, exp)) %>%
  select(a_Name,period,predicted) %>% 
  reshape2::dcast(a_Name ~ period, value.var="predicted") %>%
  rename(Y2018 = "1", Y2019 = "2")

mydf <- left_join(totalrevenue_predicted, npr_predicted, 
                  by = "a_Name", 
                  suffix = c("_totalrevenue", "_npr"))
df <- mydf
mydf[,2:5] <- apply(mydf[,2:5],2,as.numeric)

write.csv(mydf,"Result/revenue_1819forecast_1017.csv",row.names = FALSE)

# Model Accuracy ---------------------------------------------------------------------------------------------------
npr_accuracy <- npr_fit %>%
  select(a_Name,actual, fit) %>% tidyr::drop_na() %>%
  mutate(actual = map(actual,exp),
         fit = map(fit,exp)) %>%
  tidyr::unnest() %>%
  mutate(resid = fit - actual) %>% 
  group_by(a_Name) %>%
  mutate(mad = mean(abs(resid)),
         mape = mean(abs(resid/actual)),
         p = 1 - mape)

write.csv(npr_accuracy,"Result/modelaccuracy_npr_1017.csv", row.names = FALSE)

totrevenue_accuracy <- totalrevenue_fit %>%
  select(a_Name,actual, fit) %>% tidyr::drop_na() %>%
  mutate(actual = map(actual,exp),
         fit = map(fit,exp)) %>%
  tidyr::unnest() %>%
  mutate(resid = fit - actual) %>% 
  group_by(a_Name) %>%
  mutate(mad = mean(abs(resid)),
         mape = mean(abs(resid/actual)),
         p = 1 - mape)

write.csv(totrevenue_accuracy,"Result/modelaccuracy_totalrevenue_1017.csv", row.names = FALSE)
