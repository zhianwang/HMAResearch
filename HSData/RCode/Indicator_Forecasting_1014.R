library(dplyr)
library(purrr)
library(magrittr)

source("Code/GM11.R")

allhs <- read.csv("Result/allhs1116_1017.csv") %>% select(-c(5,6,9,11:14,17:21))
hma <- read.csv("Result/hmamember1116_1017.csv") %>% select(-c(5,6,9,11:14,17:21))
top100 <- read.csv("Result/top1001116_1017.csv") %>% select(-c(1,6,7,10,12:15,18:22))
hmatop100 <- read.csv("Result/hmatop1001116_1017.csv") %>% select(-c(1,6,7,10,12:15,18:22))
top100VA <- read.csv("Result/top100VA1116_1017.csv") %>% select(-c(1,6,7,10,12:15,18:22))

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

top100_summary <- top100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

hmatop100_summary <- hmatop100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

top100va_summary <- top100VA %>%
  group_by(Year) %>%
  summarise_if(is.numeric,mean)

allhs_predicted <- indicatorGM11(allhs_summary)
allhs_predicted$Scope <- "All HS"
hma_predicted <- indicatorGM11(hma_summary)
hma_predicted$Scope <- "HMA"
top100_predicted <- indicatorGM11(top100_summary)
top100_predicted$Scope <- "Top 100"
hmatop100_predicted <- indicatorGM11(hmatop100_summary)
hmatop100_predicted$Scope <- "HMAinTop100"
top100va_predicted <- indicatorGM11(top100va_summary)
top100va_predicted$Scope <- "Top 100 + VA"

mydf <- bind_rows(allhs_predicted,hma_predicted,top100_predicted,top100va_predicted,hmatop100_predicted)

write.csv(mydf, "Result/indicatior_1718_1017.csv",row.names = FALSE)

# Part 2: Forecast the sum ----------------------------------------------------------------------------------------
allhs_sum <- allhs %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

hma_sum <- hma %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

top100_sum <- top100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

hmatop100_sum <- hmatop100 %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

top100va_sum <- top100VA %>%
  group_by(Year) %>%
  summarise_if(is.numeric,sum)

allhs_predicted <- indicatorGM11(allhs_sum,descale="Y")
allhs_predicted$Scope <- "All HS"
hma_predicted <- indicatorGM11(hma_sum,"Y")
hma_predicted$Scope <- "HMA"
top100_predicted <- indicatorGM11(top100_sum,"Y")
top100_predicted$Scope <- "Top 100"
hmatop100_predicted <- indicatorGM11(hmatop100_sum,"Y")
hmatop100_predicted$Scope <- "HMAinTop100"
top100va_predicted <- indicatorGM11(top100va_sum,"Y")
top100va_predicted$Scope <- "Top 100 + VA"

mysum <- bind_rows(allhs_predicted,hma_predicted,top100_predicted,top100va_predicted,hmatop100_predicted)

write.csv(mysum, "Result/indicatiorsum_1718_1017.csv",row.names = FALSE)
