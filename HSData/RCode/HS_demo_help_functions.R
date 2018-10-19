################################################################################
# Functions for HS Demographic Data - October 2018
################################################################################
source("Code/GM11.R")

# Function to combine total revenue, npr and ,mapping table
combine_tables <- function(mappingtbl, totalrevtbl, nprtbl){
  totalrevenue_long <- reshape2::melt(totalrevtbl,
                                      # ID variables - all the variables to keep but not split apart on
                                      id.vars="a_Name",
                                      variable.name="Year",
                                      value.name="Total_Revenue") %>% tidyr::drop_na()
  NPR_long <- reshape2::melt(nprtbl,
                             id.vars="a_Name",
                             variable.name="Year",
                             value.name="NPR") %>% tidyr::drop_na()
  newdf <- left_join(totalrevenue_long,NPR_long, by = c("a_Name", "Year"))
  newdf <- left_join(newdf,mappingtbl, by = "a_Name")
  return(newdf)
}

# Growth rate model
growthmodel <- function(x,adv=0){
  n <- length(x)
  growthrate <- c()
  for (i in 1:(n-1)){
    growthrate[i] <- (x[i+1]-x[i])/x[i]
  }
  avggrowthrate <- sum(growthrate)/(n-1)
  # save average growth rate to result
  result <- list(method = 'GrowthRate', avg_growth_rate = avggrowthrate)
  # Make prediction
  if (adv > 0) {
    predict <- data.frame(period = 1:adv)
    for (i in 1:adv) {
      pre.adv <- tail(x,1)*(1+avggrowthrate)
      predict$preval[i] <- pre.adv
      x[n+i] <- pre.adv
      #i =+ 1
    }
    result[["predict"]] <- predict
  }
  class(result) <- 'growthmodel'
  return(result)
}

# Naive forecast (random walk) model 
randomwalk <- function(x,adv=0){
  #The forecasts from a random walk are flat and equal to the last observation.
  result <- list(method = 'RandomWalk')
  if (adv > 0) {
    predict <- data.frame(period = 1:adv)
    predict$preval <- x[1]
  }
  result[["predict"]] <- predict
  class(result) <- 'randomwalk'
  return(result)
}

# Wrap all models together
forecasemodel <- function(x,t){
  if(length(x) %in% 2:3) {
    model = growthmodel(x,t)
  } else if (length(x) == 1) {
    model = randomwalk(x,t)
  } else {
    model = GM11(x,t)
  }
}
