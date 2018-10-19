# Function for GM(1,1) Grey Model

# Main Function -------------------------------------------------------------------------------
GM11 <- function(
  # Args:
  x, # a matrix
  adv = 0 # Predict period
  # Returns:
  #   actual- Original Data,
  #   fit - fitted data,
  #   degree,
  #   C,
  #   P,
  #   predict - predicted value
) {
  x0 <- x
  k = length(x0)
  # AGO
  x1 = cumsum(x0)
  # construct matrix B & Y
  B = cbind(-0.5*(x1[-k]+x1[-1]),rep(1,times=k-1))
  Y = x0[-1]
  # compute BtB, BtB(-1), BtY
  BtB = t(B)%*%B
  BtB.inv = solve(BtB)
  BtY = t(B)%*%Y
  # estimate
  alpha = BtB.inv%*%BtY
  
  # construct equation
  predict <- function(k) {
    y = (x0[1] - alpha[2]/alpha[1])*exp(-alpha[1]*k)+alpha[2]/alpha[1]
    return(y)
  }
  # x1 predicted
  pre <- sapply(X=0:(k-1),FUN=predict)
  # x0 predicted (difference between x1(i-1) and x1(i-2))
  prediff <- c(pre[1],diff(pre))
  # residual test
  error <- round(abs(prediff-x0),digits=6)
  emax <- max(error)
  emin <- min(error)
  rmse <- sqrt(mean(error^2))
  mae <- mean(error)
  mape <- mean(abs((prediff - x0)/x0))*100
  precision <- 100 - mape
  # model evaluation - Degree of Grey incidence
  incidence <- function(x) {
    return((emin + 0.5*emax)/(x+0.5*emax))
  }
  degree <- mean(sapply(error,incidence))
  # C and P Criteria
  s1 <- sqrt(sum((x0-mean(x0))^2)/(length(x0)-1)) # variance for x0
  s2 <- sqrt(sum((error-mean(error))^2)/(length(x0)-1)) # residual variance
  # variance ratio: C
  C <- s2/s1
  # calculate P
  p <- length(abs(error-mean(error)) < 0.6745*s1)/length(error)
  # Save result
  result <- list(method = 'GM11',
                 actual = x0,
                 fit = prediff,
                 residual = round(prediff-x0,6),
                 rmse = rmse,
                 mae = mae,
                 mape = mape,
                 precision = precision,
                 degree = degree,
                 C = C,
                 P = p)
  
  # predict item k+adv
  if (adv > 0) {
    predictval <- data.frame(period = 1:adv)
    for (i in 1:adv) {
      pre.adv <- predict(k+i-1)-predict(k+i-2)
      predictval$preval[i] <- pre.adv
    }
    result[["predict"]] <- predictval
  }
  
  class(result) <- 'GM1.1'
  return(result)
}

# Print result ----------------------------------------------------------------------------------------------
print.GM1.1 <- function(mod){
  cat('The result of GM(1,1)\n')
  cat('Actual Data:', '\n',mod$actual ,'\n')
  cat('Fit Data:', '\n',round(mod$fit,2) ,'\n')
  cat('Residual:', '\n', round(mod$residual,2), '\n')
  cat('RMSE:', round(mod$rmse,2), '\n')
  cat('MAE:', round(mod$mae,2), '\n')
  cat('MAPE:', round(mod$mape,2), '%\n')
  cat('Precision:', round(mod$precision,2), '%\n')
  cat('Degree:', round(mod$degree,3) ,'\n')
  cat('C:', round(mod$C,3) ,'\n')
  cat('P:', round(mod$P,3) ,'\n')
  
  if(mod$C < 0.35){     
    cat("C < 0.35, GM(1,1) Forecasting Grade: Excellent",'\n','\n')
  }else{
    if(mod$C < 0.5){
      cat("C within [0.35,0.5), GM(1,1) Forecasting Grade: Good",'\n','\n')
    }else{
      if(mod$C < 0.65){
        cat("C within [0.5,0.65), GM(1,1) Forecasting Grade: Qualified",'\n','\n')
      }else{
        cat("C >= 0.65, GM(1,1) Forecasting Grade: Unqualified",'\n','\n')
      }
    }
  }
  
  if (!is.null(mod$predict)) {
    cat('Predict Data:', '\n')
    print(mod$predict)
  }
}

# Plot Function ---------------------------------------------------------------------------------------------
plot.GM1.1 <- function(mod,adv=5){
  prex <- numeric(adv)
  x <- mod$actual
  for (k in 1:adv){
    prex[k] <- GM11(x,k)$predict    
  }
  
  value = c(x,prex)
  
  res <- data.frame(index = 1:length(value),
                    value = value,
                    type = factor(c(rep(1,length(x)),rep(2,length(prex)))))
  library(ggplot2)
  p <- ggplot(res,aes(x=index,y= value))
  p + geom_point(aes(color=type),size=3)+ 
    geom_path(linetype=2) +
    theme_bw()
  return(p)
}
