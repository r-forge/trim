findsplit <- function(response, data, weights, qvar.breaks) {
#   print(">> findsplit - in")
  
  ### extract response values from data
  y <- data[[response]]
  
  logpmin <- 0
  xselect <- NULL
  
  ### cycle through all features
  for (i in which(names(data) != response)) {
    
    ### expand data
    x <- data[[i]]
    xt <- rep(x, weights)
    yt <- rep(y, weights)
    
    ### potential split points (not too many)
    qx <- do.call(qvar.breaks, list(xt))
    
    
    ### assess all potential splits by their t-test
    ### log-p-value
    logp <- sapply(qx, function(q) {
      tt <- t.test(yt[xt <= q], yt[xt > q])
      pt(-abs(tt$statistic), tt$parameter, log = TRUE) + log(2)
    })
    
    
#     print(i)
#     print(qx)
#     print(logp)
    
    ### if the best split in variable i significant AND
    ### better than what we already had store this information
    if (min(logp) < logpmin & min(logp) < log(0.05)) {
      logpmin <- min(logp)
      xselect <- i
      splitpoint <- qx[which.min(logp)]
    }
  }
  
  ### no significant split found, give up
  if (is.null(xselect)) return(NULL)
  
  ### return split as partysplit object
  return(partysplit(
    varid = as.integer(xselect),   ### which variable?
    breaks = as.numeric(splitpoint),   ### which split point?
    info = list(pvalue = exp(logpmin)  ### save p-value in addition
    )))
}