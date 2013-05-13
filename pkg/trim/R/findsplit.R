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

findsplit.shannon <- function(response, data, weights, qvar.breaks) {
  #   print(">> findsplit - in")
  
  ### extract response values from data
  y <- data[[response]]
  
  xselect <- NULL
  
  ### cycle through all features
  for (i in which(names(data) != response)) {
    
    ### expand data
    x <- data[[i]]
#     xt <- rep(x, weights)
#     yt <- rep(y, weights)
    
    ### potential split points (not too many)
    qx <- do.call(qvar.breaks, list(x)) #not weighted
    
    
    ### assess all potential splits by their t-test
    ### log-p-value
    logp <- sapply(qx, function(q) {
      tt <- t.test(yt[xt <= q], yt[xt > q])
      entropy.shannon()
    })
    
    
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

entropy.Shannon <- function(distribution, contribution = FALSE) {
  
  p <- distribution
  if (length(p) == 1) p[2] <- 1 - p[1]
  
  ### Pre-processing
  if ((sum(p) < 0.99) | (sum(p) > 1.01)) stop("entropy.Shannon: sum(distribution) != 1")
  # we need to allow a little margin due to round approximation errors
  
  ### Body
  if (!contribution) {
    result <- - sum(p * sapply(p, log2), na.rm = TRUE)
  }
  else {
    result <- c()
    for (i in 1:length(p)) {
      if (p[i] == 0) result[i] <- 0
      else result[i] <- - p[i] * log2(p[i])
    }
  }
  
  ### Post-processing
  if (is.null(result)) stop("entropy.Shannon: A problem happened: unable to compute the entropy")
  # if ((result >= 0) & (result <= 1)) return(result)
  # else stop("entropy.Shannon: A problem happened: entropy not in [0;1]")
  # ==> Shannon entropy is in [0;1] only for a two class problem
  
  return(result)
}