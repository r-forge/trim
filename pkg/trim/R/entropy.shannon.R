setGeneric("entropy.shannon", function(
  x,
  weights = rep(1, length(x)),
  quiet = T
){ standardGeneric("entropy.shannon") })

setMethod(
  f = 'entropy.shannon',
  signature = c('Distribution'), 
  definition = function (x, weights, quiet) {
    
    distrib <- x@vector
    
    nclass <- length(distrib)
    log.pi <- log(distrib, base = 2)
    mean.log.pi <- distrib * log.pi
    mean.log.pi[which(is.nan(mean.log.pi))] <- 0
    entropy <- -sum(mean.log.pi)
    
    if(!quiet) {
      message(paste('   Number of individuals:', sum(weights)))
      message(paste('   Distribution: ', paste("(", paste(distrib, collapse = ', '), ")", sep = '')))
      message(paste('   Shannon entropy:', entropy))
    }
    
    return(entropy)
  }
)


setMethod(
  f = 'entropy.shannon',
  signature = c('character'), 
  definition = function (x, weights, quiet) {
    
    if(missing(weights))
      weights <- rep(1, length(x))
    
    distrib <- distribution(x, weights)
    
    entropy <- getMethod('entropy.shannon', signature = 'Distribution')(distrib, weights, quiet)    
    
    return(entropy)
  }
)


setMethod(
  f = 'entropy.shannon',
  signature = c('factor'), 
  definition = function (x, weights, quiet) {
    
    if(missing(weights))
      weights <- rep(1, length(x))
    
    stopifnot(length(x) == length(weights))
    
    distrib <- distribution(x, weights)
    
    entropy <- getMethod('entropy.shannon', signature = 'Distribution')(distrib, weights, quiet)    
    
    return(entropy)
  }
)

# data(iris)
# entropy.shannon(iris$Species)

setMethod(
  f = 'entropy.shannon',
  signature = c('numeric'), 
  definition = function (x, weights, quiet) {
    
    distrib <- distribution(x, weights)
    
    entropy <- getMethod('entropy.shannon', signature = 'Distribution')(distrib, weights, quiet)    
    
    return(entropy)
  }
)

# entropy.shannon(distribution(c(0.5,.5)))
# entropy.shannon(c(0,1))
# entropy.shannon(c(1,0))





# ----------------------------------------------
# split evaluator
# ----------------------------------------------

entropy.shannon.gain <- function(
  y,
  kidsids,
  weights = rep(1, length(y)),
  decision.rule = decision.rule.majority,
  h0 = distribution(y),
  aggregation.rule = aggregation.rule.weighted.mean,
  quiet = T,
  rquiet = T
){
  stopifnot(length(y) == length(weights))
  
  parent.utility <- entropy.shannon(
    x = y,
    weights = weights,
    quiet = rquiet
  )
  
  kids <- unique(kidsids)
  kids.values <- numeric()
  kids.weights <- numeric()
  
  for (i in kids) {
    ids <- kidsids == i
    
    kids.values <- c(
      kids.values,
      entropy.shannon(
        x = y[ids],
        weights = weights[ids],
        quiet = rquiet
      )
    )
  }
  names(kids.values) <- kids
  
  
  out <- aggregation.rule(
    y = y,
    kidsids = kidsids,
    weights = weights,
    parent.utility = parent.utility,
    kids.utility = kids.values
  )
  
  if(!quiet) {
    message(paste('   Number of individuals:', sum(weights)))
    message(paste('   Parent distribution: ', paste("(", paste(distribution(y)@vector, collapse = ', '), ")", sep = '')))
    message(paste('   Parent utility:', parent.utility))
    message(paste('   Kids utility:', paste("(", paste(kids.values, collapse = ', '), ")", sep = '')))
    message(paste('   Aggregation:', out))
    
  }
  
  return(out)
}

# gilbert.array <- array(
#   data = c(50,5,5,40,5,8,6,12,10,0,50,6,14,30,2,10,18,2),
#   dim = c(3,3,2),
#   dimnames = list(
#     'civil status' = c('married', "single", 'divorced/widowed'),
#     'working sector' = c('primary', 'secondary', 'tertiary'), 
#     'sex' = c('man', 'woman')
#   )
# )
# gilbert.data <- expand.array(gilbert.array)
# gilbert.data
# names(gilbert.data)
# 
# entropy.shannon.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   quiet = F,
#   rquiet = F
# )
# entropy.shannon.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.maximum
# )
# entropy.shannon.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.minimum
# )
# entropy.shannon.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132))
# )
# kidsids.sector <- rep(0, nrow(gilbert.data))
# kidsids.sector[gilbert.data$working.sector == 'primary'] <- 1
# kidsids.sector[gilbert.data$working.sector == 'secondary'] <- 2
# kidsids.sector[gilbert.data$working.sector == 'tertiary'] <- 3
# entropy.shannon.gain(
#   y = gilbert.data$civil.status,
#   kidsids = kidsids.sector
# )
# entropy.shannon.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132))
# )
# entropy.shannon.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.information.gain,
#   quiet = F
# )