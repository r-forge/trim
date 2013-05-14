setGeneric("entropy.marcellin", function(
  y,
  h0, # h0 same type as y
  p.estimator = NA,
  quiet = T
){ standardGeneric("entropy.marcellin") })


setMethod(
  f = 'entropy.marcellin',
  signature = c('Distribution', 'Distribution'), 
  definition = function (y, h0, p.estimator, quiet) {
    
    stopifnot(distribution.comparable(y, h0))
    h0 <- distribution.reorder(h0, y)
    
    distrib <- y@vector
    distrib.ini <- h0@vector
    
    nclass <- length(distrib)
    entropy <- 0
    for(i in 1:nclass) {
      entropy <- entropy +
        (distrib[i] * (1 - distrib[i])) /
        ((-2 * distrib.ini[i] + 1)*distrib[i] + distrib.ini[i]^2)
    }
    entropy <- 1/nclass * entropy
    names(entropy) <- NULL # entropy keep the name of the first class, we remove it
    
    if(!quiet) {
      message(paste('   Number of individuals:', sum(weights)))
      message(paste('   Distribution: ', paste("(", paste(distrib, collapse = ', '), ")", sep = '')))
      message(paste('   Marcellin entropy:', entropy))
    }
    
    return(entropy)
  }
)

# entropy.marcellin(
#   y = distribution(c(0.5,.5)),
#   h0 = distribution(c(0.1,.9))
# )
# entropy.marcellin(
#   y = distribution(c(0.1,.9)),
#   h0 = distribution(c(0.1,.9))
# )
# entropy.marcellin(
#   y = distribution(c(0.5,.5)),
#   h0 = distribution(c(0.5,.5))
# )
# entropy.marcellin(
#   y = distribution(c(0.1,.9)),
#   h0 = distribution(c(0.4,.6))
# )
# entropy.marcellin(
#   y = distribution(c(0.9,.1)),
#   h0 = distribution(c(0.4,.6))
# )

setMethod(
  f = 'entropy.marcellin',
  signature = c('numeric', 'numeric'), 
  definition = function (y, h0, p.estimator, quiet) {

    entropy <- getMethod(
      'entropy.marcellin',
      signature = c('Distribution', 'Distribution'))(
        y = distribution(y, p.estimator = p.estimator),
        h0 = distribution(h0, p.estimator = p.estimator),
        quiet = quiet
      )

    return(entropy)
  }
)

# entropy.marcellin(
#   y = c(0.1,0.9),
#   h0 = c(0.4,0.6)
# )
# entropy.marcellin(
#   y = c(0.9,0.1),
#   h0 = c(0.4,0.6)
# )


setMethod(
  f = 'entropy.marcellin',
  signature = c('character', 'character'), 
  definition = function (y, h0, p.estimator, quiet) {
    
    entropy <- getMethod(
      'entropy.marcellin',
      signature = c('Distribution', 'Distribution'))(
        y = distribution(y, p.estimator = p.estimator),
        h0 = distribution(h0, p.estimator = p.estimator),
        quiet = quiet
      )
    
    return(entropy)
  }
)

# data(iris)
# entropy.marcellin(
#   y = as.character(iris$Species),
#   h0 = as.character(iris$Species)
# )

setMethod(
  f = 'entropy.marcellin',
  signature = c('factor', 'factor'), 
  definition = function (y, h0, p.estimator, quiet) {
    
    entropy <- getMethod(
      'entropy.marcellin',
      signature = c('Distribution', 'Distribution'))(
        y = distribution(y, p.estimator = p.estimator),
        h0 = distribution(h0, p.estimator = p.estimator),
        quiet = quiet
      )
    
    return(entropy)
  }
)

# data(iris)
# entropy.marcellin(
#   y = iris$Species,
#   h0 = iris$Species
# )

setMethod(
  f = 'entropy.marcellin',
  signature = c('WeightedVariable.categorical', 'WeightedVariable.categorical'), 
  definition = function (y, h0, p.estimator, quiet) {
    
    entropy <- getMethod(
      'entropy.marcellin',
      signature = c('Distribution', 'Distribution'))(
        y = distribution(y, p.estimator = p.estimator),
        h0 = distribution(h0, p.estimator = p.estimator),
        quiet = quiet
      )
    
    return(entropy)
  }
)


# data(iris)
# entropy.marcellin(
#   y = wvc(iris$Species),
#   h0 = wvc(iris$Species)
# )


# ----------------------------------------------
# split evaluator
# ----------------------------------------------

entropy.marcellin.gain <- function(
  y,
  kidsids,
  decision.rule = decision.rule.majority,
  h0,
  aggregation.rule = aggregation.rule.weighted.mean,
  p.estimator = NA,
  quiet = T,
  rquiet = T
){
  stopifnot(is.na(p.estimator) || is.function(p.estimator))
  
  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  stopifnot(length(y) == length(kidsids))
  
  if(inherits(h0, 'factor'))
    h0 <- as.character(h0)
  if(inherits(h0, 'character'))
    h0 <- wvc(h0)
  stopifnot(inherits(h0, 'WeightedVariable.categorical'))
  stopifnot(length(y) == length(kidsids))
  
  
  parent.utility <- entropy.marcellin(
    y = y,
    h0 = h0,
    p.estimator = p.estimator,
    quiet = rquiet
  )

  kids <- unique(kidsids)
  kids.values <- numeric()
  kids.weights <- numeric()
  
  for (i in kids) {

    ids <- kidsids == i
    
    kids.values <- c(
      kids.values,
      entropy.marcellin(
        y = y[ids],
        h0 = h0,
        p.estimator = p.estimator,
        quiet = rquiet
      )
    )
  }
  names(kids.values) <- kids
  
  
  out <- aggregation.rule(
    y = y,
    kidsids = kidsids,
    parent.utility = parent.utility,
    kids.utility = kids.values
  )
  
  if(!quiet) {
    message(paste('   Number of individuals:', sum(y@weights)))
    message(paste('   Parent distribution: ', floatprint(distribution(y)@vector), sep = ''))
    message(paste('   Parent utility:', parent.utility))
    message(paste('   Kids utility:', floatprint(kids.values), sep = ''))
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
# 
# gilbert.data <- expand.array(gilbert.array)
# gilbert.data
# names(gilbert.data)

# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   h0 = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.information.gain,
#   quiet = F,
#   rquiet = T
# )
# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.maximum
# )
# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.minimum
# )
# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132))
# )
# kidsids.sector <- rep(0, nrow(gilbert.data))
# kidsids.sector[gilbert.data$working.sector == 'primary'] <- 1
# kidsids.sector[gilbert.data$working.sector == 'secondary'] <- 2
# kidsids.sector[gilbert.data$working.sector == 'tertiary'] <- 3
# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   kidsids = kidsids.sector
# )
# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132))
# )
# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.information.gain,
#   quiet = F
# )