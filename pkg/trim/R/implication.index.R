# compute the implication indice for a given node
implication.index <- function(
  y, # character, factor, wvc
  target = decision.rule.majority(y, quiet = quiet),
  h0 = y, # character, factor, wvc, distribution
  quiet=T,
  ...
) {

  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  
  weights <- y@weights
  
  if(!inherits(h0, 'Distribution')) {
    if(inherits(h0, 'factor'))
      h0 <- as.character(h0)
    if(inherits(h0, 'character'))
      h0 <- wvc(h0)
    
    stopifnot(inherits(h0, 'WeightedVariable.categorical'))
    h0 <- distribution(h0)
  } 
  
  distri <- h0@vector
  
  nbar.expected.p <- sum(distri[names(distri) != target])
  nbar.expected.n <- nbar.expected.p * sum(weights)
  
  nbar.observed.n <- sum(weights[y@variable !=target])
  
  out <- nbar.observed.n
  
  imp <- (nbar.observed.n - nbar.expected.n + 0.5)/sqrt(nbar.expected.n)
  
  if(!quiet) {
    message(paste('   Number of individuals:', sum(weights)))
    message(paste('   target:', target))
    message(paste('   Counter-examples expected:', nbar.expected.n))
    message(paste('   Counter-examples observed:', nbar.observed.n))
    message(paste('   Implication index:', imp))
  }
  
  return(imp)
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
# distribution(gilbert.data[gilbert.data$sex == 'man','civil.status'])
# distribution(gilbert.data[gilbert.data$sex == 'woman','civil.status'])
# 
# imp.m <- implication.index(
#   y = gilbert.data[gilbert.data$sex == 'man','civil.status'],
#   h0 = distribution(gilbert.data$civil.status),
#   quiet = F
# )

# imp.w <- implication.index(
#   y = gilbert.data[gilbert.data$sex == 'woman','civil.status'],
#   target =  decision.rule.majority(gilbert.data[gilbert.data$sex == 'woman','civil.status']),
#   h0 = distribution(gilbert.data$civil.status),
#   quiet = F
# )
# 
# (
#   nrow(gilbert.data[gilbert.data$sex == 'man',]) * imp.m + 
#   nrow(gilbert.data[gilbert.data$sex == 'woman',]) * imp.w
# ) / 273


# split evaluator
implication.index.gain <- function(
  y, # character, factor, wvc
  kidsids,
  decision.rule = decision.rule.majority,
  h0 = y, # character, factor, wvc, distribution
  aggregation.rule = aggregation.rule.weighted.mean,
  quiet = T,
  rquiet = T
){
  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  
  weights <- y@weights
  
  if(!inherits(h0, 'Distribution')) {
    if(inherits(h0, 'factor'))
      h0 <- as.character(h0)
    if(inherits(h0, 'character'))
      h0 <- wvc(h0)
    
    stopifnot(inherits(h0, 'WeightedVariable.categorical'))
    h0 <- distribution(h0)
  } 
  
  stopifnot(length(y) == length(kidsids))
  
  parent.utility <- implication.index(
    y = y,
    target = decision.rule(y),
    h0 = h0,
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
      implication.index(
        y = y[ids],
        target = decision.rule(y = y[ids]),
        h0 = h0,
        weights = weights,
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
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   h0 = gilbert.data$civil.status
# )
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132))
# )
# 
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.maximum
# )
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   aggregation.rule = aggregation.rule.minimum
# )
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132))
# )
# kidsids.sector <- rep(0, nrow(gilbert.data))
# kidsids.sector[gilbert.data$working.sector == 'primary'] <- 1
# kidsids.sector[gilbert.data$working.sector == 'secondary'] <- 2
# kidsids.sector[gilbert.data$working.sector == 'tertiary'] <- 3
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = kidsids.sector,
#   h0 = distribution(gilbert.data$civil.status)
# )
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   h0 = distribution(gilbert.data$civil.status),
#   decision.rule = decision.rule.married
# )
# implication.index(
#   y = gilbert.data$civil.status,
#   h0 = distribution(gilbert.data$civil.status),
#   quiet = F
# )
# implication.index.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   h0 = distribution(gilbert.data$civil.status),
#   aggregation.rule = aggregation.rule.information.gain
# )