# notes:
# - we assure here a decision for a node depends only on the values taken by individuals of the node on the target variables
# - then, a decision rule has to have the following arguments :
# -- y: the target variable, with length(y) == number of individuals in the node
# -- kidsids: 
# -- weights: non-response weighting, with length(weights) == number of individuals in the node
# -- parent.utility = NA,
# -- kids.utility
# - a decision rule return the name of the class which won

aggregation.rule.maximum <- function(
  y,
  kidsids,
  parent.utility = NA,
  kids.utility
) {
  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  stopifnot(length(y) == length(kidsids))
  
  kids <- unique(kidsids)
  stopifnot(length(kids) == length(kids.utility))
  
  kids.names <- names(kids.utility)
  stopifnot(all(kidsids %in% kids.names))
  
  out <- max(kids.utility)
  return(out)
}

# data(iris)
# species <- iris$Species
# aggregation.rule.maximum(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 3, '2' = 0)
# )
# aggregation.rule.maximum(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 2, '2' = 1)
# )

aggregation.rule.minimum <- function(
  y,
  kidsids,
  parent.utility = NA,
  kids.utility
) {
  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  stopifnot(length(y) == length(kidsids))
  
  kids <- unique(kidsids)
  stopifnot(length(kids) == length(kids.utility))
  
  kids.names <- names(kids.utility)
  stopifnot(all(kidsids %in% kids.names))
  
  out <- min(kids.utility)
  return(out)
}

# data(iris)
# species <- iris$Species
# aggregation.rule.minimum(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 3, '2' = 0)
# )
# aggregation.rule.minimum(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 2, '2' = 1)
# )

aggregation.rule.weighted.mean <- function(
  y,
  kidsids,
  parent.utility = NA,
  kids.utility
) {
  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  stopifnot(length(y) == length(kidsids))
  
  kids <- unique(kidsids)
  stopifnot(length(kids) == length(kids.utility))
  
  weights <- y@weights
  
  kids.names <- names(kids.utility)
  stopifnot(all(kidsids %in% kids.names))
  
  kids.weights <- numeric()
  for(i in as.numeric(kids.names)) {
    kids.weights <- c(kids.weights, sum(weights[kidsids == i]))
  }
  
  out <- sum(kids.weights * kids.utility)/sum(kids.weights)
  return(out)
}

# data(iris)
# species <- iris$Species
# aggregation.rule.weighted.mean(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 3, '2' = 0)
# )
# aggregation.rule.weighted.mean(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 2, '2' = 1)
# )

aggregation.rule.information.gain <- function(
  y,
  kidsids,
  parent.utility,
  kids.utility
) {
  out <- aggregation.rule.weighted.mean(
    y = y,
    kidsids = kidsids,
    parent.utility = NA,
    kids.utility = kids.utility
  )
  
  out <- out - parent.utility
  
  return(out)
}

# data(iris)
# species <- iris$Species
# aggregation.rule.information.gain(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 3, '2' = 0),
#   parent.utility = 0
# )
# aggregation.rule.information.gain(
#   y = species,
#   kidsids = c(rep(1, 50), rep(2,100)),
#   kids.utility = c('1' = 3, '2' = 0),
#   parent.utility = 1
# )