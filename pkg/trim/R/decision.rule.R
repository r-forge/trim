# notes:
# - we assure here a decision for a node depends only on the values taken by individuals of the node on the target variables
# - then, a decision rule has to have the following arguments :
# -- y: the target variable, with length(y) == number of individuals in the node
# - a decision rule return the name of the class which won

decision.rule.majority <- function(
  y, # a wvc, a factor, a character
  quiet=T
) {

  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  
  distri <- distribution(y)@vector
  m <- max(distri)
  m.which <- which(distri == m)
  
  decision <- names(distri)[m.which[1]]
  
  if(length(m.which) > 1 && !quiet) {    
    message(paste("   [decision.rule.majority] There are more than one maximal value, the decision is given to the first match: '", decision, "', but '", paste(names(distri)[m.which[-1]], collapse = "', '"), " 'could also be choose instead.", sep = ''))
  }
  
  if(!quiet)
    message(paste('   target:', decision))
  
  return(decision)
}

# data(iris)
# species <- iris$Species
# decision.rule.majority(species)
# decision.rule.majority(species, quiet=F)
# decision.rule.majority(wvc(species, weights = c(rep(0,50), rep(1,100))), quiet=F)
# decision.rule.majority(wvc(species, weights = c(rep(0,50), rep(1,50), rep(2,50))), quiet=F)

decision.rule.minority <- function(
  y,
  quiet=T
) {
  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  
  distri <- distribution(y)@vector
  m <- min(distri)
  m.which <- which(distri == m)
  
  decision <- names(distri)[m.which[1]]
  
  if(length(m.which) > 1 && !quiet) {    
    message(paste("   [decision.rule.minority] There are more than one minimal value, the decision is given to the first match: '", decision, "', but '", paste(names(distri)[m.which[-1]], collapse = "', '"), " 'could also be choose instead.", sep = ''))
  }
  
  if(!quiet)
    message(paste('   target:', decision))
  
  return(decision)
}

#FIXME : here we return a class is 0 observation...
# data(iris)
# species <- iris$Species
# decision.rule.minority(species)
# decision.rule.minority(species, quiet=F)
# decision.rule.minority(wvc(species, weights = c(rep(0,50), rep(1,100))), quiet=F)
# decision.rule.minority(wvc(species, weights = c(rep(0,50), rep(1,50), rep(2,50))), quiet=F)

decision.rule.married <- function(
  y,
  quiet=T
) {
  if(inherits(y, 'factor'))
    y <- as.character(y)
  if(inherits(y, 'character'))
    y <- wvc(y)
  stopifnot(inherits(y, 'WeightedVariable.categorical'))
  
  decision <- 'married'
  
  distri <- distribution(y)@vector
  stopifnot(decision %in% names(distri))
  
  if(!quiet)
    message(paste('   target:', decision))
  
  return(decision)
}

# decision.rule.married(gilbert.data$civil.status)