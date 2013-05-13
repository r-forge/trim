# notes:
# - we assure here a probability estimator  depends only on the values taken by individuals on on the target variables
# - then, a decision rule has to have the following arguments :
# -- y: the target variable, with length(y) == number of individuals in the node
# -- weights: non-response weighting, with length(weights) == number of individuals in the node

# to ensure compatibility with the 'distribution' methods, a estimator of probability has to support 'character' and 'factor' objects

p.estimator.laplace <- function(y, weights = rep(1, length(y))) {
  stopifnot(length(y) == length(weights))
  
  if(inherits(y, 'factor')) {
    y <- as.character(y)
  }
  
  if(inherits(y, 'character')) {
    classes <- unique(y)
    l <- length(classes)
    n <- sum(weights)
    
    out <- numeric()
    for(i in classes) {
      out <- c(
        out,
        (sum(weights[y==i]) + 1) / (n + l)
      )
    }
    names(out) <- classes
    return(out)
  }
  
  stop(paste("y type not supported:", class(y)))
}

# data(iris)
# p.estimator.laplace(iris$Species)
# p.estimator.laplace(iris$Species, weights = c(rep(0,50),rep(1,100)))