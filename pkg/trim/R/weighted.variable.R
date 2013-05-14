setClass(
  'WeightedVariable.categorical',
  representation(
    variable = 'character',
    weights = 'numeric',
    infos = 'list'
  ),
  validity = function(object) {
    flag = TRUE
    
    # same length for variable and weights
    if (flag && (length(object@variable) != length(object@weights))) {
      message("'variable and 'weights' must have same length.")
      print(length(object@variable))
      print(length(object@weights))
      flag <- FALSE
    }
    
    # all weights > 0
    if (flag && any(object@weights < 0)) {
      message("All elements in 'weights' must be >= 0")
      print(length(object@weights))
      flag <- FALSE
    }
    
    # no NA in weights
    if (flag && any(is.na(object@weights))) {
      message("'weights' argument can't contain NA")
      print(length(object@weights))
      flag <- FALSE
    }

    return(flag)
  }
)

setMethod(
  f = 'show',
  signature = c('WeightedVariable.categorical'), 
  definition = function (object) {
    print(data.frame(
      'variable' = object@variable,
      'weights' = object@weights
    ))
  }
)
setMethod(
  f = 'print',
  signature = c('WeightedVariable.categorical'), 
  definition = function (x) {
    show(x)
  }
)


setGeneric("wvc", function(
  variable,
  weights = rep(1, length(variable)),
  infos = list()
){ standardGeneric("wvc") })

setMethod(
  f = 'wvc',
  signature = c('character'),
  definition = function(variable, weights, infos){
    return(new(
      'WeightedVariable.categorical',
      variable = variable,
      weights = weights,
      infos = infos
    ))
  }
)
# data(iris)
# a <- wvc(as.character(iris$Species))
setMethod(
  f = 'wvc',
  signature = c('factor'),
  definition = function(variable, weights, infos){
    return(new(
      'WeightedVariable.categorical',
      variable = as.character(variable),
      weights = weights,
      infos = infos
    ))
  }
)

# data(iris)
# a <- wvc(iris$Species)
wv.variable <- function(x) {
  stopifnot(
    inherits(x, 'WeightedVariable.categorical') ||
      inherits(x, 'WeightedVariable.numeric')
  )
  return(x@variable)
}
wv.weights <- function(x) {
  stopifnot(
    inherits(x, 'WeightedVariable.categorical') ||
      inherits(x, 'WeightedVariable.numeric')
  )
  return(x@weights)
}
wv.infos <- function(x) {
  stopifnot(
    inherits(x, 'WeightedVariable.categorical') ||
      inherits(x, 'WeightedVariable.numeric')
  )
  return(x@infos)
}
# data(iris)
# a <- wvc(iris$Species)
# wv.variable(a)
# wv.weights(a)
# wv.infos(a)

