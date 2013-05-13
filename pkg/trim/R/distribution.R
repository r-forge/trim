setClass(
  'Distribution',
  representation(
    vector = 'numeric',
    precision = 'numeric'
  ),
  validity = function(object) {
    flag = TRUE
    
    # all values >= 0
    if (flag && any(object@vector < 0)) {
      message("All values of a Distribution object has to be >= 0.")
      print(object@vector)
      flag <- FALSE
    }
    # all values <= 1
    if (flag && any(object@vector > 1)) {
      message("All values of a Distribution object has to be <= 1.")
      print(object@vector)
      flag <- FALSE
    }
    
    # precision > 0 and <= 0.1
    if (flag && (object@precision < 0 || object@precision > 0.1)) {
      message("The precision value has to be in [0;0.1]")
      print(object@precision)
      flag <- FALSE
    }
    
    # sum value == 1 +- precision
    if (flag) {
      prec <- sum(object@vector) - 1
      if ( prec < - object@precision || prec > object@precision) {
        message("Distribution doesn't sum to 1")
        print(object@vector)
        print(1 + prec)
        flag <- FALSE
      }
    }
    
    nam <- names(object@vector)
    # vector has names
    if (flag && is.null(nam)) {
      message("Values of a Distribution object must be named.")
      print(object@vector)
      flag <- FALSE
    }
    # names can't contain NA
    if (flag && any(is.na(nam))) {
      message("Value names of a Distribution can't contain NA.")
      print(object@vector)
      flag <- FALSE
    }
    # names are unique
    if (flag && length(nam) != length(unique(nam))) {
      message("Value names of a Distribution object must be unique.")
      print(object@vector)
      flag <- FALSE
    }
    
    return(flag)
  }
)

setMethod(
  f = 'show',
  signature = c('Distribution'), 
  definition = function (object) {
    message(paste("Valid Distribution object checked with precision", object@precision))
    print(object@vector)
  }
)
setMethod(
  f = 'print',
  signature = c('Distribution'), 
  definition = function (x) {
    show(x)
  }
)

setGeneric("distribution", function(x, weights = rep(1, length(x)), precision = 0.001){ standardGeneric("distribution") })

setMethod(
  f = 'distribution',
  signature = c('numeric'), 
  definition = function (x, weights, precision) {
    if(is.null(names(x)))
      names(x) <- paste('class.', 1:length(x), sep = '')
    
    return(new(
      Class = 'Distribution',
      vector = x,
      precision = precision
    ))
  }
)

# distribution(c(0.5,0.5))
# distribution(c(0,1))
# distribution(c(0.1,1))
# distribution(c(-0.2,8))
# distribution(c(-0.2,1.2))

setMethod(
  f = 'distribution',
  signature = c('character'), 
  definition = function (x, weights, precision) {
    
    count <- numeric()
    nam <- unique(x)
    for (i in nam) {
      count <- c(count, sum(weights[x == i]))
    }
    names(count) <- nam
    count <- count/sum(weights)
    
    return(new(
      Class = 'Distribution',
      vector = count,
      precision = precision
    ))
  }
)

# data(iris)
# species.char <- as.character(iris$Species)
# distribution(species.char)
# distribution(species.char, weights=c(rep(1,50), rep(0,100)))

setMethod(
  f = 'distribution',
  signature = c('factor'), 
  definition = function (x, weights, precision) {
    
    x.char <- as.character(x)
    
    return(
      getMethod('distribution', signature = 'character')(x.char, weights = weights, precision)    
    )
  }
)

# data(iris)
# distribution(iris$Species)
# distribution(iris$Species, weights=c(rep(1,50), rep(0,100)))

distribution.comparable <- function(x, y, quiet = T){
  stopifnot(inherits(x, 'Distribution'))
  stopifnot(inherits(y, 'Distribution'))
  vx <- x@vector
  vy <- y@vector
  
  if(length(vx) != length(vy)) {
    if(!quiet) {
      message("Lenght of x and y differs")
      print(x)
      print(y)
    }
    return(F)
  }
  
  if(!all(names(vx) %in% names(vy))) {
    if(!quiet) {
      message("Some class names in x aren't in y")
      print(x)
      print(y)
    }
    return(F)
  }
  
  if(!all(names(vy) %in% names(vx))) {
    if(!quiet) {
      message("Some class names in y aren't in x")
      print(x)
      print(y)
    }
    return(F)
  }
  
  return(T)    
}

# x <- distribution(c(0.5,0.5))
# y <- distribution(c(0.1,0.9))
# distribution.comparable(x,y)
# x <- distribution(c("well" = 0.5, "bad" = 0.5))
# y <- distribution(c("well" = 0.1, "poor" = 0.9))
# distribution.comparable(x,y)
# distribution.comparable(x,y, quiet = F)


# reorder x according to model
distribution.reorder <- function(x, model, quiet = T){
  stopifnot(inherits(x, 'Distribution'))
  stopifnot(inherits(model, 'Distribution'))
  stopifnot(distribution.comparable(x, model, quiet = quiet))
  
  vx <- x@vector
  vmodel <- model@vector
  vx.nam <- names(vx)
  vmodel.nam <- names(vmodel)
  
  ordering <- match(vx.nam, vmodel.nam)
  
  if(all(ordering == 1:length(vx))) {
    if(!quiet) {
      message("x already correctly ordered according to model")
    }
    return(x)
  } else {
    if(!quiet) {
      message("x was correctly reordered according to model")
    }
    names(vx) <- vx.nam[ordering]
    x@vector <- vx
    validObject(x)
    return(x)
  } 
}

# x <- distribution(c(0.5,0.5))
# y <- distribution(c(0.1,0.9))
# distribution.reorder(x,y)
# x <- distribution(c("well" = 0.5, "poor" = 0.5))
# y <- distribution(c("poor" = 0.1, "well" = 0.9))
# distribution.reorder(x,y)
# distribution.reorder(x,y, quiet = F)