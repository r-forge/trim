setGeneric("entropy.marcellin", function(
  y,
  weights = rep(1, length(y)),
  h0,
  quiet = T
){ standardGeneric("entropy.marcellin") })


setMethod(
  f = 'entropy.marcellin',
  signature = c('Distribution'), 
  definition = function (y, weights, h0, quiet) {
    
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
  signature = c('numeric'), 
  definition = function (y, weights, h0, quiet) {

    entropy <- getMethod(
      'entropy.marcellin',
      signature = c('Distribution'))(
        y = distribution(y, weights),
        weights = weights,
        h0 = distribution(h0, weights),
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



# setMethod(
#   f = 'entropy.marcellin',
#   signature = c('character'), 
#   definition = function (x, weights, quiet) {
#     
#     if(missing(weights))
#       weights <- rep(1, length(x))
#     
#     distrib <- distribution(x, weights)
#     
#     entropy <- getMethod('entropy.marcellin', signature = 'Distribution')(distrib, weights, quiet)    
#     
#     return(entropy)
#   }
# )

# setMethod(
#   f = 'entropy.marcellin',
#   signature = c('factor'), 
#   definition = function (x, weights, quiet) {
#     
#     if(missing(weights))
#       weights <- rep(1, length(x))
#     
#     stopifnot(length(x) == length(weights))
#     
#     distrib <- distribution(x, weights)
#     
#     entropy <- getMethod('entropy.marcellin', signature = 'Distribution')(distrib, weights, quiet)
#     
#     return(entropy)
#   }
# )
# 
# # data(iris)
# # entropy.marcellin(iris$Species)
# 
# 
# 
# 
# # ----------------------------------------------
# # split evaluator
# # ----------------------------------------------
# 
# entropy.marcellin.gain <- function(
#   y,
#   kidsids,
#   weights = rep(1, length(y)),
#   decision.rule = decision.rule.majority,
#   h0 = distribution(y),
#   aggregation.rule = aggregation.rule.weighted.mean,
#   quiet = T,
#   rquiet = T
# ){
#   stopifnot(length(y) == length(weights))
#   
#   parent.utility <- entropy.marcellin(
#     x = y,
#     weights = weights,
#     quiet = rquiet
#   )
#   
#   kids <- unique(kidsids)
#   kids.values <- numeric()
#   kids.weights <- numeric()
#   
#   for (i in kids) {
#     ids <- kidsids == i
#     
#     kids.values <- c(
#       kids.values,
#       entropy.marcellin(
#         x = y[ids],
#         weights = weights[ids],
#         quiet = rquiet
#       )
#     )
#   }
#   names(kids.values) <- kids
#   
#   
#   out <- aggregation.rule(
#     y = y,
#     kidsids = kidsids,
#     weights = weights,
#     parent.utility = parent.utility,
#     kids.utility = kids.values
#   )
#   
#   if(!quiet) {
#     message(paste('   Number of individuals:', sum(weights)))
#     message(paste('   Parent distribution: ', paste("(", paste(distribution(y)@vector, collapse = ', '), ")", sep = '')))
#     message(paste('   Parent utility:', parent.utility))
#     message(paste('   Kids utility:', paste("(", paste(kids.values, collapse = ', '), ")", sep = '')))
#     message(paste('   Aggregation:', out))
#     
#   }
#   
#   return(out)
# }
# 
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
# entropy.marcellin.gain(
#   y = gilbert.data$civil.status,
#   kidsids = c(rep(1, 141), rep(2, 132)),
#   quiet = F,
#   rquiet = F
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