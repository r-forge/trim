growtree <- function(
  id = 1L,
  response,
  data,
  weights,
  qvar.breaks 
) {
  
  ### for less than 30 obs. stop here
  if (sum(weights) < 30) return(partynode(id = id))
  
  ### find best split
  sp <- findsplit(response, data, weights, qvar.breaks = qvar.breaks)
  ### no split found, stop here
  if (is.null(sp)) return(partynode(id = id))
  
  ### actually split the data
  kidids <- kidids_split(sp, data = data)
  
  ### set-up all daugther nodes
  kids <- vector(mode = "list", length = max(kidids))
  for (kidid in 1:max(kidids)) {
    ### select obs for current node
    w <- weights
    w[kidids != kidid] <- 0
    ### get next node id
    if (kidid > 1) {
      myid <- max(nodeids(kids[[kidid - 1]]))
    } else {
      myid <- id
    }
    ### start recursion on this daugther node
    kids[[kidid]] <- growtree(id = as.integer(myid + 1), response, data, w, qvar.breaks = qvar.breaks)
  }
  
  ### return nodes
  return(partynode(id = as.integer(id), split = sp, kids = kids))
}

tree.learn.ttest <- function(
  formula,
  data,
  weights = NULL,
  qvar.breaks = qvar.breaks.quantile
) {
  
  ### name of the response variable
  response <- all.vars(formula)[1]
  ### data without missing values, response comes last
  data <- data[complete.cases(data), c(all.vars(formula)[-1], response)]
  ### data is numeric
  stopifnot(all(sapply(data, is.numeric)))
  
  if (is.null(weights)) weights <- rep(1, nrow(data))
  ### weights are case weights, i.e., integers
  stopifnot(length(weights) == nrow(data) &
              max(abs(weights - floor(weights))) < .Machine$double.eps)
  
  ### grow tree
  nodes <- growtree(id = 1L, response, data, weights, qvar.breaks = qvar.breaks)
  
  ### compute terminal node number for each obs.
  fitted <- fitted_node(nodes, data = data)
  ### return rich object
  ret <- party(nodes, 
               data = data,
               fitted = data.frame(
                 "(fitted)" = fitted,
                 "(response)" = data[[response]],
                 "(weights)" = weights,
                 check.names = FALSE),
               terms = terms(formula))
  as.constparty(ret)
}