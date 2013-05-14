expand.array <- function(array) {
  stopifnot(is.array(array))
  
  dims <- dim(array)
  n.var <- length(dims)
  n.total <- sum(array)
  
  nam <- dimnames(array)
  names.var <- names(nam)
  
  out <- data.frame(matrix(rep(0, n.var*n.total), ncol = n.var))
  names(out) <- names.var
  
  counter <- 1 # counter for rows
  allcomb <- expand.grid(dimnames(array)) # all combinations of values
  for(i in 1:nrow(allcomb)) {
    current.values <- unlist(allcomb[i,])
    current.command <- paste('array', "['", paste(current.values, collapse="','"), "']", sep = '')
    current.n <- eval(parse(text = current.command))
    
    if(current.n > 0) {
      for(j in counter:(counter + current.n - 1)) {
        out[j,] <- current.values
      }
      
      counter <- counter + current.n
    }
  }
  
  return(data.frame(out))
}
# ar <- array(
#   data = c(50,5,5,40,5,8,6,12,10,0,50,6,14,30,2,10,18,2),
#   dim = c(3,3,2),
#   dimnames = list(
#     'civil status' = c('married', "single", 'divorced/widowed'),
#     'working sector' = c('primary', 'secondary', 'tertiary'), 
#     'sex' = c('man', 'woman')
#   )
# )
# df <- gen.data.from.array(ar)
# table(df$civil.status)
# table(df$sex,df$working.sector)


floatprint <- function(x) {
  return(paste('(', paste(format(x, digits = 4), collapse = ', '), ')', sep = ''))
}
# floatprint(c(0.43956043956044, 0.43956043956044, 0.120879120879121))