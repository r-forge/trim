qvar.breaks.quantile <- function(x, prob = seq(from = 0.1, to = 0.9, by = 0.05)) {
  return(unique(quantile(x, prob = prob)))
}