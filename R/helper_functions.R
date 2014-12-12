
#' @title Calculate a weighted standard deviation
#' @description Used to weight deviations among ensembled model preditions
#'
#' @param x a vector of numerics
#' @param weights a vector of weights equal to length of x
#' @param normwt  a logical indicating whether the weights should be normalized to 1
#' @param na.rm a logical indicating how to handle missing values, default = FALSE
wtd.sd <- function (x, weights = NULL, normwt = FALSE, na.rm = FALSE) {
  if (!length(weights)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(sd(x))
  }
  if(length(weights) != length(x)){
    warning("length of the weights vector != the length of the x vector,
            weights are being recycled.")
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt){
    weights <- weights * length(x)/sum(weights)
  }
  xbar <- sum(weights * x)/sum(weights)
  out <- sqrt(sum(weights * ((x - xbar)^2))/(sum(weights)))
  return(out)
}
