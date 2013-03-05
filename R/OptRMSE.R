
#' TODO
#' @param X
#' @param Y
#' @param iter
#' @export
greedOptRMSE <- function(X, Y, iter = 100L){
  
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  
  while(sum.weights < iter) {
    
    sum.weights   <- sum.weights + 1L
    pred          <- (pred + X) * (1L / sum.weights)
    errors        <- sqrt(colSums((pred - Y) ^ 2L))
    best          <- which.min(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  return(weights)
}
