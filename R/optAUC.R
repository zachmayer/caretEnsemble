
#' TODO
#' @param X
#' @param Y
#' @param iter
#' @export
greedOptAUC <- function(X, Y, iter = 100L){ #TODO: ADD POSITIVE LEVEL IF NEEDED
  require('caTools')

  if(is.character(Y)){
    Y <- factor(Y)
  }
  stopifnot(is.factor(Y))
  
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  
  while(sum.weights < iter) {
    
    sum.weights   <- sum.weights + 1L
    pred          <-(pred + X) * (1L / sum.weights)
    errors        <- colAUC(pred, Y)
    best          <- which.max(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  return(weights)
}
