#' Greedy optimization of the area under the curve
#' @description This algorithm optimizes the area under the curve for classifcation models
#' @param X the matrix of predictors
#' @param Y the dependent variable
#' @param iter an integer for the number of iterations
#' @return A numeric of the weights for each model
#' @export
greedOptAUC <- function(X, Y, iter = 100L){ #TODO: ADD POSITIVE LEVEL IF NEEDED
  #require('caTools')

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
    errors        <- caTools::colAUC(pred, Y)
    best          <- which.max(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  return(weights)
}

#' Safe optimization of the AUC
#' @description This algorithm optimizes the AUC for regression models to avoid ensembling
#' where the ensembled model fits worse than any component model
#' @param X the matrix of predictors
#' @param Y the dependent variable
#' @param iter an integer for the number of iterations
#' @return A numeric of the weights for each model
#' @export
safeOptAUC <- function(X, Y, iter = 100L) {
  if(is.character(Y) | is.factor(Y)){
    Y <- as.numeric(factor(Y))
  }

  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  stopper     <- max(caTools::colAUC(X, Y))
  maxtest     <- 1

  while((sum.weights < iter) & (maxtest >= stopper)) {

    sum.weights   <- sum.weights + 1L
    pred          <-(pred + X) * (1L / sum.weights)
    errors        <- caTools::colAUC(pred, Y)
    best          <- which.max(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
    maxtest       <- max(errors) # check we are better than no weights
  }
  return(weights)
}
