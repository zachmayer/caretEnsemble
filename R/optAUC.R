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
    errors        <- colAUC(pred, Y)
    best          <- which.max(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  return(weights)
}

#' Quadratic optimization of the AUC
#' @description This algorithm optimizes the AUC for regression models to avoid convex combinations
#' @param X the matrix of predictors
#' @param Y the dependent variable
#' @return A numeric of the weights for each model
#' @export
qpOptAUC <- function(x, y, offset = NULL, ...) {
  if(is.character(y) | is.factor(y)){
    y <- as.numeric(factor(y))
  }
  stopifnot(is.numeric(y))
  if(missing(offset)){
    offset <- FALSE
  }
  if(offset == TRUE){
    y <- y-1
  }
  D <- crossprod(x)
  d <- crossprod(x, y)
  A <- cbind(rep(1, ncol(x)), diag(ncol(x)))
  bvec <- c(1, rep(0, ncol(x)))
  weights <- solve.QP(Dmat=D, dvec=d, Amat=A, bvec=bvec, meq=1)$solution
  return(weights)
}
