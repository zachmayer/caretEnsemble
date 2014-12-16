#' Greedy optimization of the area under the curve
#' @description This algorithm optimizes the area under the curve for classification models
#' @param X the matrix of predictors
#' @param Y the dependent variable
#' @param iter an integer for the number of iterations
#' @return A numeric of the weights for each model.
#' @details If the optimization fails to produce an error term better than the best
#' component model, a message is returned and the best optimization after N iterations
#' is returned.
#' @importFrom caTools colAUC
#' @export
#' @examples
#' x <- matrix(runif(10), ncol=2)
#' y <- sample(c('Y', 'N'), 5, replace=TRUE)
#' greedOptAUC(x, y)
greedOptAUC <- function(X, Y, iter = 100L){ #TODO: ADD POSITIVE LEVEL IF NEEDED
  if(is.character(Y)){
    Y <- factor(Y)
  }
  stopifnot(is.factor(Y))

  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  stopper     <- max(colAUC(X, Y))

  while(sum.weights < iter) {
    sum.weights   <- sum.weights + 1L
    pred          <-(pred + X) * (1L / sum.weights)
    errors        <- colAUC(pred, Y)
    best          <- which.max(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  maxtest       <- colAUC(X %*% weights, Y)
  if(stopper > maxtest){
    testresult <- round(maxtest/stopper, 5) * 100
    wstr <- paste0("Optimized weights not better than best model. Ensembled result is ",
                   testresult, "%", " of best model AUC. Try more iterations.")
    message(wstr)
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
#' @details This optimizer uses a stopping criterion that if the optimized model
#' has an AUC that is worse than any individual model, it continues optimizing
#' until this is no longer the case. If it fails to surpass any component model
#' it issues a warning and weights the best model 1 and all other models 0.
#' @importFrom caTools colAUC
#' @export
#' @examples
#' x <- matrix(runif(10), ncol=2)
#' y <- sample(c('Y', 'N'), 5, replace=TRUE)
#' safeOptAUC(x, y)
safeOptAUC <- function(X, Y, iter = 100L) {
  if(is.character(Y) | is.factor(Y)){
    Y <- as.numeric(factor(Y))
  }

  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  stopper     <- max(colAUC(X, Y))
  maxtest     <- 1

  while((sum.weights < iter) & (maxtest >= stopper)) {

    sum.weights   <- sum.weights + 1L
    pred          <-(pred + X) * (1L / sum.weights)
    errors        <- colAUC(pred, Y)
    best          <- which.max(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
    maxtest       <- max(errors) # check we are better than no weights
  }
  if(stopper > maxtest){
    testresult <- round(maxtest/stopper, 5) * 100
    wstr <- paste0("Optimized weights not better than best model. Ensembled result is ",
                      testresult, "%", " of best model AUC. Returning best model. Try more iterations.")
    warning(wstr, call.=FALSE)
    #TODO: Replace weights with best model weight  weights <-
    weights     <- rep(0L, N)
    weights[which.max(caTools::colAUC(X, Y))] <- 1
    return(weights)
  } else {
    return(weights)
  }
}
