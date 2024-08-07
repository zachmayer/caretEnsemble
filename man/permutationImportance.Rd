% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permutationImportance.R
\name{permutationImportance}
\alias{permutationImportance}
\title{Permutation Importance}
\usage{
permutationImportance(model, newdata, normalize = TRUE)
}
\arguments{
\item{model}{A train object from the caret package.}

\item{newdata}{A data.frame of new data to use to compute importances. Can be the training data.}

\item{normalize}{A logical indicating whether to normalize the importances to sum to one.}
}
\value{
A named numeric vector of variable importances.
}
\description{
Permute each variable in a dataset and use the change in predictions to
calculate the importance of each variable. Based on the scikit learn implementation
of permutation importance: \url{https://scikit-learn.org/stable/modules/permutation_importance.html}.
However, we don't compare to the target by a metric. We JUST look at the change in the
model's predictions, as measured by MAE. (for classification, this is like using a Brier score).
We shuffle each variable and recompute the predictions before and after the shuffle.
The difference in MAE. is the importance of that variable. We normalize by computing the MAE of the shuffled
original predictions as an upper bound on the MAE and divide by this value.
So a variable that, when shuffled, caused predictions as bad as shuffling the output
predictions, we know that variable is 100% of the model's predictive power.
Similarly, as with regular permutation importance, a variable that, when shuffled,
gives the same MAE as the original model has an importance of 0.

This method cannot yield negative importances. It is merely a measure of how much the models uses
the variable, and does not tell you which variables help or hurt generalization. Use the model's
cross-validated metrics to assess generalization.
}
