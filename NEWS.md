#NEWS 

## caretEnsemble 1.0.5

- Change output for predict functions to better align with other predict methods 
in R (predict.caretEnsemble and predict.caretStack)
- Update documentation for predict methods to better explain the model disagreement 
calculation
- Speed and memory improvements by switching to data.table for some internals
- Modified the formula for a weighted standard deviation in the model disagreement 
calculation

## caretEnsemble 1.0 - First CRAN release

* caretEnsemble is a new package for making ensembles of [caret](http://cran.r-project.org/web/packages/caret/index.html) models.
