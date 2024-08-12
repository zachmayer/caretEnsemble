# FEATURE REQUEST


# BUG
Please use this checklist for bug reports:
- [ ] [Write a minimal reproducible example](http://stackoverflow.com/a/5963610)
- [ ] Include example data in the minimal reproducible example
- [ ] run `sessionInfo()`

## Minimal, reproducible example:
start a NEW R session!

```{R}
rm(list=ls(all=T))
gc(reset=T)
set.seed(1L)

dat <- caret::twoClassSim(100L)
X <- dat[,1L:5L]
y <- dat[["Class"]]

models <- caretEnsemble::caretList(
  X, y, 
  methodList=c('glm', 'rpart')
)
ens <- caretEnsemble::caretStack(models)
print(ens)
```

If you have some data that would be too difficult to construct using `caret::twoClassSim` or `caret::SLC14_1`, then you can always make a subset of your original data, using e.g. `head()`, `subset()` or the indices. Then use e.g. `dput()` to give us something that can be put in R immediately, e.g. `dput(head(iris, 4))`

If you must use `dput(head())`, please first remove an columns from your dataset that are not necessary to reproduce the error.

If your data frame has a factor with many levels, the `dput` output can be unwieldy because it will still list all the possible factor levels even if they aren't present in the the subset of your data. To solve this issue, you can use the `droplevels()` function: `dput(droplevels(head(iris, 4)))`

## Session Info:
```{R}
utils::sessionInfo()
```

Please cut/paste the output. If your version of caret or caretEnsemble is old, upgrade them with:
```R
update.packages(oldPkgs="caret", ask=FALSE)
devtools::install_github("zachmayer/caretEnsemble")
```