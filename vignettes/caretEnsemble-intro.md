---
title: "A Brief Introduction to caretEnsemble"
author: "Zach Mayer"
date: "2015-07-24"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{A Brief Introduction to caretEnsemble}
  %\VignetteEngine{knitr::rmarkdown}
  %\usepackage[utf8]{inputenc}
---

`caretEnsemble` is a package for making ensembles of caret models.  You should already be somewhat familiar with the caret package before trying out `caretEnsemble`.

`caretEnsemble` has 3 primary functions: **`caretList`**, **`caretEnsemble`** and **`caretStack`**. `caretList` is used to build lists of caret models on the same training data, with the same re-sampling parameters.  `caretEnsemble` and `caretStack` are used to create ensemble models from such lists of caret models.  `caretEnsemble` uses greedy optimization to create a simple linear blend of models and `caretStack` uses a caret model to combine the outputs from several component caret models.

## caretList

`caretList` is a flexible function for fitting many different caret models, with the same resampling parameters, to the same dataset.  It returns a convenient `list` of caret objects which can later be passed to `caretEnsemble` and `caretStack`.  `caretList` has almost exactly the same arguments as `train` (from the caret package), with the exception that the `trControl` argument comes last.  It can handle both the formula interface and the explicit `x`, `y` interface to train.  As in caret, the formula interface introduces some overhead and the `x`, `y` interface is preferred.

`caretEnsemble` has 2 arguments that can be used to specify which models to fit: `methodList` and `tuneList`.  `methodList` is a simple character vector of methods that will be fit with the default `train` parameters, while `tuneList` can be used to customize the call to each component model and will be discussed in more detail later.  First, lets build an example dataset (adapted from the caret vignette):



























