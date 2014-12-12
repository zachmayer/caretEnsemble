[![Build Status](https://travis-ci.org/zachmayer/caretEnsemble.png?branch=master)](https://travis-ci.org/zachmayer/caretEnsemble)

Package: caretEnsemble    
Type: Package     
Title: Framework for combining caret models into ensembles    
2 main methods:  caretEnsemble which uses greedy optimization to combine predictive models, and caretStack, which uses a "meta" caret model to combine several caret models.    
See also the DESCRIPTION file.    

Create ensembles of [caret models](https://github.com/topepo/caret)!  Each model must be fit with the exact same resampling indexes, which requires passing the "index" argument to `trainControl()`.    

Install with:
```{R}
devtools::install_github('zachmayer/caretEnsemble@0.0')
```

Bleeding edge version here (trying to get ready for CRAN release, very different from 0.0:
```{R}
devtools::install_github('zachmayer/caretEnsemble')
```

Users can also create a list of candidate models for ensembling using the the 
`buildModels()` convenience function.

caretEnsemble was inspired by [medley](https://github.com/mewo2/medley).
