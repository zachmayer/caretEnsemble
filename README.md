[![Build Status](https://travis-ci.org/zachmayer/caretEnsemble.png?branch=master)](https://travis-ci.org/zachmayer/caretEnsemble)

Package: caretEnsemble    
Type: Package     
Title: Framework for combining caret models into ensembles    
2 main methods:  caretEnsemble which uses greedy optimization to combine predictive models, and caretStack, which uses a "meta" caret model to combine several caret models.    
See also the DESCRIPTION file.    

Create ensemlbes of [caret models](https://github.com/topepo/caret)!  Each model must be fit with the exact same resampling indexes, which requires passing the "index" argument to `trainControl()`.    

Install with:
```
devtools::install_github('zachmayer/caretEnsemble@0.0')
```

Bleeding edge version here (trying to get ready for CRAN release, very different from 0.0:
```
devtools::install_github('zachmayer/caretEnsemble')
```

caretEnseble was inspired by [medley](https://github.com/mewo2/medley).
