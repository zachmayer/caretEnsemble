[![Build Status](https://travis-ci.org/zachmayer/caretEnsemble.png?branch=master)](https://travis-ci.org/zachmayer/caretEnsemble)

Package: caretEnsemble    
Type: Package     
Title: Framework for combining caret models into ensembles    
2 main methods:  caretEnsemble which uses greedy optimization to combine predictive models, and caretStack, which uses a "meta" caret model to combine several caret models.    
See also the DESCRIPTION file.    

Create ensemlbes of [caret models](https://github.com/topepo/caret)!  Each model must be fit with the exact same resampling indexes, which requires passing the "index" argument to `trainControl()`.    

Install with:
```
library(devtools)
library(testthat)
install_github('zachmayer/caretEnsemble') #Master branch (currently has no tests)
#install_github('zachmayer/caretEnsemble', ref = 'Dev') #Dev branch
#test_package('caretEnsemble') #Run tests
```
