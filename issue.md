Modify [extractBestPreds](https://github.com/zachmayer/caretEnsemble/blob/main/R/caretPredict.R#L152) and [caretList](https://github.com/zachmayer/caretEnsemble/blob/main/R/caretList.R).  

- https://github.com/zachmayer/caretEnsemble/blob/main/R/caretPredict.R#L152
- https://github.com/zachmayer/caretEnsemble/blob/main/R/caretList.R

## [extractBestPreds](https://github.com/zachmayer/caretEnsemble/blob/main/R/caretPredict.R#L152)

Modify the  function in caretPredict.R. Add an argument for , and set the default to True.  Then add code like this:

  
## [caretList](https://github.com/zachmayer/caretEnsemble/blob/main/R/caretList.R)

Modify the  function in caretList.R. Add an argument for , and set the default to True.  Pass the argument through the lappy loop to  in this line of code:



## Guidelines:
- Write clean, lint-free code that complies with the lintr spec
- Use make format to auto format your code if needed
- Write code that complies with ALL the linters in lintr
- Write clean, modular code
- Make the MINIMAL changes needed to fix the issue.  Be surgical and precice with your changes.
- Write at least one unit test for your work
- Code should be self-documenting with good variable names and little need for commentsd
- If you make a new function, document it with roxygen2
- Don't add new packages.  If you need to install an R package, something is wrong.

## Verify your changes
- run  to make sure the linter passes (format will format your code for the linter).
- THEN: run Rscript -e "Sys.setenv(NOT_CRAN='true'); devtools::test(stop_on_failure=TRUE, stop_on_warning=TRUE)"
✔ | F W  S  OK | Context

⠏ |          0 | backwards-compatability                                        
✔ |          4 | backwards-compatability

⠏ |          0 | caretEnsemble                                                  
⠏ |          0 | Metric and residual extraction                                 
⠋ |          1 | Metric and residual extraction                                 
✔ |          2 | Metric and residual extraction

⠏ |          0 | Ensembling and prediction                                      
⠧ |          8 | Ensembling and prediction                                      
⠹ |         13 | Ensembling and prediction                                      
✔ |         26 | Ensembling and prediction

⠏ |          0 | Ensembling with models of differing predictors                 
⠋ |          1 | Ensembling with models of differing predictors                 
⠸ |          4 | Ensembling with models of differing predictors                 
✔ |          7 | Ensembling with models of differing predictors

⠏ |          0 | Ensembles with custom models                                   
⠋ |          1 | Ensembles with custom models                                   
⠦ |         17 | Ensembles with custom models                                   
✔ |         18 | Ensembles with custom models [1.5s]

⠏ |          0 | Variable importance and plotting                               
⠴ |         16 | Variable importance and plotting                               
⠼ |         25 | Variable importance and plotting                               
⠏ |         30 | Variable importance and plotting                               
⠋ |         41 | Variable importance and plotting                               
⠹ |         53 | Variable importance and plotting                               
⠼ |         55 | Variable importance and plotting                               
⠋ |         61 | Variable importance and plotting                               
✔ |         61 | Variable importance and plotting [2.6s]

⠏ |          0 | caretList                                                      
⠏ |          0 | caretModelSpec, tuneCheck, methodCheck                         
⠙ |          2 | caretModelSpec, tuneCheck, methodCheck                         
⠸ |          4 | caretModelSpec, tuneCheck, methodCheck                         
⠴ |          6 | caretModelSpec, tuneCheck, methodCheck                         
✔ |          6 | caretModelSpec, tuneCheck, methodCheck

⠏ |          0 | S3 methods for caretlist                                       
✔ |         18 | S3 methods for caretlist

⠏ |          0 | predict.caretlist                                              
⠼ |          5 | predict.caretlist                                              
✔ |          8 | predict.caretlist

⠏ |          0 | caretList                                                      
⠋ |          1 | caretList                                                      
⠙ |          2 | caretList                                                      
⠹ |          3 | caretList                                                      
⠸ |          4 | caretList                                                      
⠼ |          5 | caretList                                                      
⠴ |          6 | caretList                                                      
⠇ |          9 | caretList                                                      
⠙ |         12 | caretList                                                      
⠹ |         13 | caretList                                                      
⠙ |         32 | caretList                                                      
⠦ |         37 | caretList                                                      
⠙ |         42 | caretList                                                      
⠴ |         46 | caretList                                                      
✔ |         55 | caretList [13.4s]

⠏ |          0 | caretPredict                                                   
⠏ |          0 | caretPredict and extractMetric                                 
⠧ |          8 | caretPredict and extractMetric                                 
✔ |         13 | caretPredict and extractMetric

⠏ |          0 | S3 methods and model operations                                
✔ |         13 | S3 methods and model operations

⠏ |          0 | isClassifierAndValidate                                        
⠋ |          1 | isClassifierAndValidate                                        
✔ |          8 | isClassifierAndValidate

⠏ |          0 | validateExcludedClass                                          
✔ |         21 | validateExcludedClass

⠏ |          0 | caretStack                                                     
⠏ |          0 | caretStack                                                     
⠴ |          6 | caretStack                                                     
⠹ |         13 | caretStack                                                     
⠸ |         24 | caretStack                                                     
⠹ |         33 | caretStack                                                     
⠼ |         35 | caretStack                                                     
⠴ |         36 | caretStack                                                     
⠧ |         38 | caretStack                                                     
⠋ |         41 | caretStack                                                     
⠸ |         44 | caretStack                                                     
⠦ |         47 | caretStack                                                     
⠏ |         50 | caretStack                                                     
⠹ |         53 | caretStack                                                     
⠴ |         56 | caretStack                                                     
⠇ |         59 | caretStack                                                     
⠋ |         61 | caretStack                                                     
⠙ |         62 | caretStack                                                     
⠼ |         65 | caretStack                                                     
⠦ |         67 | caretStack                                                     
⠇ |         69 | caretStack                                                     
⠋ |         71 | caretStack                                                     
⠹ |         73 | caretStack                                                     
⠋ |         81 | caretStack                                                     
✔ |         83 | caretStack [5.4s]

⠏ |          0 | S3 methods for caretStack                                      
⠋ |         11 | S3 methods for caretStack                                      
⠇ |         19 | S3 methods for caretStack                                      
⠏ |         20 | S3 methods for caretStack                                      
✔ |         20 | S3 methods for caretStack

⠏ |          0 | varImp                                                         
⠏ |         10 | varImp                                                         
✔ |         12 | varImp

⠏ |          0 | wtd.sd                                                         
✔ |          7 | wtd.sd

⠏ |          0 | set_excluded_class_id                                          
✔ |          2 | set_excluded_class_id

⠏ |          0 | classSelection                                                 
⠏ |          0 | Do classifier predictions use the correct target classes?      
⠦ |          7 | Do classifier predictions use the correct target classes?      
⠦ |         17 | Do classifier predictions use the correct target classes?      
⠦ |         27 | Do classifier predictions use the correct target classes?      
⠦ |         37 | Do classifier predictions use the correct target classes?      
⠦ |         47 | Do classifier predictions use the correct target classes?      
⠦ |         57 | Do classifier predictions use the correct target classes?      
✔ |         64 | Do classifier predictions use the correct target classes? [5.4s]

⠏ |          0 | greedyMSE                                                      
⠇ |         19 | greedyMSE                                                      
⠴ |         36 | greedyMSE                                                      
⠇ |         39 | greedyMSE                                                      
⠏ |         40 | greedyMSE                                                      
✔ |         40 | greedyMSE [1.2s]

⠏ |          0 | multiclass                                                     
⠏ |          0 | caretList and caretStack work for multiclass problems          
⠹ |          3 | caretList and caretStack work for multiclass problems          
⠏ |         10 | caretList and caretStack work for multiclass problems          
⠹ |         13 | caretList and caretStack work for multiclass problems          
⠸ |         14 | caretList and caretStack work for multiclass problems          
⠴ |         16 | caretList and caretStack work for multiclass problems          
⠇ |         19 | caretList and caretStack work for multiclass problems          
⠋ |         21 | caretList and caretStack work for multiclass problems          
⠹ |         23 | caretList and caretStack work for multiclass problems          
⠸ |         24 | caretList and caretStack work for multiclass problems          
⠴ |         26 | caretList and caretStack work for multiclass problems          
⠦ |         27 | caretList and caretStack work for multiclass problems          
⠋ |         31 | caretList and caretStack work for multiclass problems          
⠼ |         35 | caretList and caretStack work for multiclass problems          
✔ |         35 | caretList and caretStack work for multiclass problems [10.4s]

⠏ |          0 | permutationImportance                                          
⠏ |          0 | isClassifier function                                          
⠹ |          3 | isClassifier function                                          
✔ |          4 | isClassifier function

⠏ |          0 | permutationImportance function                                 
⠹ |         13 | permutationImportance function                                 
⠼ |         35 | permutationImportance function                                 
⠸ |         54 | permutationImportance function                                 
✔ |         59 | permutationImportance function

⠏ |          0 | permutationImportance edge cases                               
⠴ |         16 | permutationImportance edge cases                               
✔ |         35 | permutationImportance edge cases

⠏ |          0 | NAN predictions from rpart                                     
⠋ |          1 | NAN predictions from rpart                                     
✔ |          1 | NAN predictions from rpart

══ Results ═════════════════════════════════════════════════════════════════════
Duration: 49.8 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 622 ]
rm -f caretEnsemble_test_plots.png to make sure unit tests pass
- THEN: run Rscript -e "devtools::check(cran = FALSE, remote = TRUE, manual = TRUE, force_suggests = TRUE, error_on = 'note')"
══ Documenting ═════════════════════════════════════════════════════════════════

══ Building ════════════════════════════════════════════════════════════════════
Setting env vars:
• CFLAGS    : -Wall -pedantic
• CXXFLAGS  : -Wall -pedantic
• CXX11FLAGS: -Wall -pedantic
• CXX14FLAGS: -Wall -pedantic
• CXX17FLAGS: -Wall -pedantic
• CXX20FLAGS: -Wall -pedantic
── R CMD build ─────────────────────────────────────────────────────────────────
* checking for file ‘/home/runner/work/caretEnsemble/caretEnsemble/DESCRIPTION’ ... OK
* preparing ‘caretEnsemble’:
* checking DESCRIPTION meta-information ... OK
* installing the package to build vignettes
* creating vignettes ... OK
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
Removed empty directory ‘caretEnsemble/tests/testthat/_snaps’
* building ‘caretEnsemble_4.0.2.tar.gz’

══ Checking ════════════════════════════════════════════════════════════════════
Setting env vars:
• _R_CHECK_CRAN_INCOMING_REMOTE_: TRUE
• _R_CHECK_CRAN_INCOMING_       : TRUE
• _R_CHECK_FORCE_SUGGESTS_      : TRUE
• NOT_CRAN                      : true
── R CMD check ─────────────────────────────────────────────────────────────────
* using log directory ‘/tmp/RtmpCnc38g/file18cf22d5e59d/caretEnsemble.Rcheck’
* using R version 4.4.2 (2024-10-31)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.5 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘caretEnsemble/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘caretEnsemble’ version ‘4.0.2’
* package encoding: UTF-8
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: ‘Zachary A. Deane-Mayer <zach.mayer@gmail.com>’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘caretEnsemble’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking code files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
  Running ‘testthat.R’
 OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE

Status: OK
── R CMD check results ──────────────────────────────── caretEnsemble 4.0.2 ────
Duration: 2m 21.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
Rscript -e "devtools::check(cran = TRUE , remote = TRUE, manual = TRUE, force_suggests = TRUE, error_on = 'note')"
══ Documenting ═════════════════════════════════════════════════════════════════

══ Building ════════════════════════════════════════════════════════════════════
Setting env vars:
• CFLAGS    : -Wall -pedantic
• CXXFLAGS  : -Wall -pedantic
• CXX11FLAGS: -Wall -pedantic
• CXX14FLAGS: -Wall -pedantic
• CXX17FLAGS: -Wall -pedantic
• CXX20FLAGS: -Wall -pedantic
── R CMD build ─────────────────────────────────────────────────────────────────
* checking for file ‘/home/runner/work/caretEnsemble/caretEnsemble/DESCRIPTION’ ... OK
* preparing ‘caretEnsemble’:
* checking DESCRIPTION meta-information ... OK
* installing the package to build vignettes
* creating vignettes ... OK
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
Removed empty directory ‘caretEnsemble/tests/testthat/_snaps’
* building ‘caretEnsemble_4.0.2.tar.gz’

══ Checking ════════════════════════════════════════════════════════════════════
Setting env vars:
• _R_CHECK_CRAN_INCOMING_REMOTE_               : TRUE
• _R_CHECK_CRAN_INCOMING_                      : TRUE
• _R_CHECK_FORCE_SUGGESTS_                     : TRUE
• _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
• NOT_CRAN                                     : true
── R CMD check ─────────────────────────────────────────────────────────────────
* using log directory ‘/tmp/RtmpnCI9pM/file1ed134fc320a/caretEnsemble.Rcheck’
* using R version 4.4.2 (2024-10-31)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.5 LTS
* using session charset: UTF-8
* using options ‘--no-manual --as-cran’
* checking for file ‘caretEnsemble/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘caretEnsemble’ version ‘4.0.2’
* package encoding: UTF-8
* checking CRAN incoming feasibility ... [4s/13s] Note_to_CRAN_maintainers
Maintainer: ‘Zachary A. Deane-Mayer <zach.mayer@gmail.com>’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘caretEnsemble’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking code files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
  Running ‘testthat.R’ [73s/49s]
 [74s/49s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [20s/16s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: OK
── R CMD check results ──────────────────────────────── caretEnsemble 4.0.2 ────
Duration: 2m 26.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔ to make sure R CMD CHECK passes


