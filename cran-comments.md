This is a re-submission of a new package to CRAN.

The re-submission fixes the following issues in our first submission:

1. 'This package' is redundant: what else might it be a description of?:
    'This package' removed from Description.
2. Describe the package, not what it is not: Decpription updated to remove
    extraneous lines.
3. Examples take too long: We updated most of the example with `\dontrun{}` 
    blocks so they will not be run on CRAN.
4. Tests take too long: We used `testthat::skip_on_cran()` to skip all of the 
    longer-runing rests.  Testing on CRAN should now take 30-40 seconds.
    
`R CMD CHECK` passes on mac, ubuntu, and windows with no errors, warnings or 
    notes
