test_that('Skips work correctly', skip('Basic skip failed'))
test_that('Skips work correctly', skip_on_travis('Skip on travis failed'))
test_that('Skips work correctly', skip_on_cran('Skip on cran failed'))
