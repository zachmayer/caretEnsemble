# Makefile for R project

.PHONY: all install document update-test-fixtures test coverage-test check-cran fix-style lint clean

# Default target
all: fix-style install document test check-cran coverage-test

# Install dependencies
install:
	Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	Rscript -e "devtools::install_deps()"
	Rscript -e "devtools::install_dev_deps()"

# Generate documentation
document:
	Rscript -e "devtools::document()"

# Update test fixtures
update-test-fixtures:
	Rscript inst/data-raw/build_test_data.R

# Run unit tests
test:
	Rscript -e "Sys.setenv(NOT_CRAN='true'); devtools::test(stop_on_failure=TRUE, stop_on_warning=TRUE)"

# Run test coverage
# Dunno why package_coverage makes the dir 'lib/'
coverage.rds: $(wildcard R/*.R) $(wildcard tests/testthat/*.R)
	Rscript -e "\
		Sys.setenv(NOT_CRAN = 'true'); \
		cov = covr::package_coverage(quiet=FALSE, clean=TRUE); \
		saveRDS(cov, 'coverage.rds'); \
	"
	rm -rf lib/

# xml coverage report in cobertura format for app.codecov.io/gh/zachmayer/caretEnsemble
cobertura.xml: coverage.rds
	Rscript -e "\
		cov = readRDS('coverage.rds'); \
		covr::to_cobertura(cov, filename='cobertura.xml'); \
	"

# html coverage report for local viewing
coverage-report.html: coverage.rds
	Rscript -e "\
		cov = readRDS('coverage.rds'); \
		covr::report(cov, file='coverage-report.html', browse=interactive()); \
	"

# Test that coverage is 100%
coverage-test: coverage.rds
	Rscript -e "\
		cov = readRDS('coverage.rds'); \
		cov_num = as.numeric(covr::percent_coverage(cov)); \
		testthat::expect_gte(cov_num, 100.0); \
	"

# Run R CMD check as CRAN
check-cran: document
	Rscript -e "devtools::check(cran=T, remote=T, manual=T, error_on='note')"

# Auto style the code
fix-style:
	Rscript -e "styler::style_pkg()"
	Rscript -e "styler::style_dir('inst/')"

# Check the code for lint
lint:
	Rscript -e "lintr::lint_package()"

# Clean up generated files
clean:
	rm -rf *.Rcheck
	rm -f *.tar.gz
	rm -f coverage.rds
	rm -f cobertura.xml
	rm -f coverage-report.html
	rm -f .Rhistory
	rm -rf lib/