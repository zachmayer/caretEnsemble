# Makefile for R project

.PHONY: all install-deps install document update-test-fixtures test coverage-test coverage check fix-style lint spell build-vignettes release clean

# Default target
all: clean fix-style document install build-vignettes lint spell test check coverage

# Install dependencies
install-deps:
	Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	Rscript -e "devtools::install_deps()"
	Rscript -e "devtools::install_dev_deps()"
	Rscript -e "devtools::update_packages()"
	Rscript -e "devtools::install_github('r-lib/lintr')"

# Install the whole package
install: install-deps
	Rscript -e "devtools::install()"

# Generate documentation
document:
	Rscript -e "devtools::document()"

# Update test fixtures
update-test-fixtures:
	Rscript inst/data-raw/build_test_data.R

# Run unit tests
test:
	Rscript -e "Sys.setenv(NOT_CRAN='true'); devtools::test(stop_on_failure=TRUE, stop_on_warning=TRUE)"
	rm -f caretEnsemble_test_plots.png

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

coverage: cobertura.xml coverage-report.html coverage-test

# Run R CMD check as CRAN
check: document
	Rscript -e "devtools::check(cran = FALSE, remote = TRUE, manual = TRUE, force_suggests = TRUE, error_on = 'note')"
	Rscript -e "devtools::check(cran = TRUE , remote = TRUE, manual = TRUE, force_suggests = TRUE, error_on = 'note')"

# Auto style the code
fix-style:
	Rscript -e "styler::style_pkg()"
	Rscript -e "styler::style_dir('inst/')"

# Check the code for lint
lint:
	Rscript -e "Sys.setenv(LINTR_ERROR_ON_LINT='true'); lintr::lint_package()"

# Check spelling
spell:
	Rscript -e " \
		results = spelling::spell_check_package(); \
		if(nrow(results) > 0) {; \
			error = paste(results[['word']], collapse = ', '); \
			error = paste('Potential spelling errors:', error); \
			stop(error); \
		}; \
	"

# Build vignettes
build-vignettes:
	Rscript -e "devtools::build_vignettes()"

# Release to CRAN
release:
	Rscript -e "devtools::release()"
	
# Clean up generated files
clean:
	rm -rf *.Rcheck
	rm -f *.tar.gz
	rm -f man/*.Rd
	rm -f coverage.rds
	rm -f cobertura.xml
	rm -f coverage-report.html
	rm -f .Rhistory
	rm -rf lib/
	rm -f caretEnsemble_test_plots.png
	Rscript -e "devtools::clean_vignettes()"
	Rscript -e "devtools::clean_dll()"
