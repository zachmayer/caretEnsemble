# Makefile for R project

.PHONY: all install document update-test-fixtures test check-cran fix-style lint clean

# Default target
all: fix-style install document test check-cran

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
