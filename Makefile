# Makefile for R project

.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all                    Run clean, fix-style, document, install, build-vignettes, lint, spell, test, check, coverage"
	@echo "  install-deps           Install dependencies"
	@echo "  install                Install the whole package, including dependencies"
	@echo "  document               Generate documentation"
	@echo "  update-test-fixtures   Update test fixtures"
	@echo "  test                   Run unit tests"
	@echo "  coverage               Generate coverage reports"
	@echo "  check                  Run R CMD check as CRAN"
	@echo "  fix-style              Auto style the code"
	@echo "  lint                   Check the code for lint"
	@echo "  spell                  Check spelling"
	@echo "  build                  Build the package"
	@echo "  build-vignettes        Build vignettes"
	@echo "  release                Release to CRAN"
	@echo "  clean                  Clean up generated files"

.PHONY: all
all: clean fix-style document install build-vignettes lint spell test check coverage

.PHONY: install-deps
install-deps:
	Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	Rscript -e "devtools::install_deps()"
	Rscript -e "devtools::install_dev_deps()"
	Rscript -e "devtools::update_packages()"
	Rscript -e "devtools::install_github('r-lib/lintr')"

.PHONY: install
install: install-deps
	Rscript -e "devtools::install()"

.PHONY: document
document:
	Rscript -e "devtools::document()"

.PHONY: update-test-fixtures
update-test-fixtures:
	Rscript inst/data-raw/build_test_data.R

.PHONY: test
test:
	Rscript -e "Sys.setenv(NOT_CRAN='true'); devtools::test(stop_on_failure=TRUE, stop_on_warning=TRUE)"
	rm -f caretEnsemble_test_plots.png

coverage.rds: $(wildcard R/*.R) $(wildcard tests/testthat/*.R)
	Rscript -e "\
		Sys.setenv(NOT_CRAN = 'true'); \
		cov = covr::package_coverage(quiet=FALSE, clean=TRUE); \
		saveRDS(cov, 'coverage.rds'); \
	"
	rm -rf lib/

cobertura.xml: coverage.rds
	Rscript -e "\
		cov = readRDS('coverage.rds'); \
		covr::to_cobertura(cov, filename='cobertura.xml'); \
	"

coverage-report.html: coverage.rds
	Rscript -e "\
		cov = readRDS('coverage.rds'); \
		covr::report(cov, file='coverage-report.html', browse=interactive()); \
	"

.PHONY: coverage-test
coverage-test: coverage.rds
	Rscript -e "\
		cov = readRDS('coverage.rds'); \
		cov_num = as.numeric(covr::percent_coverage(cov)); \
		testthat::expect_gte(cov_num, 100.0); \
	"

.PHONY: coverage
coverage: cobertura.xml coverage-report.html coverage-test

.PHONY: check
check: document
	Rscript -e "devtools::check(cran = FALSE, remote = TRUE, manual = TRUE, force_suggests = TRUE, error_on = 'note')"
	Rscript -e "devtools::check(cran = TRUE , remote = TRUE, manual = TRUE, force_suggests = TRUE, error_on = 'note')"

.PHONY: fix-style
fix-style:
	Rscript -e "styler::style_pkg()"
	Rscript -e "styler::style_dir('inst/')"

.PHONY: lint
lint:
	Rscript -e "Sys.setenv(LINTR_ERROR_ON_LINT='true'); devtools::load_all(); lintr::lint_package(cache = FALSE)"

.PHONY: spell
spell:
	Rscript -e " \
		results = spelling::spell_check_package(); \
		if(nrow(results) > 0) {; \
			error = paste(results[['word']], collapse = ', '); \
			error = paste('Potential spelling errors:', error); \
			stop(error); \
		}; \
	"

.PHONY: build
build:
	Rscript -e "devtools::build()"

.PHONY: build-vignettes
build-vignettes:
	Rscript -e "devtools::build_vignettes()"

.PHONY: release
release:
	Rscript -e "devtools::release()"

.PHONY: clean
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
	rm -f vignettes/caretEnsemble-intro.R
	Rscript -e "devtools::clean_vignettes()"
	Rscript -e "devtools::clean_dll()"