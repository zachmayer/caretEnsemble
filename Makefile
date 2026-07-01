# Makefile for R project

.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all                    Run clean, fix-style, document, install, readme, vignettes, lint, spell, test, check-many-preds, check, coverage, preview-site"
	@echo "  dev                    Run clean, fix-style, document, lint, spell, test"
	@echo "  install                Complete macOS dev env: tools, R deps, all CRAN-reachable caret model packages, and the package"
	@echo "  document               Generate documentation"
	@echo "  update-test-fixtures   Update test fixtures"
	@echo "  test                   Run unit tests"
	@echo "  coverage               Generate coverage reports"
	@echo "  view-coverage          View coverage report"
	@echo "  check                  Run R CMD check locally"
	@echo "  url-check              Check that URLs in the package resolve (part of release)"
	@echo "  fix-style              Auto style the code"
	@echo "  lint                   Check the code for lint"
	@echo "  actionlint            Check GitHub Actions workflows for lint"
	@echo "  spell                  Check spelling"
	@echo "  build                  Build the package"
	@echo "  vignettes              Build vignettes"
	@echo "  readme                 Build readme"
	@echo "  check-rev-dep          Run reverse dependencies check"
	@echo "  check-many-preds       Check that caretList can predict on ~200 caret models"
	@echo "  check-win              Run R CMD on the winbuilder service from CRAN"
	@echo "  check-rhub             Run R CMD on the rhub service"
	@echo "  release                Prepare release: run all checks, then open the release checklist issue"
	@echo "  submit-cran            Submit to CRAN (requires the release checklist issue to be closed)"
	@echo "  post-release           After CRAN accepts + you merge to main: GitHub release + dev-version bump"
	@echo "  preview-site           Preview pkgdown site"
	@echo "  project-tree.txt       Show a nice clean package directory, ignoring files you dont need to edit"
	@echo "  clean                  Clean up generated files"

.PHONY: all
all: clean fix-style document install readme vignettes lint spell test check-many-preds check coverage preview-site

.PHONY: dev
dev: clean fix-style document lint spell test

.PHONY: install
install:
	brew install actionlint gh
	gh auth setup-git
	Rscript -e "if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak')"
	Rscript -e "pak::local_install_dev_deps()"
	Rscript -e "pak::pak('r-lib/revdepcheck')"
	Rscript -e "libs <- unique(unlist(lapply(caret::getModelInfo(), function(m) m[['library']]))); avail <- rownames(available.packages(repos = 'https://cloud.r-project.org')); m <- setdiff(intersect(libs, avail), rownames(installed.packages())); if (length(m)) install.packages(m, repos = 'https://cloud.r-project.org')"
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

.PHONY: view-coverage
view-coverage: coverage-report.html 
	open coverage-report.html  

.PHONY: coverage
coverage: cobertura.xml coverage-report.html view-coverage coverage-test

# Build the PDF manual during check by default. The monthly workflow overrides
# this to FALSE for the R-devel job only: there the PDF build exercises
# R-devel's transient LaTeX toolchain, not the package. release + oldrel still
# build the manual strictly and catch any real Rd problems.
MANUAL ?= TRUE

.PHONY: check
check:
	Rscript -e "devtools::check(cran = FALSE, remote = TRUE, manual = $(MANUAL), force_suggests = TRUE, error_on = 'note')"
	Rscript -e "devtools::check(cran = TRUE , remote = TRUE, manual = $(MANUAL), force_suggests = TRUE, error_on = 'note')"

.PHONY: url-check
url-check:
	Rscript -e "urlchecker::url_check()"

.PHONY: fix-style
fix-style:
	Rscript -e "styler::style_pkg()"
	Rscript -e "styler::style_dir('inst/')"

.PHONY: lint
lint:
	Rscript -e "Sys.setenv(LINTR_ERROR_ON_LINT='true'); devtools::load_all(); lintr::lint_package(cache = FALSE)"

.PHONY: actionlint
actionlint:
	actionlint .github/workflows/*.yml

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
	Rscript -e "devtools::spell_check()"

.PHONY: build
build:
	Rscript -e "devtools::build()"

.PHONY: vignettes
vignettes:
	Rscript -e "devtools::build_vignettes()"

.PHONY: readme
readme:
	Rscript -e "devtools::build_readme()"

.PHONY: preview-site
preview-site:
	Rscript -e "pkgdown::build_site()"
	open docs/index.html

.PHONY: check-rev-dep
check-rev-dep:
	Rscript -e "revdepcheck:::revdep_check(num_workers = 4); revdepcheck::revdep_summary() "

.PHONY: check-many-preds
check-many-preds:
	Rscript inst/data-raw/test-all_models.R

.PHONY: check-win
check-win:
	Rscript -e "devtools::check_win_devel()"
	Rscript -e "devtools::check_win_release()"
	Rscript -e "devtools::check_win_oldrelease()"

.PHONY: check-rhub
check-rhub:
	Rscript -e "rhub::rhub_check(platforms = 'linux')"

.PHONY: release
release: check url-check readme check-many-preds check-rev-dep check-rhub check-win
	Rscript -e 'usethis::use_release_issue(version = as.character(read.dcf("DESCRIPTION")[, "Version"]))'

.PHONY: submit-cran
submit-cran:
	@version="$$(Rscript -e 'cat(as.character(read.dcf("DESCRIPTION")[, "Version"]))')"; \
	title="Release caretEnsemble $$version"; \
	gh issue list --state closed --search "$$title in:title" --json title --jq '.[].title' | grep -Fxq "$$title" || { echo "ERROR: no CLOSED issue '$$title' found. Run 'make release', complete and close the checklist, then retry."; exit 1; }; \
	echo "Closed release issue '$$title' found. Launching R — now run:  devtools::submit_cran()"
	R --no-save --quiet --interactive

.PHONY: post-release
post-release:
	Rscript -e "usethis::use_github_release()"
	Rscript -e "usethis::use_dev_version(push = TRUE)"

.PHONY: dev-guide
dev-guide:
	open https://r-pkgs.org/whole-game.html

.PHONY: project-tree.txt
project-tree.txt:
	tree -a -I "coverage*|lib|.DS_Store|.RHistory|.Rproj.user|*.png|doc|Meta|docs|.env|revdep|.git|man|README.md|.Rhistory|.claude" --prune > project-tree.txt

.PHONY: clean
clean:
	rm -rf *.Rcheck
	rm -f *.tar.gz
	rm -f *.Rout
	rm -rf man/
	rm -f README.md
	rm -f coverage.rds
	rm -f cobertura.xml
	rm -f coverage-report.html
	rm -f .Rhistory
	rm -rf lib/
	rm -f caretEnsemble_test_plots.png
	rm -f vignettes/caretEnsemble-intro.R
	Rscript -e "devtools::clean_vignettes()"
	Rscript -e "devtools::clean_dll()"
