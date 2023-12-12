################################################################################
# Makefile
#
# This Makefile is not only for building qwraps2 but to serve as a template and
# example for other R packages.
#
################################################################################

PKG_ROOT    = .
PKG_VERSION = $(shell gawk '/^Version:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)
PKG_NAME    = $(shell gawk '/^Package:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)

CRAN = "https://cran.rstudio.com"

# General Package Dependencies
SRC       = $(wildcard $(PKG_ROOT)/src/*.cpp)
RFILES    = $(wildcard $(PKG_ROOT)/R/*.R)
EXAMPLES  = $(wildcard $(PKG_ROOT)/examples/*.R)
TESTS     = $(wildcard $(PKG_ROOT)/tests/*.R)

# Targets
#
## Vignettes
# These are both targets for building and dependencies for the package tar.gz
# file
VIGNETTES  = $(PKG_ROOT)/vignettes/qwraps2-formatted-summary-statistics.Rmd
VIGNETTES += $(PKG_ROOT)/vignettes/qwraps2-data-sets.Rmd
VIGNETTES += $(PKG_ROOT)/vignettes/qwraps2-graphics.Rmd
VIGNETTES += $(PKG_ROOT)/vignettes/qwraps2-summary-table.Rmd

## Data targets
DATATARGETS  = $(PKG_ROOT)/data/mtcars2.rda
DATATARGETS += $(PKG_ROOT)/data/pefr.rda
DATATARGETS += $(PKG_ROOT)/data/spambase.rda

################################################################################
# Recipes

.PHONY: all check install clean covr

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(VIGNETTES) $(TESTS)
	R CMD build --md5 $(build-options) $(PKG_ROOT)

.install_dev_deps.Rout : $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)'))" \
		-e "if (!require(devtools)) {install.packages('devtools', repo = c('$(CRAN)'))}" \
		-e "options(warn = 2)" \
		-e "devtools::install_dev_deps()"
	touch $@

.document.Rout: $(SRC) $(RFILES) $(DATATARGETS) $(EXAMPLES) $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(warn = 2)" \
		-e "devtools::document('$(PKG_ROOT)')"
	touch $@

################################################################################
# Recipes for Vignettes
#
# Expecting that the vignettes are built via knitr::spin
#
# List the explicit targets above

$(PKG_ROOT)/vignettes/%.Rmd : $(PKG_ROOT)/vignette-spinners/%.R
	R --vanilla --quiet -e "knitr::spin(hair = '$<', knit = FALSE)"
	mv $(basename $<).Rmd $@

################################################################################
# Data Sets
#
$(DATATARGETS) &: vignette-spinners/qwraps2-data-sets.R inst/spambase/spambase.data inst/spambase/spambase.names
	R CMD BATCH --vanilla $< .data-export.Rout

################################################################################

covr :
	R --vanilla --quiet \
		-e 'library(covr)'\
		-e 'x <- package_coverage(type = "all", combine_types = FALSE)'\
		-e 'report(x[["tests"]], file = "covr-report-tests.html")'\
		-e 'report(x[["vignettes"]], file = "covr-report-vignettes.html")'\
		-e 'report(x[["examples"]], file = "covr-report-examples.html")'\
		-e 'x <- package_coverage(type = "all", combine_types = TRUE)'\
		-e 'report(x, file = "covr-report-all.html")'\

################################################################################
# Other Recipes for checking the package, (un)installing, and cleaning the
# working directory.
#
#

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

check-as-cran: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

uninstall :
	R --vanilla --quiet -e "try(remove.packages('$(PKG_NAME)'), silent = TRUE)"

site: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e "pkgdown::build_site()"

clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .document.Rout
	$(RM) -f .install_dev_deps.Rout

