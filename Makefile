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

R         ?= R --vanilla
RSCRIPT   ?= Rscript --vanilla
RCMDBATCH ?= R CMD BATCH --vanilla

CRAN     ?= https://cran.rstudio.com
REPOS    := "options(repos=c(CRAN='$(CRAN)'))"

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

.PHONY: all check check-no-suggests install clean covr

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(VIGNETTES) $(TESTS)
	$(R) CMD build --md5 $(build-options) $(PKG_ROOT)

.install_dev_deps.Rout : $(PKG_ROOT)/DESCRIPTION
	$(RSCRIPT) --quiet -e $(REPOS) \
	  -e "if (!requireNamespace('pak', quietly=TRUE)) \
	       install.packages('pak', repos='$(CRAN)')" \
	  -e "options(warn=2)" \
	  -e "pak::local_install_dev_deps(root = '$(PKG_ROOT)')" \
	  > $@ 2>&1

.document.Rout: $(SRC) $(RFILES) $(DATATARGETS) $(EXAMPLES) $(PKG_ROOT)/DESCRIPTION
	$(RSCRIPT) --quiet -e "options(warn = 2)" \
		-e "devtools::document('$(PKG_ROOT)')"
	@touch $@

################################################################################
# Recipes for Vignettes
#
# Expecting that the vignettes are built via knitr::spin
#
# List the explicit targets above

$(PKG_ROOT)/vignettes/%.Rmd : $(PKG_ROOT)/vignette-spinners/%.R
	$(R) --quiet -e "knitr::spin(hair = '$<', knit = FALSE)"
	mv $(basename $<).Rmd $@

################################################################################
# Data Sets
#
$(DATATARGETS) &: vignette-spinners/qwraps2-data-sets.R inst/spambase/spambase.data inst/spambase/spambase.names
	$(RCMDBATCH) $< .data-export.Rout

################################################################################

covr :
	$(R) --quiet \
	  -e "if (!requireNamespace('covr', quietly=TRUE)) \
	       install.packages('covr', repos='$(CRAN)')" \
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
	$(R) CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

check-no-suggests: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	_R_CHECK_DEPENDS_ONLY_=true _R_CHECK_FORCE_SUGGESTS_=false $(R) CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

check-as-cran: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R) CMD check --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R) CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

uninstall :
	$(R) --quiet -e "try(remove.packages('$(PKG_NAME)'), silent = TRUE)"

site: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R) --quiet \
	  -e "if (!requireNamespace('covr', quietly=TRUE)) \
	       install.packages('covr', repos='$(CRAN)')" \
		-e "pkgdown::build_site()"

clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .document.Rout
	$(RM) -f .install_dev_deps.Rout
	$(RM) -f src/*.o src/*.so

