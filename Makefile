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
TESTS     = $(wildcard $(PKG_ROOT)/tests/testthat/*.R)

# Targets
#
## Vignettes
# These are both targets for building and dependencies for the package tar.gz
# file
VIGNETTES = $(PKG_ROOT)/vignettes/summary-statistics.Rmd

## Data targets (none for qwraps2, define as needed in other packages.)
#DATATARGETS  = $(PKG_ROOT)/data/dataset.rda  # Data object
#DATATARGETS += $(PKG_ROOT)/R/dataset.R       # Data object documentation

################################################################################
# Recipes

.PHONY: all check install clean

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(VIGNETTES) $(TESTS)
	R CMD build --md5 $(build-options) $(PKG_ROOT)

.install_dev_deps.Rout : $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)'))" \
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
# qwraps2 does not have data sets but many other packages do.  Some useful
# notes:
#
# say one .R creates several data sets and documentation, i.e, multiple targets
# for one dependency.  A way to handle this situation is to have the targets
# depend on a .Rout file which depends on the .R file.  If the .R file is
# updated then the .Rout file will be updated which will update all the targets.
# If a target is missing, i.e., the .R script fails to run to completion, then
# there needs to be a way for Make to know to call the .R file again.  That is
# accomplished by the $(testiftargetexists) recipe, if the target exists, do
# nothing, if the target does not exists then delete the .Rout file and call
# Make on the .Rout file.
#
# Commented out lines are below as an example.  The .R file could be in the
# data-raw directory or in the vignette-spinners directory depending on if
#
define testiftargetexists
@if test -f $@; then :; else\
	$(RM) $<;\
	$(MAKE) $<;\
fi
endef

# $(DATATARGETS) : $(PKG_ROOT)/data-raw/makedataset.Rout
# 	$(testiftargetexists)
#
# $(PKG_ROOT)/data-raw/makedataset.Rout : $(PKG_ROOT)/data-raw/makedataset.R
# 	R CMD BATCH --vanilla $< $@

################################################################################
# Other Recipes for checking the package, (un)installing, and cleaning the
# working directory.
#

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'rcmdcheck::rcmdcheck("$<", error_on = "note")'

check-as-cran: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

uninstall :
	R --vanilla --quiet -e "try(remove.packages('pedalfast.data'), silent = TRUE)"

clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .document.Rout
	$(RM) -f .install_dev_deps.Rout

