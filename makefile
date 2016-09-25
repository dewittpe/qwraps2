PKG_VERSION = $(shell awk '/^Version:/{print $$2}' DESCRIPTION)
PKG_NAME    = $(shell awk '/^Package:/{print $$2}' DESCRIPTION)

.PHONY: vignettes

SRC    = $(wildcard src/*.cpp)
RFILES = $(wildcard R/*.R)
MANS   = $(wildcard man/*.Rd)
TESTS  = $(wildcard tests/testthat/*.R)
VIGNETTES = $(wildcard vignettes/*.Rmd)

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

vignettes: $(VIGNETTES)
	R -e "devtools::build_vignettes()"

$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(RFILES) $(SRC) $(TESTS) $(VIGNETTES)
	R -e "devtools::document()"
	R CMD build .

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

clean:
	/bin/rm -f  $(PKG_NAME)_*.tar.gz
	/bin/rm -rf $(PKG_NAME).Rcheck
