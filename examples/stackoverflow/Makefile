RFILES = $(shell find . -maxdepth 1 -type f -regex ".*[0-9]\.R$$")
REPREXS = $(addsuffix _reprex.md, $(basename $(RFILES)))
RMDFILES = $(shell find . -maxdepth 1 -type f -regex ".*[0-9]\.Rmd$$")
MDFILES  = $(addsuffix .md, $(basename $(RMDFILES)))
RNWFILES = $(shell find . -maxdepth 1 -type f -regex ".*[0-9]\.Rnw$$")
PDFFILES  = $(addsuffix .pdf, $(basename $(RNWFILES)))

all :  $(REPREXS) $(MDFILES) $(PDFFILES)

echo_vars : 
	@echo R Files
	@echo $(RFILES)
	@echo ""
	@echo reprex
	@echo $(REPREXS)
	@echo ""
	@echo rmdfiles
	@echo $(RMDFILES)
	@echo ""
	@echo mdfiles
	@echo $(MDFILES)
	@echo ""
	@echo rnwfiles
	@echo $(RNWFILES)
	@echo ""
	@echo pdffiles
	@echo $(PDFFILES)

%_reprex.md : %.R
	R --vanilla --quiet -e 'reprex::reprex(input = "$<", outfile = "$(basename $<)", venue = "so", si = TRUE)'

%.md : %.Rmd
	R --vanilla --quite -e 'knitr::knit(input = "$<", output = "$@")'

%.tex : %.Rnw
	R --vanilla --quite -e 'knitr::knit(input = "$<", output = "$@")'

%.pdf : %.tex
	latexmk --pdf $<
	latexmk -c $<

clean :
	$(RM) *_reprex.html
	$(RM) *_reprex.utf8.md
	$(RM) *_reprex.R
