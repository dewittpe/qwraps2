RFILES = $(shell find . -maxdepth 1 -type f -regex ".*[0-9]\.R")
REPREXS = $(addsuffix _reprex.md, $(basename $(RFILES)))

all :  $(REPREXS)

%_reprex.md : %.R
	R --vanilla --quiet -e 'reprex::reprex(input = "$<", outfile = "$(basename $<)", venue = "gh", si = TRUE)'
