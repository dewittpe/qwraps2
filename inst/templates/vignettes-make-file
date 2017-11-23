SOURCES = $(wildcard *.R)
TARGETS = $(addsuffix .Rmd, $(basename $(SOURCES)))

all: $(TARGETS)

%.Rmd: %.R
	Rscript --vanilla -e "knitr::spin(hair = '$<', knit = FALSE)"
