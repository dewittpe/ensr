SOURCES = $(wildcard *.R)
TARGETS = $(addsuffix .html, $(basename $(SOURCES)))

all: $(TARGETS)

%.Rmd: %.R
	Rscript --vanilla -e "knitr::spin(hair = '$<', knit = FALSE)"

%.html: %.Rmd
	Rscript --vanilla -e "rmarkdown::render('$<')"
	if [ ! -f ../vignettes/$@ ]; then ln -s ../vignette-spinners/$@ ../vignettes/$@; fi
	echo "%\VignetteIndexEntry{$(basename $<)}\n%\VignetteEngine{R.rsp::asis}\n" > ../vignettes/$@.asis


