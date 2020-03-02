PKG_ROOT    = .
PKG_VERSION = $(shell gawk '/^Version:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)
PKG_NAME    = $(shell gawk '/^Package:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)

CRAN = "https://cran.rstudio.com"

# Dependencies
SRC       = $(wildcard $(PKG_ROOT)/src/*.cpp)
RFILES    = $(wildcard $(PKG_ROOT)/R/*.R)
EXAMPLES  = $(wildcard $(PKG_ROOT)/examples/*.R)
TESTS     = $(wildcard $(PKG_ROOT)/tests/testthat/*.R)

# Targets
VIGNETTES    = $(PKG_ROOT)/vignettes/ensr-datasets.html.asis
VIGNETTES   += $(PKG_ROOT)/vignettes/ensr-examples.html.asis

DATATARGETS  = $(PKG_ROOT)/data/tbi.rda
DATATARGETS += $(PKG_ROOT)/data/landfill.rda

.PHONY: all check install clean

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

.install_dev_deps.Rout : $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)'))" \
		-e "options(warn = 2)" \
		-e "devtools::install_dev_deps()"
	touch $@

.document.Rout: $(SRC) $(RFILES) $(DATATARGETS) $(EXAMPLES) $(VIGNETTES) $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(warn = 2)" \
		-e "devtools::document('$(PKG_ROOT)')"
	touch $@

$(DATATARGETS) : $(PKG_ROOT)/vignette-spinners/ensr-datasets.Rout
	@if test -f $@; then :; else\
		$(RM) $<; \
		$(MAKE) $<; \
	fi

$(PKG_ROOT)/vignette-spinners/ensr-datasets.Rout : $(PKG_ROOT)/vignette-spinners/ensr-datasets.R
	R CMD BATCH --vanilla $< $@

$(PKG_ROOT)/vignette-spinners/%.Rmd : $(PKG_ROOT)/vignette-spinners/%.R
	R --vanilla --quiet -e "knitr::spin(hair = '$<', knit = FALSE)"

$(PKG_ROOT)/vignettes/%.html.asis : $(PKG_ROOT)/vignette-spinners/%.Rmd
	R --vanilla --quiet -e "rmarkdown::render('$<')"
	mv $(basename $<).html $(PKG_ROOT)/vignettes/
	echo "%\VignetteIndexEntry{$(basename $<)}\n%\VignetteEngine{R.rsp::asis}\n" > $@

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(TESTS)
	R CMD build --md5 $(build-options) $(PKG_ROOT)

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'rcmdcheck::rcmdcheck("$<", error_on = "note")'

check-as-cran: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .document.Rout
	$(RM) -f .install_dev_deps.Rout

