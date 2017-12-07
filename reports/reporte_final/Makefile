PROJECT=survey_mbc
TEX=pdflatex
BIBTEX=bibtex
BUILDTEX=$(TEX) $(PROJECT).tex

all:
	$(BUILDTEX)
	$(BIBTEX) $(PROJECT)
	$(BUILDTEX)
	$(BUILDTEX)

clean-all:
	rm -f *.dvi *.log *.bak *.aux *.bbl *.blg *.idx *.ps *.eps *.toc *.out *.fdb_latexmk *.fls *.lof *.lot *-blx.bib *.run.xml *~ *.synctex.gz *.bcf *.pdf

.PHONY: clean

clean:
	rm -f *.dvi *.log *.bak *.aux *.bbl *.blg *.idx *.ps *.eps *.toc *.out *.fdb_latexmk *.fls *.lof *.lot *-blx.bib *.run.xml *.synctex.gz *.bcf

