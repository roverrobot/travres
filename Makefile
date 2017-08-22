.PHONY: clean subdirs all

all: travres.pdf

travres.pdf: travres.tex DavidEarn.bib subdirs
	pdflatex travres
	bibtex travres
	pdflatex travres
	pdflatex travres
	
clean:
	rm -f *.pdf
	rm -f *.aux *.log *.bbl *.blg *.out
	$(MAKE) -C R clean

subdirs:
	$(MAKE) -C R
