FIGURES = travel.pdf westafricatravel.pdf

TABLES = wamtab.tex

.PHONY: all clean figures

all: traveldata.rda $(TABLES) $(FIGURES) figures

travel.pdf: travel.tex
	pdflatex travel

westafricatravel.pdf: westafricatravel.tex
	pdflatex westafricatravel

travel.tex: travel.R common.R traveldata.rda
	Rscript --vanilla travel.R

westafricatravel.tex wamtab.tex: westafricatravel.R common.R traveldata.rda
	Rscript --vanilla westafricatravel.R

traveldata.rda: gettraveldata.R
	Rscript --vanilla gettraveldata.R

clean:
	rm -f *.tex
	rm -f *.pdf
	rm -f *.Rout *.rda
	rm -f *.aux *.log
	$(MAKE) -C figures clean

figures:
	$(MAKE) -C figures
