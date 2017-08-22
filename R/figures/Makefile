.PHONY: all clean

all: beds_exp.pdf \
	deaths_exp.pdf

%.tex %.pdf %.png: plots.R diseases.R models.R
	R -e "source(\"plots.R\"); make.plot(file=\"$@\")"

clean:
	rm -f *.pdf *.png *.tex