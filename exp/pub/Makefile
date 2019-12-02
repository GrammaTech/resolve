TARGET ?= main
PDFLATEX_OPTS = -interactive=nonstopmode	\
	-halt-on-error				\
	-file-line-error 			\
	-shell-escape
LATEXMK = latexmk
LATEXMK_FLAGS = -f -pdf -pdflatex="pdflatex $(PDFLATEX_OPTS)" -use-make

all: $(TARGET).pdf

.PHONY: watch

watch: $(TARGET).tex
	-$(LATEXMK) -pvc $(LATEXMK_FLAGS) -jobname=$(TARGET) $<

%.pdf: %.tex
	-latexmk -f -pdf -pdflatex="pdflatex -interactive=nonstopmode -halt-on-error -file-line-error" -use-make $<

main.zip: *.tex main-figure0.pdf main-figure1.pdf main.bbl usenix2019_v3.sty
	zip main $^

clean:
	-@latexmk -CA
	-@rm -f *.{bbl,snm,nav,vrb,glo,fls,log,toc,xdy,fls,lof,lot,out,fdb_latexmk}

real-clean: clean
	-@rm -f *-figure*.{dep,dpth,md5,pdf}
