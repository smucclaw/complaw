all:
	pdflatex law2logic.tex
	bibtex law2logic
	pdflatex law2logic.tex
	pdflatex law2logic.tex

txt:
	txt2tags -ttex txt-law2logic.txt
	cat prelude law2logic.tex >tmp.tex
#	runghc Unquote.hs <tmp.tex >law2logic.tex
	mv tmp.tex law2logic.tex
