main.pdf: *.tex *.bib rail/rail
	pdflatex main; rail/rail main; bibtex main; pdflatex main; pdflatex main

rail/rail: rail/Makefile
	make -C rail

# main.pdf: *.tex *.bib main.rao
# 	bibtex main; pdflatex main; pdflatex main

# main.rao: main.rai rail/rail
# 	rail/rail main

# main.rai:
# 	pdflatex main

