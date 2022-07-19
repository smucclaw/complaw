adminfiles = acmart.cls
bibfiles = main.bib
sourcefiles = main.tex settings.tex defs.tex abstract.tex \
	introduction.tex abductive_proof.tex derived_asp.tex \
	simple_task.tex proof_finiteness.tex completeness.tex \
	proof_simplification.tex conclusion.tex
pdffiles = main.pdf

submission: 
	mkdir -p source; mkdir -p pdf; \
	cp $(adminfiles) $(bibfiles) $(sourcefiles) source; cp $(pdffiles) pdf;
	zip -r ppdp2022-4278.zip ./source ./pdf
