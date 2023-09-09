adminfiles = acmart.cls
bibfiles = main.bib
sourcefiles = main.tex settings.tex defs.tex abstract.tex \
	introduction.tex abductive_proof.tex derived_asp.tex \
	simple_task.tex proof_finiteness.tex completeness.tex \
	proof_simplification.tex conclusion.tex
pdffiles = main.pdf

bibfiles_extended = main_extended.bib main_extended.bbl
sourcefiles_extended = main_extended.tex settings.tex defs.tex abstract.tex \
	introduction.tex abductive_proof.tex derived_asp.tex \
	simple_task.tex proof_finiteness.tex completeness.tex \
	proof_simplification.tex conclusion.tex \
	extra.tex proof_term_substitution.tex gen_sol.tex

submission: 
	mkdir -p source; mkdir -p pdf; \
	cp $(adminfiles) $(bibfiles) $(sourcefiles) source; cp $(pdffiles) pdf;
	zip -r ppdp2022-4278.zip ./source ./pdf

arxiv: 
	mkdir -p arxiv; \
	cp $(adminfiles) $(bibfiles_extended) $(sourcefiles_extended) arxiv; 
	zip -r arxiv.zip ./arxiv
