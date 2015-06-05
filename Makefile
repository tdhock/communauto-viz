HOCKING-communauto.pdf: HOCKING-communauto.tex figure-difference.pdf figure-difference-2.pdf
	pdflatex HOCKING-communauto
figure-difference.pdf: figure-difference.R
	R --no-save < $<
figure-difference-2.pdf: figure-difference-2.R
	R --no-save < $<
