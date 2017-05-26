all:
	racket setup.rkt
	#scribble --latex ++style texstyle.tex paper.scrbl && rubber -d paper.tex
	#scribble --latex ++style texstyle.tex +m paper.scrbl
	scribble --latex ++style texstyle.tex ++xref-in setup/xref load-collections-xref +m paper.scrbl
	pdflatex paper.tex
	pdflatex paper.tex
	#raco doodle --extra mathpartir.sty
	open paper.pdf

fast:
	scribble --latex ++style texstyle.tex ++xref-in setup/xref load-collections-xref +m paper.scrbl
	pdflatex paper.tex 

