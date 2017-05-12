all:
	racket setup.rkt
	#scribble --latex ++style texstyle.tex paper.scrbl && rubber -d paper.tex
	raco doodle --extra mathpartir.sty
	open paper.pdf

