all:
	racket setup.rkt
	raco doodle --extra mathpartir.sty
	open paper.pdf

