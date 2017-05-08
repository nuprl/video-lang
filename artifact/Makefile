all:
	pushd ../racket-video;\
	git archive --prefix=icfp-2017-artifact/ -o icfp-2017-artifact.tar master;\
	popd;\
	mv ../racket-video/icfp-2017-artifact.tar .
	packer build artifact.json

32-bit:
	pushd ../racket-video;\
	git archive --prefix=icfp-2017-artifact/ -o icfp-2017-artifact.tar master;\
	popd;\
	mv ../racket-video/icfp-2017-artifact.tar .
	packer build artifact-32.json

readme:
	scribble +m --htmls README.scrbl
