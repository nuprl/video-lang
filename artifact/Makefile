all:
	git archive --prefix=icfp-2017-artifact/ --remote=../ -o icfp-2017-artifact.tar icfp-2017-artifact
	packer build artifact.json

32-bit:
	git archive --prefix=icfp-2017-artifact/ --remote=../ -o icfp-2017-artifact.tar icfp-2017-artifact
	packer build artifact-32.json

readme:
	scribble +m --htmls README.scrbl
