#!/bin/bash
if [[ -z "$@" ]]
then
	echo "run me with a list of .pdf files as argument (or just '*.pdf'), I will compress them in place"
	exit 42
fi

for pdf in "$@"
do
	printf "compressing %s... " "$pdf"
	pdf2ps $pdf $pdf.ps
	ps2pdf $pdf.ps $pdf
	echo "done"
done

