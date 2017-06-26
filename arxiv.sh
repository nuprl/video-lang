#!/bin/bash
mkdir arxiv || { echo "you have an existing arxiv/ directory \
  that we don't want to overwrite, please delete it first"
  exit 1
}

# the actual sources
cp paper.scrbl arxiv/
cp bib.rkt arxiv/
cp *.sty arxiv/

# the following are not available on my machines so I just download them
# (arxiv may also not have them)
wget -P arxiv/ http://www.sigplan.org/sites/default/files/acmart/current/acmart.cls
wget -P arxiv http://mirror.unl.edu/ctan/macros/latex/contrib/harpoon/harpoon.sty
wget -P arxiv/ http://web.mit.edu/ghudson/dev/nokrb/third/tetex/texmf/tex/latex/misc/ccaption.sty


# the stuff arXiv knows how to handle
cp paper.tex arxiv/
cp pict*.pdf arxiv/

# compressed for size
sh compress.sh arxiv/*.pdf

# and self-contained build instructions
echo "pdflatex paper.tex" > arxiv/build.sh

zip -r arxiv arxiv
echo "feel free to test the packed source in arxiv/ ('sh build.sh'), archive is arxiv.zip"
