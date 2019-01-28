#!/usr/bin/env sh

command -v pdflatex && command -v bibtex
if [[ "$?" != 0 ]]; then
    echo "Command 'pdflatex' or 'bibtex' not exist, both must be installed. For Ubuntu, try:"
    echo "sudo apt install texlive-latex-base texlive-binaries"
    echo
    echo "You may also need to install additional packages like fonts, etc. For Ubuntu, try:"
    echo "sudo apt-get install texlive-fonts-recommended latex-xcolor texlive-latex-extra cm-super"
    exit 1;
fi

pdflatex sigma
bibtex sigma
pdflatex sigma
pdflatex sigma
rm sigma.aux
rm sigma.out
rm sigma.toc
rm sigma.log
