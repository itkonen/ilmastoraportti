#!/bin/sh

set -ev

rsvg-convert -f pdf -o venn.pdf venn.svg

Rscript -e "sessionInfo()"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::epub_book')"
Rscript -e "bookdown::calibre('_book/_main.epub', 'mobi')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
cp cover-image.png _book/
