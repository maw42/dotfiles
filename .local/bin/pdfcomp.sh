#!/bin/bash
inputFile=$1

gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dBATCH -sOutputFile="$(basename $inputFile .pdf).pdf_comp" "$inputFile" && rm $inputFile && mv "$(basename $inputFile .pdf).pdf_comp"  $inputFile
