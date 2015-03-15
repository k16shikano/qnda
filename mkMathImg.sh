imagename=$1

# body=$2
#echo $body | nkf -e |
#             sed \
#                 -e "1 i \\\\\\documentclass{minimal}" \
#                 -e "1 i \\\\\\usepackage{lucidabr}" \
#                 -e "1 i \\\\\\usepackage{amsmath, amstext, amssymb}" \
#                 -e "1 i \\\\\\begin{document}"\
#                 -e "1 i \$\$" \
#                 -e "$ a \$\$" \
#                 -e "$ a \\\\\\end{document}" \
#        | platex # -interaction=batchmode 

nkf -eW temp | platex > /dev/null
dvipdfmx -q -f lucida -f hiraginox -l texput.dvi > /dev/null
#convert -density 200 -strip -trim +repage texput.pdf $imagename
pdfcrop texput.pdf temp.pdf > /dev/null
pdf2svg temp.pdf $imagename 
rm -fr texput* temp

