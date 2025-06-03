#!/bin/bash
# Build the C compiler
Args=("$@")
Outfile=${Args[0]}
printf 'CM.make "lang.cm";\nSMLofNJ.exportML("%s");\n' $Outfile

