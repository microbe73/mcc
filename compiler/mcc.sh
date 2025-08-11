#!/bin/bash
# Fill in name of image file here:
Imgfile='comp.amd64-darwin'
Args=("$@")
Infile=${Args[0]}
Outfile=${Args[1]}

printf 'Main.compile("%s","%s");\n' $Infile $Outfile | sml @SMLload $Imgfile
sed -i '' 's/~/-/g' $Outfile

# arch -x86_64 clang $file.s -o $file
