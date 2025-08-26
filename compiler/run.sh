Args=("$@")
Infile=${Args[0]}
Outfile=${Args[1]}

./mcc.sh $Infile $Outfile
arch -x86_64 clang $Outfile -o a.out
./a.out
