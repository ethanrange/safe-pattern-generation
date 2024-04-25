folders=(bench bin lib/generators lib/implementations lib/utilities test)

for f in ${folders[@]}
do
    dune ocaml dump-dot-merlin $f > ./$f/.merlin && echo "REC" >> ./$f/.merlin
done