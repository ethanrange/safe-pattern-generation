folders=(bench bin lib/generators lib/implementations test)

for f in ${folders[@]}
do
    dune ocaml dump-dot-merlin $f > ./$f/.merlin && echo "REC" >> ./$f/.merlin
done