
source=$1
stack ghc -- $source.hs -o a.out && oj test && rm a.out $source.hi $source.o

