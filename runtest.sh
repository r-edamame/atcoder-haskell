
source=`find . -type f -iregex '.*.hs$'`
stack ghc -- $source -o a.out && oj test 
rm *.o *.hi a.out

