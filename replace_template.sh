
source=`find . -maxdepth 1 -type f -iregex '.*.hs$'`
cp $ROOTDIR/template.hs $source

