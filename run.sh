
source=`find . -type f -iregex '.*.hs$'`
stack runghc -- $source

