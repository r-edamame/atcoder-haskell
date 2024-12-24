export PATH="$PATH:$PWD"
export ROOTDIR=`pwd`
cp ./template.hs ~/.config/atcoder-cli-nodejs/haskell/main.hs
cp ./template.json ~/.config/atcoder-cli-nodejs/haskell/template.json

alias new="source $ROOTDIR/new.sh"
alias go="source $ROOTDIR/go.sh"

