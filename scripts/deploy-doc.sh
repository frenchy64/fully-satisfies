#!/bin/bash

set -e

CURRENT_VERSION=`lein pprint --no-pretty -- :version`

./scripts/gen-doc.sh
cd target
if [ ! -d fully-satisfies-gh-pages ]; then
  git clone git@github.com:frenchy64/fully-satisfies.git fully-satisfies-gh-pages
fi
cd fully-satisfies-gh-pages
git checkout gh-pages || git checkout --orphan gh-pages
git reset --hard
if git ls-remote --exit-code --heads git@github.com:frenchy64/fully-satisfies.git gh-pages ; then
  git pull -f origin gh-pages
fi
rm -fr latest
#https://askubuntu.com/a/86891
cp -a ../doc/. latest
cp -a ../doc/. "$CURRENT_VERSION"
git add .
git commit --allow-empty -m "Docs for $CURRENT_VERSION"
git push origin --set-upstream gh-pages
