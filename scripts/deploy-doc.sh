#!/bin/bash

set -e

CURRENT_VERSION=`cat dev/latest-version-tag`
if [ -z $CURRENT_VERSION ]; then
  echo "Current version not set -- run ./scripts/regen-latest-version-info.sh"
  exit 1
fi

./scripts/gen-doc.sh
mkdir -p tmp
cd tmp
if [ ! -d fully-satisfies-gh-pages ]; then
  if [ "$GITHUB_ACTIONS" == "true" ]; then
    echo "Missing gh-pages clone"
    exit 1
  else
    git clone git@github.com:frenchy64/fully-satisfies.git fully-satisfies-gh-pages
  fi
fi
cd fully-satisfies-gh-pages
git checkout gh-pages || git checkout --orphan gh-pages
git reset --hard
if git ls-remote --exit-code --heads git@github.com:frenchy64/fully-satisfies.git gh-pages ; then
  git pull -f origin gh-pages
fi
rm -fr latest
#https://askubuntu.com/a/86891
cp -a ../../target/doc/. latest
cp -a ../../target/doc/. "$CURRENT_VERSION"
git add .
git commit --allow-empty -m "Docs for $CURRENT_VERSION"
git push origin --set-upstream gh-pages
