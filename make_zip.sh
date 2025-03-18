#!/bin/bash

mkdir -p FunKit

cp FunKit.m FunKit
cp PacletInfo.m FunKit
cp LICENSE FunKit
cp README.md FunKit
cp -r examples FunKit

mkdir FunKit/modules
cp -r modules/*.m FunKit/modules/

if [[ -e ./TensorBases.zip ]]; then
  rm ./FunKit.zip
fi
zip -r FunKit.zip FunKit

rm -rfv ./FunKit/
