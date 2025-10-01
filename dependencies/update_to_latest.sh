#!/bin/bash

wget https://github.com/QMeS-toolbox/QMeS-Derivation/archive/refs/heads/main.zip -O QMeS.zip

wget https://github.com/satfra/TensorBases/archive/refs/heads/main.zip -O TensorBases.zip

# This version has a patch for tform usage
wget https://github.com/satfra/FormTracer/archive/refs/heads/master.zip -O FormTracer-of.zip
unzip ./FormTracer-of.zip
mv FormTracer-master/FormTracer.zip ./
rm -rf ./FormTracer-master ./FormTracer-of.zip
