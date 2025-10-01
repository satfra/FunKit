#!/bin/bash

wolfram_app_dir=$1
mkdir -p ${wolfram_app_dir}

# Get the path where this script is located
script_path="$(
  cd -- "$(dirname "$0")" >/dev/null 2>&1
  pwd -P
)"

if [[ ! -d "${wolfram_app_dir}/QMeSderivation" ]]; then
  echo "Installing QMeS to ${wolfram_app_dir}/QMeSderivation"
  rm -rf ${script_path}/QMeS-Derivation-main
  unzip ${script_path}/QMeS.zip -d ${script_path} &>/dev/null
  mv ${script_path}/QMeS-Derivation-main ${wolfram_app_dir}/QMeSderivation
fi

if [[ ! -d "${wolfram_app_dir}/FormTracer" ]]; then
  echo "Installing FormTracer to ${wolfram_app_dir}/FormTracer"
  rm -rf ${script_path}/FormTracer/
  unzip ${script_path}/FormTracer.zip -d ${script_path} &>/dev/null
  mv ${script_path}/FormTracer ${wolfram_app_dir}/FormTracer
fi

if [[ ! -d "${wolfram_app_dir}/TensorBases" ]]; then
  echo "Installing TensorBases to ${wolfram_app_dir}/TensorBases"
  rm -rf ${script_path}/TensorBases-main
  unzip ${script_path}/TensorBases.zip -d ${script_path} &>/dev/null
  mv ${script_path}/TensorBases-main ${wolfram_app_dir}/TensorBases
fi
