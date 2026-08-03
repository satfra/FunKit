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

# TensorBases carries a minimum version, unlike the other two dependencies.
# From 1.3.0 on, TBMakePropagator expands the inverse propagator with all momenta
# incoming; older versions return every momentum-odd propagator dressing -- for
# instance the pslash dressing of the quark propagator -- with the wrong sign
# relative to the momentum-even ones, and do so silently. So this block does what
# the QMeS/FormTracer blocks above do not: it replaces an installation that is
# already there but too old.
tb_required="1.3.0"
tb_dir="${wolfram_app_dir}/TensorBases"

# Pull x.y.z out of a TensorBases PacletInfo.m; 0.0.0 if it cannot be read.
tb_paclet_version() {
  local info="$1" v=""
  if [[ -f "${info}" ]]; then
    v=$(sed -n 's/.*"Version"[[:space:]]*->[[:space:]]*"\([0-9][0-9.]*\)".*/\1/p' "${info}" | head -n1)
  fi
  echo "${v:-0.0.0}"
}

# version_ge A B -- true if A >= B. sort -V is not available on BSD sort.
version_ge() {
  [[ "$1" == "$2" ]] && return 0
  [[ "$(printf '%s\n%s\n' "$1" "$2" | sort -t. -k1,1n -k2,2n -k3,3n | head -n1)" == "$2" ]]
}

tb_bundled=$(unzip -p "${script_path}/TensorBases.zip" 'TensorBases-main/PacletInfo.m' 2>/dev/null |
  sed -n 's/.*"Version"[[:space:]]*->[[:space:]]*"\([0-9][0-9.]*\)".*/\1/p' | head -n1)
tb_bundled="${tb_bundled:-0.0.0}"

if ! version_ge "${tb_bundled}" "${tb_required}"; then
  echo "ERROR: dependencies/TensorBases.zip bundles TensorBases ${tb_bundled}, which is below" >&2
  echo "       the required ${tb_required}. Run dependencies/update_to_latest.sh to refresh it." >&2
  exit 1
fi

if [[ -d "${tb_dir}" ]]; then
  tb_have=$(tb_paclet_version "${tb_dir}/PacletInfo.m")
  if ! version_ge "${tb_have}" "${tb_required}"; then
    echo "TensorBases ${tb_have} is installed at ${tb_dir}, but FunKit requires ${tb_required} or newer."
    if [[ -L "${tb_dir}" ]]; then
      # A symlinked development checkout. Renaming it would move the user's own
      # working tree out from under them, so refuse and let them update it.
      echo "  ${tb_dir} is a symlink, so it is left untouched." >&2
      echo "  Update the TensorBases checkout it points at to ${tb_required} or newer." >&2
      exit 1
    fi
    echo "  Moving the old installation to ${tb_dir}.${tb_have}.bak"
    rm -rf "${tb_dir}.${tb_have}.bak"
    mv "${tb_dir}" "${tb_dir}.${tb_have}.bak"
    # The basis cache is deliberately not carried over: its serialisation format
    # changed between 1.1.6 and 1.3.0, so the first load rebuilds it.
    echo "  The basis cache is not carried over; the first load will rebuild it."
  fi
fi

if [[ ! -d "${tb_dir}" ]]; then
  echo "Installing TensorBases ${tb_bundled} to ${tb_dir}"
  rm -rf ${script_path}/TensorBases-main
  unzip ${script_path}/TensorBases.zip -d ${script_path} &>/dev/null
  mv ${script_path}/TensorBases-main "${tb_dir}"
fi
