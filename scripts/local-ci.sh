#!/usr/bin/env bash
# scripts/local-ci.sh — run the CI workflow's steps locally as the
# `github-actions` user without going through GitHub. Use to iterate on the
# runner setup or the workflow itself without paying the cloud round-trip.
#
# Usage (needs sudo to switch user; auto-elevates):
#
#   scripts/local-ci.sh smoke           # ~30s: license + DoFun + kernels + compilers
#   scripts/local-ci.sh build           # cmake + make install (no tests)
#   scripts/local-ci.sh test [SUITE...] # build, then targeted suite (e.g. FEDeriK)
#   scripts/local-ci.sh full            # build + full make test (slow, ~15 min)
#
# What it emulates from the systemd unit:
#   - User=github-actions, HOME=/var/lib/github-actions  (real)
#   - Environment SystemIDList="Linux-x86-64 Linux"      (real, via -E)
#   - Environment PATH=/usr/local/sbin:/usr/local/bin:...
#
# What it does NOT emulate:
#   - ProtectSystem=strict / ReadWritePaths / InaccessiblePaths / PrivateTmp.
#     `/home/franz` is unreadable to the runner user anyway via filesystem perms
#     (mode 700), so the most important containment still applies.
#
# The host working tree at /mnt/data/Documents/Uni/Code/FunKit is rsync'd into
# /var/lib/github-actions/_work-local/FunKit (excluding build/ and .git) so
# uncommitted edits are exercised. Push not required.

set -euo pipefail

MODE="${1:-smoke}"
shift || true

REPO_HOST="/mnt/data/Documents/Uni/Code/FunKit"
WORK="/var/lib/github-actions/_work-local"
RUNNER_USER="github-actions"
RUNNER_HOME="/var/lib/github-actions"
RUNNER_PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin:/bin"

if [ "$(id -u)" -ne 0 ]; then
  exec sudo "$0" "$MODE" "$@"
fi

mkdir -p "$WORK"
chown "$RUNNER_USER:$RUNNER_USER" "$WORK"

# Mirror the host working tree (incl. uncommitted edits) into a runner-owned
# workspace. Exclude build/ so we don't carry stale CMake state across runs.
rsync -a --delete --exclude=build/ --exclude=.git/ \
  "$REPO_HOST/" "$WORK/FunKit/"
chown -R "$RUNNER_USER:$RUNNER_USER" "$WORK/FunKit"

# `sudo -u USER env -i ...`: env -i clears all inherited vars, then we set only
# the ones the systemd unit sets. Matches the failure conditions we hit
# (uname / OS detection in subkernels, license search path).
#
# We also explicitly cd into the runner's HOME before exec'ing the command.
# Without that, CWD is inherited from whoever invoked sudo — likely /home/franz,
# which the github-actions user cannot enter (mode 700). The kernel would then
# print `ResetDirectory::cdir` warnings on startup, and FORM (invoked via
# RunProcess, which inherits CWD) would exit code 1 trying to write scratch
# files. The real systemd unit avoids this with WorkingDirectory=.
as_runner() {
  local cwd="${CD_TARGET:-$RUNNER_HOME}"
  sudo -u "$RUNNER_USER" env -i \
    HOME="$RUNNER_HOME" \
    USER="$RUNNER_USER" \
    LOGNAME="$RUNNER_USER" \
    SHELL="/bin/bash" \
    LANG="C.UTF-8" \
    TERM="dumb" \
    PATH="$RUNNER_PATH" \
    SystemIDList="Linux-x86-64 Linux" \
    bash -c 'cd "$1" && shift && exec "$@"' bash "$cwd" "$@"
}

case "$MODE" in
  smoke)
    as_runner wolfram -script "$WORK/FunKit/scripts/smoke.m"
    ;;

  build)
    as_runner bash -c '
      set -euo pipefail
      cd "'"$WORK"'/FunKit"
      mkdir -p build
      cd build
      cmake -DCMAKE_BUILD_TYPE=Release ..
      make install
    '
    ;;

  test)
    SUITE="${*:-}"
    as_runner bash -c "
      set -euo pipefail
      cd \"$WORK/FunKit\"
      mkdir -p build && cd build
      cmake -DCMAKE_BUILD_TYPE=Release ..
      make install
      if [ -n \"$SUITE\" ]; then
        make test-single FILE=\"$SUITE\"
      else
        make test
      fi
    "
    ;;

  full)
    as_runner bash -c "
      set -euo pipefail
      cd \"$WORK/FunKit\"
      mkdir -p build && cd build
      cmake -DCMAKE_BUILD_TYPE=Release ..
      make install
      set -o pipefail
      make test 2>&1 | tee make-test.log
    "
    ;;

  *)
    echo "Unknown mode: $MODE" >&2
    echo "Usage: $0 {smoke|build|test [SUITE]|full}" >&2
    exit 2
    ;;
esac
