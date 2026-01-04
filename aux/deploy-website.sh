#!/usr/bin/env bash

readonly SCRIPT_DIR="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"

if command -v git >/dev/null ; then
    echo "Git found! Using Git to find Synnax root."
    readonly SYNNAX_ROOT="$(git rev-parse --show-toplevel)"
else
    echo "Git not found. Assuming Synnax hierarchy!"
    readonly SYNNAX_ROOT="$(realpath "$SCRIPT_DIR/..")"
fi
echo "SYNNAX_ROOT=$SYNNAX_ROOT"
exit 0

guix deploy "$SYNNAX_ROOT/deploys/website.scm"

guix deploy "$SYNNAX_ROOT/deploys/website.scm" \
     --execute -- herd restart website-deploy-personal
