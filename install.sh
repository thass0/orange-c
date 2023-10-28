#!/usr/bin/env bash


# Get the install path for the orange-c driver script.
maybeinstallpath="$HOME/.local/bin"
installpath=""
previous_IFS="$IFS"
export IFS=":"
for p in $PATH; do
    [ "$p" = "$maybeinstallpath" ] && installpath="$maybeinstallpath"
done
export IFS="$previous_IFS"

if [ -z "$installpath" ]; then
    >&2 echo "$maybeinstallpath is not part of PATH"
    >&2 echo "Add it to PATH and run this script again"
    exit 1
fi

set -xe

stack install
cp orange-c "$installpath"

set +x

echo "Done"
