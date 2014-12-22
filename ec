#!/bin/bash

silent= err=0
[[ "$1" = '--' ]] && { silent=true; shift; }
[[ -n "$silent" ]] && exec &>/dev/null

if [[ "$1" =~ ^- ]]
then
	emacsclient "$@"
	err=$?
else
	for file in "$@"; do
		emacsclient -n "$file"
		errn=$?
		[[ "$errn" -ne 0 ]] && err=$errn
	done
fi

[[ -z "$silent" ]] && exit $err || exit 0
