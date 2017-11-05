#!/bin/bash

silent= err=0
[[ "$1" = '--' ]] && { silent=true; shift; }
[[ -n "$silent" ]] && exec &>/dev/null

if [[ "$1" =~ ^- ]]
then
	emacsclient "$@"
	err=$?
else
	offset=()
	for file in "$@"; do
		[[ "$file" =~ ^\+ ]] && {
			offset+=( "$file" )
			continue
		}
		emacsclient -n "${offset[@]}" "$file"
		err_file=$?
		[[ "$err_file" -ne 0 ]] && err=$err_file
		offset=()
	done
fi

[[ -z "$silent" ]] || err=0
exit $err
