#!/bin/bash

err=0 silent= # silent - no stderr or exit code, to run from keybind or such
[[ "$1" != '--' ]] || { silent=t; shift; exec &>/dev/null; }

if [[ "$1" =~ ^- ]]
# Mode 1: "ec -opt1 --opt2 file ..." - pass options/files as-is to emacsclient
then emacsclient "$@"; err=$?
else
	# Mode 2: default one - pass files and do extra stuff
	#  - "ec file1 file2 ..." - just open stuff
	#  - "ec +123 file1 +12 file2 ..." - open file(s) on specific lines
	#  - "ec ++trim file1 ..." - pre-strips leading/trailing empty lines from file(s)
	offset=() cmd=() err=0
	for a in "$@"; do
		[[ ! "$a" =~ ^\+ ]] || {
			if [[ "$a" = ++trim ]]; then cmd=( sed -i -e :a -e '/./,$!d;/^\n*$/{$d;N;};/\n$/ba' )
			# elif [[ "$a" =~ ... ]]; then ...
			else offset=( "$a" ); fi
			continue; }
		[[ ${#cmd[@]} -eq 0 ]] || "${cmd[@]}" "$a"
		emacsclient -n "${offset[@]}" "$a"
		err_file=$?; [[ "$err_file" -eq 0 ]] || err=$err_file
		offset=()
	done
fi

[[ -z "$silent" ]] || err=0
exit $err
