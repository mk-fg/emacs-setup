#!/usr/bin/env python3

import itertools as it, operator as op, functools as ft
import os, sys, pathlib, contextlib, re

## Example format:
# # Empty lines and comments are translated,
# #  comma/space is sep, pre-: prefix will be translated to comment
# CJK Unified Ideographs: 4E00-62FF, 6300-77FF, 7800-8CFF, 8D00-9FFF
# CJK-UI Ext A: 3400-4DBF
# CJK-UI Ext B: 20000-215FF, 21600-230FF, 23100-245FF, 24600-260FF, 26100-275FF, 27600-290FF, 29100-2A6DF
# CJK-UI Ext C: 2A700-2B73F
# CJK-UI Ext D: 2B740–2B81F
# CJK-UI Ext E: 2B820–2CEAF
# CJK Compatibility Ideographs: F900–FAFF
# Legacy: 3300–33FF FE30–FE4F F900–FAFF 2F800–2FA1F

src, = sys.argv[1:]
with pathlib.Path(src).open() as src:
	for line in src:
		line = line.strip()
		if not line:
			print()
			continue
		if line.startswith('#'):
			print(';;;;', line.lstrip('# '))
			continue
		if ':' in line:
			comment, line = line.split(':', 1)
			print(';;', comment.rstrip())

		line = re.sub(r'\s*[-–]\s*', '-', line).replace(',', ' ').split()
		cons_list, char_re = list(), re.compile(r'^[0-9A-F]+\Z')
		for cons in line:
			a, b = map(str.upper, cons.split('-', 1))
			assert char_re.search(a) and char_re.search(b)
			cons_list.append('(#x{} . #x{})'.format(a, b))
		print(' '.join(cons_list))
