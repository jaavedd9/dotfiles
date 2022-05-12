#!/usr/bin/env python

import glob
from pathlib import Path

files = glob.glob("/mnt/data/Dropbox/emacs/org_files/*.org")

with open('build.ninja', 'w') as ninja_file:
    ninja_file.write("""
rule org2md
  command = emacs --batch -l ~/.emacs.d/init.el -l ~/.emacs.d/jak/publish.el --eval \"(jethro/publish \\"$in\\")"
  description = org2md $in
""")

    for f in files:
        path = Path(f)
        output_file = f"/mnt/data/Dropbox/emacs/hugo/braindump/content/posts/{path.with_suffix('.md').name}"
        ninja_file.write(f"""
build {output_file}: org2md {path}
""")

import subprocess
subprocess.call(["ninja"])
