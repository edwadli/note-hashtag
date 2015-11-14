#!/bin/bash

GIT_HASH=`git describe --abbrev --dirty --always`
GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
GIT_TAG=`git describe --abbrev=0 --tags`

OUT_PATH='version.ml'

echo "(* Build script automatically updates these strings *)" > $OUT_PATH
echo "let release () = \"$GIT_TAG\"" >> $OUT_PATH
echo "let build () = \"$GIT_HASH on branch $GIT_BRANCH\"" >> $OUT_PATH
