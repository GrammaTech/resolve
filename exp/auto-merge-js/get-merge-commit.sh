#!/bin/bash
#
# Output the merge commit in PROJECT corresponding to the
# given git hashes representing the branches involved
# in the merge.
#
# $1 - JavaScript project directory
# $2 - Commit hash for the left branch of the merge
# $3 - Commit hash for the right branch of the merge

PROJECT=$1
LEFT=$2
RIGHT=$3

pushd $1 > /dev/null
for commit in $(git log --pretty="format:%h");
do
    git show $commit | egrep -q "^Merge: $LEFT $RIGHT"
    if [ $? -eq 0 ]; then
        RESOLUTION=$commit
        break
    fi
done
popd > /dev/null

if [ ! -z $RESOLUTION ]; then
    echo $RESOLUTION
    exit 0
else
    exit 1
fi
