#!/bin/bash
#
# Test harness for auto-merge JavaScript experiments.
#
# Run the test suite for the project given as the first argument.
# For those conflicted merges which touch the test directory,
# copy the test files from the resolved merge.
#
# $1 - JavaScript project directory
# $2 - Commit hash for the resolution of the merge

# Enter the project directory.
cd $1

# Store the resolution commit hash.
RESOLUTION=$2

# Determine if a test file has been modified.
# If so, checkout the file from the merge resolution.
for file in $(git diff --name-only $RESOLUTION);
do
    echo $file | grep -q -E "^test/|^tests/|^__tests__/"
    if [ $? -eq 0 ]; then
        git checkout $RESOLUTION -- $file
    fi
done

# Run the tests
timeout --signal 9 300 npm install && \
timeout --signal 9 300 npm test
