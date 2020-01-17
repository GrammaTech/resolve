#!/bin/bash
# $1 = FILE
# $2 = test case number
# exit 0 = success

case $2 in
    0) grep "fast" $1 ;;
    1) grep "brown" $1 ;;
    2) grep "hops" $1 ;;
esac
