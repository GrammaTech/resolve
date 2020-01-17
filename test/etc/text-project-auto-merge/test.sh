#!/bin/bash
# $1 = DIR
# $2 = test case number
# exit 0 = success

case $2 in
    0) grep -r "fast" $1 ;;
    2) grep -r "brown" $1 ;;
    2) grep -r "hops" $1 ;;
esac
