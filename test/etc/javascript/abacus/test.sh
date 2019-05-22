#!/bin/bash
# $1 = EXE
# $2 = Flavor
# $2 = test case number
# exit 0 = success
EXE=$1
FLAVOR=$2
TEST_CASE=$3

# prevent tests from hanging
ulimit -t 1

# prevent generation of core files
ulimit -c 0

pass(){ echo PASS;exit 0; }
fail(){ echo FAIL;exit 1; }
match(){ if $($EXE $1|grep -q -E "$2");then pass;else fail;fi }
count(){ if [ $($EXE $1|grep -c -E "$2") -eq $3 ];then pass;else fail;fi }

set -x
case $FLAVOR in
  animate)
    case $TEST_CASE in
      0) match "" "^Usage: .* NUMBER$";;
      1) match 0 "[ .]{7}[X∘]{}9";;
      2) match 1 "[ .]{7}[X∘]{}9";
         match 1 "[X∘][ .]{6}[X∘]{}8";;
      3) count 15 "[X∘]" 21;;    # (+ 9 (* 6 2)) ;=> 21
      *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
    esac;;
  bead)
    case $TEST_CASE in
      0) match "" "^Usage: .* NUMBER$";;
      1) match 0 "[ .]{7}∘{}9";;
      2) match 1 "[ .]{7}∘{}9";
         match 1 "∘[ .]{6}∘{}8";;
      3) count 10 "∘" 2;;
      *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
    esac;;
  borders);;
  min-lines);;
  orig|calc-lines);;
  space);;
  *) echo "Unmatched FLAVOR:$FLAVOR" >&2; exit 1;;
esac
