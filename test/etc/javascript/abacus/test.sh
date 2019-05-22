#!/bin/bash
if [ $1 == "-h" ] || [ $1 == "--help" ];then
  cat <<EOF
Usage: $0 EXE [FLAVOR]... [TEST_CASE]
  Test EXE against tests for FLAVOR.  Exit 0 on success.

  EXE is the path to a abacus executable.

  FLAVOR if specified is a ,-delimited list of flavors to test.
         Flavors may be one of animate, bead, borders, min-lines,
         orig, calc-lines, or space.

  TEST_CASE if specified gives the specific number of the test to run.
            If not specified all tests are run.
EOF
  exit 1
elif [ $1 == "-v" ];then
  VERBOSE="YES"
  shift
else
  VERBOSE=
fi
EXE=$1
if [ -z $2 ];then
  FLAVORS=animate,bead,borders,min-lines,orig,calc-lines,space
else
  FLAVORS=$2
fi
TEST_CASE=$3

# prevent tests from hanging
ulimit -t 1

# prevent generation of core files
ulimit -c 0

pass(){ echo PASS; }
fail(){ echo FAIL;exit 1; }
match(){ if $($EXE $1|grep -q -E "$2");then pass;else fail;fi }
count(){ if [ $($EXE $1|grep -c -E "$2") -eq $3 ];then pass;else fail;fi }
test_case(){
  local case=$1;
  if [ ! -z $VERBOSE];then echo CASE $case;fi
  case $FLAVOR in
    animate)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 "[ .]{7}[X∘]{9}";;
        2) match 1 "[ .]{7}[X∘]{9}";
           match 1 "[X∘][ .]{7}[X∘]{8}";;
        3) count 15 "[X∘]" 22;;   # (+ 10 #|0..9|# (* 6 2)) ;=> 21
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
      esac;;
    bead)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 "[ .]{7}∘{9}";;
        2) match 1 "∘[ .]{7}∘{8}";;
        3) count 10 "∘" 2;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
      esac;;
    borders)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) count 3 "^----------------$" 2;
           count 20 "^----------------$" 3;;
        2) match 1 "[X∘][ .]{7}[X∘]{8}";;
        3) count 10 "[X∘]" 2;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
      esac;;
    min-lines)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) count     3 "[ .X∘]{16}" 10;
           count 33333 "[ .X∘]{16}" 10;; # Always 10 lines.
        2) match 1 "[X∘][ .]{7}[X∘]{8}";;
        3) match 0 "[ .]{7}[X∘]{9}";;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
      esac;;
    orig|calc-lines)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 "[ .]{7}[X∘]{9}";;
        2) match 1 "[X∘][ .]{7}[X∘]{8}";;
        3) count 10 "[X∘]" 2;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
      esac;;
    space)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 " {7}[X∘]{9}";;
        2) match 1 "[X∘] {7}[X∘]{8}";;
        3) count 10 "[X∘]" 2;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 2;
      esac;;
    *) echo "Unmatched FLAVOR:$FLAVOR" >&2; exit 1;;
  esac
}

if [ ! -z $VERBOSE ];then set -x;fi

IFS=,
for FLAVOR in $FLAVORS;do
  if [ -z $TEST_CASE ];then
    for i in {0..3};do
      test_case $i
    done
  else
    test_case $TEST_CASE
  fi
done

exit 0
