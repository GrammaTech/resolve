#!/bin/bash
#
# Test this script works against all original with:
#
#     for script in abacus-*.js;do
#       echo "#### $(echo $script|sed 's/^[^-]*-//;s/.js//')"
#       ./test.sh ./$script $(echo $script|sed 's/^[^-]*-//;s/.js//')
#     done
#
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
SUCCESS=0
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
fail(){ echo FAIL; ((SUCCESS++)); }
match(){ if $($EXE $1|grep -q -E "$2");then pass;else fail;fi }
# Wipe all output except the last frame.
# Useful to normalize output in the face of animation.
last_frame(){ tr '\033[H' 'CLR'|tr '\n' 'z'|sed 's/^.*zCLR//'|tr 'z' '\n'; }
count(){
  local count=$3
  if $(echo $FLAVORS|grep -q min-lines);then count=$4;fi
  if [ $($EXE $1|grep -c -E "$2") -eq $count ];then pass;else fail;fi }
count_last_frame(){
  local count=$3
  if $(echo $FLAVORS|grep -q min-lines);then count=$4;fi
  if [ $($EXE $1|last_frame|grep -c -E "$2") -eq $count ];then pass;else fail;fi }
test_case(){
  local case=$1;
  if [ ! -z $VERBOSE];then echo CASE $case;fi
  case $FLAVOR in
    animate)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 "[ .]{7}[XO]{9}";;
        2) match 1 "[ .]{7}[XO]{9}";
           match 1 "[XO][ .]{7}[XO]{8}";;
        3) count 15 "[XO]" 22 160;;   # (+ 10 #|0..9|# (* 6 2)) ;=> 21
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 127;
      esac;;
    bead)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 "[ .]{7}O{9}";;
        2) match 1 "O[ .]{7}O{8}";;
        3) count_last_frame 10 "O" 2 10;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 127;
      esac;;
    borders)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) count_last_frame 3 "^----------------$" 2 11;
           count_last_frame 20 "^----------------$" 3 11;
           # Last line should be a border
           if $($EXE 4|tail -1|grep -q "^----------------$");then pass;else fail;fi;;
        2) match 1 "[XO][ .]{7}[XO]{8}";;
        3) count_last_frame 10 "[XO]" 2 10;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 127;
      esac;;
    min-lines)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) count_last_frame   3 "[ .XO]{16}" NaN 10;
           count_last_frame 333 "[ .XO]{16}" NaN 10;; # Always 10 lines.
        2) match 1 "[XO][ .]{7}[XO]{8}";;
        3) match 0 "[ .]{7}[XO]{9}";;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 127;
      esac;;
    orig|calc-line)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 "[ .]{7}[XO]{9}";;
        2) match 1 "[XO][ .]{7}[XO]{8}";;
        3) count_last_frame 10 "[XO]" 2 10;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 127;
      esac;;
    space)
      case $case in
        0) match "" "^Usage: .* NUMBER$";;
        1) match 0 " {7}[XO]{9}";;
        2) match 1 "[XO] {7}[XO]{8}";;
        3) count_last_frame 10 "[XO]" 2 10;;
        *) echo "Test case $TEST_CASE is not implemented." >&2;exit 127;
      esac;;
    *) echo "Unmatched FLAVOR:$FLAVOR" >&2; exit 126;;
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

exit $SUCCESS
