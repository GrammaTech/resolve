#!/bin/bash
cp gt-harness-common.sh /gt-harness-common.sh
cp gt-harness.sh /gt-harness.sh

TOTAL=$(git rev-list --all --count)
COUNT=0
FAIL=0
for COMMIT in $(git rev-list --all);do
  ((COUNT++))
  printf "\r%6d/%d\t%s" $COUNT $TOTAL $COMMIT
  git reset --hard $COMMIT
  git clean -fdx
  cp /gt-harnes-common.sh ./
  cp /gt-harnes.sh ./
  ./gt-harness.sh prep && bear ./gt-harness.sh build
  if [ $? -eq 0 ];then
    mv compile_commands.json /cdbs/${COMMIT}.json
  else
    ((FAIL++))
  fi
done
printf "\n%6d/%d\tFAILED\n" $FAIL $TOTAL
