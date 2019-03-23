#!/bin/bash
if [ ! -f gt-harness-common.sh ];then
  echo "missing gt-harness-common.sh"
  exit 1
else
  cp gt-harness-common.sh /gt-harness-common.sh
fi
if [ ! -f gt-harness.sh ];then
  echo "missing gt-harness.sh"
  exit 1
else
  cp gt-harness.sh /gt-harness.sh
fi

TOTAL=$(git rev-list --all --count)
COUNT=0
FAIL=0
for COMMIT in $(git rev-list --all);do
  ((COUNT++))
  printf "\r%6d/%d\t%s" $COUNT $TOTAL $COMMIT
  git reset --hard $COMMIT >/dev/null 2>/dev/null
  git clean -fdx >/dev/null 2>/dev/null
  cp /gt-harness-common.sh ./ || exit 1
  cp /gt-harness.sh ./ || exit 1
  (./gt-harness.sh prep && bear ./gt-harness.sh build) >/dev/null 2>/dev/null
  if [ $? -eq 0 ];then
    mv compile_commands.json /cdbs/${COMMIT}.json
  else
    touch /cdbs/${COMMIT}.fail 
    ((FAIL++))
  fi >/dev/null 2>/dev/null
done
printf "\n%6d/%d\tFAILED\n" $FAIL $TOTAL
