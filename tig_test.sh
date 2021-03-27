#!/bin/bash

test_dir=$1
test_out=$3
tigs=$(find $test_dir -type f -not -name custom.tig)
rm -f $test_out
$(echo "Test Directory: ${test_dir}" >> $test_out)
$(echo "" >> $test_out)
for file in $tigs; do
    $(echo - $(basename $file) >> $test_out)
    ./$2 -P $file >> $test_out 2>&1
    $(echo "" >> $test_out)
done