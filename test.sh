#!/usr/bin/env bash

BIN="stack exec -- nrrdchecker"

test() {
    echo "# TEST =================================="
    cmd=$1
    resultExpect=$2
    out=$($cmd)
    result=$(echo $out | cut -d"," -f2)
    if [[ "$result" == "$resultExpect" ]]; then
        echo "Test passed."
    else
        echo "Test failed"
    fi
    echo "========================================="
}

test "$BIN -i test-data/mask.nrrd -r test-data/mask.nrrd" "pass"
test "$BIN -i test-data/mask.nrrd -r test-data/ref.nhdr" "fail"
test "$BIN -i test-data/dwi.nhdr -r test-data/dwi.nhdr" "pass"
test "$BIN -i test-data/bad.nhdr -r test-data/ref.nhdr" "fail"
test "$BIN -i test-data/dwi.nhdr -r test-data/dwi2.nhdr" "pass"
test "$BIN -i test-data/dwi.nhdr -r test-data/dwi3.nhdr" "fail"
