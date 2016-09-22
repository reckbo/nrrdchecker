#!/bin/bash

test() {
    echo "# TEST =================================="
    cmd=$1
    resultExpect=$2
    out=$($cmd) 
    result=$(echo $out | cut -d"," -f2)
    if [[ "$result" == "$resultExpect" ]]; then
        echo "# PASS"
    else
        echo "# FAIL"
    fi
    echo "========================================="
}

test "nrrdchecker -i test-data/mask.nrrd -r test-data/mask.nrrd" "pass"
test "nrrdchecker -i test-data/mask.nrrd -r test-data/ref.nhdr" "fail"
test "nrrdchecker -i test-data/dwi.nhdr -r test-data/dwi.nhdr" "pass"
test "nrrdchecker -i test-data/bad.nhdr -r test-data/ref.nhdr" "fail"
