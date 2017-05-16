#!/usr/bin/env bash

BIN="stack exec -- nrrdchecker"

TESTDIR=testdata

nhdrs=$(ls $TESTDIR/*-*nhdr)
for nhdr in $nhdrs; do
    echo "###################################################"
    echo "# $nhdr"
    bn=$(basename $nhdr)
    stem=${bn%.*}
    csv=$TESTDIR/$stem.csv
    ref=${nhdr%%-*}.nhdr
    cmd="$BIN -i $ref -i $nhdr"
    echo $cmd
    if [ ! -e $csv ]; then
        $cmd > $csv
        echo "Made '$csv'"
    else
        d=$(diff <($cmd) $csv)
        if [ "$d" ]; then
            echo "FAIL: $nhdr"
            echo $d
        else
            echo "PASS: $nhdr"
        fi
    fi
    echo
done
