#!/bin/bash -eu
stack exec nrrdchecker -- -i test-data/mask.nrrd -r test-data/mask.nrrd
stack exec nrrdchecker -- -i test-data/mask.nrrd -r test-data/ref.nhdr
stack exec nrrdchecker -- -i test-data/dwi.nhdr -r test-data/dwi.nhdr
stack exec nrrdchecker -- -i test-data/bad.nhdr -r test-data/ref.nhdr
