# NrrdChecker

Compares the [NRRD](http://teem.sourceforge.net/nrrd/) header keys of two or
more structural or diffusion weighted MRI images and prints (or saves) a
csv with one row per key.

## Run

```
nrrdchecker -h

Usage: nrrdchecker (-i|--in NRRD) (-i|--in NRRD) [-i|--in NRRD]
                   [-o|--out OUTFILE] [--spaceDirections DOUBLE]
                   [--measurementFrame DOUBLE] [--spaceOrigin DOUBLE]
                   [--gradientDirection DOUBLE]

Available options:
  -i,--in NRRD             a Nrrd file
  -i,--in NRRD             a Nrrd file
  -i,--in NRRD             a Nrrd file
  -o,--out OUTFILE         Output csv (if omitted, prints to stdout)
  --spaceDirections DOUBLE Epsilon tolerance for
                           spaceDirections (default: 5.0e-2)
  --measurementFrame DOUBLE
                           Epsilon tolerance for
                           measurementFrame (default: 5.0e-2)
  --spaceOrigin DOUBLE     Epsilon tolerance for spaceOrigin (default: 5.0e-2)
  --gradientDirection DOUBLE
                           Epsilon tolerance for
                           gradientDirection (default: 5.0e-2)
  -h,--help                Show this help text
```



## Example

    nrrdchecker -i testdata/dwi.nhdr -i testdata/dwi-gradientdirection-diff.nhdr
    nrrdchecker -i testdata/dwi.nhdr -i testdata/dwi-decimal-measurementframe.nhdr -o nrrdcompare.csv
