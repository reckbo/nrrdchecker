# NrrdChecker

Checks that two structural or diffusion weighted MRI images in [NRRD](http://teem.sourceforge.net/nrrd/) format have the same values for matching keys.

## Run

```
nrrdchecker --help
The nrrdcheckerargs program

nrrdcheckerargs [OPTIONS]

Common flags:
  -i --innrrd=NRRD
  -r --refnrrd=NRRD
  -? --help          Display help message
  -V --version       Print version information
```

## Example

```
nrrdchecker -i test-data/test.nhdr -r test-data/ref.nhdr
test-data/test.nhdr,pass
```

```
nrrdchecker -i test-data/test.nhdr -r test-data/ref2.nhdr
INPUT: test-data/test.nhdr
  REF: test-data/ref2.nhdr
* space directions:
 input value: VSpaceDirections (StructuralSpace (0.0,1.0,0.0) (0.0,0.0,-1.0) (-1.0,0.0,0.0))
   ref value: VSpaceDirections (StructuralSpace (0.0,2.0,0.0) (0.0,0.0,-1.0) (-1.0,0.0,0.0))
test-data/test.nhdr,fail
```
