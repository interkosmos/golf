# Conway’s Game of Life
Simple text-based clone of the popular cellular automaton written in Fortran
2003/2008. Should run on must Unix-like operating systems. Uses `f90getopt` by
[Hani Andreas Ibrahim](https://github.com/haniibrahim/f90getopt).

## Build
CMake and GNU Fortran are required to build the game. If you have GCC 7
installed, run:
```
$ cmake -DCMAKE_Fortran_COMPILER=gfortran7 -DCMAKE_INSTALL_RPATH=/usr/local/lib/gcc7
$ make
```
Depending on your operating system, you may change `gfortran7` to `gfortran` and
`gcc7` to `gcc`.

## Run
Execute the game with the desired number of generations (default is 60):
```
$ ./life --generations 20
```

## Licence
ISC
