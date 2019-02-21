# Game of Life (Fortran)
Simple text-based clone of the popular cellular automaton written in Fortran
2003. Should run on most Unix-like operating systems.

![Screen Shot](screenshot.png)

## Build
CMake and a Fortran 2003 compiler are required to build the game:
```
$ mkdir build && cd build/
$ cmake ..
$ make
```
If you do not want to use CMake, run:
```
$ gfortran8 -c f90getopt.f90
$ gfortran8 -Wl,-rpath=/usr/local/lib/gcc8/ -o life life.f90 f90getopt.o
```

## Run
```
$ ./life --file world.txt --columns 30 --rows 10 --generations 100
```

## Licence
ISC
