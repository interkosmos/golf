cmake_minimum_required(VERSION 3.9)
project(Life)

set(VERSION 1.0)

include(CMakeForceCompiler)

CMAKE_FORCE_Fortran_COMPILER(gfortran* "GNU")

set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -ffree-form -std=f2008 -fbackslash -fimplicit-none -fall-intrinsics")
set(CMAKE_Fortran_FLAGS_DEBUG   "-Wall -O0 -g3 -fbounds-check")
set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -march=native")

enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release")
endif()

set(SOURCE_FILES life.f90)
add_library(f90getopt STATIC f90getopt.f90)

add_executable(life ${SOURCE_FILES})
set_target_properties(life PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries(life f90getopt)