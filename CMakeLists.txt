cmake_minimum_required(VERSION 3.5)
project(f90life)
set(VERSION 0.1)
enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE RELEASE)
endif()

if(CMAKE_Fortran_COMPILER MATCHES "gfortran*")
    set(CMAKE_SKIP_BUILD_RPATH            FALSE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH    TRUE)
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

    set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -ffree-form -std=f2008 -fimplicit-none -fall-intrinsics -fbackslash")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-Wall -O0 -g3 -fbounds-check")
    set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -march=native")
endif()

set(TARGET life)
set(SOURCE_FILES life.f90)

add_library(f90getopt STATIC f90getopt.f90)

add_executable(${TARGET} ${SOURCE_FILES})
target_link_libraries(${TARGET} f90getopt)
set_target_properties(${TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
