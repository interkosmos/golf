cmake_minimum_required(VERSION 3.5)
project(golf)
set(VERSION 0.4)
enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE RELEASE)
endif()

if(CMAKE_Fortran_COMPILER MATCHES "gfortran*")
    set(CMAKE_SKIP_BUILD_RPATH            FALSE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH    TRUE)
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

    set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS}")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-Wall -O0 -g -fbounds-check")
    set(CMAKE_Fortran_FLAGS_RELEASE "-Wall")
endif()

set(TARGET golf)
set(SOURCE_FILES src/golf.f90)

add_library(getopt STATIC src/getopt.f90)
add_executable(${TARGET} ${SOURCE_FILES})
set_target_properties(${TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries(${TARGET} getopt)
