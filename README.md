# Precipitation-Runoff Modeling System (PRMS)

## Table of contents
* [General information](#general-information)
* [Technologies](#technologies)
* [Setup](#setup)

## General information
The Precipitation-Runoff Modeling System is a modular, deterministic,
distributed-parameter, physical-process-based hydrologic simulation
code developed to evaluate effects of various combinations of climate,
physical characteristics, and simulation options on hydrologic
response and water distribution at the watershed scale.

For more information, please see [Documentation of the Dynamic Parameter, Water-Use, Stream and Lake Flow Routing, and Two Summary Output Modules and Updates to Surface-Depression Storage Simulation and Initial Conditions Specification Options With the Precipitation-Runoff Modeling System (PRMS)](https://pubs.usgs.gov/tm/06/b8/tm6b8.pdf).

## Technologies
The system is built with:
* [GNU Fortran](https://gcc.gnu.org/fortran/) version: 8.3.0
* [CMake](https://cmake.org/) version: 3.8.0 or greater
	
## Setup
To compile PRMS from source on Unix/Linux/POSIX systems:

### Build and Install Coretran
1. Clone [Coretran](https://github.com/leonfoks/coretran) from GitHub.
2. In the `coretran` directory:

```
    mkdir build
    cd build
    cmake -DCMAKE_Fortran_COMPILER=/usr/bin/gfortran -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_INSTALL_PREFIX="/usr/local/coretran/debug" -DBUILD_SHARED_LIBS=ON ../src
    make
    make install
    cmake -DCMAKE_Fortran_COMPILER=/usr/bin/gfortran -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX="/usr/local/coretran/release" -DBUILD_SHARED_LIBS=ON ../src
    make
    make install
```

### Other prerequisite libraries:
The following libraries are required to build PRMS 6. They are likely available as binary packages via your distro's package
manager:
* [zlib](http://www.zlib.net/)
* [HDF5](https://www.hdfgroup.org/solutions/hdf5)
* [netCDF-C](https://www.unidata.ucar.edu/downloads/netcdf/index.jsp)
* [netCDF-Fortran](https://github.com/Unidata/netcdf-fortran/releases/tag/v4.5.3)

### Build PRMS:
```
cd prms
mkdir build
cd build
cmake -DCMAKE_FORTRAN_COMPILER=/usr/bin/gfortran -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_INSTALL_PREFIX=/usr/local -DBUILD_SHARED_LIBS=ON -DCMAKE_PREFIX_PATH=/usr/local/lib/cmake ../src
make
make install
```
