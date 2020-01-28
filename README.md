# PRMS
Precipitation Runoff Modeling System

# Provisional software
This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

# Building From Source
PRMS6 uses the cmake build system (version 3.8 or greater). To compile PRMS from source on Unix/Linux/POSIX systems run the following from the top-level prms directory:

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=<install_location> -DBUILD_SHARED_LIBS=OFF ../src
make
make install
```

# Debug builds
To build prms for debugging purposes, run:

```
cmake -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_INSTALL_PREFIX=<install_location> -DBUILD_SHARED_LIBS=OFF ../src
make
make install
```

To output additional compile-time messages add VERBOSE=1 to the make command.
