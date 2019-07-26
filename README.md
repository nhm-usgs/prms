# PRMS
Precipitation Runoff Modeling System

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
