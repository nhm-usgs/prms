Compile PRMS6 LINUX OS

For compiling a new (recent - supports Fortran 2008) version of gFortran is required.  I used Devtoolset-7 from https://www.softwarecollections.org/en/scls/rhscl/devtoolset-7 . Once downloaded it can be instantiated by:
1) scl enable devtoolset-7 bash

 # Coretran
1) clone coretran from: [GitHub - leonfoks/coretran: An easy to follow library to make Fortran easier in general with wrapped interfaces, sorting routines, kD-Trees, and other algorithms to handle scientific data and concepts. The library contains core fortran routines and object-oriented classes.](https://github.com/leonfoks/coretran)
2) In coretran directory - watch for long lines below
``` bash
mkdir build
cd build
cmake -DCMAKE_Fortran_COMPILER=/opt/rh/devtoolset-7/root/usr/bin/gfortran -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_INSTALL_PREFIX="/usr/local/coretran/debug" -DBUILD_SHARED_LIBS=ON ../src
make
sudo make install
cmake -DCMAKE_Fortran_COMPILER=/opt/rh/devtoolset-7/root/usr/bin/gfortran -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX="/usr/local/coretran/release" -DBUILD_SHARED_LIBS=ON ../src
make
sudo make install
```
 # HDF and NetCDF
The following directions essentially follow the directions from Here: [NetCDF: Getting and Building netCDF](https://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html)

 ## zlib
1) Download zlib from [zlib Home Site](http://www.zlib.net/)
2) Enter the following commands
``` bash 
cd zlib-1.2.11
ZDIR=/usr/local
./configure --prefix=${ZDIR}
make check
surdo make install
```

 ## HDF
1) Download HDF from [HDF5Â® Source Code - The HDF Group](https://www.hdfgroup.org/downloads/hdf5/source-code/)
2) Enter the following commands
``` bash
cd hdf5-1.10.5/
H5DIR=/usr/local
./configure --with-zlib=${ZDIR} --prefix=${H5DIR} --enable-hl
make check
sudo make install
```
 ## Netcdf-c
1) Download NetCDF (netcdf-c-4.6.2.tar.gz and netcdf-fortran-4.4.4.tar.gz) from [NetCDF Downloads](https://www.unidata.ucar.edu/downloads/netcdf/index.jsp)
2) Enter the following commands 
``` bash
cd netcdf-c-4.7.2/
NCDIR=/usr/local
CPPFLAGS='-I${H5DIR}/include -I${ZDIR}/include' LDFLAGS='-L${H5DIR}/lib -L${ZDIR}/lib' ./configure --prefix=${NCDIR}
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${H5DIR}/lib
make check
sudo make install
```



 ## NetCDF-Fortran
``` bash
cd netcdf-fortran-4.5.2/
CC=/usr/rh/devtoolset-y/root/usr/bin/cc
FC=/usr/rh/devtoolset-y/root/usr/bin/cc/gfortran
NFDIR=/usr/local
CPPFLAGS=-I${NCDIR}/include LDFLAGS=-L${NCDIR}/lib ./configure --prefix=${NFDIR}
export LD_LIBRARY_PATH=${NCDIR}/lib:${LD_LIBRARY_PATH}
make check
sudo make install
```

 ## PRMS6
``` bash
git clone https://github.com/rmcd-mscb/prms.git
cd prms
git checkout 6.0.0_dev_bmi
mkdir build
cd build
cmake -DCMAKE_FORTRAN_COMPILER=/opt/rh/devtoolset-7/root/usr/bin/gfortran -CDMAKE_BUILD_TYPE=DEBUG -DCMAKE_INTSALL_PREFIX=/usr/local -DBUILD_SHARED_LIBS=ON -DCMAKE_PREFIX_PATH=/usr/local/lib/cmake ../src
make
sudo make install
```

 ## Test PRMS6
```bash
PRMSLIBDIR=/usr/local
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PRMSLIBDIR}/lib
cd /prms/pipestem
/usr/local/bin/prms -C control.simple1
```

# Windows OS recipe for building libs required by prms6
## Build External Libs
### Build Cortran

#### Create a root folder location
* Create a folder for prms6 and lib source code in a path with no spaces in my case I use __B:\BuildNetcdf__
#### Create a location to install libs to
* In my root source directory as above I added a lib called __install__  This will be the install directory for all the builds below 

#### Configure build with cmake
* Open "__Intel Compiler 18.0 Update 5 Intel(R) 64 Visual Studio 2017__" from your Visual Studio 2017 installation.  Easily found by typing  __intel__... into the search on the taskbar

* cd to your repos folder and clone [GitHub - leonfoks\coretran: An easy to follow library to make Fortran easier in general with wrapped interfaces, sorting routines, kD-Trees, and other algorithms to handle scientific data and concepts. The library contains core fortran routines and object-oriented classes.](https:\\github.com\leonfoks\coretran)
```bash
 git clone https:\\github.com\leonfoks\coretran.git
 cd coretran
 mkdir _build
 cd _build
cmake -DBUILD_SHARED_LIBS=ON^
-DCMAKE_INSTALL_PREFIX=B:\BuildNetcdf\install^
-DCMAKE_BUILD_TYPE=DEBUG^
-G"Visual Studio 15 2017 Win64" ../src 
```  

#### Build with Visual Studio
1. Open solution coretran.sln in VS, select the Debug mode and for the projects coretran; cortranScale and coretranTest open the property page by right-clicking on the project in the “Solution Explorer” and selecting Property. In the Fortran|command line property page remove the -g option.

2. Right click on ALL_BUILD and from the drop-down menu select build
3. When build is completed right click on INSTALL and from the drop-down menu select build
4. Select Release mode and repeat steps 1-3 above 


### Build HDF

#### Configure with cmake
* HDF source code downloads are available from here: [HDF5® Source Code - The HDF Group](https://www.hdfgroup.org/downloads/hdf5/source-code/) 
* Download this cmake version:   [ Download the HDF5](https://www.hdfgroup.org/package/source-cmake-windows-4/?wpdmdl=13052&refresh=5be48acc203951541704396) and uncompress into same root folder.

* Again continue using the VS command prompt as before
* cd to CMake-hdf-1.10.5 folder and type the following command and take a break.
    *build-VS2017-64.bat
* In the CMake-hdf-1.10.5 folder the file __HDF5-1.10.5-win64.exe__ is created, Run the .exe and install into the __install__ folder*
    
### Build Netcdf-c
#### Configure with cmake
* HDF source code downloads are available from here: [NetCDF Downloads](https://www.unidata.ucar.edu/downloads/netcdf/index.jsp)
* Download the netcdf-c-4.7.2.zip & netcdf-fortran-4.5.2.zip and uncompress into your root folder


##### BUILD
```bash
cd netcdf-c-4.7.2
mkdir _build
cd _build
cmake .. -G"NMake Makefiles" -DCMAKE_BUILD_TYPE:STRING=RELEASE^
-DBUILD_SHARED_LIBS=FALSE^
-DHDF5_DIR:STRING="B:\BuildNetcdf\install\1.10.5\cmake\hdf5"^
-DZLIB_LIBRARY:STRING="B:\BuildNetcdf\install\1.10.5\lib\libzlib.lib"^
-DSZIP="B:\BuildNetcdf\install\1.10.5\lib\libszip.lib"^
-DZLIB_INCLUDE_DIR="B:\BuildNetcdf\install\1.10.5\include"^
-DNC_ENABLE_HDF_16_API=TRUE^
-DENABLE_DAP=FALSE^
-DENABLE_TESTS=FALSE^
-DBUILD_TESTING=FALSE^
-DNC_FIND_SHARED_LIBS=FALSE^
-DCMAKE_INSTALL_PREFIX="B:\BuildNetcdf\install"
nmake 
nmake install
```

### Build Netcdf-Fortran

* Modify netcdf-fortran-4.5.2\CmakeLists.txt as follows
* Line 693: change OPTION(ENABLE_TESTS "Enable netcdf-fortran tests." ON) to OPTION(ENABLE_TESTS "Enable netcdf-fortran tests." OFF) 
* Line 830 change OPTION(BUILD_EXAMPLES "Enable compilation of examples." ON) to OPTION(BUILD_EXAMPLES "Enable compilation of examples." OFF) *

* Continue using the __Intel Compiler 18.0 Update 5 Intel(R) 64 Visual Studio 2017__

 ```bash
 cd netcdf-fortran-4.5.2
 mkdir _build
 cd _build
 cmake .. -G"NMake Makefiles" -DCMAKE_BUILD_TYPE:STRING=RELEASE^
 -DBUILD_SHARED_LIBS=FALSE^
 -DBUILD_V2=FALSE^
 -DENABLE_TESTS=FALSE^
 -DNETCDF_INCLUDE_DIR:STRING="B:/BuildNetcdf/install/include"^
 -DNETCDF_C_LIBRARY:STRING="B:/BuildNetcdf/install/lib/netcdf.lib"^
 -DENABLE_FORTRAN_TYPE_CHECKS=FALSE^
 -DCMAKE_INSTALL_PREFIX="B:\BuildNetcdf\install"
 nmake
 nmake install
 ```

### Build PRMS6

* cd to your repos folder and clone prms6 ([GitHub - rmcd-mscb/prms: Precipitation Runoff Modeling System](https://github.com/rmcd-mscb/prms)
* Execute the following commands in the __x64 Native Tools Command Prompt for VS 2017__ command prompt

```bash
cd prms
git checkout 6.0.0_dev_bmi
mkdir _build
cd _build
cmake ../src -DCMAKE_INSTALL_PREFIX="B:\BuildNetcdf\install"^
-DBUILD_SHARED_LIBS=FALSE^
-DCMAKE_BUILD_TYPE=DEBUG^
-Dcoretran_DIR="B:/BuildNetcdf/install/lib/cmake"^
-DNETCDF_F90_INCLUDE_DIR="B:/BuildNetcdf/install/include"^
-DNETCDF_F90_LIBRARY="B:/BuildNetcdf/install/lib/netcdff.lib"^
-DNETCDF_INCLUDE_DIR="B:/BuildNetcdf/install/include"^
-DNETCDF_LIBRARY="B:/BuildNetcdf/install/lib/netcdf.lib"
```

##### Build in Visual Studio 2017
* Open the prms_project.sln in your ___build__ directory*
* Make the following changes to the Properties dialog of both the __prms__ and __prmslib__ projects
* In the Fortran | Command Line tab in the Additions Options box remove the -g
* For the PRMS Property In the Linker | Command Line tab in hte Additional Options make sure there is /machine:x64
* For the PRMSLIB Property In the Librarian | Command Line tab in hte Additional Options make sure there is /machine:x64)
* For PRMSLIB Property, add to the Linker | General - Additional Library Directories: add the lib directories as follows "B:\BuildNetcdf\install\lib;B:\BuildNetcdf\install\1.10.5\lib"
* For PRMSLIB Property, add to the Linker | Input - Additional Dependencies: add libhdf5.lib libhdf5_hl.lib libszip.lib libzlib.lib to the end of the exiting entries

