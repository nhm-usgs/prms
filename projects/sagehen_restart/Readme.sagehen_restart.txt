
                  Sagehen Restart Sample Problem for GSFLOW
                                 October 2016
 
This sample is for the Sagehen Creek Watershed and demonstrates use of the 
PRMS restart option. The problem is described in Regan and others (2015, 
USGS TM 6-D3).

Control and batch files are provided in the 'windows' subdirectory to run the 
problem in three model modes. 

The prms.bat batch file for this series of restart simulations 
runs PRMS a total of 16 times. The first simulation runs the entire simulation
time period from October 1, 1980 through January 30, 1984. PRMS is then run 15
additional times to reproduce the restart simulations described in the report. 
The first of these simulations is the 'hindcast' simulation, which runs from 
October 1, 1980 through September 1, 1983. Note that for this run, the 
simulation 'end_time' has been reset to '1983,9,1,0,0,0' in the batch file; 
several other input-control parameters also are set in the batch file for this
and each of the subsequent runs.

Output for the series of simulations is provided in the 'output-test' 
subdirectory. The output includes the unformatted binary restart files with
names 'prms_ic_#,' in which # refers to the restart simulation number.



Reference:

Regan, R.S., Niswonger, R.G., Markstrom, S.L., and Barlow, P.M., 2015, 
Documentation of a restart option for the U.S. Geological Survey coupled 
groundwater and surface-water flow (GSFLOW) model: U.S. Geological Survey 
Techniques and Methods, book 6, chap. D3, 19 p., 
http://dx.doi.org/10.3133/tm6D3/.

