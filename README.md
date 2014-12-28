REORS
=====

A collection of scripts to work with raster remotely sensed data. Currently rather disorganised & in early stages of development, many of the files are still works in progress ect.

##Required libraries

REORS requires the "raster" library for all functions, some may include "maptools" later - especially when working with vector files.

##General notes
All functions in the R folder should 
The style code has been written in is fairly comment-heavy, somewhat unwieldy but means each function can be self-contained.
Where possible the functions here work in a "memory-friendly" manner - i.e. they only load in as much of the data as needed at a time.
In general, the aim is for functions to be generalised, though some are for specific sensors - e.g. to process Landsat data.