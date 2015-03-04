REORS
=====

A collection of scripts to work with raster remotely sensed data. Currently in early stages of development, the scripts in the "master" branch should function, any work in progress is generally isolated in its own branch/fork.

Further notes will be made in the wiki.

##Required libraries

REORS requires the "raster" package for all functions.

##Instalation
To install the package, copy the following text into the R console:

    install.packages(c("devtools", "raster"), dependencies = TRUE)
    library("devtools")
    install_github("JosephMcGrath/REORS")
    library("REORS")
The devtools package may suggest that RTools needs to be installed for packages to be built - this does not seem to impede workings here.  
For any later sessions use:

    library("REORS")
To load the library (also loads the raster package and all its dependencies).  
To update the package:

    library("devtools")
    install_github("JosephMcGrath/REORS")
    library("REORS")

##General notes
All functions in the R folder should at least run from start to finish, any missing functionality should be clearly marked.

The style code has been written in is fairly comment-heavy, somewhat unwieldy but means each function can be self-contained.

Where possible the functions here work in a "memory-friendly" manner - i.e. they only load in as much of the data as needed at a time.

In general, the aim is for functions to be generalised, though some are for specific sensors - e.g. to process Landsat data.
