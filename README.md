REORS
=====

A collection of scripts to work with raster remotely sensed data. Currently rather disorganised & in early stages of development, many of the files are still works in progress ect.

Further notes will be made in the wiki.

##Required libraries

REORS requires the "raster" package for all functions. The "maptools" package is currently suggested for vector files and at a later date may become required.

##Instalation
To install the package, copy the following text into the R console:

    install.packages(c("devtools", "raster"), dependencies = TRUE)
    library("devtools")
    install_github("JosephMcGrath/REORS")
    library("REORS")
The devtools package may suggest that RTools needs to be installed for packages to be built - this does not seem to impede workings here. And for any later sessions use:

    library("REORS")
To load the library (also loads the raster package and all its dependancies).

##General notes
All functions in the R folder should at least run from start to finish, any missing functionality should be clearly marked.

The style code has been written in is fairly comment-heavy, somewhat unwieldy but means each function can be self-contained.

Where possible the functions here work in a "memory-friendly" manner - i.e. they only load in as much of the data as needed at a time.

In general, the aim is for functions to be generalised, though some are for specific sensors - e.g. to process Landsat data.
