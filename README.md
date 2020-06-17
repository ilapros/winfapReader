`winfapReader` allows a user to obtain data on long-term annual maxima and peaks over threshold for instantaneous river flow measurements in the UK. The user can also access the corollary catchment descriptors files which characterise the catchments upstream the gauging stations. 

Two sets of functions are available: the `read_*` functions and the `get_*` functions. Both these sets of functions interact with annual maxima information (`read_amax` and `get_amax`), peaks over threshold information (`read_pot` and `get_pot`) and information on the catchment descriptors (`read_cd3` and `get_cd`). 

The read_\* functions allow the user to read into their R console the data from the .AMAX, .PT and .CD3 files made available by the NRFA *once these have been downloaded* into the users' computer. At time of writing, Version 8 and previous versions of the winfap files can be downloaded from the [NRFA website](https://nrfa.ceh.ac.uk/peak-flow-dataset). This means that users who have compiled information for sites not included in the NRFA in the form of WINFAP files will be able to load these into R. The get_\* functions allow the user to interact the the NRFA data API ([terms and conditions](http://nrfa.ceh.ac.uk/costs-terms-and-conditions)). 

The functions in the package rework the information of the WINFAP files and the NRFA API to provide user-friendly R data.frames derived from the original information. 



One can install the latest version of `winfapReader` from github using the `remotes` package: 
 
```
remotes::install_github("ilapros/winfapReader")
```


When reading files saved locally, by default the package assume that the winfap files are saved in the working directory, but this can be overwritten as in

```
max <- read_amax(72014, loc_WinFapFiles = "locationWhereWINFAPfilesAre")
```


When accessing the files via the API simply ask for one or more station to obtain the information requested

```
max <- get_amax(c(70005,72014))
```


