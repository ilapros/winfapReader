`winfapReader` allows a user to read into their R console the data from the .AMAX, .PT and .CD3 files made available by the NRFA *once these have been downloaded* into the users' computer. At time of writing, Version 7 of the winfap files can be downloaded from the [NRFA website](https://nrfa.ceh.ac.uk/peak-flow-dataset). 

One can install `winfapReader` from github using `devtools`: 
 
```
devtools:: install_github("ilapros/winfapReader")
```


By default the functions assume that the winfap files are saved in the working directory, but this can be overwritten as in


```
read_amax(72014, loc_WinFapFiles = "locationWhereWINFAPfilesAre")
```



