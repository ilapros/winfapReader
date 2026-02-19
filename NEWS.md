# winfapReader 0.1-6
- many internal changes to adapt to the new format of winfap files after v14  
- one visible change is that water_year now also deals with time (since now amax files contain datetime information) 
- the API calls are now handled with httr2 (since httr is superseeded)


# winfapReader 0.1-6
- changes in get_amax and get_pot (and in the internal read functions): this was needed to adjust to the changes in what the API outputs now
- changes in links in many places to adapt to changes in the NRFA website 

# winfapReader 0.1-5.1
- changes in known_Oct1 to include new events which occurred on Oct 1st which have been identified in version 12 of the winfap files 
- changes in test functions: realized I needed them after the API broke for some days 


# winfapReader 0.1-5
- internal function changes to please R CMD check --as-cran 
- changes in known_Oct1 to include new events which occurred on Oct 1st which have been identified in version 11 of the winfap files 

# winfapReader 0.1-4

## Minor changes:

1. read_amax and get_amax could give trouble when the user R locale was different from English as dates would not be parsed correctly. This is now fixed (relying on lubridate to do the right thing automatically). 
2. changes in known_Oct1 to include new events which occurred on Oct 1st which have been identified in version 10 of the winfap files 

# winfapReader 0.1-3

## Minor changes:

1.  In compliance with CRAN's request: changes made in the vignette to ensure that packages in Suggests should be used conditionally. This was caused by a decision taken after writing the vignette to move some packages from Imports to Suggests.

# winfapReader 0.1-2 

## Major changes:

1.  Updated the ways in which the nrfa API is queried to comply with changes in the way in which the winfap files are returned. The version also can now process the data derived from winfap files v9.0 - in which several new events which happened on 1st October have been added.

2.  In compliance with CRAN's request: ensured that the package fails gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error).
