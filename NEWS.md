
Updated to v0.1-2 and submitted to CRAN (4-Feb-21).
----------------------------------------

Minor changes:

1. In compliance with CRAN's request: changes made in the vignette to ensure that packages in Suggests should be used conditionally. This was caused by a decision taken after writing the vignette to move some pacjages from Imports to Suggests. 



Updated to v0.1-2 and submitted to CRAN (4-Feb-21).
----------------------------------------

Major changes:

1. Updated the ways in which the nrfa API is queried to comply with changes in the way in which the winfap files are returned. The version also can now process the data derived from winfap files v9.0 - in which several new events which happened on 1st October have been added. 

2. In compliance with CRAN's request: ensured that the package fails gracefully with an informative message
if the resource is not available or has changed (and not give a check warning nor error).
