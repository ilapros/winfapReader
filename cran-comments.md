### not a CRAN comment - in rhub, use devtools::check_rhub(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")) 
## R CMD check --as-cran winfapReader_0.1-2.tar.gz gave Status: OK 
## I keep getting an error (which is related to linux builds) 
## The problem is caused by rgdal (on which rnrfa depends) 
#> checking GDAL version >= 1.11.4... no
# 2469#> configure: error: upgrade GDAL to 1.11.4 or later
# 2470#> ERROR: configuration failed for package ‘rgdal’
# 2471#> * removing ‘/home/docker/R/rgdal’

## CRAN email on 08/02/2021
*and replies* 

Cran Message (prof Ripley): 

Packages in Suggests should be used conditionally: see 'Writing R Extensions'.
This needs to be corrected even if the missing package becomes available.

*Packages in Suggests are now used conditionally. This was caused by a decision taken after writing the vignette to move some packages from Imports to Suggests. I apologise for not catching this.* 

## CRAN email on 24/01/2021
*and replies* 

Cran Message (prof Ripley): 

It seems we need to remind you of the CRAN policy:

'Packages which use Internet resources should fail gracefully with an informative message
if the resource is not available or has changed (and not give a check warning nor error).'

*The resource on which the package relies had some issues: this highlighted that the package was indeed not complying with the policy. I apologize for not noticing this issue before. The package should now fail gracefully and give informative messages*  


##comments for submission of 0.1-1

This is a resubmission of a new submission 

## CRAN comments on 6/6/2020 
*and replies* 

Found the following (possibly) invalid file URI: URI: nrfa.ceh.ac.uk
From: inst/doc/winfapReader.html
*Fixed, sorry I missed this one*  


## CRAN comments on 5/6/2020 
*and replies* 

Please always explain all acronyms in the description text.
*Done*

Please use only undirected quotation marks in the description text
*Done*

\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace
\dontrun{} with \donttest{}.
*Done, I thought one should avoid running examples requiring an internet connection. Example in get_pot takes longer than 5s so it is wrapped into a \donttest*

Please use fully specified URLs incl the protocol, e.g. starting https://..... 
*Done* 

## Test environments
* local Ubuntu 18.04.4, 3.6.3 

Via Rhub
* Debian Linux, R-devel, GCC
* macOS 10.13.6 High Sierra, R-release, CRAN's setup
* Windows Server 2008 R2 SP1, R-release, 32/64 bit

## R CMD check results
Status: OK
0 errors | 0 warnings | 0 notes
