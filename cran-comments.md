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
