# Run JAGS and regularly save output to files.

This has the following advantages:

* In the event of a power outage or other unforeseen system crash, you still have most of the results so far.

* You can get interim results without stopping the main process: start a new instance of R and read in the files generated so far. Abort the run if already good enough.

* No need to exactly calculate the time needed for a run: make sure you ask for enough iterations, then abort when time's up.

* Reduce memory needed in R (and never again get a "cannot allocate..." error after a long JAGS run): save as many iterations and parameters as you like to disk, then load only a subset of parameters or thin the chains before loading into R.



