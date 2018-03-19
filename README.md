Run JAGS and regularly save output to files.

This has the following advantages:

* In the event of a power outage or other unforeseen system crash, you still have most of the results so far.

* You can get interim results without stopping the main process: start a new instance of R and read in the files generated so far. Abort the run if already good enough.

* No need to exactly calculate the time needed for a run: make sure you ask for enough iterations, then abort when time's up.

* Manage memory by loading only a subset of parameters saved or thinning before loading into R.



