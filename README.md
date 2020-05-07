# Run JAGS and regularly save output to files.

This has the following advantages:

* In the event of a power outage or other unforeseen system crash (eg, Windows 10 update), you still have most of the results so far.

* You can get interim results without stopping the main process: start a new instance of R and read in the files generated so far. Abort the run if already good enough.

* No need to exactly calculate the time needed for a run: make sure you ask for enough iterations, then abort when time's up.

* Reduce memory needed in R (and never again get a "cannot allocate..." error after a long JAGS run): save as many iterations and parameters as you like to disk, then load only a subset of parameters or thin the chains before loading into R.

* If some chains exit with errors (usually due to incompatible starting values), you can still recover the results for those that ran properly.

For more information see [here](https://mmeredith.net/blog/2018/Intro_saveJAGS.htm).

## `mcmcOutput` package

A new - and still being developed - package, `mcmcOutput`, provides a class for storing MCMC output and a range of methods and functions to manipulate, summarise and display output. More information is [here](https://mmeredith.net/blog/2020/storing_MCMC.htm). You need to install `mcmcOutput` from GitHub before installing `saveJAGS`.


