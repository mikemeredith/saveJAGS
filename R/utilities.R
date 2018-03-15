

# loads/unloads more than one JAGS module

# rjags::load.module and unload.module will only load/unload one module!
loadJagsModules <- function(modules)  {
  for(i in seq_along(modules))
    rjags::load.module(modules[i])
}

unloadJagsModules <- function(modules)  {
  for(i in seq_along(modules))
    rjags::unload.module(modules[i])
}
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
