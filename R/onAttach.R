

.onAttach <- function(libname, pkgname) {
  version <- try(utils::packageVersion('saveJAGS'), silent=TRUE)
  if(!inherits(version, "try-error"))
    packageStartupMessage("This is saveJAGS ", version,
      ". For overview type ?saveJAGS.")
}
