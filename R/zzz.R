.onLoad <- function(libname, pkgname) {
  if (packageVersion("terra") < "1.7.55") {
    stop("The pamfit package requires terra version >= 1.7-55", call. = FALSE)
  }
}
