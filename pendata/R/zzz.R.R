.onLoad <- function(libname, pkgname) {
  delayedAssign("data", load_frs_data(), assign.env = frs)
}
