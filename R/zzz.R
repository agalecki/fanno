.onLoad <- function(libname, pkgname) {
  op <- options() 
  op.fanno <- list(fannotator = "fannotator_simple")
  toset <- !(names(op.fanno) %in% names(op))
  if(any(toset)) options(op.fanno[toset])
  invisible()
}
