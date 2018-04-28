.onLoad <- function(libname, pkgname) {
  op <- options() 
  op.fanno <- list( fanno.bfanno  = "bfanno_default")
  toset <- !(names(op.fanno) %in% names(op))
  if(any(toset)) options(op.fanno[toset])
  invisible()
}
