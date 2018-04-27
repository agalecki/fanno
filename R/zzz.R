.onLoad <- function(libname, pkgname) {
  op <- options() 
  op.fanno <- list(
    fanno.finfo  = c("test_flbl",  ".GlobalEnv",  "bfanno_default"),    # flbl, where, bfanno
    fanno.idx    = 0
  )
  toset <- !(names(op.fanno) %in% names(op))
  if(any(toset)) options(op.fanno[toset])
  invisible()
}
