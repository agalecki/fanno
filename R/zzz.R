.onLoad <- function(libname, pkgname) {
  op <- options() 
  op.fanno <- list(
    fanno.finfo    = list( flbl     = "test_flbl",
                           where    = ".GlobalEnv",
                           idx      = 0,
                           bfanno   = "bfanno_default")
                 )
  toset <- !(names(op.fanno) %in% names(op))
  if(any(toset)) options(op.fanno[toset])
  invisible()
}
