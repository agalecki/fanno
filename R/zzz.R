.onLoad <- function(libname, pkgname) {
  op <- options() 
  op.fanno <- list(
    fanno.finfo    = list( flbl     = "test_flbl",
                           where    = ".GlobalEnv",
                           idx      = 0,
                           bfanno   = "bfanno_default",
                           preamble =  function(finfo){
                                         expr <- substitute(message("-- Function <", idx, ":", flbl, ">",
                                         " from [",  where, "]",
                                         " annotated using [", bfanno, "]"), finfo)
                           return(as.expression(expr))
                     })
  toset <- !(names(op.fanno) %in% names(op))
  if(any(toset)) options(op.fanno[toset])
  invisible()
}
