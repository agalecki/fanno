bfanno_simple <- function(x,  where =".GlobalEnv", idx = 0){
# x is a character string containing function name
  getx   <- getAnywhere(x)
  whrAny <- getx[["where"]]
  len <- length(nx <- grep(where, whrAny))
  if (len == 0)  stop("Function: ", x, " not found in: ", where)
  fun  <- getx[["objs"]][nx]
  fun  <- fun[[1]]
  if ( !(is.function(fun))) stop("Object: ", x, " in: ", where, " is NOT a function")
  b_f <- body(fun)
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  msg1 <- substitute(message("-- Function [", idx, ":", x, "] from  [", where, "]"), list(idx = idx, x=x, where = where))
  msg2 <- substitute(message("-- bannotator [banno_simple] used"))
  bf <- as.call(c(as.name("{"), msg1, msg2, b_f))
  return(bf) 
}
