fanno_simple <- function(x,  where =".GlobalEnv"){
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
  msg <- substitute(message("Function := <", x, ">, from := <", where, ">"), list(x=x, where = where))
  bf <- as.call(c(as.name("{"), msg, b_f))
  return(bf) 
}
