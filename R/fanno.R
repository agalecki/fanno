 preprocess_FUN <- function(x, where = ".GlobalEnv") {
   # x is a character string containing function name
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  stop("Function: ", x, " not found in: ", where)
   fun  <- getx[["objs"]][nx]
   fun  <- fun[[1]] 
   if (!(is.function(fun))) stop("Object: ", x, " in: ", where, " is NOT a function")
   if (is.null(attr(fun, "original_body"))) {
      attr(fun, "original_body") <- body(fun)  # save original body 
      } else {
      body(fun)  <- attr(fun, "original_body")
    }
    return(fun)
}
 
assign_fanno <- function(x, where = ".GlobalEnv", idx = 0, bfanno = "bfanno_simple"){
 # x is a character string containing function name

   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  stop("Function: ", x, " not found in: ", where)
   fun  <- getx[["objs"]][nx]
   fun  <- fun[[1]] 
   if (!(is.function(fun))) stop("Object: ", x, " in: ", where, " is NOT a function")
   
   if (is.null(attr(fun, "original_fun"))) {
      attributes(fun) <- NULL
      attr(fun, "original_fun") <- fun  # save original function 
   } else {
      fun <- attr(fun, "original_fun") 
   }
 
  ofun <- fun
  attributes(ofun) <- NULL
  bfanno_body <- bfanno_simple(ofun, flbl = "fx", idx = idx) 
  body(ofun)  <- bfanno_body
  attributes(fun) <- NULL
  attr(ofun, "original_fun") <- fun
  assign(x, ofun, as.environment(where))
   
  return(ofun)
}

