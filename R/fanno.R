 
assign_fanno <- function(x, where = ".GlobalEnv", idx = 0, bfanno = "bfanno_msg1"){
 # x is a character string containing function name
   getx   <- NULL
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  stop("Function: ", x, " not found in: ", where)
   fun  <- getx[nx]                      # fun may have some attributes             
   if (!(is.function(fun))) stop("Object: ", x, " in: ", where, " is NOT a function")
   
 # In preparation for bfanno
 # store original function both in fun and in original_fun attribute  
   if (is.null(attr(fun, "original_fun"))) {
      attr(fun, "original_fun") <- fun  
   } else {
      fun <- attr(fun, "original_fun") 
   }
  fattr <- attributes(fun)
  ofun <- fun 
  attributes(ofun) <- NULL
  bfanno_body <- do.call(bfanno, list(fun = fun, flbl = x, idx = idx)) 
  body(fun)  <- bfanno_body
  attr(fun, "original_fun") <- ofun
  attr(fun, "bfanno") <- bfanno
  whr1 <- stringr::word(where, 1, sep = ":")
  if (whr1 == "namespace") {
     ns <-  stringr::word(where, 2, sep = ":")
     unlockBinding(x,getNamespace(ns))  
     assign(x, fun, getNamespace(ns))
  }
  
  if (whr1 == "package") {
     unlockBinding(x, as.environment(where))  
     assign(x, fun, as.environment(where))
  }

  if (where == ".GlobalEnv") assign(x, fun, as.environment(where))

  return(fun)
}

