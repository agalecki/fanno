update_finfo <- function(finfo, update = NULL) {
# finfo is a named list with components: flbl  
if (is.null(update))  update <- list(flbl = "ourfname", idx = 0, wher1 = ".GlobalEnv?", wher2 = "", bfanno = "bfanno_?")
nmsu <- names(update)
finfo[nmsu] <- update
return(finfo)
}

assign_fanno <- function(x, where = ".GlobalEnv", bfanno = "bfanno_msg1"){
 # x is a character string containing function name
   getx   <- NULL
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  stop("Function: ", x, " not found in: ", where)
   fun  <- getx[nx]                      # fun may have some attributes             
   if (!(is.function(fun))) stop("Object: ", x, " in: ", where, " is NOT a function")
   
 # In preparation for bfanno: update finfo
 finfo <- attr(fun, "finfo")
 whr1 <- stringr::word(where, 1, sep = ":")
 whr2 <- stringr::word(where, 2, sep = ":")
 if (is.na(whr2)) whr2 <- ""
 fupdt <- list(flbl = x, wher1 = whr1, wher2 = whr2, bfanno = bfanno)
 finfo <- update_finfo(finfo, fupdt)
 # store original function both in fun and in original_fun attribute
 
   if (is.null(attr(fun, "original_fun"))) {
      attr(fun, "original_fun") <- fun  
   } else {
      fun <- attr(fun, "original_fun") 
   }
  attr(fun, "finfo") <- finfo
  fattr <- attributes(fun)
  ofun <- fun 
  attributes(ofun) <- NULL
  
  bfanno_body <- do.call(bfanno, list(fun = fun)) 
  body(fun)  <- bfanno_body
  attr(fun, "original_fun") <- ofun
  attr(fun, "finfo")  <- finfo
  
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

