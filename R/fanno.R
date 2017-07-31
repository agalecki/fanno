isEligibleFun <- function (fun) {
  is.function(fun) && class(fun) %in%  c("function")
}

update_finfo <- function(finfo, update = NULL) {
# finfo is a named list with components: flbl  
if (is.null(update))  update <- list(flbl = "ourfname", idx = 0, where = ".GlobalEnv?", bfanno = "bfanno_?")
nmsu <- names(update)
finfo[nmsu] <- update
return(finfo)
}

assign_fanno <- function(x, idx = 0, where = ".GlobalEnv", bfanno = "bfanno_msg1"){
 # x is a character string containing function name
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  return(message("Object <", idx, ":", x, "> not found in  <", where , "> ... skipped"))
   fun  <- getx[nx]                      # fun may have some attributes
   if (!is.function(fun)) return(message("Object <", idx, ":", x, "> in <", where, "> is not a function ... skipped"))
   if (!isEligibleFun(fun)) return(message("Function <", idx, ":", x, "> in <", where, "> is not of eligible class <", class(fun)[1], "> ... skipped"))
 
 # In preparation for bfanno: update finfo

 finfo <- attr(fun, "finfo")
 fbf <- finfo$bfanno
 if (!is.null(fbf) && fbf == bfanno) 
      return(message("Object <", idx, ":", x, "> in <", where, "> already annotated with <", bfanno, "> ... skipped"))
 
 whr1 <- stringr::word(where, 1, sep = ":")
 whr2 <- stringr::word(where, 2, sep = ":")
 if (is.na(whr2)) whr2 <- ""
 fupdt <- list(flbl = x, idx = idx, where = where, bfanno = bfanno)
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
     return(message("Function <", idx, ":", x, "> annotated with <", bfanno, ">  assigned in namespace <", ns, "> ..."))
  }
  
  if (whr1 == "package") {
     unlockBinding(x, as.environment(where))  
     assign(x, fun, as.environment(where))
     return(message("Function <", idx, ":", x, "> annotated with <", bfanno, ">  assigned in package <", whr2, "> ..."))
  }

  if (where == ".GlobalEnv"){ 
   assign(x, fun, as.environment(where))
   return(message("Function <", idx, ":", x, ">  annotated with <", bfanno, ">  assigned in <.GlobalEnv> ..."))
  }
  return(message("???"))
}

assign_fanno_ns <- function (ns, fnms = NULL, bfanno = "bfanno_msg1"){
   if (is.null(ns))   stop ("namespace needs to be specified")
   if (is.null(fnms)) fnms <- ls(asNamespace(ns), all.names = TRUE)
   len <- length(fnms)
   if (!( len > 0)) stop ("select at least one function")  
   whr  <-  paste("namespace", ns, sep = ":")
   for (i in seq_along(fnms)) {
     fnm <- fnms[i]
     assign_fanno(fnm, where = whr, idx = i, bfanno = bfanno)
   }
   return(message("--- ", len, " objects in <", ns, "> processed. Use <", ns, ":::object_name> to retrieve."))
}

assign_fanno_pkg <- function (pkg, fnms = NULL, bfanno = "bfanno_msg1"){
   if (is.null(pkg))  stop ("package name needs to be specified")
   whr  <-  paste("package", pkg, sep = ":")
   if (is.null(fnms)) fnms <- ls(as.environment(whr), all.names = TRUE )
   len <- length(fnms)
   if (len == 0) stop ("select at least one function")  
   
   for (i in seq_along(fnms)) {
     fnm <- fnms[i]
     assign_fanno(fnm, where = whr, idx = i, bfanno = bfanno)
   }
   return(message("--- ", len, " objects in package <", pkg, "> processed."))
}




