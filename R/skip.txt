bfanno_msg1 <- function(fun){
  if (!isFun(fun)) stop("Arg fun of ineligible class",  class(fun)[1])
  bfx <- attr(fun, "original_fun")
  if (is.null(bfx))  b_f <- body(fun) else b_f <- body(bfx) 
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  finfo <- attr(fun, "finfo")           # Attribute containing finfo list 
  finfo <- pad_finfo (finfo) 
  msg1  <- substitute(message("-- Function [", idx, ":", flbl, "]",
                             " from [",  where, "]",
                             " annotated using [", bfanno, "]"
                            ), finfo)
  bf <- as.call(c(as.name("{"), msg1, b_f))
  return(bf) 
}

bfanno_strip <- function(fun){
# returns original function body (Note: This function is not needed)
  if (!isFun(fun)) stop("Arg fun of ineligible class",  class(fun)[1])
  finfo <- attr(fun, "finfo")           # Attribute containing finfo list 
  bfx <- if (is.null(finfo)) body(fun) else attr(fun, "original_fun") 
  return(bfx) 
}


assign_1fanno_skip <- function(x, idx = 0, where = ".GlobalEnv", bfanno = "bfanno_msg1", verbose = TRUE){
 # x is a character string containing function name
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  return(message("Object <", idx, ":", x, "> not found in  <", where , "> ... skipped"))
   fun  <- getx[nx]                      # fun may have some attributes
   if (verbose)  message ("assign_1fanno: nx = ", nx) 
   if (!is.function(fun)) return(message("Object <", idx, ":", x, "> in <", where, "> is not a function ... skipped"))
   if (!isFun(fun)) return(message("Function <", idx, ":", x, "> in <", where, "> is not of eligible class <", class(fun)[1], "> ... skipped"))
 
 # In preparation for bfanno: pad finfo
  
 finfo <- attr(fun, "finfo")
   
 fbf <- finfo$bfanno
 if (verbose) message ("fbf = ", fbf, ":", bfanno)
 if (!is.null(fbf) && fbf == bfanno) 
      return(message("Object <", idx, ":", x, "> in <", where, "> already annotated with <", bfanno, "> ... skipped"))
 
 whr1 <- suppressMessages(stringr::word(where, 1, sep = ":"))
 whr2 <- suppressMessages(stringr::word(where, 2, sep = ":"))
 if (is.na(whr2)) whr2 <- ""
 fupdt <- list(flbl = x, idx = idx, where = where, bfanno = bfanno)
 finfo <- pad_finfo(finfo)
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
     ns <-  suppressMessages(stringr::word(where, 2, sep = ":"))
     if (verbose) message("ns =" , ns) 
     unlockBinding(x, getNamespace(ns))  
     if (verbose) print(fun)
     assign(x, fun, getNamespace(ns))
     return(message("Function <", idx, ":", x, "> annotated with <", bfanno, ">  assigned in namespace <", ns, "> ..."))
  }
  
  if (whr1 == "package") {
     if (verbose) message ("package =", where)
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

assign_fanno_skip <- function (fnms = NULL, where = ".GlobalEnv", bfanno = "bfanno_msg1"){
  if (is.null(where))   stop ("<where> arguent  needs to be specified.")
  whr1 <- suppressMessages(stringr::word(where,1, sep =":"))
  whr2 <- suppressMessages(stringr::word(where,2, sep = ":"))
   if (is.null(fnms)){ 
        fnms <- if (whr1 == "namespace")  ls(asNamespace(whr2), all.names = TRUE) else ls(as.environment(where), all.names = TRUE)
   }  
   len <- length(fnms)
   if (len == 0) stop ("select at least one object")  
   for (i in seq_along(fnms)) {
     fnm <- fnms[i]
     assign_1fanno(fnm, where = where, idx = i, bfanno = bfanno)
   }
   return(message("--- ", len, " object(s) in <", where, "> processed."))
}

