funinfoCreate <- function(fnm, where = ".GlobalEnv"){
# Examples: fnm is function name (charcater string)
# funinfoCreate("fx")
# funinfoCreate("word", where = "namespace:stringr")
# funinfoCreate("word", where = "package:stringr")
 getFun <- getAnywhere(fnm)
 wherex    <- getFun[["where"]]
 whr1 <- suppressMessages(stringr:::word(where, 1, sep =":"))
 whr2 <- suppressMessages(stringr:::word(where, 2, sep =":"))
 
 # Where position
 whrnms <- which(getFun[["where"]] == where)
 is.found <-   if (length(whrnms) == 0) FALSE else TRUE
 if (!(where %in% wherex)) is.found <- FALSE
 is.function <- is.found
 
 # Extract object and  set is.function to FALSE (if appropriate) 
 fun <- NULL
 if (is.found) {
  lst1 <- getFun[["objs"]]
  fun <- getFun[["objs"]][[whrnms]]
  if (!is.function(fun)) is.function <- FALSE
  if (!(c("function") %in% class(fun))) is.function <- FALSE
  if (!is.function) fun <- NULL
 } 
 
 # return expression containing body of an _original_ function
  ebf <- NULL
  attrnms <- NULL
   if (is.found && is.function) {
   attrnms <- names(attributes(fun))
   bfx <- attr(fun, "original_fun")
   b_f <- if (is.null(bfx))  body(fun) else  body(bfx) 
   if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
   ebf <- expression()
   ebf <- c(ebf, b_f)    
 }
  funinfo <- list(
     fnm   = fnm,
     where = where,
     is.found = is.found,
     is.function = is.function,
     fun = fun,
     orig_ebf = ebf,
     attrnms = attrnms 
  )
   if (!is.function) funinfo[c("fun","orig_ebf","attrnms")] <- NULL
   class(funinfo) <- "funinfo"
  return(funinfo)
}

fanno <- function(fnm, where = ".GlobalEnv", ebfanno= "ebfanno_simple"){
 # returns annotated function 

 funinfo <-  funinfoCreate(fnm, where = where)
 fun <- funinfo$fun
 ebf <- funinfo$orig_ebf
 origin_fun <- attr(fun, "original_fun")
 ff <- fun
 attributes(fun)  <- NULL
 ofun <- if (is.null(origin_fun)) fun else origin_fun 
 attributes(ofun)  <- NULL
 if (ebfanno == "ebfanno_strip" ) return(ofun)
 
 argsl <- list(fnm = fnm, where= where)
 
 ebff <- do.call(ebfanno, argsl) # !!! Additional args ...
 bff  <- as.call(c(as.name("{"), ebff))
 body(ff) <- bff 
 
 ## attributes
 attr(ff, "fnm") <- fnm
 attr(ff, "original_fun")  <- ofun
 attr(ff, "where1")    <- where
 attr(ff, "ebfanno")     <- ebfanno
 return(ff)
}
