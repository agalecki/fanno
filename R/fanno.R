isFun <- function (fun) {
  is.function(fun) && class(fun) %in%  c("function")
}


pad_finfo <- function(finfo, padfinfo = options()$fanno.finfo) { 
 nms <- names(finfo)
 res <- padfinfo
 res[nms] <- finfo
return(res)
}

# pad_finfo(list())
# pad_finfo(list(flbl ="new_lbl"))

fannotate <- function(fun){
# fun is a function ( possibly with attributes:   )
 origin_fun <- attr(fun, "original_fun")
 finfo      <- attr(fun, "finfo")
 finfo      <- pad_finfo(finfo) 
 bfanno     <- finfo$bfanno

 ff <- fun
 attributes(fun)  <- NULL
 ofun <- if (is.null(origin_fun)) fun else origin_fun 
 attributes(ofun)  <- NULL
 if (bfanno == "bfanno_strip" )  return(ofun)
 bfanno_body  <- do.call(bfanno, list(fun = ff)) 
 attributes(ff) <- NULL
 body(ff) <-  bfanno_body
 attr(ff, "original_fun") <- ofun
 attr(ff, "finfo")   <- finfo 
 return(ff)
}

fannotatex <- function(x, idx = 0, where = ".GlobalEnv", bfanno = "bfanno_default"){
 # x is a character string containing function name
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  return(message("Object <", idx, ":", x, "> not found in  <", where , "> ... skipped"))
   fun  <- getx[nx]                      # fun may have some attributes 
   if (!is.function(fun)) return(message("Object <", idx, ":", x, "> in <", where, "> is not a function ... skipped"))
   if (!isFun(fun)) return(message("Function <", idx, ":", x, "> in <", where, "> is not of eligible class <", class(fun)[1], "> ... skipped"))
   finfo <- list(flbl = x, idx = idx, where = where, bfanno = bfanno) 
   attr(fun, "finfo") <- finfo
   ff <- fannotate(fun)
   return(ff)
 }

 assign_fanno <- function (fnms = NULL, where = ".GlobalEnv", bfanno = "bfanno_default", all.names = FALSE){
  if (is.null(where))   stop ("<where> argument  needs to be specified.")
  whr1 <- suppressMessages(stringr::word(where,1, sep =":"))
  whr2 <- suppressMessages(stringr::word(where,2, sep = ":"))
   if (is.null(fnms)){ 
        fnms <- if (whr1 == "namespace")  ls(asNamespace(whr2), all.names = all.names) else ls(as.environment(where), all.names = all.names)
   }  
   len <- length(fnms)
   if (len == 0) stop ("select at least one object!")  
   for (i in seq_along(fnms)) {
     fnm <- fnms[i]
     ff <- fannotatex(fnm, where = where, idx = i, bfanno = bfanno) 
    
     if (whr1 == "namespace" && isFun(ff)) {
     ns <-  whr2 
     unlockBinding(fnm, getNamespace(ns))  
     assign(fnm, ff, getNamespace(ns))
     message("Function <", i, ":", fnm, "> annotated with <", bfanno, ">  assigned in namespace <", ns, "> ...")
     }
     
  if (whr1 == "package" && isFun(ff)) {
     unlockBinding(fnm, as.environment(where))  
     assign(fnm, ff, as.environment(where))
     message("Function <", i, ":", fnm, "> annotated with <", bfanno, ">  assigned in package <", whr2, "> ...")
     }
     
  if (where %in% (".GlobalEnv") && isFun(ff)) { 
     assign(fnm, fun, as.environment(where))
     message("Function <", i, ":", fnm, "> annotated with <", bfanno, ">  assigned in <", where, "> ...")
     } 
   }  # for i
   return(message("--- ", len, " object(s) in <", where, "> processed."))
}






