fanno_extractx <- function(x, where = ".GlobalEnv"){
 # x is a character string containing function name
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   nx <-  match(where, whrAny)
   len <- length(nx)
   if (len == 0)  return(message("Object <:", x, "> not found in  <", where , "> ... skipped"))
   fun  <- getx[nx]                    
   return(fun)
 }

fanno_assign <- function(nms = NULL,  where = ".GlobalEnv", fannotator = character(), all.names = FALSE, verbose = FALSE){
 if (length(where) != 1)   stop ("<where> argument  is mandatory.")
 lenw <-  length(where)
 for (i in 1:lenw){
 resi <- mapply(fanno_assign1, nms = nms, where = where[i], fannotator = fannotatotaor, all.names= all.names, verbose = verbose)
 resL[i] <- resi
 }
 return(resL)
}
 
fanno_assign1 <- function (nms = NULL,  where = ".GlobalEnv", fannotator = character(), all.names = FALSE, verbose = FALSE){
 if (!length(fannotator)) fannotator <-  options()$fannotator
# assigns annotated function in namespace:*, package:* specified in where argument ( by default in .GlobalEnv) 
  if (length(where) != 1)   stop ("<where> argument  is mandatory.")
  whr1 <- suppressMessages(stringr::word(where,1, sep =":"))  # Extracts word1: package, namespace, .GlobalEnv
  whr2 <- suppressMessages(stringr::word(where,2, sep = ":"))

  
  if (is.null(nms)){ 
        nms <- if (whr1 == "namespace")  ls(asNamespace(whr2), all.names = all.names) else ls(as.environment(where), all.names = all.names)
  }
        res <- replicate(length(nms), "?")
        names(res) <- nms
   
    
   len <- length(nms)
   if (len == 0) stop ("select at least one object!")  
   
    ###  ff <- fannotatex(fnm, where = where, idx = i, bfanno = bfanno) 
     if (verbose) print("1")
     
   
  if (verbose) print("fanno_assign: 3")    
  for (i in seq_along(nms)) {
     if(verbose) print("fanno_assign: 41")
     fnm <- nms[i] 
     aux0 <- list(fnm = fnm, whr = where, idx = i) 
  
     if (verbose) print("fanno_assign: 51")
     fun <- fanno_extractx(fnm, where = where)
     process_fun <- if (class(fun)[1] %in%  c("function", "call")) TRUE else FALSE 
     if (verbose) print("fanno_assign: 55")
     ff <- if (process_fun)  do.call(fanno, list(x = fun, faux= aux0)) else  NULL
     res[i] <- mode(fun)
     if (!process_fun) {
     message ("?<", i, ":", fnm, " in ", where, " of mode ", mode(fun), " skipped!!!")
     } 
     if (verbose) print("fanno_assign for i: if whr1 == namespace")
      
     if (whr1 == "namespace" && process_fun) {
     ns <-  whr2 
     unlockBinding(fnm, getNamespace(ns))  
     assign(fnm, ff, getNamespace(ns))
     message("<", i, ":", fnm, "> object of mode _",  mode(fun), "_ assigned in namespace <", ns, "> [", fannotator, "]   ...")

     }
     if (verbose) print("fanno_assign: 15")
   if (whr1 == "package" && process_fun) {
     unlockBinding(fnm, as.environment(where))  
     assign(fnm, ff, as.environment(where))
     message("Object <", i, ":", fnm, "> annotated with <", fannotator, ">  assigned in package <", whr2, "> ...")
     }
     
  if (where %in% (".GlobalEnv") && process_fun) { 
     assign(fnm, ff, as.environment(where))
     message("Function <", i, ":", fnm, "> annotated with <", fannotator, ">  assigned in <", where, "> ...")
     } 
   
   }  # for i
   # return(message("--- ", len, " object(s) in <", where, "> processed."))
   attr(res, "where") <- where
   return(res)
}
# fanno_assign("fx")
