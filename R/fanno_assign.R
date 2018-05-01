isFun <- function (fun) {
  is.function(fun) && class(fun) %in%  c("function")  ## ??
}

fanno_assign <- function (fnms = NULL, where = ".GlobalEnv", ebfanno = "ebfanno_simple", all.names = FALSE){

# assigns annotated function in namespace:*, package:* specified in where argument ( by default in .GlobalEnv) 
  if (is.null(where))   stop ("<where> argument  needs to be specified.")
  whr1 <- suppressMessages(stringr::word(where,1, sep =":"))
  whr2 <- suppressMessages(stringr::word(where,2, sep = ":"))
  ebfanno_finfo <- funinfoCreate(ebfanno, where = "namespace:fanno")

  #~~~~
  if (is.null(fnms)){ 
        fnms <- if (whr1 == "namespace")  ls(asNamespace(whr2), all.names = all.names) else ls(as.environment(where), all.names = all.names)
   } 
    
   len <- length(fnms)
   if (len == 0) stop ("select at least one object!")  
   
   for (i in seq_along(fnms)) {
     fnm <- fnms[i]
   
   ###  ff <- fannotatex(fnm, where = where, idx = i, bfanno = bfanno) 
     argsl <- list(fnm = fnm, where = where, idx = i, ebfanno= ebfanno)
     ff <- do.call("fanno",argsl)
       
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
     assign(fnm, ff, as.environment(where))
     message("Function <", i, ":", fnm, "> annotated with <", bfanno, ">  assigned in <", where, "> ...")
     } 
   }  # for i
   return(message("--- ", len, " object(s) in <", where, "> processed."))
   
}
