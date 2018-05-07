isFun <- function (fun) {
  is.function(fun) && class(fun) %in%  c("function")  ## ??
}

fanno_assign <- function (nms = NULL, fannotator = "fannotator_simple", all.names = FALSE,
                          aux = list (nm = "?fanno_assign?", where = ".GlobalEnv", idx =0)){

# assigns annotated function in namespace:*, package:* specified in where argument ( by default in .GlobalEnv) 
  wher <- aux$where
  if (!length(wher))   stop ("<where> argument  needs to be specified.")
  whr1 <- suppressMessages(stringr::word(wher,1, sep =":"))
  whr2 <- suppressMessages(stringr::word(wher,2, sep = ":"))
  ##?? ebfanno_finfo <- funinfoCreate(ebfanno, where = "namespace:fanno")


  if (is.null(nms)){ 
        nms <- if (whr1 == "namespace")  ls(asNamespace(whr2), all.names = all.names) else ls(as.environment(where), all.names = all.names)
   } 
    
   len <- length(nms)
   if (len == 0) stop ("select at least one object!")  
   
   for (i in seq_along(nms)) {
     fnm <- nms[i] 

     ###  ff <- fannotatex(fnm, where = where, idx = i, bfanno = bfanno) 
     aux0 <- formals(get(fannotator))$aux
     
     aux$idx <- i
     if (length(names(aux))) aux0[names(aux)] <- aux
     
     ## args <- list(expr = fnm, aux = aux0)
     ### argsl <- list(fnm = fnm, where = where, idx = i, ebfanno= ebfanno)
     ff <- do.call(fannotator, aux0)
       
     if (whr1 == "namespace" && isFun(ff)) {
     ns <-  whr2 
     unlockBinding(fnm, getNamespace(ns))  
     assign(fnm, ff, getNamespace(ns))
     message("Function <", i, ":", fnm, "> annotated with <", fannotator, ">  assigned in namespace <", ns, "> ...")
     }
     
  if (whr1 == "package" && isFun(ff)) {
     unlockBinding(fnm, as.environment(where))  
     assign(fnm, ff, as.environment(where))
     message("Function <", i, ":", fnm, "> annotated with <", fannotator, ">  assigned in package <", whr2, "> ...")
     }
     
  if (where %in% (".GlobalEnv") && isFun(ff)) { 
     assign(fnm, ff, as.environment(where))
     message("Function <", i, ":", fnm, "> annotated with <", fannotator, ">  assigned in <", where, "> ...")
     } 
   }  # for i
   return(message("--- ", len, " object(s) in <", where, "> processed."))
   
}
# fanno_assign("fx")
