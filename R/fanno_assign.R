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

fanno_assign <- function (nms = NULL,  where = ".GlobalEnv", fannotator = options()$fannotator, all.names = FALSE, verbose = TRUE,
                          aux = list(flbl = "?fanno_assign?",  idx = 0)){

# assigns annotated function in namespace:*, package:* specified in where argument ( by default in .GlobalEnv) 
 
  if (length(where) != 1)   stop ("<where> argument  needs to be specified.")
  whr1 <- suppressMessages(stringr::word(where,1, sep =":"))
  whr2 <- suppressMessages(stringr::word(where,2, sep = ":"))
  ##?? ebfanno_finfo <- funinfoCreate(ebfanno, where = "namespace:fanno")

  if (is.null(nms)){ 
        nms <- if (whr1 == "namespace")  ls(asNamespace(whr2), all.names = all.names) else ls(as.environment(where), all.names = all.names)
   } 
    
   len <- length(nms)
   if (len == 0) stop ("select at least one object!")  
   
    ###  ff <- fannotatex(fnm, where = where, idx = i, bfanno = bfanno) 
     fannotator_fun <- get(fannotator) 
     frmls_fannotator <- formals(fannotator_fun)
     frmls0 <- frmls_fannotator$aux  
     aux0 <- eval(frmls0)  # default arguments of fannotator function
  
  # over-write aux0 elements with values of aux argument 
     if (length(names(aux))) aux0[names(aux)] <- aux # $idx in aux is optional
     aux0[["where"]] <- where

    
  for (i in seq_along(nms)) {
     fnm <- nms[i] 
     aux0$flbl <- "!!!??"   # !!!?? fnm??
     if ("idx" %in% names(aux0))  aux0[["idx"]] <-i
     ### if (length(names(aux))) aux0[names(aux)] <- aux
        
     ## args <- list(expr = fnm, aux = aux0)
     ### argsl <- list(fnm = fnm, where = where, idx = i, ebfanno= ebfanno)
     fun <- fanno_extractx(fnm, where = where)
     process_fun <- if (inherits(fun, what = c("function", "call"))) TRUE else FALSE 
     ff <- if (process_fun)  do.call(fanno, list(x = fun, aux= aux0)) else  NULL
     if (!process_fun) {
     message ("?<", i, ":", fnm, " in ", where, " of mode ", mode(fun), " skipped!!!")
     } 
       
     if (whr1 == "namespace" && process_fun) {
     ns <-  whr2 
     unlockBinding(fnm, getNamespace(ns))  
     assign(fnm, ff, getNamespace(ns))
     message("<", i, ":", fnm, "> object of mode _",  mode(fun), "_ assigned in namespace <", ns, "> [", fannotator, "]   ...")
     }
     
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
   return(message("--- ", len, " object(s) in <", where, "> processed."))
}
# fanno_assign("fx")
