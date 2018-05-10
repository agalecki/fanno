
ofun_extract <- function(fun){
# Extract original function from fun  
 if (!(is.function(fun) &&  ("function" %in% class(fun)))) stop ("Argument is not a function")
 ofun <- if (is.null(attr(fun, "orig_fun"))) fun else attr(fun, "orig_fun") 
return(ofun)
}

afunCreate <- function(flist){

 # Create anotated function 
 fun <- flist$fun      # Function (possibly annotated)
 ebfun <- flist$ebfun  # expression for annotated body  
 aux  <- flist$aux
 ofun  <- ofun_extract(fun) # Function (not annotated)
 obfun <- body(ofun)   # body of (not annotated) function
 
 # Construct body of annotated function
 abf <- if (is.null(obfun)) NULL else as.call(c(as.name("{"), ebfun))
 
 # Construct annotated function 

afun <- if(is.null(abf)) ofun else fun
if (!is.null(abf)) body(afun) <- abf  
attributes(afun) <- attributes(ofun) 
if(!is.null(abf)) {
  attr(afun, "fnm")   <- aux$fnm
  attr(afun, "where") <- aux$where
  attr(afun, "idx")   <- aux$idx
  attr(afun, "orig_fun") <- ofun

 }
 return(afun)
}

fanno_simple <- function(fun, aux = list(fnm = as.character(substitute(fun)), where = "?")) {
 ofun  <- ofun_extract(fun)
 obfun <- body(ofun)
 oebfun <- as.expression(obfun)
 fnm   <- aux$fnm
 where <- aux$where 
 
 ## Annotate oebfun expression 
 e    <- expression()
 msg1 <- expression(message("Created on", Sys.time()))
 msg2 <- substitute(message("Function ", fnm, " in ", where ," executed."), list(fnm = fnm, where = where)) 
 ebfun <- c(e, msg1, msg2, oebfun)
 
 # Construct annotated function
 afun <- afunCreate (list(fun = fun, ebfun = ebfun, aux = aux))
 return (afun)
}

fanno_traceR <- function(fun, aux = list(fnm = as.character(substitute(fun)), where = "placeholder", idx = 0)) {
 ofun  <- ofun_extract(fun)
 obfun <- body(ofun)
 ebf   <- as.expression(obfun)
 fnm   <- aux$fnm
 where <- aux$where 
 idx   <- aux$idx
 
 # Prepare preamble expression 
   e <- expression()
   msg1 <- substitute(message(
         "- Function <", idx, ":", fnm, ">",
         " from [",  where,
         "] annotated using [ebfanno_traceR]"),
         list(fnm=fnm, where=where, idx = idx))
   tr1  <- expression( .functionLabel <- fnm)
   tr2  <- expression (.traceR <- attr(options()$traceR, "fun"))
   tr3  <- expression (.traceR <- if (is.null(.traceR)) function(...) {} else .traceR)
   tx   <- paste(idx, "00", sep = '.')  
   tr4  <- substitute(.traceR(tx , "`{`", first = TRUE, auto = TRUE), list(tx = tx))
   trx  <- c(tr1,tr2, tr3, tr4)                  
   prex <- c(e, msg1, trx) 
                       
   # expand body expression using ebf 
   flblx <- paste(idx, fnm, sep=":")
   bexpr <- expression()
   for (i in seq_along(ebf)){
     bi <- ebf[i]
     bic <- as.character(bi)
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = fnm, i = i, bic = bic)) 
     ix <- idx + i/100
     ti <- substitute(.traceR(ix, bic, auto =TRUE), list(ix =ix, bic=bic))  
     if (i == length(ebf)) ti <- NULL 
     bexpr <- c(bexpr, ei, bi, ti)
 }                       
   ebfun <- c(prex, bexpr)              
 
 # Construct annotated function
 afun <- afunCreate (list(fun = fun, ebfun = ebfun, aux = aux))
 return (afun)                
}
