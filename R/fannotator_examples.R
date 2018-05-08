fannotator_simple <- function(expr, aux = list(nm = "?init_nm?", where = ".GlobalEnv")){
 ## Annotate  expression 
 e    <- expression()
 msg1 <- expression(message("Created on", Sys.time()))
 msg2 <- substitute(message("Function/call/expr ", nm, " in ", where ," executed/evaluated."), aux) 
 eanno <- c(e, msg1, msg2, expr)
 return(eanno)
}

fannotator_traceR <- function(expr, aux = list(nm = "?expr?", where = ".GlobalEnv", idx = 0)) {

 # Prepare preamble expression 
   e <- expression()
   msg1 <- substitute(message(
         "- Function <", idx, ":", nm, ">",
         " from [",  where,
         "] annotated using [fannotator_traceR]"), aux)
   tr1  <- expression( .functionLabel <- nm)
   tr2  <- expression (.traceR <- attr(options()$traceR, "fun"))
   tr3  <- expression (.traceR <- if (is.null(.traceR)) function(...) {} else .traceR)
   tx   <- paste(aux$idx, "00", sep = '.')  
   tr4  <- substitute(.traceR(tx , "`{`", first = TRUE, auto = TRUE), list(tx = tx))
   trx  <- c(tr1,tr2, tr3, tr4)                  
   prex <- c(e, msg1, trx) 
   
   # expand expression expr  
   flblx <- paste(aux$idx, aux$nm, sep=":")
   bexpr <- expression()
   for (i in seq_along(expr)){
     bi <- expr[i]
     bic <- as.character(bi)
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = aux$nm, i = i, bic = bic)) 
     ix <- aux$idx + i/100
     ti <- substitute(.traceR(ix, bic, auto =TRUE), list(ix =ix, bic=bic))  
     if (i == length(expr)) ti <- NULL 
     bexpr <- c(bexpr, ei, bi, ti)
 }                       
   eanno <- c(prex, bexpr)  
 return (eanno)                
}
