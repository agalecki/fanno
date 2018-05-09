expr_transf <- function(expr, aux = list(flbl = "Unnamed fun/call/expr ",  idx = 99)){
   # expr is  an expression vector 
   # Creates a list with different 1-1 mappings of expr vector
   flblx <- paste(aux$idx, aux$flbl, sep=":")  # Auxiliary
   bexpr <- expression()
   for (i in seq_along(expr)){
     bi <- expr[i]
     bic <- as.character(bi)
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = aux$flbl, i = i, bic = bic)) 
     ix <- aux$idx + i/100
     ti <- substitute(.traceR(ix, bic, auto =TRUE), list(ix =ix, bic=bic))  
     if (i == length(expr)) ti <- NULL  # Last expression not 
     bexpr <- c(bexpr, ei, bi, ti)
 }
 
}

fannotator_preamble <- function(expr, aux = list(flbl = "Unnamed fun/call/expr ")){
 ## Insert preamble   expression 
 e    <- expression()
 msg1 <- expression(message("--> Executed on:", Sys.time(), ", annotated using [fannotator_simple]"))
 msg2 <- substitute(message(flbl), aux) 
 eanno <- c(e, msg1, msg2, expr)
 return(eanno)
}

fannotator_simple <- function(expr, aux = list(flbl = "Unnamed fun/call/expr ", idx = 98)){
 ## Annotate  expression 
 epreamble <- fannotator_preamble(expr)
 eanno <- c(epreamble)
}
 
 return(eanno)
}

fannotator_traceR <- function(expr, aux = list(flbl = "Unnamed fun/call/expr", idx = 99)) {
  ## Prepare preamble expression 
   e <- expression()
   msg1 <- substitute(message(
         "--> Function <", idx, ":", flbl, ">",
           " annotated using [fannotator_traceR]"
         ), aux)
   tr1  <- expression( .functionLabel <- flbl)
   tr2  <- expression (.traceR <- attr(options()$traceR, "fun"))
   tr3  <- expression (.traceR <- if (is.null(.traceR)) function(...) {} else .traceR)
   tx   <- paste(aux$idx, "00", sep = '.')       # auxiliary 
   tr4  <- substitute(.traceR(tx , "`{`", first = TRUE, auto = TRUE), list(tx = tx))
   trx  <- c(tr1,tr2, tr3, tr4)                  
   prex <- c(e, msg1, trx) 
   
   # expand expression expr  
   flblx <- paste(aux$idx, aux$flbl, sep=":")  # Auxiliary
   bexpr <- expression()
   for (i in seq_along(expr)){
     bi <- expr[i]
     bic <- as.character(bi)
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = aux$flbl, i = i, bic = bic)) 
     ix <- aux$idx + i/100
     ti <- substitute(.traceR(ix, bic, auto =TRUE), list(ix =ix, bic=bic))  
     if (i == length(expr)) ti <- NULL  # Last expression not 
     bexpr <- c(bexpr, ei, bi, ti)
 }                       
   eanno <- c(prex, bexpr)  
 return (eanno)                
}
