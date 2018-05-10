

epreamble_simple <- function(aux = list(flbl = "flbl from epreamble_simple")){
 ## Creates expression preamble
 e    <- expression()
 msg1 <- expression(message("--> Executed on:", Sys.time()))
 msg2 <- substitute(message(flbl), aux) 
 epre <- c(e, msg1, msg2)
 return(epre)
}

epreamble_traceR <- function(aux = list(flbl = "flbl:epreamble_traceR", idx = 98)){
  e    <- expression()
  msg1 <- substitute(message(
         "--> Function <", idx, ":", flbl, ">"
        ), aux)
   tr1  <- expression( .functionLabel <- flbl)
   tr2  <- expression (.traceR <- attr(options()$traceR, "fun"))
   tr3  <- expression (.traceR <- if (is.null(.traceR)) function(...) {} else .traceR)
   tx   <- paste(aux$idx, "00", sep = '.')       # auxiliary 
   tr4  <- substitute(.traceR(tx , "`{`", first = TRUE, auto = TRUE), list(tx = tx))
   trx  <- c(tr1,tr2, tr3, tr4)                  
   epre <- c(e, msg1, trx)
   return(epre)
}

expr_transform <- function(expr, aux = list(flbl = "flbl:expr_transform", idx = 98)){
   # function is called by 
   # expr is  an expression vector 
   # Creates a list with different 1-1 mappings of expr vector
   idx   <- aux$idx
   flblx <- if (is.null(idx)) NULL else paste(idx, aux$flbl, sep=":")  # Auxiliary
   
   msg1 <- trcR1 <- expression()  
   
   for (i in seq_along(expr)){
     bi <- expr[i]
     bic <- as.character(bi)
     
     ### msg1
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = aux$flbl, i = i, bic = bic)) 
     msg1 <- c(msg1, ei)
     # if (verbose > 1) message("msg1_i= ", i,  ":", as.character(ei))

    ### trcR1
     ix <- aux$idx + i/100
     ti <- substitute(.traceR(ix, bic, auto =TRUE), list(ix =ix, bic=bic))  
     trcR1 <- c(trcR1, ti) 
     # if (verbose > 1) message("trcR1_i= ", i, ":", as.character(ti))
     ### if (i == length(expr)) ti <- NULL  # Last expression not 
    #  bexpr <- c(bexpr, ei, bi, ti)
  }
  
  return (list(msg1 = msg1, trcR1 =trcR1)) 
}


fannotator_simple <- function(expr, aux = list(flbl = "flbl:fannotator_simple")){
 ## Annotate  expression 
 epre <- epreamble_simple(aux = aux)
 eanno <- c(epre, expr)
 return(eanno)
}

                       
fannotator_traceR <- function(expr, aux = list(flbl = "flbl", idx = 99)) {
  ## Prepare preamble expression 
   epre <- epreamble_traceR(aux = aux)
   ex <- expr_transform(expr = expr)   # list 
   msg1 <- ex$msg1
   trcR1 <- ex$trcR1
   if (verbose > 0) message("Names:", names(ex)) 
   e <- expression()
   for (i in seq_along(expr)){
      e <- c(e, msg1[i])  # msg with expression line number and expression 
      e <- c(e, expr[i])  # Original
      trcR1x <- if (i == length(expr)) expression() else trcR1[i]
      e <- c(e, trcR1x) 
   }
   eanno <- c(epre, e)  
 return (eanno)                
}
 
