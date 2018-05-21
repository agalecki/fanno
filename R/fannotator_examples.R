faux_pad <- function(faux){
 # pads faux list with ...
 faux_pad <- list(fnm = character(0), whr = character(0), idx = 0)  # ... remaining default components:
 if (is.list(options()$faux_pad)) faux_pad <- options()$faux_pad    # Extract from options()
 nms_pad <- names(faux_pad)
 nms <- names(faux)
 nmsi <- intersect(nms, nms_pad)
 if (length(nmsi)) faux_pad[nmsi] <- faux[nmsi]
return(faux_pad)
}


expr_transform <- function(expr, aux = list(), verbose =0){ 
   # expr is  vector of _one_ line expressions 
   # Creates a list with different  vectors of expressions
   fnm   <- aux$fnm 
   whr   <- aux$whr
   idx   <- aux$idx 
   
   msg1 <- trcR1 <- expression()  
   ec <- as.character(expr)  # Character containing expression
   
   for (i in seq_along(expr)){
     bi <- expr[i]
     bic <- as.character(bi)
     
     ### msg1
     ei <- substitute(message("   -  <", fnm, "> ln.", i, ":", bic), list(fnm = aux$fnm, whr = aux$whr, i = i, bic = bic)) 
     msg1 <- c(msg1, ei)
     if (verbose > 1) message("msg1_i= ", i,  ":", as.character(ei))

     ### trcR1
     ix <- aux$idx + i/100
     ti <- substitute(.traceR(ix, bic, auto =TRUE), list(ix =ix, bic=bic))  
     trcR1 <- c(trcR1, ti) 
     if (verbose > 1) message("trcR1_i= ", i, ":", as.character(ti))
     
  }
    return (list(msg1 = msg1, trcR1 =trcR1)) 
}


fannotator_simple2 <- function(expr, faux = list()){
   aux <- faux_pad(faux)    # mandatory
   
   ## Annotate  expression
   e <- expression()
   msg <- substitute(message("## ", idx, ":", fnm, " in [", whr, "] \n"), aux) 
   ex  <- expression()
   
   # Going through expressions one by one
   for (i in seq_along(expr)){
    ei  <- expr[i]
    eic <- as.character(ei)
    msgi1 <- substitute(message("* ln:", i, ".", idx, ":", fnm, "in [", whr, "]\n"), 
                        list (i=i, idx = aux$idx, fnm= aux$fnm, whr = aux$whr))
    msgi2 <- substitute(message(" ``` \n", eic, "\n ```"), list(eic = eic))
    ex <-c(ex, msgi1, msgi2,   ei) 
  }
 return(c(e, msg, ex))
}


fannotator_simple <- function(expr, faux = list()){
 aux <- faux_pad(faux)    # mandatory
 ## Annotate  expression 
 e    <- expression()
 msg1 <- substitute(message("## ", idx, ":", fnm, " in [", whr, "] \n"), aux) 
 msg2 <- expression(message("--> Executed on:", Sys.time()))
 
 ex <- c(e, msg1, msg2, expr)
return(ex)
}
                    
fannotator_traceR <- function(expr, faux = list()) {
   aux <- faux_pad(faux)    # mandatory
  ## Prepare preamble expression 
   e    <- expression()
   msg1 <- substitute(message("## ", idx , ":", fnm, " in [", whr, "] \n"), aux) 
   tr1  <- expression( .functionLabel <- flbl)
   tr2  <- expression (.traceR <- attr(options()$traceR, "fun"))
   tr3  <- expression (.traceR <- if (is.null(.traceR)) function(...) {} else .traceR)
   tx   <- paste(aux$idx, "00", sep = '.')       # auxiliary 
   tr4  <- substitute(.traceR(tx , "`{`", first = TRUE, auto = TRUE), list(tx = tx))
   trx  <- c(tr1,tr2, tr3, tr4)                  
   epre <- c(e, msg1, trx)

   #--- Extract vectors of expressions 
   ex <- expr_transform(expr = expr, aux = aux)   # list 
   msg1 <- ex$msg1                     # vector of expressions
   trcR1 <- ex$trcR1
   
   #--- Body                    
   e <- expression()
   for (i in seq_along(expr)){
      e <- c(e, msg1[i])  # msg with expression and line number 
      e <- c(e, expr[i])  # Original
      trcR1x <- if (i == length(expr)) expression() else trcR1[i]
      e <- c(e, trcR1x) 
   }
   ex <- c(epre, e)                     
 return (ex)                
}

fannotator_revert <- function(expr, aux = list()){
  # reverts to original object 
  fannotated <-!is.null(attr(expr, "fannotator"))
  exprx <- if (fannotated) attr(expr,"original") else expr
  attr(exprx, "fannotator") <- NULL
return(exprx)
}
 
 
