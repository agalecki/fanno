faux_pad <- function(faux){
 # pads faux list with ...
 faux_pad <- list(fnm = character(0), whr = character(0), idx = 0)  # ... remaining default components.
 if (is.list(options()$faux_pad)) faux_pad <- options()$faux_pad    # Extract from options()
 nms_pad <- names(faux_pad)
 nms <- names(faux)
 nmsi <- intersect(nms, nms_pad)
 if (length(nmsi)) faux_pad[nmsi] <- faux[nmsi]
return(faux_pad)
}

fannotator_simple <- function(expr, faux = list()){
 aux <- faux_pad(faux)    # mandatory. Pads faux with fnm, whr, idx.
 ## Annotate  expression 
 e    <- expression()
 msg1 <- substitute(message("## Function ", idx, ":", fnm, " in [", whr, "] \n"), aux) 
 msg2 <- expression(message(" Executed on:", Sys.time()))
 
 ex <- c(e, msg1, msg2, expr)
return(ex)
}

fannotator_simple2 <- function(expr, faux = list()){
   aux <- faux_pad(faux)    # mandatory. Pads faux with fnm, whr, idx.
   
   ## Annotate  expression
   e <- expression()
   msg <- substitute(message("## Function ", idx, ":", fnm, " in [", whr, "] \n"), aux) 
   ex  <- expression()
   
   # Going through expressions one by one
   for (i in seq_along(expr)){
    ei  <- expr[i]
    eic <- as.character(ei)
    auxi <- c(i=i, eic= eic, aux)
    msgi1 <- substitute(message("* ln:", i, " in ", idx, ":", fnm, " in [", whr, "]\n"), auxi) 
    msgi2 <- substitute(message(" ``` \n", eic, "\n ```"), auxi)
    ex <-c(ex, msgi1, msgi2,   ei) 
  }
 return(c(e, msg, ex))
}
                    
fannotator_traceR <- function(expr, faux = list()) {
   aux <- faux_pad(faux)    # mandatory. Pads faux with fnm, whr, idx.

 ## Preamble expression 
   e    <- expression()
   msg1 <- substitute(message("## Function ", idx , ":", fnm, " in [", whr, "] \n"), aux) 
   tr1  <- substitute(.functionLabel <- fnm, aux)
   tr2  <- expression (.traceR <- attr(options()$traceR, "fun"))
   tr3  <- expression (.traceR <- if (is.null(.traceR)) function(...) {} else .traceR)
   tx   <- paste(aux$idx, "00", sep = '.')       # auxiliary 
   tr4  <- substitute(.traceR(tx , "`{`", first = TRUE, auto = TRUE), list(tx = tx))
   trx  <- c(tr1,tr2, tr3, tr4)                  
   epre <- c(e, msg1, trx)
                       
   #--- Body                    
   e <- expression()
   for (i in seq_along(expr)){
      ei <- expr[i]
      eic <- as.character(ei)
      auxi <-  c(i=i, eic = eic, aux)

      msg1i <-  substitute(message("   -  <", fnm, "> ln.", i, ":", eic), auxi) 
      # e <- c(e, msg1[i])  # msg with expression and line number 
      # e <- c(e, expr[i])  # Original
      trcR_i <- substitute(.traceR(eval(idx + i/100), eic, auto =TRUE), auxi)
      trcR1x <- if (i == length(expr)) expression() else trcR_i
      e <- c(e, msg1i, ei, trcR1x) 
   }
   ex <- c(epre, e)                     
 return (ex)                
}

fannotator_revert <- function(expr, faux = list()){
  # reverts to original object 
  fannotated <-!is.null(attr(expr, "fannotator"))
  exprx <- if (fannotated) attr(expr,"original") else expr
  attr(exprx, "fannotator") <- NULL
return(exprx)
}
 
 
