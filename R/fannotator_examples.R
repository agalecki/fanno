

expr_transform <- function(expr, aux = list(flbl = "flbl:expr_transform", idx = 98), verbose =0){
   # function is called by 
   # expr is  vector of _one_ line expressions 
   # Creates a list with different  of expr vector
   idx   <- aux$idx
   flblx <- if (is.null(idx)) NULL else paste(idx, aux$flbl, sep=":")  # Auxiliary
   
   msg1 <- trcR1 <- expression()  
   ec <- as.character(expr)  # Character containing expression
   
  
   for (i in seq_along(expr)){
     bi <- expr[i]
     bic <- as.character(bi)
     
     ### msg1
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = aux$flbl, i = i, bic = bic)) 
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

fannotator_simple2 <- function(expr, aux = list(flbl = "flbl:fannotator_simple2")){
 ## Annotate  expression 
 e <- expression()
 msg1 <- substitute(message("#", flbl,"\n"), aux) 
 
 msg2 <- expression(message("--> Executed on:", Sys.time()))
 
 ex <- c(e, msg1, msg2)
  for (i in seq_along(expr)){
   ei <- expr[i]
   ci <- as.character(ei)
   msgi <- substitute(message("* ", i, ".",  ci), list(aux = aux , i = i, ci =ci))
   ex <-c(ex, msgi,  ei) 
  }
 return(ex)
}


fannotator_simple <- function(expr, aux = list(flbl = "flbl:fannotator_simple")){
 ## Annotate  expression 
 e <- expression()
 msg1 <- substitute(message("#", flbl,"\n"), aux) 
 
 msg2 <- expression(message("--> Executed on:", Sys.time()))
 
 ex <- c(e, msg1, msg2, expr)
return(ex)
}
                    
fannotator_traceR <- function(expr, aux = list(flbl = "flbl:fannotator_traceR", idx = 99)) {
  ## Prepare preamble expression 
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

   #--- Extract vectors of expressions 
   ex <- expr_transform(expr = expr, aux = aux)   # list 
   msg1 <- ex$msg1                     # vector of expressions
   trcR1 <- ex$trcR1
   
   #--- Body                    
   e <- expression()
   for (i in seq_along(expr)){
      e <- c(e, msg1[i])  # msg with expression line number and expression 
      e <- c(e, expr[i])  # Original
      trcR1x <- if (i == length(expr)) expression() else trcR1[i]
      e <- c(e, trcR1x) 
   }
   ex <- c(epre, e)                     
 return (ex)                
}

fannotator_revert <- function(expr, aux = list(flbl = "flbl:fannotator_revert")){
  # reverts to original object 
  fannotated <-!is.null(attr(expr, "fannotator"))
  exprx <- if (fannotated) attr(expr,"original") else expr
  atttr(exprx, "fannotator") <- NULL
return(exprx)
}
 
 
