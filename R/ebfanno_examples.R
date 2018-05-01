ebfanno_simple <- function(fnm, where = ".GlobalEnv", aux = list()) {
 ebf  <-  funinfoCreate(fnm, where = where)$orig_ebf
 ebfanno <- expression()
 msg1 <- expression(message("Created on", Sys.time()))
 msg2 <- substitute(message("Function ", fnm, " in ", where ," executed."), list(fnm = fnm, where = where)) 
 ebfanno <- c(ebfanno, msg1, msg2, ebf)
 return (ebfanno)
}

ebfanno_traceR <- function(fnm, where = ".GlobalEnv", aux = list(idx = 0)) {
 ebf  <-  funinfoCreate(fnm, where = where)$orig_ebf
 idx  <- aux[["idx"]]
 # Prepare preamble expression 
   e <- expression()
   msg1 <- message("- Function <", idx, ":", fnm, ">",
                    " from [",  where,
                    "] annotated using [ebfanno_traceR]")
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
   ebfanno <- c(prex, bexpr)              
   return(ebfanno)
}
