einfo <- function(expr){
# extracts info from R expression
oe <- attr(expr, "orig_expr")
expr_anms <- names(attributes(expr))
einfo <-list(
   expr      = expr,
   orig_expr = oe,
   expr_anms = expr_anms
   )
class(einfo) <- "einfo"
return(einfo)
}

orig_expr_extract <- function(expr){
# Extract original expression from expr  
 if (!(is.expression(expr) )) stop ("Argument is not an expression")
 oexpr <- if (length(attr(expr, "orig_expr"))) attr(expr, "orig_expr") else expr
 #if (is.null(expr)) oexpr <- expression() 
return(oexpr)
}

eanno_simple <- function(expr, aux = list(fnm = "?expr?", where = "?where?")){
oe <- orig_expr_extract(expr)   # Extract original expression

## Annotate oe expression 
 e    <- expression()
 msg1 <- expression(message("Created on", Sys.time()))
 msg2 <- substitute(message("Function ", fnm, " in ", where ," executed."), aux) 
 eanno <- c(e, msg1, msg2, oe)
 attr(eanno, "orig_expr") <- oe
 if (!length(oe)) eanno <- expression()
 return(eanno)
}

eanno_traceR <- function(expr, aux = list(fnm = "?expr?", where = "?where?", idx = 0)) {
 oe <- orig_expr_extract(expr)   # Extract original expression

 # Prepare preamble expression 
   e <- expression()
   msg1 <- substitute(message(
         "- Function <", idx, ":", fnm, ">",
         " from [",  where,
         "] annotated using [eanno_traceR]"), aux)
   tr1  <- expression( .functionLabel <- fnm)
   tr2  <- expression (.traceR <- attr(options()$traceR, "fun"))
   tr3  <- expression (.traceR <- if (is.null(.traceR)) function(...) {} else .traceR)
   tx   <- paste(aux$idx, "00", sep = '.')  
   tr4  <- substitute(.traceR(tx , "`{`", first = TRUE, auto = TRUE), list(tx = tx))
   trx  <- c(tr1,tr2, tr3, tr4)                  
   prex <- c(e, msg1, trx) 
   
   # expand expression using oe 
   flblx <- paste(aux$idx, aux$fnm, sep=":")
   bexpr <- expression()
   for (i in seq_along(oe)){
     bi <- oe[i]
     bic <- as.character(bi)
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = aux$fnm, i = i, bic = bic)) 
     ix <- aux$idx + i/100
     ti <- substitute(.traceR(ix, bic, auto =TRUE), list(ix =ix, bic=bic))  
     if (i == length(oe)) ti <- NULL 
     bexpr <- c(bexpr, ei, bi, ti)
 }                       
   eanno <- c(prex, bexpr)  
   attr(eanno, "orig_expr") <- oe 
   if (!length(oe)) eanno <- expression()
 return (eanno)                
}

