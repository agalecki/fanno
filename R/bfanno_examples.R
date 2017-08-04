epreamble_default <-  function(finfo){
           expr <- expression()
           msg1 <- substitute(message("- Function <", idx, ":", flbl, ">",
                              " from [",  where, "]",
                              " annotated using [", bfanno, "]"), finfo)
           traceR <- NULL                   
           expr <- c(expr, msg1, traceR) 
           return(expr)
          }

ebody_default <- function(b_f, finfo){
# Note b_f is not a call
 expr <- expression()
 for (i in seq_along(b_f)){
  bi <- b_f[i]
  bic <- as.character(bi)
  ei <- substitute(message("   - l.", i, ":", bic), list(i = i, bic = bic)) 
  expr <- c(expr, ei, bi)
 }
 return(expr)
}

bfanno_default <- function(fun){
  if (!isFun(fun)) stop("Arg fun of ineligible class",  class(fun)[1])
  bfx <- attr(fun, "original_fun")
  b_f <- if (is.null(bfx))  body(fun) else  body(bfx) 
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  finfo <- attr(fun, "finfo")             # Attribute containing finfo list 
  finfo <- pad_finfo (finfo) 
  epreamble <- epreamble_default(finfo)     #
  eb_f    <- ebody_default(b_f, finfo)         
  bf      <- as.call(c(as.name("{"), epreamble, eb_f))
  return(bf) 
}




