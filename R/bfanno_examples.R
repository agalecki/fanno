bfanno_msg1 <- function(fun){
  if (!isFun(fun)) stop("Arg fun of ineligible class",  class(fun)[1])
  bfx <- attr(fun, "original_fun")
  if (is.null(bfx))  b_f <- body(fun) else b_f <- body(bfx) 
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  finfo <- attr(fun, "finfo")           # Attribute containing finfo list 
  finfo <- pad_finfo (finfo) 
  msg1  <- substitute(message("-- Function [", idx, ":", flbl, "]",
                             " from [",  where, "]",
                             " annotated using [", bfanno, "]"
                            ), finfo)
  bf <- as.call(c(as.name("{"), msg1, b_f))
  return(bf) 
}

bfanno_strip <- function(fun){
# returns original function body (Note: This function is not needed)
  if (!isFun(fun)) stop("Arg fun of ineligible class",  class(fun)[1])
  finfo <- attr(fun, "finfo")           # Attribute containing finfo list 
  bfx <- if (is.null(finfo)) body(fun) else attr(fun, "original_fun") 
  return(bfx) 
}
