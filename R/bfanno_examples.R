bfanno_msg1 <- function(fun){
  if (!is.function(fun)) stop("Arg fun needs to be a function")
  finfo <- attr(fun, "finfo")           # Attribute containing finfo list 
  finfo  <- update_finfo (finfo, finfo) 
  bfx <- attr(fun, "original_fun")
  if (is.null(bfx))  b_f <- body(fun) else b_f <- body(bfx) 
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  
  msg1 <- substitute(message("-- Function [", idx, ":", flbl, "]",
                             " from [",  wher1, ":", wher2,     "]",
                             " annotated using [", bfanno, "]"
                            ), finfo)
  bf <- as.call(c(as.name("{"), msg1, b_f))
  return(bf) 
}

bfanno_strip <- function(fun){
  if (!is.function(fun)) stop("Arg fun needs to be a function")
  bfx <- attr(fun, "original_fun") 
  return(bfx) 
}
