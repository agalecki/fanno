bfanno_msg1 <- function(fun,  flbl = "_flbl_", idx = 0){
# x is a character string containing function name
  if (!is.function(fun)) stop("Arg fun needs to be a function")
  bfx <- attr(fun, "original_fun")
  if (is.null(bfx))  b_f <- body(fun) else b_f <- body(bfx) 
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  msg1 <- substitute(message("-- Function [", idx, ":", flbl, "]"), list(idx = idx, flbl = flbl))
  bf <- as.call(c(as.name("{"), msg1, b_f))
  return(bf) 
}
