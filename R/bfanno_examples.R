bfanno_simple <- function(fun,  flbl = "Fun", idx = 0){
# x is a character string containing function name
  if (!is.function(fun)) stop("fun needs to be a function")
  bfx <- attr(fun, "original_body")
  if (is.null(bfx))  b_f <- body(fun) else b_f <- bfx 
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  msg1 <- substitute(message("-- Function [", idx, ":", flbl, "]"), list(idx = idx, flbl = flbl))
  bf <- as.call(c(as.name("{"), msg1, b_f))
  return(bf) 
}
