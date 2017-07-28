bfanno_simple <- function(x,  where = ".GlobalEnv", idx = 0){
# x is a character string containing function name
  fun <- fanno::preprocess_FUN(x, where = where)
  b_f <- attr(fun, "original_body")
  if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  msg1 <- substitute(message("-- Function [", idx, ":", x, "] from  [", where, "]"), list(idx = idx, x=x, where = where))
  msg2 <- substitute(message("- bfannotator [bfanno_simple] used"))
  bf <- as.call(c(as.name("{"), msg1, msg2, b_f))
  return(bf) 
}
