bfanno_init <- function(fun){
   if (!isFun(fun)) stop("Arg fun of ineligible class",  class(fun)[1])
  
  # Pre processing (typically no changes needed)
  bfx <- attr(fun, "original_fun")
  b_f <- if (is.null(bfx))  body(fun) else  body(bfx) 
  # if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
  if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
  ebf <- expression()
  ebf <- c(ebf, b_f)           
  finfo <- attr(fun, "finfo")               # Attribute containing finfo list 
  finfo <- pad_finfo (finfo)                # pad finfo
  res <- list(finfo = finfo, ebf = ebf)       
return (res)
}

bfanno_default <- function(fun){
# Pre - processing function: bf_list has two elements: finfo and ebf 
   bfl   <-  bfanno_init(fun)
   finfo <- bfl[["finfo"]]         # Padded finfo
   ebf   <- bfl[["ebf"]]           # Expression representing body (fun)

   # Prepare preamble expression using info stored in finfo
   prex <- expression()
   msg1 <- substitute(message("- Function <", idx, ":", flbl, ">",
                              " from [",  where, "]",
                              " annotated using [", bfanno, "]"), finfo)
   traceR <- NULL                   
   prex <- c(prex, msg1, traceR) 
           
   # expand body expression using info in ebf and finfo
   
   flblx <- paste(finfo$idx, finfo$flbl, sep=":")
   bexpr <- expression()
 
   for (i in seq_along(ebf)){
     bi <- ebf[i]
     bic <- as.character(bi)
     ei <- substitute(message("   -  <", flbl, "> ln.", i, ":", bic), list(flbl = flblx, i = i, bic = bic)) 
     bexpr <- c(bexpr, ei, bi)
 }
        
  bf      <- as.call(c(as.name("{"), prex, bexpr))
  return(bf) 
}




