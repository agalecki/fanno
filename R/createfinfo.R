funinfoCreate <- function(fnm, where = ".GlobalEnv"){

# Examples: fnm is function name (charcater string)
# funinfoCreate("fx")
# funinfoCreate("word", where = "namespace:stringr")
# funinfoCreate("word", where = "package:stringr")
 getFun <- getAnywhere(fnm)
 wherex    <- getFun[["where"]]
 if (!(where %in% wherex)) return(message("Object ", fnm, " not found in ", where)) 
 whr1 <- suppressMessages(stringr:::word(where, 1, sep =":"))
 whr2 <- suppressMessages(stringr:::word(where, 2, sep =":"))
 
 # Where position
 whrnms <- which(getFun[["where"]] == where)
 
 is.found <-   if (length(whrnms) == 0) FALSE else TRUE  
 is.function <- TRUE
 # Extract object
 if (is.found) {
  lst1 <- getFun[["objs"]]
  fun <- getFun[["objs"]][[whrnms]]
  if (!is.function(fun)) is.function <- FALSE
  if (!(c("function") %in% class(fun))) is.function <- FALSE
  if (!is.function) fun <- NULL
 } 
 
 # return expression containing body of an original function
   if (is.found && is.function) {
   bfx <- attr(fun, "original_fun")
   b_f <- if (is.null(bfx))  body(fun) else  body(bfx) 
   if (b_f[[1]] == as.name("{") ) b_f[[1]] <- NULL
   ebf <- expression()
   ebf <- c(ebf, b_f)         
 } else ebf <- NULL
 
 funinfo <- list(
     fnm   = fnm,
     fun   = fun,
     where = where,
     ebf   = ebf,
     found = is.found,
     is.function = is.function)
 class(funinfo) <- "funinfo"
 
 return(funinfo)
}
