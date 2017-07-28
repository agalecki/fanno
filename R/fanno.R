 extract_FUN <- function(x, where = ".GlobalEnv") {
   # x is a character string containing function name
   getx   <- getAnywhere(x)
   whrAny <- getx[["where"]]
   len <- length(nx <- grep(where, whrAny))
   if (len == 0)  stop("Function: ", x, " not found in: ", where)
   fun  <- getx[["objs"]][nx]
   fun  <- fun[[1]]
   if ( !(is.function(fun))) stop("Object: ", x, " in: ", where, " is NOT a function")
   return(fun)
}
 
