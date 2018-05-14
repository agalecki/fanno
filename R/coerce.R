coerce_bf_to_bcall <- function(bf){
## creates bracketed call from body function
if (is.null(bf)) return (NULL)
bcl <- if (is.call(bf) && as.character(bf)[1] == "{") bf else as.call(c(as.name("{"), bf))
return(bcl)
}

coerce_bcall_to_exprList  <- function(bcl){
## bcl is bracketed call

cList <- as.list(bcl)
cList[[1]] <- NULL  # Bracket symbol skipped

eList <- vector(length(cList), mode= "list")
for (i in seq_along(cList)){
  ei <- as.expression(cList[[i]])
  eList[[i]] <- ei
}
return (eList)
}

coerce_bcall_to_exprvList  <- function(bcl){
## bcl is bracketed call

cList <- as.list(bcl)
cList[[1]] <- NULL  # Bracket symbol skipped

evList <- expression()
for (i in seq_along(cList)){
  ei <- as.expression(cList[[i]])
  evList <- c(evList, ei)
}
return (evList)
}

coerce_expressionList_to_bcall <- function(eList){
e <- expression()
for (i in seq_along(eList)){
  ei <- eList[[i]]
  e <- c(e, ei)
}
bcl_out <- as.call(c(as.name("{"), e))
### Remember to assign attributes of bcls used to create bcl_out
return(bcl_out)
}

coerce_expressionvList_to_bcall <- function(evList){
bcl_out <- as.call(c(as.name("{"), evList))
### Remember to assign attributes of bcl used to create bcl_out
return(bcl_out)
}

coerse_check1 <- function(fun, nms = c("exprL")){
 # Checks auxiliary coerce functions for coerce_bcall_to_exprList
 bf <- body(fun)                              # bf
 bcl <- coerce_bf_to_bcall(bf)                # Creates (left) bracketed call
 exprL <- coerce_bcall_to_exprList(bcl)       # list with expressions
 bclx <-coerce_expressionList_to_bcall(exprL) # Reconstructed bracketed call
 attributes(bclx) <- attributes(bcl)          
 chk <- identical(bcl, bclx)
 message("chk =", chk, ", exprL_len = ", length(exprL), "\n")
 res_all <- list(chk = chk , bf = bf, exprL = exprL)
 return (res_all[nms])
}

coerse_check2 <- function(fun, nms = c("exprvL")){
 # Checks auxiliary coerce functions for coerce_bcall_to_exprvList(bcl) 
 bf <- body(fun)                              # bf
 bcl <- coerce_bf_to_bcall(bf)                # Creates (left) bracketed call
 exprvL <- coerce_bcall_to_exprvList(bcl)       # list with expressions
 bclx   <- coerce_expressionvList_to_bcall(exprvL) # Reconstructed bracketed call
 attributes(bclx) <- attributes(bcl)          
 chk <- identical(bcl, bclx)
 message("chk =", chk, ", exprvL_len = ", length(exprvL), "\n")
 res_all <- list(chk = chk , bf = bf, exprvL = exprvL)
 return (res_all[nms])
}

