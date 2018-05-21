

fanno <- function (x, ...) {
   UseMethod("fanno", x)
}


fanno.call <- function(x, fannotator =  character(0), faux = list()){
# annotates object x of class call 
if (!length(fannotator)) fannotator =  options()$fannotator
fannotated <-!is.null(attr(x, "fannotator"))
if (fannotated && fannotator == "fannotator_revert") return(attr(x,"original"))
obf <- if (fannotated) attr(x,"original") else x
obcl <- coerce_bf_to_bcall(obf)
oexprvL <- coerce_bcall_to_exprvList(obcl)
  
##aux0 <- formals(get(fannotator))$aux
aux   <- faux_pad(faux)        # faux padded
faux0 <- formals(get(fannotator))$faux    
aux0  <- faux_pad(faux0)        # faux for fannotator padded
 
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = oexprvL, faux = aux0)
exprx  <- do.call(fannotator, args)

callx   <- coerce_expressionvList_to_bcall(exprx)
attributes(callx) <- attributes(obcl)
callx <- as.call(as.list(exprx))
attributes(callx) <- attributes(x)
attr(callx, "original") <- obf
attr(callx, "fannotator") <- fannotator 
if (fannotator == "fannotator_revert") callx <- obf
return(callx)
}

 #bf <- body(fun)                                # bf
 #bcl <- coerce_bf_to_bcall(bf)                  # Creates (left) bracketed call
 #exprvL <- coerce_bcall_to_exprvList(bcl)       # list with expressions
 #bclx   <- coerce_expressionvList_to_bcall(exprvL) # Reconstructed bracketed call
 #attributes(bclx) <- attributes(bcl)          


fanno.function <- function(x, fannotator =  character(0), faux = list()){

# annotates object x of class function 
if (!length(fannotator)) fannotator =  options()$fannotator
fannotated <-!is.null(attr(x, "fannotator"))
   
# Extract original function
ofun <- if (fannotated) attr(x,"original") else x
   
# Extract vector of one line expressions representing body of ofun
obf <- body(ofun)
obcl <- coerce_bf_to_bcall(obf)
oexprvL <- coerce_bcall_to_exprvList(obcl)

# prepare args for fannotator
aux   <- faux_pad(faux)        # faux padded
faux0 <- formals(get(fannotator))$faux    
aux0  <- faux_pad(faux0)        # faux for fannotator padded
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = oexprvL, faux = aux0)

oexprvL.new  <- do.call(fannotator, args) 
bclx   <- coerce_expressionvList_to_bcall(oexprvL.new)
attributes(bclx) <- attributes(obcl)
funx <- x
if (!is.null(obf)) body(funx) <- bclx
attributes(funx) <- attributes(x)
attr(funx, "original") <- ofun
attr(funx, "fannotator") <- fannotator 
if (fannotator == "fannotator_revert") funx <- ofun
return(funx)
}
