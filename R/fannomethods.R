

fanno <- function (x, ...) {
   UseMethod("fanno", x)
}

fanno.expression <- function(x, fannotator = options()$fannotator, aux = list(flbl ="?-fanno.expression-?")){
# annotates object x of class expression
# update aux0
fannotated <-!is.null(attr(x, "fannotator"))
if (fannotated && fannotator == "fanno_revert") return(attr(x,"original"))
oexpr <- if (fannotated) attr(x,"original") else x
aux0 <- formals(get(fannotator))$aux
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = oexpr, aux = aux0)
exprx  <- do.call(fannotator, args)
attr(exprx, "original") <- oexpr
attr(exprx, "fannotator") <- fannotator 
return(exprx)
}

fanno.call <- function(x, fannotator =  options()$fannotator, aux = list(nm = "?-fanno.call-?")){
# annotates object x of class call 
fannotated <-!is.null(attr(x, "fannotator"))
if (fannotated && fannotator == "fannotator_revert") return(attr(x,"original"))
obf <- if (fannotated) attr(x,"original") else x
oexpr_list <- as.list(obf)
oexpr <- as.expression(oexpr_list)
if (x[[1]] == as.name("{")) {
    x[[1]] <- NULL
    }
aux0 <- formals(get(fannotator))$aux
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = oexpr, aux = aux0)
exprx  <- do.call(fannotator, args)
callx <- as.call(as.list(exprx))
attr(callx, "original") <- obf
attr(callx, "fannotator") <- fannotator 
return(callx)
}

fanno.function <- function(x, fannotator =  options()$fannotator, aux = list(flbl="?-fannno.function-?")){
# annotates object x of class call 
fannotated <-!is.null(attr(x, "fannotator"))
## if (fannotated && fannotator == "fannotator_revert") return(attr(x,"original"))
ofun <- if (fannotated) attr(x,"original") else x
obf <- body(ofun)
obfl <- as.list(obf)
if (obfl[[1]] == as.name("{")) obfl[[1]] <- NULL
oexpr <- as.expression(obfl) # original expression
aux0 <- formals(get(fannotator))$aux
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = oexpr, aux = aux0)
exprx  <- do.call(fannotator, args)
funx <- x
if (!is.null(obf)) body(funx) <- as.call(c(as.name("{"), exprx))
if (fannotator != "fannotator_revert") {
attr(funx, "original") <- ofun
attr(funx, "fannotator") <- fannotator 
}
return(funx)
}
