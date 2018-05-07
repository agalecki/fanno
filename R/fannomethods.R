fannotator_simple <- function(expr, aux = list(onm = "?expr?", where = "?where?")){

 ## Annotate  expression 
 e    <- expression()
 msg1 <- expression(message("Created on", Sys.time()))
 msg2 <- substitute(message("Function ", onm, " in ", where ," executed."), aux) 
 eanno <- c(e, msg1, msg2, expr)
 return(eanno)
}

#~~~~~~ 
fanno <- function (x, ...) {
   UseMethod("fanno", x)
}

fanno.default <- function(x) x

fanno.expression <- function(x, fannotator = "fannotator_simple", aux = list()){
# annotates object x of class expression
# update aux0
fannotated <-!is.null(attr(x, "fannotator"))
if (fannotated && fannotator == "fannotator_revert") return(attr(x,"original"))
oexpr <- if (fannotated) attr(x,"original") else x
aux0 <- formals(get(fannotator))$aux
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = oexpr, aux = aux0)
exprx  <- do.call(fannotator, args)
attr(exprx, "original") <- oexpr
attr(exprx, "fannotator") <- fannotator 
return(exprx)
}

fanno.call <- function(x, fannotator = "eanno_simple", aux = list()){
# annotates object x of class call 
fannotated <-!is.null(attr(x, "fannotator"))
if (fannotated && fannotator == "fannotator_revert") return(attr(x,"original"))
obf <- if (fannotated) attr(x,"original") else x
oexpr <- as.expression(as.list(obf))
if (x[[1]] == as.name("{")) {
    x[[1]] <- NULL
    }
aux0 <- formals(get(fannotator))$aux
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = oexpr, aux = aux0)
exprx  <- do.call(fannotator, args)
callx <- as.call(c(as.name("{"), exprx))
attr(exprx, "original") <- obf
attr(exprx, "fannotator") <- fannotator 
return(callx)
}
## fanno.call(substitute({ y <- x^2; return(y) }))
## fanno.call(substitute({ y <- x^2; return(y) }), aux = list(where="?where1"))
