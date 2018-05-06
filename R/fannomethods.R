 


fanno.call <- function(x, efanno= "eanno_simple", aux = list()){
# annotates object x of class call 

if (x[[1]] == as.name("{")) {
    x[[1]] <- NULL
    }
expr <- as.expression(as.list(x))
# update aux0
aux0 <- formals(get(efanno))$aux
if (length(names(aux))) aux0[names(aux)] <- aux
args <- list(expr = expr, aux = aux0)
exprx  <- do.call(efanno, args)
return(exprx)
}
fanno.call(substitute({ y <- x^2; return(y) }))
fanno.call(substitute({ y <- x^2; return(y) }), aux = list(where="?where1"))
