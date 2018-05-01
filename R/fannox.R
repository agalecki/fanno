fanno <- function(fnm, where = ".GlobalEnv", aux = list(), ebfanno= "ebfanno_simple"){
 # returns annotated function by invoking ebfanno function
 if (!is.character(fnm)) stop ("fnm needs to be  character")
 funinfo <-  funinfoCreate(fnm, where = where)
 fun <- funinfo$fun 
 ebf <- funinfo$orig_ebf
 origin_fun <- attr(fun, "original_fun")
 attrsfun <-attributes(fun)
 attrfnms <- names(attrsfun)
 
 # attrsfun[c("fnm", "where", "ebfanno")] <- NULL
 ff <- fun
 attributes(fun)  <- NULL
 ofun <- if (is.null(origin_fun)) fun else origin_fun 
 aofun <- if (is.null(origin_fun)) attributes(fun)  else  attributes(origin_fun)
 if (ebfanno == "ebfanno_strip" ) return(ofun)
 argsl <- list(fnm = fnm, where = where, aux = aux)
 ebff <- do.call(ebfanno, argsl)
 bff  <- as.call(c(as.name("{"), ebff))
 body(ff) <- bff 
 
 ## attributes
 attr(ff, "fnm") <- fnm
 attr(ff, "where")    <- where
 attr(ff, "original_fun")  <- ofun
 attr(ff, "ebfanno")     <- ebfanno
 return(ff)
}
