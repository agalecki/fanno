fanno_try2 <- function(fnm, where = ".GlobalEnv", efanno= "efanno_simple", aux = list()){
if (!is.character(fnm)) stop("Argument fnm in fanno() needs to be a character")
 if (!is.character(where) || !length(where) == 1 ) stop("invalid where argument in fanno()")
 getFun <- getAnywhere(fnm)
 wherex    <- getFun[["where"]]  # vector with one or more elements
 whr1 <- suppressMessages(stringr:::word(where, 1, sep =":"))
 whr2 <- suppressMessages(stringr:::word(where, 2, sep =":"))
 
 # Where position
 whrnms <- which(getFun[["where"]] == where)
 is.found <-   if (length(whrnms) == 0) FALSE else TRUE
 if (!(where %in% wherex)) is.found <- FALSE
 
 # Extract object and creates is.function 
 is.function <- is.found
 fun <- NULL
 if (is.found) {
  lst1 <- getFun[["objs"]]
  fun <- getFun[["objs"]][[whrnms]]
  if (!is.function(fun)) is.function <- FALSE
  if (!(c("function") %in% class(fun))) is.function <- FALSE
  if (!is.function) fun <- NULL
 } 
 
 b_f <- body(fun)
 ebf  <- if (is.null(b_f)) expression() else as.expression(b_f)
 aux0 <- formals(efanno)   # Default arguments from annotating function
 aux0$expr <- NULL
 print(ebf)
 res <- do.call(efanno, aux0)
 return(res)
 }

#fanno("fx")



fanno_deprecated <- function(fnm, where = ".GlobalEnv", aux = list(ebfanno= "ebfanno_simple", idx = 0)){
 # returns annotated function by invoking ebfanno function
 # Returned function has additional attributes: fnm, where, original_fun, ebfanno
 funinfo <-  funinfoCreate(fnm, where = where)
 fun <- funinfo$fun       
 ebf <- funinfo$orig_ebf
 origin_fun <- attr(fun, "original_fun")
 attrsfun <-attributes(fun)
 attrfnms <- names(attrsfun)
 
 # attrsfun[c("fnm", "where", "ebfanno")] <- NULL
 ff <- fun
 ## attributes(fun)  <- NULL
 ofun <- if (is.null(origin_fun)) fun else origin_fun 
 aofun <- if (is.null(origin_fun)) attributes(fun)  else  attributes(origin_fun)
 attributes(ofun) <- aofun
 if (ebfanno == "ebfanno_revert" ) return(ofun)
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
