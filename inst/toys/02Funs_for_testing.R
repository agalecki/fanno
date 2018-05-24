## Assign selected objects in global env to test fannotators

### User defined functions

f0 <- function(x){}  # length
f1 <- function() pi  # Mode() == "name" 
f1a <- function(){pi}
f2  <- mean
f3 <- function(x) x*2
f4 <- stringr:::word
f5 <- is.function 


### Functions extracted from a package

assign ("f21", testthat:::all_passed, envir = .GlobalEnv)  # <environment: namespace:testthat>

library(testthat)
env_pkg <- as.environment("package:testthat")
assign ("f22", env_pkg$is_null, envir = .GlobalEnv)
assign ("f99", env_pkg$CheckReporter, envir = .GlobalEnv)   # R6ClassGenerator

### Select a function and test it

f <- f21    # <- Select function
bf <- body(f)                                # bf
bcl <- coerce_bf_to_bcall(bf)                # Creates (left) bracketed call
exprvL <- coerce_bcall_to_exprvList(bcl)     # vlist with expressions

ex1 <- fannotator_simple(exprvL, faux = list(fnm = "Fannotated by fannotator_simple", whr ="???", idx = 98))
ex2 <- fannotator_simple2(exprvL, faux = list(fnm = "Fannotated by fannotator_simple2", whr ="???", idx = 99))
ex3 <- fannotator_traceR(exprvL, faux = list(fnm = "Fannotated by fannotator_traceR", whr ="???", idx = 97))

ex <- ex3             ## select
as.call(c(as.name("{"), ex))

res1 <- coerse_check1(f)
exList <- res1[["exprL"]]
res2 <- coerse_check2(f)
ex  <- res2[["exprvL"]]
ex.simple <- fannotator_simple(ex)
as.call(c(as.name("{"), ex.simple))

ex.traceR <- fannotator_traceR(ex)
as.call(c(as.name("{"), ex.traceR))

### Annotate expression/body/function


options(fannotator ="fannotator_simple")
o <- f1                   # --- select e0, e1, e2, bf1, bf2, bf3, f0,f1,f2
oa1 <- fanno(o)                     
oa2  <- fanno(oa1)
identical(oa1, oa2)      # TRUE
orv <- fanno(oa1, fannotator ="fannotator_revert")
identical(orv, o)        # TRUE

options(fannotator ="fannotator_traceR")
o3  <- fanno(oa1)
o4 <- fanno(o3)
identical(o3, o4)      # TRUE




