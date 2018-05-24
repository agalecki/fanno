# fanno
For internal use

## Installation
```
rm(list = ls())
library("devtools")
install_github("agalecki/fanno")
```
## Setup

```
options("fannotator")   # NULL at the beginning of R session
library (fanno)
options()$fannotator    # Check 
options(fannotator = "fannotator_traceR")
```

## Assign selected objects in global env to test fannotators

[See Functions](./inst/toys/02Functions_for testing.R)



### Functions

```
```
```
f <- f21
bf <- body(f)                                # bf
bcl <- coerce_bf_to_bcall(bf)                # Creates (left) bracketed call
exprvL <- coerce_bcall_to_exprvList(bcl)     # vlist with expressions

ex1 <- fannotator_simple(exprvL, faux = list(fnm = "Fannotated by fannotator_simple", whr ="???", idx = 98))
ex2 <- fannotator_simple2(exprvL, faux = list(fnm = "Fannotated by fannotator_simple2", whr ="???", idx = 99))
ex3 <- fannotator_traceR(exprvL, faux = list(fnm = "Fannotated by fannotator_traceR", whr ="???", idx = 97))

ex <- ex3 ## select
as.call(c(as.name("{"), ex))

res1 <- coerse_check1(f)
exList <- res1[["exprL"]]
res2 <- coerse_check2(f)
ex  <- res2[["exprvL"]]
ex.simple <- fannotator_simple(ex)
as.call(c(as.name("{"), ex.simple))

ex.traceR <- fannotator_traceR(ex)
as.call(c(as.name("{"), ex.traceR))


```
### Calls

```
cl <- bf2                  # Select object of mode call
length(cl)
mode(cl)
```
### Expressions


### Annotate expression/body/function

```
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

```


# Examples with package annotations

## Package stringr 

Initiate session

```
library(stringr)
sentences <- c("Jane saw a cat", "Jane sat down")
ls(asNamespace("stringr"))
options(fannotator = "fannotator_simple2")
options()$fannotator
```

### Light annotation 

Function word in `stringr` package/namespace annotated only  

```
fanno_assign("word", where = "namespace:stringr")
stringr:::word
fanno_assign("word", where = "package:stringr")
capture.output (word(sentences, 1), type = "message", file ="tmp1.md")
```


###  Full annotation

```
fanno_assign(where = "namespace:stringr")  # all functions in ns annotated
stringr:::word
fanno_assign(where = "package:stringr")    
capture.output (word(sentences, 1), type = "message", file ="tmp2.md")
```

### Revert annotation (needs work)

```
options(fannotator = "fannotator_revert")
options()$fannotator

fanno_assign( where = "namespace:stringr")
fanno_assign( where = "package:stringr")
stringr:::word
```

## Package `nlme`

Initiate session

```
library(nlme)
options(fannotator = "fannotator_simple2")
options()$fannotator
```
```
fanno_assign(where = "namespace:nlme")  # all functions in ns annotated
fanno_assign(where = "package:nlme")    
capture.output (
     fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3)), 
            type = "message", file ="nlme.md")
# revert back 
fanno_assign(where = "namespace:nlme", fannotator = "fannotator_revert")  # all functions in ns annotated
fanno_assign(where = "package:nlme", fannotator = "fannotator_revert")    


           
```

### Package lme4

```
library(lme4)
fanno_assign(where = "namespace:lme4")  # all functions in ns annotated
fanno_assign(where = "package:lme4")    
capture.output ( fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy),
                 type = "message", file ="lme4.md")

```

## Create function for testing

* Create function fx in .GlobalEnv for testing

```
fx <- function(x) x^2
environment(fx)                
environment(fx) <- .GlobalEnv
str(fx)                               # function(`name`, `args`)
formals(fx)                           # list with formal arguments and values
formalArgs(fx)                        # names of formal arguments
```

 ```
 tt <- fanno_simple(fx)
 fanno_simple(tt)

 fanno_traceR(tt)
```

## Test `funinfoCreate` function

### Create finfo objects

```
library(fanno)
finfo0 <- funinfoCreate(fanno)  # Error: funinfo:fnm argument needs to be a character
finfo1 <- funinfoCreate("fx")   # List  of class: funinfo
finfo2 <- funinfoCreate("ttx")

library(stringr)
finfo3 <- funinfoCreate("word", where = "namespace:stringr")
finfo4 <- funinfoCreate("word", where = "package:stringr")
zzz    <- funinfoCreate("zword", where = "package:stringr")
```
### Check the contents of selected finfo object

```
finfo <- finfo3                 # assign selected finfo object
names(finfo)
finfo[c("fnm","where", "is.found", "is.function","fattrnms")]  
names(attributes(finfo$fun))                 # Note: attribute scrcref preserved in finfo1
```

## Test examples of `ebf_fanno` functions

`ebf_fanno` function takes a function and returns expression representing function body 

```
ebf_simple <- ebfanno_simple("fx")
ebf_traceR <- ebfanno_traceR("fx")
```

```
ebf <- ebf_simple
length(ebf) 
mode(ebf)
```

## Testing fanno function

```
fanno("fx")
fanno("word", where = "namespace:stringr")
fanno("word", where = "namespace:stringr", ebfanno = "ebfanno_traceR")
```



# Older version
```
bfanno_init(fx)         # creates a ist with two elements: options()$fanno and $ebf (expression)  
bfanno_default(fx)                    
fannotate(fx)           # generates annotated function with attributes
fannotatex("fx")
```
```
ls(asNamespace("stringr"))
fanno_bftest(where = "namespace:stringr")

library(stringr)
ls(as.environment("package:stringr"))
fanno(where = "package:stringr")
```


````
 fx <- function(x) x^2
 bfanno_msg1(tt)      # error object not found
 bfanno_msg1(fx)    
 getAnywhere("fx")[["where"]]           # ".GlobalEnv"
 assign_fanno("fx")
 fx
 
 getAnywhere("nlme")[["where"]]         # character(0)
 library(nlme)   
 getAnywhere("nlme")[["where"]]         #"package:nlme"   "namespace:nlme"
 detach(package:nlme)
 getAnywhere("nlme")[["where"]]         # "namespace:nlme"
 
 library(nlme)
 assign_fanno("nlme")                   # Object <0:nlme> not found in  <.GlobalEnv> ... skipped
 assign_fanno_ns("nlme") 
 
 
 assign_fanno("word", where = "namespace:stringr")
 getAnywhere("word")[1]
 stringr::word("A B C", 2)
 
 getAnywhere("print")[["where"]]       # "package:base" "namespace:base"
 assign_fanno("print", where = "package:base")
 getAnywhere("print")[1]
 assign_fanno("print", where = "namespace:base")
 getAnywhere("print")[2]
 print("A B C")

 
````
