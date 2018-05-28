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
options(fannotator = "fannotator_traceR") # _simple, _simple2, fannotator_traceR, fannotator_revert
```

# Examples of fanno package  applications


## Package stringr 

Initiate session

```

library(stringr)
sentences <- c("Jane saw a cat", "Jane sat down")
ls(asNamespace("stringr"))
```

### Light annotation 

Function word in `stringr` package/namespace is annotated only  

```
fanno_assign("word", where = "package:stringr")
stringr:::word
fanno_assign("word", where = "namespace:stringr")

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
