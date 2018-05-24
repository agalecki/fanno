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
