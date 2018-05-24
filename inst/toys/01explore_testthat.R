## Explore contents of testhat library

### testthat namesapce

library(testthat)
ns <- asNamespace("testthat")   # 269 items
env <- as.environment(ns)
nms <- ls(env)
(classx <-sapply(nms, 
                FUN = function(x) class(env[[x]]))) 

class(testthat:::all_passed)         # ::: is used for not exported objects in ns 
class(env[["all_passed"]])           #  all_passed is function


### testthat package

env_pkg <- as.environment("package:testthat")
nms <- ls(env_pkg)                 # 133 items
idx <- 1:length(nms)
classx <- lapply(nms, 
                FUN = function(x) class(env[[x]]))
names(classx) <- nms
env_pkg$is_null             # in package
class(env_pkg$CheckReporter)    # R6ClassGenerator

