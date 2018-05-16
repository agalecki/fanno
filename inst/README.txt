### Simple expressions (May 2018)

```
e2 <- expression(x+y, a+b)
e <- e2                    # <- select
el <- expr_transform(e)
lapply(as.list(el), function(x) cat(as.character(x), sep ="\n"))
test
