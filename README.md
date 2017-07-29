# fanno
For internal use

```
rm(list = ls())
library("devtools")
install_github("agalecki/fanno")
library (fanno)
````

````
 fx <- function(x) x^2
 bfanno_msg1(tt)      # error object not found
 bfanno_msg1(fx)    
 assign_fanno("fx")
 #bfanno_simple('mean',  where = "namespace:base")
 #bfanno_simple('mean')
 #bfanno_simple('round', where = "namespace:base")
````
