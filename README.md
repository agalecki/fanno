# annotator
For internal use

```
rm(list = ls())
library("devtools")
install_github("agalecki/annotator")
library (annotator)
````

````
 fx <- function(x) x^2
 fanno_simple('tt')
 fanno_simple('fx')  
 fanno_simple('mean',  where = "namespace:base")
 fanno_simple('mean')
 fanno_simple('round', where = "namespace:base")
````
