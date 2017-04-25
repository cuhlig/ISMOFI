To install the code, do one of the following:

1. Download it then load with `source(addNewData.R)`
2. Load it directly from github

```
library(devtools, quietly=TRUE)
source_gist("https://gist.github.com/dfalster/5589956")
```

To use it, simply type

```
addNewData("dataNew.csv", myData, allowedVars)
```

where ` allowedVars` is vector of permissible variable names for the columns of myData.