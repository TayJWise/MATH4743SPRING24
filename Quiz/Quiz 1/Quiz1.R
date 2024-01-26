ddt <- read.csv(file.choose())
ddt
ddt[1,2]
# First row, second column^

ddt[which.max(ddt$LENGTH), "DDT"]

ddt[ddt$LENGTH > 49 & ddt$WEIGHT < 1200,]
