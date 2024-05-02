library(dplyr)
library(Intro2R)
ddt <- read.csv(file.choose())
#filter

ddt %>% filter(LENGTH > 30 & DDT < 1000)

df1 <- ddt %>% filter(LENGTH > 30 & DDT < 1000)

# slice

ddt %>% slice(1)

ddt %>% slice(which.max(LENGTH))

ddt %>% slice(which.min(WEIGHT))

ddt %>% slice_sample(n = 5)

# arrange

ddt %>% arrange(SPECIES)

ddt %>% arrange( SPECIES ) %>% slice( 101:n() )

# Columns
#select

ddt %>% select(c(SPECIES,RIVER))

ddt %>% select(c(SPECIES,RIVER)) %>% table() %>% addmargins()

# rename

ddt %>% rename(length_fish = LENGTH)

ddt %>% rename(length_fish = LENGTH) %>% slice( 1 )


# mutate

ddt %>% mutate(Lsq = LENGTH^2)

ddt %>% mutate(zL = scale(LENGTH) )


ddt %>% select(LENGTH) %>% mutate(zL = scale(LENGTH))


# relocate

ddt %>% relocate(LENGTH, WEIGHT,MILE,SPECIES,RIVER)


# summarise

ddt %>% summarise(meanL = mean(LENGTH))

ddt %>% group_by(RIVER) %>% summarise( mean = mean(LENGTH), n = n())


