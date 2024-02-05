library(dplyr)
ddt <- read.csv(file.choose())

# filter!

ddt %>% filter(SPECIES == "CCATFISH" & DDT > 30)

exampleUNO <- ddt %>% filter(SPECIES == "CCATFISH" & DDT > 30)

# slice !

ddt %>% slice(1)

ddt %>% slice(which.max(LENGTH))

ddt %>% slice(which.min(WEIGHT))

ddt %>% slice_sample(n=5)

# arrange !

ddt %>% arrange(SPECIES)

ddt %>% arrange( SPECIES ) %>% slice( 101:n() )

# columns select

ddt %>% select(c(SPECIES, RIVER))

ddt %>% select(c(SPECIES, RIVER)) %>%  table() %>% addmargins()

# rename

ddt %>% rename(length_fish = LENGTH)

ddt %>% rename(length_fish = LENGTH) %>% slice( 1 )

# mutate



# relocate

ddt %>% relocate(LENGTH, WEIGHT, MILE, SPECIES, RIVER)

# summarize

ddt %>% summarise(meanL = mean(LENGTH))

ddt %>% group_by(RIVER) %>% summarise(mean = mean(LENGTH), n = n())
