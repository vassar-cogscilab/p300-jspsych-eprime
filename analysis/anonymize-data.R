set.seed(12604)
library(readr)
library(dplyr)
library(hashmap)
library(httr)

code.table <- expand.grid(sw=c('js', 'ep'), el=1:128)
code.table$combined.code <- NA
for(i in 1:nrow(code.table)){
  code.table[i, 'combined.code'] <- paste0(code.table[i,'sw'], code.table[i, 'el'])
}
code.table$code <- sample(nrow(code.table))
code.lookup <- hashmap(code.table$combined.code, code.table$code)

# this grabs the eeg-subset.csv file from the OSF, since it is too big to commit to the GH repository
all.data <- httr::GET("https://osf.io/v4sfk/?action=download", progress()) %>% content(as='parsed')
coded.data <- all.data %>% rowwise() %>% mutate(code = code.lookup[[paste0(software,electrode)]])

coded.data$software <- NULL
coded.data$electrode <- NULL

write_csv(code.table, "data/eeg/generated/eeg-code-table.csv")
write_csv(coded.data, "data/eeg/generated/eeg-coded.csv")


timestamp()
data <- read.csv(url('https://osf.io/v4sfk/?action=download'))
timestamp()