library(readr)
library(dplyr)
library(hashmap)
all.data = read_csv("data/eeg/generated/eeg-subset.csv")
code.table <- expand.grid(sw=c('js', 'ep'), el=1:128)
code.table$combined.code <- NA
for(i in 1:nrow(code.table)){
  code.table[i, 'combined.code'] <- paste0(code.table[i,'sw'], code.table[i, 'el'])
}
code.table$code <- sample(nrow(code.table))
code.lookup <- hashmap(code.table$combined.code, code.table$code)

coded.data <- all.data %>% rowwise() %>% mutate(code = code.lookup[[paste0(software,electrode)]])

coded.data$software <- NULL
coded.data$electrode <- NULL

write_csv(code.table, "data/eeg/generated/eeg-code-table.csv")
write_csv(coded.data, "data/eeg/generated/eeg-coded.csv")
