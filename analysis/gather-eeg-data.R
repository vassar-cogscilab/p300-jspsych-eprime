library(tidyr)
library(dplyr)

subjects <- 1:8
conditions <- c('Visual Common', 'Visual Rare', 'Audio Common', 'Audio Rare')
software <- c('ep', 'js')
electrodes.to.include <- c(55,62,72)

extract.data <- function(file, subject, condition, software){
  data <- read.delim(paste0('data/eeg/',file), header=F)
  data$V129 <- NULL
  colnames(data) <- 1:128
  data$subject <- subject
  data$modality <- ifelse(grepl('Visual', condition), 'visual', 'audio')
  data$frequency <- ifelse(grepl('Common', condition), 'common', 'rare')
  data$software <- software
  data$t <- -199:800
  data <- data %>% gather("electrode", "voltage", 1:128) %>% filter(electrode %in% electrodes.to.include)
  return(data)
}
all.data <- NA
for(s in subjects){
  for(condition in conditions){
    for(sw in software){
      rx <- paste0('.*-0*',s,'-.*',condition,'.*',sw,'.*')
      file <- list.files(path='data/eeg', pattern=rx)
      data <- extract.data(file[1], s, condition, sw)
      if(is.na(all.data)){
        all.data <- data
      } else {
        all.data <- rbind(all.data, data)
      }
    }
  }
}

write.csv(all.data, file="data/eeg/generated/eeg-subset.csv")
