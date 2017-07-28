library(ggplot2)
library(dplyr)

all.data <- read.csv('data/eeg/generated/eeg-subset.csv')

# plot grand average by electrode site

plot.data <- all.data %>% group_by(electrode, t, frequency) %>% summarize(v=mean(voltage))
ggplot(plot.data, aes(x=t,y=v, colour=frequency))+geom_line()+
  facet_wrap(~electrode)


# difference wave

difference.data <- all.data %>% group_by(subject, electrode, t, software, modality) %>% mutate(v.diff = voltage - lag(voltage)) %>% filter(!is.na(v.diff))
difference.plot.data <- difference.data %>% filter(electrode == 55) %>% group_by(subject,electrode, software, t) %>% summarize(v = mean(v.diff))
ggplot(difference.plot.data, aes(x=t,y=v, colour=software)) +
  geom_line(size=1.2) +
  facet_wrap(~subject) + 
  theme_bw()
