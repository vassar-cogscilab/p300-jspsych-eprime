library(ggplot2)
library(dplyr)

all.data <- read.csv('data/eeg/generated/eeg-subset.csv')

baseline.window <- -200:-1
p300.window.visual <- 300:500
p300.window.auditory <- 200:400

# plot grand average by electrode site

plot.data <- all.data %>% group_by(electrode, time, frequency) %>% summarize(amplitude=mean(amplitude))
ggplot(plot.data, aes(x=time,y=amplitude, colour=frequency))+geom_line()+
  facet_wrap(~electrode)+
  theme_bw()

# difference wave by modality, electrode site
difference.data <- all.data %>% 
  group_by(subject, electrode, time, modality) %>% 
  mutate(amp.diff = amplitude - lag(amplitude)) %>% 
  filter(!is.na(amp.diff)) %>%
  group_by(electrode, time, modality) %>%
  summarize(amp.diff = mean(amp.diff))

ggplot(difference.data, aes(x=time,y=amp.diff, color=modality)) +
  scale_color_manual(values = c('red','blue'), name="Modality", labels=c("Audio", "Visual"))+
  geom_rect(aes(xmin=min(p300.window.auditory), xmax=max(p300.window.auditory), ymax=-0.5, ymin=-0.6), color='white', fill='red')+
  geom_rect(aes(xmin=min(p300.window.visual), xmax=max(p300.window.visual), ymax=-0.65, ymin=-0.75), color='white', fill='blue')+
  geom_line() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  facet_wrap(~electrode) +
  labs(x="Time (ms)", y="Difference in voltage (mV; rare - common)")+
  theme_bw()+
  theme(panel.grid = element_blank())

# calculate SNR for windows defined above, ignoring software
snr.function <- function(x){
  snr <- 0
  if(x$modality[1] == 'audio'){
    signal.window <- p300.window.auditory
  } else {
    signal.window <- p300.window.visual
  }
  noise.window <- -200:800
  noise.window <- noise.window[!noise.window %in% signal.window]
  signal <- (x %>% filter(time %in% signal.window) %>% summarize(m = mean(amp.diff^2)))$m[1]
  noise <- (x %>% filter(time %in% noise.window) %>% summarize(m = mean(amp.diff^2)))$m[1]
  return(sqrt(signal/noise))
}
snr.modality <- difference.data %>% group_by(electrode, modality) %>% do(snr = snr.function(.))
snr.modality$snr <- as.numeric(snr.modality$snr)
snr.modality.spread <- snr.modality %>% spread(modality, snr)
ggplot(snr.modality.spread, aes(x=audio, y=visual, label=electrode))+ 
  geom_point(size=2) + 
  geom_text(nudge_y = 0.1)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 3, linetype=2, color='red')+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = 1.75, linetype=2, color='blue')+
  labs(x="Audio SNR", y="Visual SNR")+
  theme_bw()

audio.electrodes <- (snr.modality %>% filter(modality=='audio', snr>=1.75))$electrode
visual.electrodes <- (snr.modality %>% filter(modality=='visual', snr >=3))$electrode

# calculate SNR with software

difference.data.sftw.audio <- all.data %>% 
  filter(modality == 'audio', electrode %in% audio.electrodes) %>% 
  group_by(subject, electrode, time, software) %>% 
  mutate(amp.diff = amplitude - lag(amplitude)) %>% 
  filter(!is.na(amp.diff))
snr.audio.sftw <- difference.data.sftw.audio %>% group_by(subject, electrode, software) %>% do(snr = snr.function(.))
snr.audio.sftw$snr <- as.numeric(snr.audio.sftw$snr)
snr.audio.sftw$electrode <- factor(snr.audio.sftw$electrode)
difference.data.sftw.visual <- all.data %>% 
  filter(modality == 'visual', electrode %in% visual.electrodes) %>% 
  group_by(subject, electrode, time, software) %>% 
  mutate(amp.diff = amplitude - lag(amplitude)) %>% 
  filter(!is.na(amp.diff))
snr.visual.sftw <- difference.data.sftw.visual %>% group_by(subject, electrode, software) %>% do(snr = snr.function(.))
snr.visual.sftw$snr <- as.numeric(snr.visual.sftw$snr)
snr.visual.sftw$electrode <- factor(snr.visual.sftw$electrode)

snr.visual.sftw.diff <- snr.visual.sftw %>% group_by(subject,electrode) %>% mutate(snr.diff = snr - lag(snr)) %>% filter(!is.na(snr.diff))
snr.audio.sftw.diff <- snr.audio.sftw %>% group_by(subject,electrode) %>% mutate(snr.diff = snr - lag(snr)) %>% filter(!is.na(snr.diff))


ggplot(snr.visual.sftw.diff, aes(x=electrode, y=snr.diff))+
  geom_hline(yintercept = 0)+
  geom_boxplot(fill="grey90",outlier.shape=NA)+
  geom_jitter(width=0.1)+
  ylim(-4.5,4.5)+
  labs(x="Electrode", y="Difference in Signal/Noise Ratio\njsPsych - E-Prime\n")+
  theme_bw()

ggplot(snr.audio.sftw.diff, aes(x=electrode, y=snr.diff))+
  geom_hline(yintercept = 0)+
  geom_boxplot(fill="grey90",outlier.shape=NA)+
  geom_jitter(width=0.1)+
  ylim(-4.5,4.5)+
  labs(x="Electrode", y="Difference in Signal/Noise Ratio\njsPsych - E-Prime\n")+
  theme_bw()

# Show P300 waves for different software
difference.data.sftw.audio.agg <- difference.data.sftw.audio %>%
  group_by(electrode, time, software) %>%
  summarize(mean.amp = mean(amp.diff))
ggplot(difference.data.sftw.audio.agg, aes(x=time, y=mean.amp, color=software))+
  annotate("rect", xmin=min(p300.window.auditory), xmax=max(p300.window.auditory), ymax=Inf, ymin=-Inf, alpha=0.1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_line()+
  facet_wrap(~electrode)+
  scale_color_brewer(palette="Set1", labels=c('E-Prime', 'jsPsych'), name="Software")+
  labs(x="Time (ms)", y="Difference in voltage (mV; rare - common)")+
  theme_bw()

difference.data.sftw.visual.agg <- difference.data.sftw.visual %>%
  group_by(electrode, time, software) %>%
  summarize(mean.amp = mean(amp.diff))
ggplot(difference.data.sftw.visual.agg, aes(x=time, y=mean.amp, color=software))+
  annotate("rect", xmin=min(p300.window.visual), xmax=max(p300.window.visual), ymax=Inf, ymin=-Inf, alpha=0.1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_line()+
  facet_wrap(~electrode)+
  scale_color_brewer(palette="Set1", labels=c('E-Prime', 'jsPsych'), name="Software")+
  labs(x="Time (ms)", y="Difference in voltage (mV; rare - common)")+
  theme_bw()


# OLD ####

# difference wave, collapsed

difference.data <- all.data %>% group_by(subject, electrode, time, modality) %>% mutate(amp.diff = amplitude - lag(amplitude)) %>% filter(!is.na(amp.diff))
average.p300.amplitude <- difference.data %>% group_by(electrode, modality) %>% filter(time %in% p300.window) %>% summarize(mean.amp = mean(amp.diff))
average.p300.amplitude.spread <- average.p300.amplitude %>% spread(modality,mean.amp)
ggplot(average.p300.amplitude.spread, aes(x=audio, y=visual, label=electrode))+ 
  geom_point(size=2) + 
  geom_text(nudge_y = 0.008)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = .15, linetype=2, color='red')+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = .15, linetype=2, color='blue')+
  labs(x="Average Audio P300 Amplitude", y="Average Visual P300 Amplitude")+
  theme_bw()

audio.electrodes <- (average.p300.amplitude %>% filter(modality=='audio', mean.amp >=0.15))$electrode
visual.electrodes <- (average.p300.amplitude %>% filter(modality=='visual', mean.amp >=0.15))$electrode

# difference waves, electrode x software
difference.data.audio.sftw <- all.data %>% 
  filter(electrode %in% audio.electrodes, modality=='audio') %>% 
  group_by(subject, electrode, time, modality, software) %>%  
  mutate(amp.diff = amplitude - lag(amplitude)) %>% 
  filter(!is.na(amp.diff)) %>%
  group_by(electrode, time, modality, software) %>%
  summarize(amp.diff = mean(amp.diff))
ggplot(difference.data.audio.sftw, aes(x=time,y=amp.diff, color=software)) +
  geom_line(size=1.2) + 
  facet_wrap(~electrode) +
  theme_bw()

# SNR
snr.baseline <- difference.data %>% filter(t %in% baseline.window) %>% group_by(subject, electrode, software, modality) %>% summarize(base.power = mean(v.diff^2))
snr.p300 <- difference.data %>% filter(t %in% p300.window) %>% group_by(subject, electrode, software, modality) %>% summarize(p300.power = mean(v.diff^2))
snr <- snr.baseline %>% inner_join(snr.p300)
snr <- snr %>% mutate(snr = p300.power / base.power)

ggplot(snr, aes(x=modality, y=snr, colour=software))+
  geom_boxplot()+
  #geom_jitter()+
  facet_wrap(~electrode)

snr.subject <- snr %>% group_by(subject, electrode, modality) %>% mutate(snr.diff = lag(snr) - snr) %>% filter(!is.na(snr.diff))
snr.diff <- snr.subject %>% group_by(electrode, modality) %>% summarize(mean.p300.power = mean(p300.power), mean.snr.diff = mean(snr.diff))
ggplot(snr.diff, aes(x=mean.p300.power, y=mean.snr.diff, color=modality))+geom_point()
