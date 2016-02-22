library("dplyr")
library("ggplot2")
library("tidyr")

# Select class data set
fn<-file.choose()
f_path <- dirname(fn)
setwd(f_path)

# open data as comma-separated values
data_set <- read.csv(file = fn, header = TRUE, stringsAsFactors = FALSE)
change_detection<-as.tbl(data_set)

# create performance rate for (no)flicker conds 
change_detection_perf <- change_detection %>% 
  group_by(randGator,flick) %>% 
  summarise(proportion=sum(Accuracy)/8)

# factorize flick for ordering No.flicker before Flicker
change_detection_perf$flick_f<-factor(change_detection_perf$flick,ordered=T, levels=c("No flicker","Flicker"))

## plot performance X flick for each subject
vals_perf = c("#00008B","#A52A2A")
ggplot(change_detection_perf, 
       aes(x=flick_f, y=proportion, fill=flick)) + 
  scale_fill_manual(values=vals_perf)+
  geom_bar(stat ="identity") +
  facet_wrap(~randGator)

  
## plot performance X flick for group + sdl bars
perf_plot <- ggplot(change_detection_perf, aes(x=flick_f, y=proportion, fill=flick_f, col=flick_f)) +
  stat_summary(fun.y=mean, geom = "bar")+ 
  stat_summary(fun.data=mean_sdl, geom = "errorbar", width=0.1,fun.args=list(mult = 1))+
  guides(col="none",fill = "none") +
  theme_bw(28)

perf_plot + scale_x_discrete("Condition") + 
  scale_y_continuous("Performance (correct / total)")+ 
  scale_fill_manual(values=vals_perf)+
  scale_color_manual(values=vals_perf)+ 
  guides(col="none",fill = "none")+
  theme_bw(28)



CDG_resp_ct <- change_detection %>% group_by(randGator,flick,change) %>% summarise(n=n(),mean_rt=mean(RT))

# average RT by subj response
#CDG_resp_ct <- summarise(group_by(change_detection,randGator,flick,change,Response),n=n(),mean_rt=mean(RT))

# factorize flick for ordering No.flicker before Flicker
CDG_resp_ct$flick_f<-factor(CDG_resp_ct$flick,ordered=T, levels=c("No flicker","Flicker"))

posn.d <- position_dodge(0.9)

# plots of subj avgRT

subj_rt_plot <- ggplot(CDG_resp_ct,aes(x=flick_f,y=mean_rt,fill=change)) +
  geom_bar(stat="identity",position = posn.d) +
  facet_wrap(~randGator) + 
  theme_bw(12)

subj_rt_plot + scale_fill_brewer(palette = "Dark2")

# plot of group avgRT
vals_rt = c("#E41A1C", "#377EB8")
rt_plot <- ggplot(CDG_resp_ct,aes(x=flick_f,y=mean_rt, fill=flick, col=flick)) +
  stat_summary(fun.y=mean, geom = "bar")+ 
  stat_summary(fun.data=mean_sdl, geom = "errorbar", width=0.1,fun.args=list(mult = 1))
  
rt_plot + scale_x_discrete("Condition") + 
  scale_y_continuous("RT (ms)")+ 
  scale_fill_manual(values=vals_rt)+
  scale_color_manual(values=vals_rt)+ 
  guides(col="none",fill = "none")+
  theme_bw(28)

# spread out flicker column to compute performance (proportion correct)
change_detection_perf_wide <- spread(change_detection_perf, flick,proportion) %>%
  select(randGator, Flicker.perf=Flicker, No.flicker.perf=contains("No"))

CDG_resp_dRT <- change_detection %>% 
  group_by(randGator,flick) %>% 
  summarise(mean_rt=mean(RT)) %>% 
  spread(flick,mean_rt) %>% 
  select(randGator,Flicker.avgRT = Flicker, No.flicker.avgRT = contains("No")) %>%
  mutate(d_meanRT = (No.flicker.avgRT) - (Flicker.avgRT))

## merge RT & Performance for flicker conds
CDG_RT_perf_dif <- merge(change_detection_perf_wide, CDG_resp_dRT, by="randGator")

# could do some correlations here?? 
# between RT & performance?

###

# consider tidy-stack of performance & avgRT
# dRT & dperf can't be stacked: no flick cond

###
