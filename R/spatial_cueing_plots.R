library("dplyr")
library("ggplot2")
library("tidyr")

# Select class data set
fn<-file.choose()
f_path <- dirname(fn)
setwd(f_path)

# open data as comma-separated values
data_set <- read.csv(file = fn, header = TRUE, stringsAsFactors = FALSE)
SpCue_data <- as.tbl(data_set)

SpCue_data$Condition<-factor(SpCue_data$Condition, ordered=T, levels=c("Valid","Neutral","Invalid"))

# re-order Conditions, group, average over trials for each condition & subject
SpCue_avg <- SpCue_data %>% 
  arrange(randGator,Trial,Condition) %>%
  group_by(randGator,Condition) %>%
  summarise(avgRT=mean(RT))


# plot RT across trials for each condition (with regression model)
ggplot(SpCue_data,aes(x=Trial,y=RT,color=Condition,group=Condition))+ 
  geom_point()+ 
  geom_smooth(se = F, method = "lm", na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 500))+ facet_wrap(~randGator)

# plot smoothed data for each subject
ggplot(SpCue_data,aes(x=factor(Condition), y=RT,))+
  #geom_line(alpha=0.4,,aes(group=1))+
  geom_smooth(se = T, method = "loess", na.rm = TRUE,aes(group=1))+
  scale_y_continuous(limits = c(0, 500))+ facet_wrap(~randGator)

# plot RT for each cond with line each subject & group mean
ggplot(SpCue_avg,aes(x=Condition,y=avgRT)) +
  stat_summary(geom="line",size=1.25, fun.y ="mean",color="red",aes(group=1))+
  stat_summary(fun.data ="mean_sdl",color="red",aes(group=1))+
  geom_line(aes(group=randGator),alpha=0.25) +
  ylim(0,500)+theme_bw(28)


