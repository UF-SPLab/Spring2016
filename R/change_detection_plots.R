library("dplyr")
library("ggplot2")
library("tidyr")

# Select class data set
fn<-file.choose()
f_path <- dirname(fn)
setwd(f_path)

data_set <- read.csv(file = fn, header = TRUE, stringsAsFactors = FALSE)
change_detection<-as.tbl(data_set)

change_detection$flick<-factor(change_detection$flick,ordered=T, levels=c("No flicker","Flicker"))

change_detection_pcorrect <- change_detection %>% group_by(randGator,flick) %>% summarise(proportion=sum(Accuracy)/8)

ggplot(change_detection_pcorrect,aes(x=flick,y=proportion, fill=flick)) + 
  geom_bar(stat ="identity") +
  facet_wrap(~randGator)

ggplot(change_detection_pcorrect,aes(x=flick,y=proportion, fill=flick, col=flick)) +
  stat_summary(fun.y=mean, geom = "bar")+ 
  stat_summary(fun.data=mean_sdl, geom = "errorbar", width=0.1,fun.args=list(mult = 1))+
  theme_bw(28)+guides(col="none",fill = "none")


CD_group <- change_detection %>% arrange(randGator,flick,change) %>% group_by(randGator,flick,change) 
CDG_avg <- summarise(CD_group,avgRT=mean(RT))


CDG_resp_ct <- summarise(group_by(CDG_avg,randGator,flick,change,Response),n=n(),mean_rt=mean(RT))


posn.d <- position_dodge(2)
ggplot(CDG_resp_ct,aes(x=flick,y=mean_rt,fill=change,group=Response))+geom_bar(stat="identity",position = posn.d)+facet_wrap(~randGator)

val = c("#E41A1C", "#377EB8")
rt_plot <- ggplot(CDG_resp_ct,aes(x=flick,y=mean_rt, fill=flick, col=flick)) +
  stat_summary(fun.y=mean, geom = "bar")+ 
  stat_summary(fun.data=mean_sdl, geom = "errorbar", width=0.1,fun.args=list(mult = 1))+
  
rt_plot + 
  scale_x_discrete("Condition") + 
  scale_y_continuous("RT (ms)")+ 
  scale_fill_manual(values=val)+
  scale_color_manual(values=val)+ 
  guides(col="none",fill = "none")+
  theme_bw(28)
