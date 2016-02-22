library("dplyr")
library("ggplot2")
library("tidyr")

# Select class data set
fn<-file.choose()
f_path <- dirname(fn)
setwd(f_path)


data_set <- read.csv(file = fn, header = TRUE, stringsAsFactors = FALSE)
attentional_blink<-as.tbl(data_set)

lutboth<-c("K only"="K","Both"="KJ","J only"="J","Neither"="")
attentional_blink$TargResp<-lutboth[attentional_blink$Response]

ab_tot_1st<-attentional_blink %>% group_by(randGator,Separation,First.Target) %>% tally(n())
ab_tot_2nd<-attentional_blink %>% group_by(randGator,Separation,Second.Target) %>% tally(n())

ab_stat_1st <- attentional_blink %>% group_by(randGator,Separation,First.Target,Response) %>% tally(n(),sort=T) 
ab_stat_2nd <- attentional_blink %>% group_by(randGator,Separation,Second.Target,Response) %>% tally(n(),sort=T) 

ab_stat <- merge(ab_stat_1st,ab_stat_2nd)

ab_perf<-attentional_blink %>% rowwise %>% mutate(first=ifelse(grep(First.Target,TargResp,value = FALSE),1,0),second=ifelse(grep(Second.Target,TargResp,value = FALSE),1,0))

ab_perf_summary <- ab_perf %>% group_by(randGator,Separation) %>% summarise(first_pf=sum(first,na.rm=T),second_pf=sum(second,na.rm=T))
#ab_perf_summary <- ab_perf %>% group_by(randGator,Separation) %>% summarise(first_pf=sum(first,na.rm=T))
ab_perfs_tidy <- gather(ab_perf_summary,"Targ","Correct",-randGator,-Separation)

ggplot(ab_perfs_tidy,aes(x=Separation,y=Correct,col=Targ,group=Targ)) + 
  geom_line()+
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12))+
  scale_x_discrete(breaks = c(0,2,4,6,8))+
  coord_cartesian(ylim = c(0, 12),xlim=c(0,8))+
  theme_bw(12)+
  facet_wrap(~randGator)

ggplot(ab_perfs_tidy,aes(x=Separation,y=Correct,col=Targ))+
  stat_summary(geom="line",size=1.25, fun.y ="mean")+
  stat_summary(fun.data ="mean_se",geom = "errorbar",size=1.25)+
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12))+
  scale_x_discrete(breaks = c(0,2,4,6,8))+
  coord_cartesian(ylim = c(0, 12),xlim=c(0,8))+
  theme_bw(28)
