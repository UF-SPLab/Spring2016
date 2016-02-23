library("dplyr")
library("ggplot2")
library("tidyr")

# Select class data set
fn<-file.choose()
f_path <- dirname(fn)
setwd(f_path)

data_set <- read.csv(file = fn, header = TRUE, stringsAsFactors = FALSE)
vis_search<-as.tbl(data_set)

vis_search_subj <- vis_search %>% 
  unite(Distractor_Target, Distractor.type,Target) %>% 
  group_by(randGator,Number.of.distractors,Distractor_Target) %>% 
  summarise(sd=sd(RT),avg=mean(RT),ymin=avg-sd,ymax=avg+sd)

ggplot(vis_search_subj, aes(x=Number.of.distractors, y=avg, color=factor(Distractor_Target) ,group= factor(Distractor_Target))) +
  facet_wrap(~randGator)+
  geom_line(size=0.75)+
  geom_point(size=0.5) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.7)+
  scale_x_continuous(breaks = c(4,16,32,64))+
  scale_y_continuous("RT (ms)") + theme_bw(12)

ggsave("visual_search_line_facet_gator.png")

vis_search_grp <- vis_search_subj %>% 
  group_by(Distractor_Target, Number.of.distractors) %>% 
  summarise(sd=sd(avg),avg=mean(avg),ymin=avg-sd,ymax=avg+sd)

ggplot(vis_search_grp,aes(x=Number.of.distractors, y=avg, group= Distractor_Target,color = Distractor_Target)) +
  geom_line(size=2) +
  geom_point(size = 3) +
  geom_errorbar(aes(y=avg,ymin = ymin, ymax = ymax),width=0.7)+
  scale_x_continuous(breaks = c(4,16,32,64))+
  scale_y_continuous("RT (ms)") + theme_bw(24)

ggsave("visual_search_line_color_gator.png")

