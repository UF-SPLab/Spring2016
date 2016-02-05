library("dplyr")
library("ggplot2")
library("tidyr")

# Type getwd() to find address of project folder 
#    paste location as string for path
# example: f_path<-"/Users/R/Documents/Teaching/Spring2016/SensLab/CogLabs/"

# filename string
# if f_csv<-"SignalDetection/SigDet.csv"
# data_d<-read.csv(file= paste0(f_path,f_csv),header = T,stringsAsFactors=F)

data_set <- read.csv(file = "class_simple_det.csv", header = TRUE, stringsAsFactors = FALSE)
simple_det <- as.tbl(data_set)

ggplot(simple_det,aes(x=Trial,y=RT..ms.))+
  geom_smooth(se = T, method = "loess",span = 0.3)+
  scale_y_continuous(limits = c(200, 400))

ggplot(simple_det,aes(x=Trial,y=RT..ms.))+
  geom_line(aes(group = randGator),alpha=0.4)+
  geom_smooth(se = T, method = "loess",span = 0.1)+
  scale_y_continuous(limits = c(0, 500))
