library("dplyr")
library("ggplot2")
library("tidyr")

# Select class data set
fn<-file.choose()
f_path <- dirname(fn)
setwd(f_path)

data_set <- read.csv(file = fn, header = TRUE, stringsAsFactors = FALSE)
sig_det <- as.tbl(data_set)

contTab<-table(sig_det$Condition,sig_det$Accuracy,sig_det$Number.of.Dots,sig_det$randGator)

sigdet_resp <- ggplot(sig_det, aes(x=factor(Accuracy),fill=factor(Condition))) + 
  geom_bar(position="dodge") +facet_wrap(~Number.of.Dots)

sigdet_resp_subj_144 <- ggplot(filter(sig_det,Number.of.Dots==144), aes(x=factor(Accuracy),color=Condition,fill=Condition)) + 
  geom_bar(position="dodge") + facet_wrap(~randGator) 

sigdet_resp_subj_400 <- ggplot(filter(sig_det,Number.of.Dots==400), aes(x=factor(Accuracy),color=Condition,fill=Condition)) + 
  geom_bar(position="dodge") + facet_wrap(~randGator)

sigdet_resp_subj_1000 <- ggplot(filter(sig_det,Number.of.Dots==1000), aes(x=factor(Accuracy),color=Condition,fill=Condition)) + 
  geom_bar(position="dodge") + facet_wrap(~randGator) 


val <- c("#E41A1C", "#377EB8")
sigdet_resp_subj_144 + 
  scale_x_discrete("Accuracy",labels=c("Incorrect","Correct")) + 
  scale_y_continuous("Number") + 
  scale_fill_manual("Condition",values=val)+
  theme_bw(12)

sigdet_resp_subj_400 + 
  scale_x_discrete("Accuracy",labels=c("Incorrect","Correct")) + 
  scale_y_continuous("Number") + 
  scale_fill_manual("Condition",values=val)+
  theme_bw(12)

sigdet_resp_subj_1000 + 
  scale_x_discrete("Accuracy",labels=c("Incorrect","Correct")) + 
  scale_y_continuous("Number") + 
  scale_fill_manual("Condition",values=val)+
  theme_bw(12)

sigdet_resp + 
  scale_x_discrete("Accuracy",labels=c("Incorrect","Correct")) + 
  scale_y_continuous("Number") + 
  scale_fill_manual("Condition",values=val) +
  theme_bw(24)


sig_det[sig_det$Accuracy == 0 & sig_det$Response == "Present","resp_code"]<-"fa"
sig_det[sig_det$Accuracy == 0 & sig_det$Response == "Absent","resp_code"]<-"miss"
sig_det[sig_det$Accuracy == 1 & sig_det$Response == "Present","resp_code"]<-"hit"
sig_det[sig_det$Accuracy == 1 & sig_det$Response == "Absent","resp_code"]<-"crej"

sig_det_ct <- tally(group_by(sig_det,resp_code, randGator, Number.of.Dots), sort = F)

df_RG<-expand.grid(randGator=unique(sig_det_ct$randGator),
                   Number.of.Dots=c(144,400,1000),resp_code=c("fa","miss","hit","crej"),stringsAsFactors = F)

df_RG_tbl<-as.tbl(df_RG)
sig_det_ct_full<-full_join(df_RG_tbl,sig_det_ct)
sig_det_ct_full<-arrange(sig_det_ct_full,randGator,Number.of.Dots,resp_code)
sig_det_ct_full[is.na(sig_det_ct_full$n),"n"]<-0
sig_det_ct_full<-mutate(sig_det_ct_full,propn=n*0.1)

ggplot(sig_det_ct_full,aes(x=resp_code,y=n,color=factor(Number.of.Dots)))+geom_jitter()

ggplot(sig_det_ct_full,aes(x=resp_code,y=n,fill=factor(Number.of.Dots))) +
  geom_bar(stat="identity",position="dodge")+facet_wrap(~randGator)

sig_det_ct_fullavg <-sig_det_ct_full %>% group_by(resp_code,Number.of.Dots) %>% summarise(mean(propn))


df_RG_tbl$n<-0
df_RG_tbl<-arrange(df_RG_tbl,randGator,Number.of.Dots,resp_code)
sig_det_ct<-arrange(sig_det_ct,randGator,Number.of.Dots,resp_code)
sig_det_ct_full<-full_join(sig_det_ct,df_RG_tbl, by = c("randGator","Number.of.Dots","resp_code"))
sig_det_ct_full<-arrange(sig_det_ct_full,randGator,Number.of.Dots,resp_code)

sdt_grp<-sig_det_ct_full %>% select(-n.y) %>%spread(resp_code,n.x) 
sdt_grp[is.na(sdt_grp$crej),"crej"]<-0
sdt_grp[is.na(sdt_grp$fa),"fa"]<-0
sdt_grp[is.na(sdt_grp$hit),"hit"]<-0
sdt_grp[is.na(sdt_grp$miss),"miss"]<-0

sdt_grp<-mutate(sdt_grp,hr=hit/(hit+miss),fr=fa/(fa+crej),pc=(hit+crej)/(hit+miss+fa+crej),dp=qnorm(hr)-qnorm(fr))


ggplot(sdt_grp,aes(x=fr,y=hr,color=factor(Number.of.Dots)))+
  geom_jitter()+
  facet_wrap(~Number.of.Dots)+
  coord_fixed(ratio = 1)+xlim(0,1)+ylim(0,1)+theme_bw(28)


