###### Final Copy of Chapter 9 #######
### May 11 2020
rm(list=ls())
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Voter2016.RData")
source("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Analysis/Functions/BookFunctions.R")
require(msm)
require(dplyr)
require(ggplot2)
library(ggpubr)
full.data$authoritarianism<-rowMeans(cbind(full.data$auth1, full.data$auth2, full.data$auth3, full.data$auth4), na.rm=T)
full.data<-subset(full.data, white.2011==1)
prop.table(table(full.data$approve.2012, full.data$approve.2016), 1)
prop.table(table(full.data$approve.2016, full.data$approve.2017), 1)
names(full.data[,grepl("pid", names(full.data))])
## PID Recode
pid.recode<-function(data){
  rep<-car::recode(data, "1:2=0; 3:5=0; 6:7=1; else=NA" )
  dem<-car::recode(data, "1:2=1; 3:5=0; 6:7=0; else=NA" )
  ind<-car::recode(data, "1:2=0; 3:5=1; 6:7=0; else=NA" )
  return(data.frame(rep, dem, ind))
}
full.data$republican1<-pid.recode(full.data$pid.2016)[,1]
full.data$democrat1<-pid.recode(full.data$pid.2016)[,2]
full.data$independent1<-pid.recode(full.data$pid.2016)[,3]
full.data$republican2<-pid.recode(full.data$pid.2017)[,1]
full.data$democrat2<-pid.recode(full.data$pid.2017)[,2]
full.data$independent2<-pid.recode(full.data$pid.2017)[,3]
full.data$republican3<-pid.recode(full.data$pid.2018)[,1]
full.data$democrat3<-pid.recode(full.data$pid.2018)[,2]
full.data$independent3<-pid.recode(full.data$pid.2018)[,3]


interaction<-function(a, b){
  return(a*b)
}
full.data$republicanXauthoritarianism1<-with(full.data, interaction(republican1, authoritarianism))  
full.data$democratXauthoritarianism1<-with(full.data, interaction(democrat1, authoritarianism))  
full.data$independentXauthoritarianism1<-with(full.data, interaction(independent1, authoritarianism))  
full.data$republicanXauthoritarianism2<-with(full.data, interaction(republican2, authoritarianism))  
full.data$democratXauthoritarianism2<-with(full.data, interaction(democrat2, authoritarianism))  
full.data$independentXauthoritarianism2<-with(full.data, interaction(independent2, authoritarianism))  
full.data$republicanXauthoritarianism3<-with(full.data, interaction(republican3, authoritarianism))  
full.data$democratXauthoritarianism3<-with(full.data, interaction(democrat3, authoritarianism))  
full.data$independentXauthoritarianism3<-with(full.data, interaction(independent3, authoritarianism))  

full.data$party2_1<-car::recode(full.data$pid.2017, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )
full.data$party2_2<-car::recode(full.data$pid.2018, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )

full.data$unbound<-rowSums(cbind(full.data$president.congress.2017, 
                                 full.data$president.courts.2017, 
                                 full.data$president.media.2017), na.rm=T)

# ######  Chapter 9, Figure 2 #####
# a<-as.formula(as.factor(close.democracy.2018)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+jewish.2017+income.2016+
#                 catholic.2017+other.2017)
# 
# aa<-as.formula(as.factor(close.democracy.2018)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+jewish.2017+income.2016+
#                  catholic.2017+other.2017)
# 
# m1a<-nnet::multinom(a, data=full.data)
# m1b<-nnet::multinom(aa, data=full.data)
# new<-c("Democracy is preferable", "Democracy is sometimes preferable", "It doesn't matter")
# p3<-main.effect.V2(m1a, "2017", new)
# p4<-interactive.effect.V2(m1b, "2017", new)
# 
# subset(p4, category=="Democracy is sometimes preferable" & 
#          authoritarianism==0 & year==year &
#          PID=="Republican")[,1:5]-
#   subset(p4, category=="Democracy is sometimes preferable" & 
#            authoritarianism==1 & year==year &
#            PID=="Republican")[,1:5]
# 
# subset(p4, category=="Democracy is sometimes preferable" & 
#          authoritarianism==0 & year==year &
#          PID=="Democrat")[,1:5]-
#   subset(p4, category=="Democracy is sometimes preferable" & 
#            authoritarianism==1 & year==year &
#            PID=="Democrat")[,1:5]
# 
# 
# subset(p4, category=="Democracy is sometimes preferable" & 
#          authoritarianism==0 & year==year &
#          PID=="Independent")[,1:5]-
#   subset(p4, category=="Democracy is sometimes preferable" & 
#            authoritarianism==1 & year==year &
#            PID=="Independent")[,1:5]
# 
# 
# 
# subset(p4, category=="It doesn't matter" & 
#          authoritarianism==0 & year==year &
#          PID=="Independent")[,1:5]-
#   subset(p4, category=="It doesn't matter" & 
#            authoritarianism==1 & year==year &
#            PID=="Independent")[,1:5]
# a<-plot.f2("Which system of government is preferable", "Always Democracy, Sometimes Democracy, or Govnt form doesn't matter?")
# a
# 
# dev.copy(png,'ch9_2.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# 
# 
# 
# # ######  Chapter 9, Figure 3 #####
# a<-as.formula(free.speech~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+jewish.2017+income.2016+
#                 catholic.2017+other.2017)
# aa<-as.formula(free.speech~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+jewish.2017+income.2016+
#                  catholic.2017+other.2017)
# m1a<-glm(a, full.data, family=binomial("logit"))
# m1b<-glm(aa, full.data, family=binomial("logit"))
# new<-c("Restricted", "Unrestricted")
# p3<-main.effect.logit.V1(m1a, "2017",new)
# p4<-interactive.effect.logit.V1(m1b, "2017", new)
# a<-plot.f3("Free Speech or Restricted Speech?", "")
# a
# dev.copy(png,'ch9_3.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# ###### Chapter 9, FIgure 4 #######
# a<-as.formula(as.factor(unbound)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+jewish.2017+income.2016+
#                 catholic.2017+other.2017)
# aa<-as.formula(as.factor(unbound)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+jewish.2017+income.2016+
#                  catholic.2017+other.2017)
# m1a<-MASS::polr(a, full.data)
# m1aa<-MASS::polr(aa, full.data)
# new=c("0", "1", "2", "3")
# p3<-main.effectV1(m1a, "2017", new)
# p4<-interactive.effectV1(m1aa, "2017", new)
# 
# subset(p4, category>1 & 
#             authoritarianism==1&
#             PID=="Independent")
# subset(p4, category>1 & 
#          authoritarianism==0&
#          PID=="Independent")
# subset(p4, category>1 & 
#          authoritarianism==1&
#          PID=="Democrat")
# subset(p4, category>1 & 
#          authoritarianism==0&
#          PID=="Democrat")
# 
# subset(p4, category>1 & 
#          authoritarianism==1&
#          PID=="Republican")
# subset(p4, category>1 & 
#          authoritarianism==0&
#          PID=="Republican")
# a<-plot.f("Authoritarian Support for an Unbound Executive", "")
# a
# dev.copy(png,'ch9_4.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# #### Chapter 9, Figure 5 ######
# a<-as.formula(as.factor(strong.leader.2018)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+jewish.2017+income.2016+
#                 catholic.2017+other.2017)
# aa<-as.formula(as.factor(strong.leader.2018)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+jewish.2017+income.2016+
#                  catholic.2017+other.2017)
# m1a<-MASS::polr(a, full.data)
# m1aa<-MASS::polr(aa, full.data)
# new=c("Very Bad", "Fairly Bad", "Fairly Good", "Very Good")
# p3<-rbind(main.effectV1(m1a, "2017", new))
# p4<-rbind(interactive.effectV1(m1aa, "2017", new))
# View(p3)
# view<-(subset(p4, p4$category=="Fairly Good"|p4$category=="Very Good"))
# tapply(view[,5], list(view$authoritarianism, view$PID), sum)
# a<-plot.f("Strong Leader, without congress/election", "Is it very good, fairly good, fairly bad or very bad way of governing this country?")
# a
# dev.copy(png,'ch9_6.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# 
# 
# 
# 
# 

##### Chapter 9, Figure 6 #######
a<-as.formula(as.factor(army.rule.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(army.rule.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Very Bad", "Fairly Bad", "Fairly Good", "Very Good")
p3<-rbind(main.effectV1(m1a, "2017", new))
p4<-rbind(interactive.effectV1(m1aa, "2017", new))
a<-plot.f("Governing by Military Rule", "Is it very good, fairly good, fairly bad or very bad way of governing this country?")
a
dev.copy(png,'ch9_7.jpg',
         width = 750, height = 500)
dev.off()
##### Satisfaction with Democracy Rule #######
a<-as.formula(as.factor(satisfaction.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(satisfaction.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Not at all Satisfied", "Not Very Satisfied", "Fairly Satisfied", "Very Satisfied")
p3<-rbind(main.effectV1(m1a, "2017", new))
p4<-rbind(interactive.effectV1(m1aa, "2017", new))
a<-plot.f("Satisfaction with Democracy", "")
a
dev.copy(png,'ch9_8.jpg',
         width = 750, height = 500)
dev.off()

### President and Laws #####
a<-as.formula(as.factor(president.laws.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(president.laws.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-glm(a, full.data, family=binomial("logit"))
m1b<-glm(aa, full.data, family=binomial("logit"))
ti<-rep(c("President not bound by laws", "Bound by laws"), each=11)
new=c("Very Bad", "Fairly Bad", "Fairly Good", "Very Good")
p3<-main.effect.logit.V1(m1a, "2017",c("President not bound by laws", "Bound by laws"))
p4<-interactive.effect.logit.V1(m1b, "2017", c("President not bound by laws", "Bound by laws"))
view<-(subset(p4, as.numeric(p4$category)>0))
tapply(view[,5], list(view$authoritarianism, view$PID), mean)         
plot.f<-function(lab1, lab2, lab3){
  p1<-ggplot(p3,aes(x=authoritarianism,y=mean.score
                    , fill=Category)) + 
    geom_area(color = "black", size = 0.2, alpha = 0.4) + 
    scale_fill_brewer(direction=10)+
    geom_area(colour="black", size=.2, alpha=.8)+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    labs(title = lab1,
         subtitle= lab2)+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,1.04))+
    scale_x_continuous("Authoritarianism")
  
  p1
  
  p2<-ggplot(p4,aes(x=authoritarianism,y=mean.score
                    , fill=Category)) + 
    facet_wrap(~PID)+
    geom_area(color = "black", size = 0.2, alpha = 0.4) + 
    scale_fill_brewer(direction=-10)+
    geom_area(colour="black", size=.2, alpha=.8)+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    labs(caption = "Data source: 2016-2019 Voter Study Group")+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,1.02))+
    scale_x_continuous("Authoritarianism")
  
  p2
  return(ggarrange(p1,p2,
                   nrow = 2,
                   common.legend = TRUE, 
                   legend = "right"))
  
  
}
a<-plot.f("Should the president be bound by laws?", "")
a
dev.copy(png,'ch9_10.jpg',
         width = 750, height = 500)
dev.off()
### President and Laws #####
a<-as.formula(free.speech~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(free.speech~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-glm(a, full.data, family=binomial("logit"))
m1b<-glm(aa, full.data, family=binomial("logit"))
new<-c("Restricted", "Unrestricted")
p3<-main.effect.logit.V1(m1a, "2017",new)
p4<-interactive.effect.logit.V1(m1b, "2017", new)
a<-plot.f("Free Speech or Restricted Speech?", "")
a
dev.copy(png,'ch9_11.jpg',
         width = 750, height = 500)
dev.off()
############# Flag Burning ###########
a<-as.formula(as.factor(flag.burn)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(flag.burn)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Strongly Oppose", "Somewhat Oppose", "Somewhat Favor", "Strongly Favor")
p3<-main.effectV1(m1a, "2017", new)
p4<-interactive.effectV1(m1aa, "2017", new)
plot.f<-function(lab1, lab2, lab3){
  p1<-ggplot(p3,aes(x=authoritarianism,y=ONE.mean.score
                    , fill=Category)) + 
    geom_area(color = "black", size = 0.2, alpha = 0.4) + 
    scale_fill_brewer(direction=10)+
    geom_area(colour="black", size=.2, alpha=.8)+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    labs(title = lab1,
         subtitle= lab2)+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,1.04))+
    scale_x_continuous("Authoritarianism")
  
  p1
  
  p2<-ggplot(p4,aes(x=authoritarianism,y=ONE.mean.score
                    , fill=Category)) + 
    facet_wrap(~PID)+
    geom_area(color = "black", size = 0.2, alpha = 0.4) + 
    scale_fill_brewer(direction=-10)+
    geom_area(colour="black", size=.2, alpha=.8)+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    labs(caption = "Data source: 2016-2019 Voter Study Group")+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,1.02))+
    scale_x_continuous("Authoritarianism")
  
  p2
  return(ggarrange(p1,p2,
                   nrow = 2,
                   common.legend = TRUE, 
                   legend = "right"))
  
  
}
a<-plot.f("Constitutional Amendment to Bans Flag Burning", "")
a
dev.copy(png,'ch9_12.jpg',
         width = 750, height = 500)
dev.off()
#### Kneeling during the National Anthem ####
a<-as.formula(as.factor(kneeling)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(kneeling)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Strongly Approve", "Somewhat Approve", "Somewhat Disapprove", "Strongly Disapprove")
p3<-main.effectV1(m1a, "2017", new)
p4<-interactive.effectV1(m1aa, "2017", new)
a<-plot.f("Protest by Kneeling during the National Anthem", "")
a
dev.copy(png,'ch9_13.jpg',
         width = 750, height = 500)
dev.off()
}
### Estimate Effects for all groups 
rm(list=ls())
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Voter2016.RData")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
require(msm)
require(dplyr)
require(ggpubr)
full.data$authoritarianism<-rowMeans(cbind(full.data$auth1, full.data$auth2, full.data$auth3, full.data$auth4), na.rm=T)
#full.data<-subset(full.data, white.2011==1)
full.data<-subset(full.data, black.2011==1 | hispanic.2011==1)
names(full.data[,grepl("pid", names(full.data))])
## PID Recode
pid.recode<-function(data){
  rep<-car::recode(data, "1:2=0; 3:5=0; 6:7=1; else=NA" )
  dem<-car::recode(data, "1:2=1; 3:5=0; 6:7=0; else=NA" )
  ind<-car::recode(data, "1:2=0; 3:5=1; 6:7=0; else=NA" )
  return(data.frame(rep, dem, ind))
}

full.data$republican1<-pid.recode(full.data$pid.2016)[,1]
full.data$democrat1<-pid.recode(full.data$pid.2016)[,2]
full.data$independent1<-pid.recode(full.data$pid.2016)[,3]

full.data$republican2<-pid.recode(full.data$pid.2017)[,1]
full.data$democrat2<-pid.recode(full.data$pid.2017)[,2]
full.data$independent2<-pid.recode(full.data$pid.2017)[,3]

full.data$republican3<-pid.recode(full.data$pid.2018)[,1]
full.data$democrat3<-pid.recode(full.data$pid.2018)[,2]
full.data$independent3<-pid.recode(full.data$pid.2018)[,3]

interaction<-function(a, b){
  return(a*b)
}
full.data$republicanXauthoritarianism1<-with(full.data, interaction(republican1, authoritarianism))  
full.data$democratXauthoritarianism1<-with(full.data, interaction(democrat1, authoritarianism))  
full.data$independentXauthoritarianism1<-with(full.data, interaction(independent1, authoritarianism))  
full.data$republicanXauthoritarianism2<-with(full.data, interaction(republican2, authoritarianism))  
full.data$democratXauthoritarianism2<-with(full.data, interaction(democrat2, authoritarianism))  
full.data$independentXauthoritarianism2<-with(full.data, interaction(independent2, authoritarianism))  
full.data$republicanXauthoritarianism3<-with(full.data, interaction(republican3, authoritarianism))  
full.data$democratXauthoritarianism3<-with(full.data, interaction(democrat3, authoritarianism))  
full.data$independentXauthoritarianism3<-with(full.data, interaction(independent3, authoritarianism))  

full.data$party2_1<-car::recode(full.data$pid.2017, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )
full.data$party2_2<-car::recode(full.data$pid.2018, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )

full.data$unbound<-rowSums(cbind(full.data$president.congress.2017, 
                                 full.data$president.courts.2017, 
                                 full.data$president.media.2017), na.rm=T)


minority<-full.data
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Voter2016.RData")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
full.data$authoritarianism<-rowMeans(cbind(full.data$auth1, full.data$auth2, full.data$auth3, full.data$auth4), na.rm=T)
#full.data<-subset(full.data, white.2011==1)
full.data<-subset(full.data, white.2011==1)
names(full.data[,grepl("pid", names(full.data))])
## PID Recode
pid.recode<-function(data){
  rep<-car::recode(data, "1:2=0; 3:5=0; 6:7=1; else=NA" )
  dem<-car::recode(data, "1:2=1; 3:5=0; 6:7=0; else=NA" )
  ind<-car::recode(data, "1:2=0; 3:5=1; 6:7=0; else=NA" )
  return(data.frame(rep, dem, ind))
}

full.data$republican1<-pid.recode(full.data$pid.2016)[,1]
full.data$democrat1<-pid.recode(full.data$pid.2016)[,2]
full.data$independent1<-pid.recode(full.data$pid.2016)[,3]

full.data$republican2<-pid.recode(full.data$pid.2017)[,1]
full.data$democrat2<-pid.recode(full.data$pid.2017)[,2]
full.data$independent2<-pid.recode(full.data$pid.2017)[,3]

full.data$republican3<-pid.recode(full.data$pid.2018)[,1]
full.data$democrat3<-pid.recode(full.data$pid.2018)[,2]
full.data$independent3<-pid.recode(full.data$pid.2018)[,3]

interaction<-function(a, b){
  return(a*b)
}
full.data$republicanXauthoritarianism1<-with(full.data, interaction(republican1, authoritarianism))  
full.data$democratXauthoritarianism1<-with(full.data, interaction(democrat1, authoritarianism))  
full.data$independentXauthoritarianism1<-with(full.data, interaction(independent1, authoritarianism))  
full.data$republicanXauthoritarianism2<-with(full.data, interaction(republican2, authoritarianism))  
full.data$democratXauthoritarianism2<-with(full.data, interaction(democrat2, authoritarianism))  
full.data$independentXauthoritarianism2<-with(full.data, interaction(independent2, authoritarianism))  
full.data$republicanXauthoritarianism3<-with(full.data, interaction(republican3, authoritarianism))  
full.data$democratXauthoritarianism3<-with(full.data, interaction(democrat3, authoritarianism))  
full.data$independentXauthoritarianism3<-with(full.data, interaction(independent3, authoritarianism))  

full.data$party2_1<-car::recode(full.data$pid.2017, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )
full.data$party2_2<-car::recode(full.data$pid.2018, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )

full.data$unbound<-rowSums(cbind(full.data$president.congress.2017, 
                                 full.data$president.courts.2017, 
                                 full.data$president.media.2017), na.rm=T)

whites<-full.data
######################################################################
## Important to have a strong leader.
######################################################################
a<-as.formula(as.factor(strong.leader.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+jewish.2017+other.2017)
aa<-as.formula(as.factor(strong.leader.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)
require(MASS)
m1a<-polr(a, whites)
m1aa<-polr(aa, whites)
m1b<-polr(a, minority)
m1bb<-polr(aa, minority)
p3<-main.effect(m1a)
p4<-interactive.effect(m1aa)
p5<-main.effect(m1b)
p6<-interactive.effect(m1bb)
###################################################################################################################
temp1<-rbind(p3,p4,p5,p6)
temp1$Question<-"Importance of a Strong Leader"
temp1$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
###################################################################################################################
###################################################################################################################
###################################################################################################################
# Next Question
a<-as.formula(as.factor(army.rule.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)
aa<-as.formula(as.factor(army.rule.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)
m1a<-polr(a, whites)
m1aa<-polr(aa, whites)
m1b<-polr(a, minority)
m1bb<-polr(aa, minority)
p3<-main.effect(m1a)
p4<-interactive.effect(m1aa)
p5<-main.effect(m1b)
p6<-interactive.effect(m1bb)
###################################################################################################################
temp2<-rbind(p3,p4,p5,p6)
temp2$Question<-"Should the Army lead the country?"
temp2$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
###################################################################################################################
###################################################################################################################
##### Satisfaction with Democracy Rule #######
a<-as.formula(as.factor(satisfaction.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)
aa<-as.formula(as.factor(satisfaction.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)
m1a<-polr(a, whites)
m1aa<-polr(aa, whites)
m1b<-polr(a, minority)
m1bb<-polr(aa, minority)
p3<-main.effect(m1a)
p4<-interactive.effect(m1aa)
p5<-main.effect(m1b)
p6<-interactive.effect(m1bb)
###################################################################################################################
temp3<-rbind(p3,p4,p5,p6)
temp3$Question<-"Satisfaction with Democracy"
temp3$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
###################################################################################################################
###################################################################################################################
###################################################################################################################
#####  Close Democracy Questions   #######
a<-as.formula(as.factor(close.democracy.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)

aa<-as.formula(as.factor(close.democracy.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)

m1a<-nnet::multinom(a, data=whites)
m1aa<-nnet::multinom(aa, data=whites)
m1b<-nnet::multinom(a, data=minority)
m1bb<-nnet::multinom(aa, data=minority)
p3<-main.effect2(m1a)
p4<-interactive.effect2(m1aa)
p5<-main.effect2(m1b)
p6<-interactive.effect2(m1bb)
###################################################################################################################
temp4<-rbind(p3,p4,p5,p6)
temp4$Question<-"Anti-Democratic Preferences"
temp4$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)

###################################################################################################################
###################################################################################################################
#####  President and Laws   #######
### President and Laws #####
a<-as.formula(as.factor(president.laws.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)
aa<-as.formula(as.factor(president.laws.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)

m1a<-glm(a, whites, family=binomial("logit"))
m1aa<-glm(aa, whites, family=binomial("logit"))
m1b<-glm(a, minority, family=binomial("logit"))
m1bb<-glm(aa, minority, family=binomial("logit"))
p3<-main.effect2(m1a)
p4<-interactive.effect2(m1aa)
p5<-main.effect2(m1b)
p6<-interactive.effect2(m1bb)
###################################################################################################################
temp5<-rbind(p3,p4,p5,p6)
temp5$Question<-"President Should Not be Bound by Laws"
temp5$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)

### President and Laws #####
a<-as.formula(free.speech~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)
aa<-as.formula(free.speech~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)
m1a<-glm(a, whites, family=binomial("logit"))
m1aa<-glm(aa, whites, family=binomial("logit"))
m1b<-glm(a, minority, family=binomial("logit"))
m1bb<-glm(aa, minority, family=binomial("logit"))
p3<-main.effect2(m1a)
p4<-interactive.effect2(m1aa)
p5<-main.effect2(m1b)
p6<-interactive.effect2(m1bb)
###################################################################################################################
temp6<-rbind(p3,p4,p5,p6)
temp6$Question<-"Preference for Free Speech Restrictions"
temp6$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)

###################################################################################################################
############# Flag Burning ###########
a<-as.formula(as.factor(flag.burn)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)
aa<-as.formula(as.factor(flag.burn)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)
m1a<-MASS::polr(a, whites)
m1aa<-MASS::polr(aa, whites)
m1b<-MASS::polr(a, minority)
m1bb<-MASS::polr(aa, minority)
p3<-main.effect(m1a)
p4<-interactive.effect(m1aa)
p5<-main.effect(m1b)
p6<-interactive.effect(m1bb)
###################################################################################################################
temp7<-rbind(p3,p4,p5,p6)
temp7$Question<-"Flag Burning"
temp7$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)

###################################################################################################################
###################################################################################################################
###################################################################################################################
#### Kneeling during the National Anthem ####
a<-as.formula(as.factor(kneeling)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)
aa<-as.formula(as.factor(kneeling)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)
m1a<-MASS::polr(a, whites)
m1aa<-MASS::polr(aa, whites)
m1b<-MASS::polr(a, minority)
m1bb<-MASS::polr(aa, minority)
p3<-main.effect(m1a)
p4<-interactive.effect(m1aa)
p5<-main.effect(m1b)
p6<-interactive.effect(m1bb)
###################################################################################################################
temp8<-rbind(p3,p4,p5,p6)
temp8$Question<-"Ban Kneeling during National Anthem"
temp8$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
###################################################################################################################
###################################################################################################################
###################################################################################################################
###### Unbound Executive #######
a<-as.formula(as.factor(unbound)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+income.2016+
                catholic.2017+jewish.2017+other.2017)
aa<-as.formula(as.factor(unbound)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+income.2016+
                 catholic.2017+jewish.2017+other.2017)
m1a<-MASS::polr(a, whites)
m1aa<-MASS::polr(aa, whites)
m1b<-MASS::polr(a, minority)
m1bb<-MASS::polr(aa, minority)
p3<-main.effect(m1a)
p4<-interactive.effect(m1aa)
p5<-main.effect(m1b)
p6<-interactive.effect(m1bb)
###################################################################################################################
temp9<-rbind(p3,p4,p5,p6)
temp9$Question<-"Preference for an Unbound Executive"
temp9$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)

dat<-data.frame(rbind(
  temp1, temp2, temp3,
  temp4, temp5, temp6,
  temp7, temp8, temp9))
dat$Item<-paste0(dat$group,":", dat$Question)
dat$Authoritarian<-car::recode(dat$authoritarianism, "1='Authoritarian';0='Non-Authoritarian'; else=NA")

d1<-subset(dat, PID=="Average Effect")
d2<-subset(dat, PID=="Republican")
d3<-subset(dat, PID=="Independent")
d4<-subset(dat, PID=="Democrat")

gg.func<-function(dat, lab){
  dat$Question<-factor(dat$Question, levels = rev(c("Satisfaction with Democracy",
                                                    "Importance of a Strong Leader",
                                                    "Anti-Democratic Preferences",
                                                    "President Should Not be Bound by Laws",
                                                    "Should the Army lead the country?",
                                                    "Preference for Free Speech Restrictions",
                                                    "Flag Burning",
                                                    "Ban Kneeling during National Anthem",
                                                    "Preference for an Unbound Executive"
  )))
  return(
    ggplot(dat, aes(x=as.factor(Question),
                    y=mean.score, ymax=min.2.5, ymin=max.97.5,
                    colour=as.factor(Authoritarian))) +
      facet_wrap(~Majority)+
      scale_colour_manual(name="Authoritarianism", values=c("black", "gray"))+
      geom_errorbar( width = 0.1) +
      geom_point(size = 1, alpha = 0.4) + 
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle(lab) +
      theme(plot.title=element_text(hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353", angle=90)) +
      theme(axis.text.y=element_text(size=10, colour="#535353")) +
      theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
      theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
      scale_y_continuous("Probability", limits=c(0,1))+
      scale_x_discrete("Question")+
      coord_flip()
  )
}


gg.func(d1, lab="Average Effect of Authoritarianism")
dev.copy(png,'ch9_15.jpg',
         width = 750, height = 500)
dev.off()

gg.func(d2, lab="Authoritarianism, Republican Respondents")
dev.copy(png,'ch9_16.jpg',
         width = 750, height = 500)
dev.off()

gg.func(d3, lab="Authoritarianism, Independent Respondents")
dev.copy(png,'ch9_17.jpg',
         width = 750, height = 500)
dev.off()

gg.func(d4, lab="Authoritarianism, Democratic Respondents")
dev.copy(png,'ch9_18.jpg',
         width = 750, height = 500)
dev.off()





