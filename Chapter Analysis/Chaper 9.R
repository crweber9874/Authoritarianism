### Final Copy of Chapter 9 #######
### July 28 2020
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

full.data$egalitarianism<-zero.one(rowSums(cbind(full.data$egal1, 
                                 full.data$egal2, 
                                 full.data$egal3, full.data$egal4), na.rm=T))

full.data$party7<-zero.one(full.data$pid.2017)
full.data$authoritarianismXegalitarianism<-with(full.data, interaction(authoritarianism, egalitarianism))  
full.data$pidXauthoritarianism<-with(full.data, interaction(party7, authoritarianism))  
with(full.data,
     psych::alpha(cbind(ccongress.2018, cfbi.2018, cmedia.2018,
                        cmilitary.2018, cjustice.2018))
)
with(full.data,
     psych::alpha(cbind(ccongress.2019, cfbi.2019, cmedia.2019,
                        cmilitary.2019, cjustice.2019))
)


full.data$confidence1<-zero.one(with(full.data,
                           rowMeans(cbind(ccongress.2018, cfbi.2018, cmedia.2018,
                                              cmilitary.2018, cjustice.2018), na.rm=T)
))

full.data$confidence2<-zero.one(with(full.data,
                                     rowMeans(cbind(ccongress.2019, cfbi.2019, cmedia.2019,
                                                    cmilitary.2019, cjustice.2019), na.rm=T)
))

## Chapter 9, Figure 1 
a<-as.formula(confidence1~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(confidence1~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
b<-as.formula(confidence2~
                authoritarianism+
                female.2018+age.2018+
                college.2018+jewish.2018+income.2016+
                catholic.2018+other.2018)
bb<-as.formula(confidence2~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2018+age.2018+
                 college.2018+jewish.2018+income.2016+
                 catholic.2018+other.2018)
m1a<-lm(a, data=full.data)
m1b<-lm(aa, data=full.data)
m2a<-lm(b, data=full.data)
m2b<-lm(bb, data=full.data)
p1<-main.effect.ols.V1(m1a, "2018")
p2<-main.effect.ols.V1(m2a, "2019")
p3<-interactive.effect.ols.V1(m1b, "2018")
p4<-interactive.effect.ols.V1(m2b, "2019")

a<-plot.f3("Confidence", "U.S. Institutions. 2018", "", 0.55, p1, p3)

b<-plot.f3("Confidence", "U.S. Institutions. 2019", "", 0.55, p2, p4)

a
dev.copy(png,'ch9_1a.jpg',
         width = 750, height = 500)
dev.off()

b
dev.copy(png,'ch9_1b.jpg',
         width = 750, height = 500)
dev.off()



# ######  Chapter 9, Figure 2 #####
a<-as.formula(as.factor(close.democracy.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)

aa<-as.formula(as.factor(close.democracy.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)

m1a<-nnet::multinom(a, data=full.data)
m1b<-nnet::multinom(aa, data=full.data)
new<-c("Democracy is preferable", "Democracy is sometimes preferable", "It doesn't matter")
p3<-main.effect.V2(m1a, "2017", new)
p4<-interactive.effect.V2(m1b, "2017", new)

subset(p4, category=="Democracy is sometimes preferable" &
         authoritarianism==0 & year==year &
         PID=="Republican")[,1:5]-
  subset(p4, category=="Democracy is sometimes preferable" &
           authoritarianism==1 & year==year &
           PID=="Republican")[,1:5]

subset(p4, category=="Democracy is sometimes preferable" &
         authoritarianism==0 & year==year &
         PID=="Democrat")[,1:5]-
  subset(p4, category=="Democracy is sometimes preferable" &
           authoritarianism==1 & year==year &
           PID=="Democrat")[,1:5]


subset(p4, category=="Democracy is sometimes preferable" &
         authoritarianism==0 & year==year &
         PID=="Independent")[,1:5]-
  subset(p4, category=="Democracy is sometimes preferable" &
           authoritarianism==1 & year==year &
           PID=="Independent")[,1:5]



subset(p4, category=="It doesn't matter" &
         authoritarianism==0 & year==year &
         PID=="Independent")[,1:5]-
  subset(p4, category=="It doesn't matter" &
           authoritarianism==1 & year==year &
           PID=="Independent")[,1:5]
a<-plot.f2("Which system of government is preferable", "Always Democracy, Sometimes Democracy, or Govnt form doesn't matter?")
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
a<-plot.f3("Free Speech or Restricted Speech?", "")
# a
# dev.copy(png,'ch9_3.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# ###### Chapter 9, FIgure 4 #######
a<-as.formula(as.factor(unbound)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(unbound)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("0", "1", "2", "3")
p3<-main.effectV1(m1a, "2017", new)
p4<-interactive.effectV1(m1aa, "2017", new)

subset(p4, category>=1 &
            authoritarianism==1&
            PID=="Independent")
subset(p4, category>=1 &
         authoritarianism==0&
         PID=="Independent")


subset(p4, category>=1 &
         authoritarianism==1&
         PID=="Democrat")
subset(p4, category>=1 &
         authoritarianism==0&
         PID=="Democrat")

subset(p4, category>=1 &
         authoritarianism==1&
         PID=="Republican")
subset(p4, category>=1 &
         authoritarianism==0&
         PID=="Republican")
a<-plot.f("Authoritarian Support for an Unbound Executive", "")
a
dev.copy(png,'ch9_4.jpg',
         width = 750, height = 500)
dev.off()


# #### Chapter 9, Figure 5 ######
a<-as.formula(as.factor(strong.leader.2017)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(strong.leader.2017)~
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
View(p3)
view<-(subset(p4, p4$category=="Fairly Good"|p4$category=="Very Good"))
tapply(view[,5], list(view$authoritarianism, view$PID), sum)
subset(p4, category== "Very Bad"&
         authoritarianism==1&
         PID=="Republican")
subset(p4, category== "Very Bad"&
         authoritarianism==0&
         PID=="Republican")


a<-plot.f("Strong Leader, without congress/election", "Is it very good, fairly good, fairly bad or very bad way of governing this country?")
a
dev.copy(png,'ch9_6.jpg',
         width = 750, height = 500)
dev.off()
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# ##### Chapter 9, Figure 6 #######
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
#Test
new=c("Very Bad", "Fairly Bad", "Fairly Good", "Very Good")
p3<-rbind(main.effectV1(m1a, "2017", new))
p4<-rbind(interactive.effectV1(m1aa, "2017", new))
a<-plot.f("Governing by Military Rule", "Is it very good, fairly good, fairly bad or very bad way of governing this country?")
a
dev.copy(png,'ch9_6.jpg',
         width = 750, height = 500)
dev.off()






# ############# Flag Burning, Figure 7 ###########
# a<-as.formula(as.factor(flag.burn)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+jewish.2017+income.2016+
#                 catholic.2017+other.2017)
# aa<-as.formula(as.factor(flag.burn)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+jewish.2017+income.2016+
#                  catholic.2017+other.2017)
# m1a<-MASS::polr(a, full.data)
# m1aa<-MASS::polr(aa, full.data)
# new=c("Strongly Oppose", "Somewhat Oppose", "Somewhat Favor", "Strongly Favor")
# p3<-main.effectV1(m1a, "2017", new)
# p4<-interactive.effectV1(m1aa, "2017", new)
# 
# a<-plot.f("Constitutional Amendment to Bans Flag Burning", "")
# a
# dev.copy(png,'ch9_8.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# 
# 


# #### Chapter 9 Figure 8 ####
# a<-as.formula(as.factor(kneeling)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+jewish.2017+income.2016+
#                 catholic.2017+other.2017)
# aa<-as.formula(as.factor(kneeling)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+jewish.2017+income.2016+
#                  catholic.2017+other.2017)
# m1a<-MASS::polr(a, full.data)
# m1aa<-MASS::polr(aa, full.data)
# new=c("Strongly Approve", "Somewhat Approve", "Somewhat Disapprove", "Strongly Disapprove")
# p3<-main.effectV1(m1a, "2017", new)
# p4<-interactive.effectV1(m1aa, "2017", new)
# a<-plot.f("Protest by Kneeling during the National Anthem", "")
# a
# dev.copy(png,'ch9_8.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# 
# 
# # ###### Chapter 9 Figure 9,10,11,12, 13 #####
v.s2=c("egalitarianism", "authoritarianism", "party7",
       "authoritarianismXegalitarianism", "pidXauthoritarianism", "female.2016",
       "age.2016", "college.2017", "jewish.2017", "income.2016",
       "catholic.2017", "other.2017")
comp.analysis<-function(outcome, cov.vector){
  aa <- as.formula(
    paste(paste0("as.factor(", outcome, ")"),
          paste(cov.vector, collapse = " + "),
          sep = " ~ "))
  return(aa)
}
outcome="flag.burn"
m1aa<-MASS::polr(comp.analysis(outcome, v.s2), full.data)
flag<-comp.plot(m1aa)
flag.m<-plot.cue(m1aa)

outcome="kneeling"
m1aa<-MASS::polr(comp.analysis(outcome, v.s2), full.data)
kneel<-comp.plot(m1aa)
kneel.m<-plot.cue(m1aa)

outcome="strong.leader.2017"
m1aa<-MASS::polr(comp.analysis(outcome, v.s2), full.data)
strong.leader<-comp.plot(m1aa)
strong.m<-plot.cue(m1aa)

outcome="unbound"
m1aa<-MASS::polr(comp.analysis(outcome, v.s2), full.data)
unbound<-comp.plot(m1aa)
unbound.m<-plot.cue(m1aa)

plot.m<-data.frame(rbind(flag.m, kneel.m, strong.m, unbound.m),
           Item=rep(c("Flag Burn", "Kneeling", "Strong Leader", "Unbound"), each=11*2)
)


#save(strong.leader, file="/users/chrisweber/Desktop/dat.Rda")

plot.function<-function(dat, lab1, upper){
p1<-ggplot(dat[[1]], aes(x = cov,
        y = mean.score, ymin=min.2.5,
        ymax=max.97.5, group=Egalitarianism))+
    geom_ribbon(fill="lightgray", alpha=0.5)+
    geom_line(aes(x=cov, y=mean.score, colour=Egalitarianism))+
    theme(text=element_text(size=10),
        axis.text.y=element_text(angle=45))+
    scale_colour_manual(name="AE:", values=c("gray", "black"))+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    labs(title = lab1,
         subtitle="Anti-Egalitarianism"
         )+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="black",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,upper))+
    scale_x_continuous("Authoritarianism")
  p1

  p2<-ggplot(dat[[2]], aes(x = cov,
                           y = mean.score, ymin=min.2.5,
                           ymax=max.97.5, group=Party))+
    geom_ribbon(fill="lightgray", alpha=0.5)+
    geom_line(aes(x=cov, y=mean.score, colour=Party))+
    theme(text=element_text(size=10),
          axis.text.y=element_text(angle=45))+
    scale_colour_manual(name="PID:", values=c("gray", "black"))+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    labs(title = "",
         subtitle="PID"
    )+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="black",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,upper))+
    scale_x_continuous("Authoritarianism")
  p2

  return(ggarrange(p1,p2,
                   ncol = 2,
                   common.legend = FALSE,
                   legend = "bottom"))


}
plot.function(unbound, "Support an Unbound Executive", 0.7)
dev.copy(png,'ch9_9.jpg',
          width = 750, height = 500)
dev.off()

plot.function(kneel, "Protest Kneeling during Anthem", 1)
dev.copy(png,'ch9_10.jpg',
          width = 750, height = 500)
dev.off()

plot.function(flag, "Constitutional Amendment to Ban Flag Burning", 1)
dev.copy(png,'ch9_11.jpg',
         width = 750, height = 500)
dev.off()

plot.function(strong.leader, "Strong Leader", 0.9)
dev.copy(png,'ch9_12.jpg',
         width = 750, height = 500)
dev.off()


p1<-ggplot(plot.m, aes(x = authoritaritarianism,
                         y = mean, ymin=min,
                         ymax=max, group=group))+
  facet_wrap(~Item)+
  geom_ribbon(fill="lightgray", alpha=0.5)+
  geom_line(aes(x=authoritaritarianism, y=mean, colour=group))+
  theme(text=element_text(size=10),
        axis.text.y=element_text(angle=45))+
  scale_colour_manual(name="Group:", values=c("gray", "black"))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  labs(title = "Marginal Effect",
       subtitle="by Group and Item"
  )+
  theme(plot.title=element_text(hjust=-.03,vjust=2,colour="black",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
  theme(axis.text.y=element_text(size=10, colour="#535353")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Marginal Effect", limits=c(-.4,1))+
  scale_x_continuous("Authoritarianism")+
  geom_hline(yintercept=0, size=0.25, linetype=2)
p1

dev.copy(png,'ch9_13.jpg',
         width = 750, height = 500)
dev.off()




# 
### Estimate Effects for all groups, Appendix  ######
# rm(list=ls())
# setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
# load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Voter2016.RData")
# source("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Analysis/Functions/BookFunctions.R")
# require(msm)
# require(dplyr)
# require(ggplot2)
# library(ggpubr)
# full.data$authoritarianism<-rowMeans(cbind(full.data$auth1, full.data$auth2, full.data$auth3, full.data$auth4), na.rm=T)
# 
# full.data<-subset(full.data, black.2011==1 | hispanic.2011==1)
# 
# prop.table(table(full.data$approve.2012, full.data$approve.2016), 1)
# prop.table(table(full.data$approve.2016, full.data$approve.2017), 1)
# names(full.data[,grepl("pid", names(full.data))])
# ## PID Recode
# pid.recode<-function(data){
#   rep<-car::recode(data, "1:2=0; 3:5=0; 6:7=1; else=NA" )
#   dem<-car::recode(data, "1:2=1; 3:5=0; 6:7=0; else=NA" )
#   ind<-car::recode(data, "1:2=0; 3:5=1; 6:7=0; else=NA" )
#   return(data.frame(rep, dem, ind))
# }
# full.data$republican1<-pid.recode(full.data$pid.2016)[,1]
# full.data$democrat1<-pid.recode(full.data$pid.2016)[,2]
# full.data$independent1<-pid.recode(full.data$pid.2016)[,3]
# full.data$republican2<-pid.recode(full.data$pid.2017)[,1]
# full.data$democrat2<-pid.recode(full.data$pid.2017)[,2]
# full.data$independent2<-pid.recode(full.data$pid.2017)[,3]
# full.data$republican3<-pid.recode(full.data$pid.2018)[,1]
# full.data$democrat3<-pid.recode(full.data$pid.2018)[,2]
# full.data$independent3<-pid.recode(full.data$pid.2018)[,3]
# 
# 
# interaction<-function(a, b){
#   return(a*b)
# }
# full.data$republicanXauthoritarianism1<-with(full.data, interaction(republican1, authoritarianism))  
# full.data$democratXauthoritarianism1<-with(full.data, interaction(democrat1, authoritarianism))  
# full.data$independentXauthoritarianism1<-with(full.data, interaction(independent1, authoritarianism))  
# full.data$republicanXauthoritarianism2<-with(full.data, interaction(republican2, authoritarianism))  
# full.data$democratXauthoritarianism2<-with(full.data, interaction(democrat2, authoritarianism))  
# full.data$independentXauthoritarianism2<-with(full.data, interaction(independent2, authoritarianism))  
# full.data$republicanXauthoritarianism3<-with(full.data, interaction(republican3, authoritarianism))  
# full.data$democratXauthoritarianism3<-with(full.data, interaction(democrat3, authoritarianism))  
# full.data$independentXauthoritarianism3<-with(full.data, interaction(independent3, authoritarianism))  
# 
# full.data$party2_1<-car::recode(full.data$pid.2017, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )
# full.data$party2_2<-car::recode(full.data$pid.2018, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )
# 
# full.data$unbound<-rowSums(cbind(full.data$president.congress.2017, 
#                                  full.data$president.courts.2017, 
#                                  full.data$president.media.2017), na.rm=T)
# 
# full.data$egalitarianism<-zero.one(rowSums(cbind(full.data$egal1, 
#                                                  full.data$egal2, 
#                                                  full.data$egal3, full.data$egal4), na.rm=T))
# 
# full.data$party7<-zero.one(full.data$pid.2017)
# full.data$authoritarianismXegalitarianism<-with(full.data, interaction(authoritarianism, egalitarianism))  
# full.data$pidXauthoritarianism<-with(full.data, interaction(party7, authoritarianism))  
# with(full.data,
#      psych::alpha(cbind(ccongress.2018, cfbi.2018, cmedia.2018,
#                         cmilitary.2018, cjustice.2018))
# )
# with(full.data,
#      psych::alpha(cbind(ccongress.2019, cfbi.2019, cmedia.2019,
#                         cmilitary.2019, cjustice.2019))
# )
# 
# 
# full.data$confidence1<-zero.one(with(full.data,
#                                      rowMeans(cbind(ccongress.2018, cfbi.2018, cmedia.2018,
#                                                     cmilitary.2018, cjustice.2018), na.rm=T)
# ))
# 
# full.data$confidence2<-zero.one(with(full.data,
#                                      rowMeans(cbind(ccongress.2019, cfbi.2019, cmedia.2019,
#                                                     cmilitary.2019, cjustice.2019), na.rm=T)
# ))
# 
# 
# minority<-full.data
# 
# load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Voter2016.RData")
# require(msm)
# require(dplyr)
# require(ggplot2)
# library(ggpubr)
# full.data$authoritarianism<-rowMeans(cbind(full.data$auth1, full.data$auth2, full.data$auth3, full.data$auth4), na.rm=T)
# 
# full.data<-subset(full.data, white.2011==1)
# 
# prop.table(table(full.data$approve.2012, full.data$approve.2016), 1)
# prop.table(table(full.data$approve.2016, full.data$approve.2017), 1)
# names(full.data[,grepl("pid", names(full.data))])
# ## PID Recode
# pid.recode<-function(data){
#   rep<-car::recode(data, "1:2=0; 3:5=0; 6:7=1; else=NA" )
#   dem<-car::recode(data, "1:2=1; 3:5=0; 6:7=0; else=NA" )
#   ind<-car::recode(data, "1:2=0; 3:5=1; 6:7=0; else=NA" )
#   return(data.frame(rep, dem, ind))
# }
# full.data$republican1<-pid.recode(full.data$pid.2016)[,1]
# full.data$democrat1<-pid.recode(full.data$pid.2016)[,2]
# full.data$independent1<-pid.recode(full.data$pid.2016)[,3]
# full.data$republican2<-pid.recode(full.data$pid.2017)[,1]
# full.data$democrat2<-pid.recode(full.data$pid.2017)[,2]
# full.data$independent2<-pid.recode(full.data$pid.2017)[,3]
# full.data$republican3<-pid.recode(full.data$pid.2018)[,1]
# full.data$democrat3<-pid.recode(full.data$pid.2018)[,2]
# full.data$independent3<-pid.recode(full.data$pid.2018)[,3]
# 
# 
# interaction<-function(a, b){
#   return(a*b)
# }
# full.data$republicanXauthoritarianism1<-with(full.data, interaction(republican1, authoritarianism))  
# full.data$democratXauthoritarianism1<-with(full.data, interaction(democrat1, authoritarianism))  
# full.data$independentXauthoritarianism1<-with(full.data, interaction(independent1, authoritarianism))  
# full.data$republicanXauthoritarianism2<-with(full.data, interaction(republican2, authoritarianism))  
# full.data$democratXauthoritarianism2<-with(full.data, interaction(democrat2, authoritarianism))  
# full.data$independentXauthoritarianism2<-with(full.data, interaction(independent2, authoritarianism))  
# full.data$republicanXauthoritarianism3<-with(full.data, interaction(republican3, authoritarianism))  
# full.data$democratXauthoritarianism3<-with(full.data, interaction(democrat3, authoritarianism))  
# full.data$independentXauthoritarianism3<-with(full.data, interaction(independent3, authoritarianism))  
# 
# full.data$party2_1<-car::recode(full.data$pid.2017, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )
# full.data$party2_2<-car::recode(full.data$pid.2018, "1:2=1; 3=2; 4=3; 5=4; 6:7=5; else=NA" )
# 
# full.data$unbound<-rowSums(cbind(full.data$president.congress.2017, 
#                                  full.data$president.courts.2017, 
#                                  full.data$president.media.2017), na.rm=T)
# 
# full.data$egalitarianism<-zero.one(rowSums(cbind(full.data$egal1, 
#                                                  full.data$egal2, 
#                                                  full.data$egal3, full.data$egal4), na.rm=T))
# 
# full.data$party7<-zero.one(full.data$pid.2017)
# full.data$authoritarianismXegalitarianism<-with(full.data, interaction(authoritarianism, egalitarianism))  
# full.data$pidXauthoritarianism<-with(full.data, interaction(party7, authoritarianism))  
# with(full.data,
#      psych::alpha(cbind(ccongress.2018, cfbi.2018, cmedia.2018,
#                         cmilitary.2018, cjustice.2018))
# )
# with(full.data,
#      psych::alpha(cbind(ccongress.2019, cfbi.2019, cmedia.2019,
#                         cmilitary.2019, cjustice.2019))
# )
# 
# 
# full.data$confidence1<-zero.one(with(full.data,
#                                      rowMeans(cbind(ccongress.2018, cfbi.2018, cmedia.2018,
#                                                     cmilitary.2018, cjustice.2018), na.rm=T)
# ))
# 
# full.data$confidence2<-zero.one(with(full.data,
#                                      rowMeans(cbind(ccongress.2019, cfbi.2019, cmedia.2019,
#                                                     cmilitary.2019, cjustice.2019), na.rm=T)
# ))
# 
# 
# whites<-full.data
# #######
# a<-as.formula(as.factor(strong.leader.2018)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+jewish.2017+other.2017)
# aa<-as.formula(as.factor(strong.leader.2018)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# require(MASS)
# m1a<-polr(a, whites)
# m1aa<-polr(aa, whites)
# m1b<-polr(a, minority)
# m1bb<-polr(aa, minority)
# p3<-main.effect(m1a)
# p4<-interactive.effect(m1aa)
# p5<-main.effect(m1b)
# p6<-interactive.effect(m1bb)
# ###################################################################################################################
# temp1<-rbind(p3,p4,p5,p6)
# temp1$Question<-"Importance of a Strong Leader"
# temp1$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# ###################################################################################################################
# ###################################################################################################################
# ###################################################################################################################
# # Next Question
# a<-as.formula(as.factor(army.rule.2018)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(as.factor(army.rule.2018)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-polr(a, whites)
# m1aa<-polr(aa, whites)
# m1b<-polr(a, minority)
# m1bb<-polr(aa, minority)
# p3<-main.effect(m1a)
# p4<-interactive.effect(m1aa)
# p5<-main.effect(m1b)
# p6<-interactive.effect(m1bb)
# ###################################################################################################################
# temp2<-rbind(p3,p4,p5,p6)
# temp2$Question<-"Should the Army lead the country?"
# temp2$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# ###################################################################################################################
# ###################################################################################################################
# ##### Satisfaction with Democracy Rule #######
# a<-as.formula(as.factor(satisfaction.2018)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(as.factor(satisfaction.2018)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-polr(a, whites)
# m1aa<-polr(aa, whites)
# m1b<-polr(a, minority)
# m1bb<-polr(aa, minority)
# p3<-main.effect(m1a)
# p4<-interactive.effect(m1aa)
# p5<-main.effect(m1b)
# p6<-interactive.effect(m1bb)
# ###################################################################################################################
# temp3<-rbind(p3,p4,p5,p6)
# temp3$Question<-"Satisfaction with Democracy"
# temp3$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# ###################################################################################################################
# ###################################################################################################################
# ###################################################################################################################
# #####  Close Democracy Questions   #######
# a<-as.formula(as.factor(close.democracy.2018)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# 
# aa<-as.formula(as.factor(close.democracy.2018)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# 
# m1a<-nnet::multinom(a, data=whites)
# m1aa<-nnet::multinom(aa, data=whites)
# m1b<-nnet::multinom(a, data=minority)
# m1bb<-nnet::multinom(aa, data=minority)
# p3<-main.effect2(m1a)
# p4<-interactive.effect2(m1aa)
# p5<-main.effect2(m1b)
# p6<-interactive.effect2(m1bb)
# ###################################################################################################################
# temp4<-rbind(p3,p4,p5,p6)
# temp4$Question<-"Anti-Democratic Preferences"
# temp4$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# 
# ###################################################################################################################
# ###################################################################################################################
# #####  President and Laws   #######
# ### President and Laws #####
# a<-as.formula(as.factor(president.laws.2018)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(as.factor(president.laws.2018)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# 
# m1a<-glm(a, whites, family=binomial("logit"))
# m1aa<-glm(aa, whites, family=binomial("logit"))
# m1b<-glm(a, minority, family=binomial("logit"))
# m1bb<-glm(aa, minority, family=binomial("logit"))
# p3<-main.effect2(m1a)
# p4<-interactive.effect2(m1aa)
# p5<-main.effect2(m1b)
# p6<-interactive.effect2(m1bb)
# ###################################################################################################################
# temp5<-rbind(p3,p4,p5,p6)
# temp5$Question<-"President Should Not be Bound by Laws"
# temp5$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# 
# ### President and Laws #####
# a<-as.formula(free.speech~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(free.speech~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-glm(a, whites, family=binomial("logit"))
# m1aa<-glm(aa, whites, family=binomial("logit"))
# m1b<-glm(a, minority, family=binomial("logit"))
# m1bb<-glm(aa, minority, family=binomial("logit"))
# p3<-main.effect2(m1a)
# p4<-interactive.effect2(m1aa)
# p5<-main.effect2(m1b)
# p6<-interactive.effect2(m1bb)
# ###################################################################################################################
# temp6<-rbind(p3,p4,p5,p6)
# temp6$Question<-"Preference for Free Speech Restrictions"
# temp6$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# 
# ###################################################################################################################
# ############# Flag Burning ###########
# a<-as.formula(as.factor(flag.burn)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(as.factor(flag.burn)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-MASS::polr(a, whites)
# m1aa<-MASS::polr(aa, whites)
# m1b<-MASS::polr(a, minority)
# m1bb<-MASS::polr(aa, minority)
# p3<-main.effect(m1a)
# p4<-interactive.effect(m1aa)
# p5<-main.effect(m1b)
# p6<-interactive.effect(m1bb)
# ###################################################################################################################
# temp7<-rbind(p3,p4,p5,p6)
# temp7$Question<-"Flag Burning"
# temp7$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# 
# ###################################################################################################################
# ###################################################################################################################
# ###################################################################################################################
# #### Kneeling during the National Anthem ####
# a<-as.formula(as.factor(kneeling)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(as.factor(kneeling)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-MASS::polr(a, whites)
# m1aa<-MASS::polr(aa, whites)
# m1b<-MASS::polr(a, minority)
# m1bb<-MASS::polr(aa, minority)
# p3<-main.effect(m1a)
# p4<-interactive.effect(m1aa)
# p5<-main.effect(m1b)
# p6<-interactive.effect(m1bb)
# ###################################################################################################################
# temp8<-rbind(p3,p4,p5,p6)
# temp8$Question<-"Ban Kneeling during National Anthem"
# temp8$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# ###################################################################################################################
# ###################################################################################################################
# ###################################################################################################################
# ###### Unbound Executive #######
# a<-as.formula(as.factor(unbound)~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(as.factor(unbound)~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-MASS::polr(a, whites)
# m1aa<-MASS::polr(aa, whites)
# m1b<-MASS::polr(a, minority)
# m1bb<-MASS::polr(aa, minority)
# p3<-main.effect(m1a)
# p4<-interactive.effect(m1aa)
# p5<-main.effect(m1b)
# p6<-interactive.effect(m1bb)
# ###################################################################################################################
# temp9<-rbind(p3,p4,p5,p6)
# temp9$Question<-"Preference for an Unbound Executive"
# temp9$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# 
# 
# ###### Unbound Executive #######
# 
# a<-as.formula(confidence1~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(confidence1~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-lm(a, whites)
# m1aa<-lm(aa, whites)
# m1b<-lm(a, minority)
# m1bb<-lm(aa, minority)
# p3<-main.effect.linear(m1a)
# p4<-interactive.effect.linear(m1aa)
# p5<-main.effect.linear(m1b)
# p6<-interactive.effect.linear(m1bb)
# ###################################################################################################################
# temp10<-rbind(p3,p4,p5,p6)
# temp10$Question<-"Confidence (2018)"
# temp10$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# 
# 
# 
# a<-as.formula(confidence2~
#                 authoritarianism+
#                 female.2016+age.2016+
#                 college.2017+income.2016+
#                 catholic.2017+jewish.2017+other.2017)
# aa<-as.formula(confidence2~
#                  authoritarianism+republican2+
#                  independent2+republicanXauthoritarianism2+
#                  independentXauthoritarianism2+
#                  female.2016+age.2016+
#                  college.2017+income.2016+
#                  catholic.2017+jewish.2017+other.2017)
# m1a<-lm(a, whites)
# m1aa<-lm(aa, whites)
# m1b<-lm(a, minority)
# m1bb<-lm(aa, minority)
# p3<-main.effect.linear(m1a)
# p4<-interactive.effect.linear(m1aa)
# p5<-main.effect.linear(m1b)
# p6<-interactive.effect.linear(m1bb)
# ###################################################################################################################
# temp11<-rbind(p3,p4,p5,p6)
# temp11$Question<-"Confidence (2019)"
# temp11$Majority<-rep(c("White", "Minority"), each=nrow(temp1)/2)
# 
# 
# 
# 
# dat<-data.frame(rbind(
#   temp1, temp2, temp3,
#   temp4, temp5, temp6,
#   temp7, temp8, temp9,
#   temp10, temp11))
# dat$Item<-paste0(dat$PID,":", dat$Question)
# dat$Authoritarian<-car::recode(dat$authoritarianism, "1='Authoritarian';0='Non-Authoritarian'; else=NA")
# 
# d1<-subset(dat, PID=="Average Effect")
# d2<-subset(dat, PID=="Republican")
# d3<-subset(dat, PID=="Independent")
# d4<-subset(dat, PID=="Democrat")
# 
# gg.func<-function(dat, lab){
#   dat$Question<-factor(dat$Question, levels = rev(c("Satisfaction with Democracy",
#                                                     "Importance of a Strong Leader",
#                                                     "Anti-Democratic Preferences",
#                                                     "President Should Not be Bound by Laws",
#                                                     "Should the Army lead the country?",
#                                                     "Preference for Free Speech Restrictions",
#                                                     "Flag Burning",
#                                                     "Ban Kneeling during National Anthem",
#                                                     "Preference for an Unbound Executive",
#                                                     "Confidence (2018)",
#                                                     "Confidence (2019)"
#   )))
#   return(
#     ggplot(dat, aes(x=as.factor(Question),
#                     y=mean.score, ymax=min.2.5, ymin=max.97.5,
#                     colour=as.factor(Authoritarian))) +
#       facet_wrap(~Majority)+
#       scale_colour_manual(name="Authoritarianism", values=c("black", "gray"))+
#       geom_errorbar( width = 0.1) +
#       geom_point(size = 1, alpha = 0.4) + 
#       theme(panel.background=element_rect(fill="white")) +
#       theme(plot.background=element_rect(fill="white")) +
#       theme_bw()+
#       theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
#       theme(axis.ticks=element_blank())+
#       ggtitle(lab) +
#       theme(plot.title=element_text(hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
#       theme(axis.text.x=element_text(size=10,colour="#535353", angle=90)) +
#       theme(axis.text.y=element_text(size=10, colour="#535353")) +
#       theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
#       theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
#       scale_y_continuous("Probability", limits=c(0,1))+
#       scale_x_discrete("Question")+
#       coord_flip()
#   )
# }
# 
# 
# gg.func(d1, lab="Average Effect of Authoritarianism")
# dev.copy(png,'ch9_A1.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# gg.func(d2, lab="Authoritarianism, Republican Respondents")
# dev.copy(png,'ch9_A2.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# gg.func(d3, lab="Authoritarianism, Independent Respondents")
# dev.copy(png,'ch9_A3.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# gg.func(d4, lab="Authoritarianism, Democratic Respondents")
# dev.copy(png,'ch9_A4.jpg',
#          width = 750, height = 500)
# dev.off()
# 
# 
# 
# 
# 
#### ANES  ####
rm(list=ls())
#Figure working directory#
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
detach("package:dplyr")
require(car)
require(foreign)
require(ggplot2)
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Recoded Data/data.anes.2016.Rdata")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
data<-data.anes.2016
data<-subset(data, mode==1)
data<-subset(data, white==1)

data$authoritarianism<-(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T)-1)/2
data$republican<-recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
data$democrat<-recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
data$independent<-recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$split<-recode(as.numeric(data$split.house),"1=0; 4=0; 2=1; 3=1; else=NA")
data$AuthXRep<-data$authoritarianism*data$republican
data$AuthXInd<-data$authoritarianism*data$independent
data$egalitarianism<-zero.one(rowMeans(with(data, cbind(egal1, egal2, egal3, egal4)),
                                       na.rm=T))


data$authoritarianismXegalitarianism=data$authoritarianism*data$egalitarianism
data$pidXauthoritarianism=data$authoritarianism*data$pid
data$party7=data$pid



 v.s2=c("egalitarianism", "authoritarianism", "party7",
        "authoritarianismXegalitarianism", "pidXauthoritarianism", "female",
        "age", "college", "jewish", "income",
        "catholic", "otherrelig")
outcome="will.majority"
m1aa<-MASS::polr(comp.analysis(outcome, v.s2), data)
will<-comp.plot3(m1aa)

outcome="strong.leader"
m1aa<-MASS::polr(comp.analysis(outcome, v.s2), data)
strong.leader<-comp.plot3(m1aa)

plot.function<-function(dat, lab1, upper){
      p1<-ggplot(dat[[1]], aes(x = cov,
         y = mean.score, ymin=min.2.5,
         ymax=max.97.5, group=Egalitarianism))+
     geom_ribbon(fill="lightgray", alpha=0.5)+
     geom_line(aes(x=cov, y=mean.score, colour=Egalitarianism))+
     theme(text=element_text(size=10),
         axis.text.y=element_text(angle=45))+
    scale_colour_manual(name="AE:", values=c("gray", "black"))+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    labs(title = lab1,
         subtitle="Anti-Egalitarianism"
         )+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="black",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,upper))+
    scale_x_continuous("Authoritarianism")
  p1

  p2<-ggplot(dat[[2]], aes(x = cov,
                           y = mean.score, ymin=min.2.5,
                           ymax=max.97.5, group=Party))+
    geom_ribbon(fill="lightgray", alpha=0.5)+
    geom_line(aes(x=cov, y=mean.score, colour=Party))+
    theme(text=element_text(size=10),
          axis.text.y=element_text(angle=45))+
    scale_colour_manual(name="PID:", values=c("gray", "black"))+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    labs(title = "",
         subtitle="PID"
    )+
    theme(plot.title=element_text(hjust=-.08,vjust=2,colour="black",size=12)) +
    theme(axis.text.x=element_text(size=10,colour="#535353", angle=0)) +
    theme(axis.text.y=element_text(size=10, colour="#535353")) +
    theme(axis.title.y=element_text(size=10,colour="#535353",vjust=1.5)) +
    theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
    scale_y_continuous("Predicted Probability", limits=c(0,upper))+
    scale_x_continuous("Authoritarianism")
  p2

  return(ggarrange(p1,p2,
                   ncol = 2,
                   common.legend = FALSE,
                   legend = "bottom"))


}
plot.function(strong.leader, "Support Strong Leader", 1)
dev.copy(png,'ch9_14.jpg',
          width = 750, height = 500)
dev.off()

plot.function(will, "Will of Majority", 1)
dev.copy(png,'ch9_15.jpg',
         width = 750, height = 500)
dev.off()
