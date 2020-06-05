rm(list=ls())
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Voter2016.RData")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
full.data$authoritarianism<-rowMeans(cbind(full.data$auth1, full.data$auth2, full.data$auth3, full.data$auth4), na.rm=T)
full.data<-subset(full.data, white.2011==1)
require(msm)
require(dplyr)
require(rstanarm)

##Construct 1992-1996 data frame
## There is an error in the coding for 1995. I have respondents, but all the race items are missing
prop.table(table(full.data$approve.2012, full.data$approve.2016), 1)
prop.table(table(full.data$approve.2016, full.data$approve.2017), 1)
##### 
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
# 


## Chapter 10, Figure 1 
 a<-as.formula(as.factor(approve.2017-1)~
                 authoritarianism+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
 b<-as.formula(as.factor(approve.2018-1)~
                 authoritarianism+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
 cc<-as.formula(as.factor(approve.2019-1)~
                 authoritarianism+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
 m1a<-MASS::polr(a, data=full.data)
 m1b<-MASS::polr(b, data=full.data)
 m1c<-MASS::polr(cc, data=full.data)
 
p1<-main.effect3(m1a, "2017")
p2<-main.effect3(m1b, "2018")
p3<-main.effect3(m1c, "2019")

a<-plot.f_ch10(p1, p2, p3)
a

dev.copy(png,'ch10_1.jpg',
         width = 750, height = 500)
dev.off()

a<-as.formula(as.factor(approve.2017)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(approve.2017)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Strongly Disapprove", "Somewhat Disapprove", "Somewhat Approve", "Strongly Approve")
p3<-main.effectV1(m1a, "2017", new)
p4<-interactive.effectV1(m1aa, "2017", new)
a<-plot.f("Presidential Approval", "2017")
a
dev.copy(png,'ch9_1a.jpg',
         width = 750, height = 500)
dev.off()

a<-as.formula(as.factor(approve.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(approve.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Strongly Disapprove", "Somewhat Disapprove", "Somewhat Approve", "Strongly Approve")
p3<-main.effectV1(m1a, "2017", new)
p4<-interactive.effectV1(m1aa, "2017", new)
a<-plot.f("Presidential Approval", "2018")
a
dev.copy(png,'ch9_1b.jpg',
         width = 750, height = 500)
dev.off()

a<-as.formula(as.factor(approve.2019)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(approve.2019)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Strongly Disapprove", "Somewhat Disapprove", "Somewhat Approve", "Strongly Approve")
p3<-main.effectV1(m1a, "2017", new)
p4<-interactive.effectV1(m1aa, "2017", new)
a<-plot.f("Presidential Approval", "2019")
a
dev.copy(png,'ch9_1c.jpg',
         width = 750, height = 500)
dev.off()




a<-as.formula(as.factor(approve.2018)~
                authoritarianism+
                female.2016+age.2016+
                college.2017+jewish.2017+income.2016+
                catholic.2017+other.2017)
aa<-as.formula(as.factor(approve.2018)~
                 authoritarianism+republican2+
                 independent2+republicanXauthoritarianism2+
                 independentXauthoritarianism2+
                 female.2016+age.2016+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(a, full.data)
m1aa<-MASS::polr(aa, full.data)
new=c("Strongly Disapprove", "Somewhat Disapprove", "Somewhat Approve", "Strongly Approve")
p3<-main.effectV1(m1a, "2017", new)
p4<-interactive.effectV1(m1aa, "2017", new)
a<-plot.f("Presidential Approval", "2018")
a
dev.copy(png,'ch9_1b.jpg',
         width = 750, height = 500)
dev.off()

#### Egalitarianism Interaction ######
psych::alpha(with(full.data, cbind(approve.2017, approve.2018, approve.2019)))
full.data$approval<-rowMeans(with(full.data,
                                  cbind(approve.2017, approve.2018, approve.2019))) 
aa<-as.formula(as.factor(approve.2017)~
                 authoritarianism+egalitarianism+
                 authoritarianismXegalitarianism+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1a<-MASS::polr(aa, data=full.data)

aa<-as.formula(as.factor(approve.2018)~
                 authoritarianism+egalitarianism+
                 authoritarianismXegalitarianism+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1b<-MASS::polr(aa, data=full.data)

aa<-as.formula(as.factor(approve.2019)~
                 authoritarianism+egalitarianism+
                 authoritarianismXegalitarianism+
                 college.2017+jewish.2017+income.2016+
                 catholic.2017+other.2017)
m1c<-MASS::polr(aa, data=full.data)
p1<-comp.ch10(m1a)
p2<-comp.ch10(m1b)
p3<-comp.ch10(m1c)






plot.f3.ch10(p1, p2, p3)
dev.copy(png,'ch10_2.jpg',
         width = 750, height = 500)
dev.off()

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
outcome="approve.2017"
m1aa<-MASS::polr(comp.analysis(outcome, v.s2), full.data)
app1<-comp.plot(m1aa)
app1<-plot.cue(m1aa)

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


 data1<-data.frame(id=rep(1:length(full.data[,1]), times=3),
                   state=c(full.data$approve.2017, full.data$approve.2018, full.data$approve.2019),
                   authoritarianism=rep(full.data$authoritarianism, times=3),
                   sex=rep(full.data$female.2018, times=3),
                   college=rep(full.data$college.2017, times=3),
                   age=rep(full.data$age.2018, times=3),
                   jewish=rep(full.data$jewish.2017, times=3),
                   catholic=rep(full.data$catholic.2017, times=3),
                   income=rep(full.data$income.2016, times=3),
                   party=rep(full.data$pid3.2017, times=3),
                   egalitarianism=rep(full.data$egalitarianism, times=3),
                   AExAU=rep(full.data$authoritarianismXegalitarianism, times=3),
                   time=c(rep(c(1:3), each=length(full.data$catholic.2017) )))
 data1$state=as.factor(car::recode(data1$state, "1:2=1; 3:4=2; else=NA"))


data1<-data1%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data1)


twoway3.q<-rbind(c(0,0.10),
                 c(0.1,0)
)
twoway3.q
model<-msm(state~time, covariates=~authoritarianism+sex+college+catholic+age+income+jewish, 
           subject=id, data=data1,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))

pmatrix.msm(model, covariates=list(authoritarianism=1))  
pmatrix.msm(model, covariates=list(authoritarianism=0))  
t<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=0, college=0,income=1,
                                                        sex=1, catholic=0, age=mean(data1$age, na.rm=T)))

o<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=1, college=0,
                                                        sex=1, catholic=0, age=mean(data1$age, na.rm=T)))

options("scipen"=10, "digits"=2)
# Non authoritarians stay in the consistency quadrants
t
# Authoritarians, however, are quite likely to defect in the approve direction!
o



model<-msm(state~time, covariates=~egalitarianism+authoritarianism+
             sex+college+catholic+age+income+jewish, 
           subject=id, data=data1,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))

pmatrix.msm(model, covariates=list(authoritarianism=1))  
pmatrix.msm(model, covariates=list(authoritarianism=0))  
#Low AE, Low AU
t1<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=0, college=0,income=1,egalitarianism=0,
                                                        sex=1, catholic=0, age=mean(data1$age, na.rm=T)))
#Low AE, High AU
t2<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=0, college=0,income=1,egalitarianism=0.85,
                                                         sex=1, catholic=0, age=mean(data1$age, na.rm=T)))
#High AE, Low AU
t3<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=1, college=0,income=1,egalitarianism=0,
                                                         sex=1, catholic=0, age=mean(data1$age, na.rm=T)))
#High AE, High AU
t4<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=1, college=0,income=1,egalitarianism=0.85,
                                                         sex=1, catholic=0, age=mean(data1$age, na.rm=T)))

options("scipen"=10, "digits"=2)
# Non authoritarians stay in the consistency quadrants
t
# Authoritarians, however, are quite likely to defect in the approve direction!
o









tt<-as.formula(I(state-1)~
                 authoritarianism+
                 sex+age+college+jewish+income+
                 catholic+as.factor(time))
data = subset(data1, time==1)

fit1 <- stan_glm(switch ~ dist100, data = wells, 
                 family = binomial(link = "logit"), 
                 prior = t_prior, prior_intercept = t_prior,  
                 cores = 2, seed = 12345)

a<-stan_glm(state~authoritarianism+
               sex+age+college+jewish+income+
               catholic,     
             data=data,
             family=binomial(link="logit"),
             seed = 349,
             cores=3,
             chains=4,
             iter=1000
)
### Need to pull out the subject intercepts ###
shift_draws <- function(draws) {
  sweep(draws[, -1], MARGIN = 1, STATS = draws[, 1], FUN = "+")
}
invlogit <- plogis  # function(x) 1/(1 + exp(-x))
summary_stats <- function(posterior) {
  x <- invlogit(posterior)  # log-odds -> probabilities
  t(apply(x, 2, quantile, probs = c(0.5, 0.5, 0.975))) 
}
r.effects<-as.data.frame(a, regex_pars="(Intercept)")
r.effects<-r.effects[,1:(ncol(r.effects)-1)]
alphas <- shift_draws(as.matrix(r.effects))
partialpool <- summary_stats(alphas)
s.alpha<-apply(alphas, 2, mean)
r.effects<-as.data.frame(a, regex_pars="(Intercept)")
coefficient.vector<-coef(a)[[1]]
#coef<-apply(as.matrix(coefficient.vector), 2, mean)  ### Mean posterior estimates
coef<-coefficient.vector
temp<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.1) 
#beta.sim<-MASS::mvrnorm(1000, coef, vcov(a))  # 1000 x 8
raw.data<-do.call("rbind", replicate(length(temp), data1, simplify = FALSE)) # predict for obsverd
raw.data$authoritarianism<-rep(temp, each=nrow(data1))
raw.data<-raw.data[,names(raw.data) %in% names(coef)] 
#### Constrain means and modes ###
nd <- data.frame(authoritarianism=temp, 
                 sex=1,
                 college=1,
                 age=mean(raw.data$age),
                 jewish=0,
                 catholic=0,
                 income=1)
plot.data<- posterior_predict(a, nd, 500, re.form=~0)
gg<-data.frame(min= c(apply(plogis(plot.data), 2, quantile, 0.025)),
               mean= c(apply(plogis(plot.data), 2, quantile, 0.5)),
               max= c(apply(plogis(plot.data), 2, quantile, 0.9725)),
               authoritarianism=temp
)


library(ggplot2)
plot1<-ggplot(data = gg,
              aes(x = authoritarianism, 
                  y = mean, ymin=min, 
                  ymax=max))+
  geom_ribbon(fill="lightgray", alpha=0.75)+
  geom_ribbon(aes(ymin=min.25, 
                  ymax=max.75), fill="black", alpha=0.05)+
  geom_line(aes(x=authoritarianism, y=mean.score))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  #scale_colour_manual(name="Party", values=c("black", "black", "black"))+
  # Format the grid
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Approval of President Trump. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Predicted Value", limits=c(0,1))+
  scale_x_continuous("Authoritarianism")
plot1

# Authoritarianism predicts approval in obvious ways.
dev.copy(png,'ch9_1.jpg',
         width = 750, height = 500)
dev.off()

twoway3.q<-rbind(c(0,0.10),
                 c(0.1,0)
)
twoway3.q
model<-msm(state~time, covariates=~authoritarianism+sex+college+catholic+age+income+jewish, 
           subject=id, data=data1,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))

pmatrix.msm(model, covariates=list(authoritarianism=1))  ## Transitions at min auth
pmatrix.msm(model, covariates=list(authoritarianism=0))  ## Transitions at min auth
t<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=0, college=0,income=1,
                                                        sex=1, catholic=0, age=mean(data1$age, na.rm=T)))

o<-pmatrix.msm(model, t=1, ci="normal", covariates=list(authoritarianism=1, college=0,
                                                        sex=1, catholic=0, age=mean(data1$age, na.rm=T)))

options("scipen"=10, "digits"=2)
# Non authoritarians stay in the consistency quadrants
t
# Authoritarians, however, are quite likely to defect in the approve direction!
o

###########    Obama #######
model<-msm(state~time, covariates=~authoritarianism+sex+college+catholic+age+income+jewish, 
           subject=id, data=data2,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))

pmatrix.msm(model, covariates=list(authoritarianism=1))  ## Transitions at min auth
pmatrix.msm(model, covariates=list(authoritarianism=0))  ## Transitions at min auth
t<-pmatrix.msm(model, t=1/4, ci="normal", covariates=list(authoritarianism=0, college=0,
                                                          sex=1, catholic=0, age=mean(data2$age, na.rm=T)))

o<-pmatrix.msm(model, t=1/4, ci="normal", covariates=list(authoritarianism=1, college=0,
                                                          sex=1, catholic=0, age=mean(data2$age, na.rm=T)))

options("scipen"=10, "digits"=2)
# Diagonal effects
t
# 
o
