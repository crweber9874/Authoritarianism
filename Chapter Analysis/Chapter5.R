##### Chapter 3:  Authoritarianism and its Relationship to PID among Whites #####
# Relies on pooled data.
# Source file generates effects
### Load Packages and Data ###
rm(list=ls())
set.seed(5)
detach("package:dplyr")
require(car)
require(foreign)
require(emIRT)
require(ggplot2)
load("/Users/chrisweber/Dropbox/working projects/Authoritarianism_BookProject/Data/Recoded Data/pooled.auth.Rdata")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/Functions/Funtion89.R")
#Figure working directory#
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
#data<-subset(data, year!=1990) ## Drop 1990
### Create summary measures
#### ideology and pid
data<-subset(data, year!=1990) ## Drop 1990
data<-subset(data, year!=1994) ## Drop 1990
data$mode<-as.character(data$mode)
data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
data$ideology<-(data$ideology-1)/6
data$racial.resentment<-(rowMeans(cbind(data$rr1, data$rr2, data$rr3, data$rr4), na.rm=T)-1)/4
data$authoritarianism<-(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T)-1)/2
#data$knowledge<-rowMeans(cbind(data$knowledge.defensec, data$knowledge.govc, data$knowledge.governmentc), na.rm=T)
data$split.ticket<-recode(as.character(data$split.house), "'DP-DC'=0; 'DP-RC'=1; 'RP-DC'=1; 'RP-RC'=0; else=NA")
data$efficacy<-rowMeans(cbind(data$efficacy1, data$efficacy2, data$efficacy3, data$efficacy5, data$efficacy6, data$efficacy7), na.rm=T)
data$knowledge<-((rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4)
data$moral.traditionalism<-(rowMeans(cbind(data$moral1, data$moral2, data$moral3, data$moral4), na.rm=T)-1)/4
data$age<-(data$age-17)/80
data$interaction<-data$authoritarianism*data$pid
data$republican<-recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
data$democrat<-recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
data$independent<-recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
#data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
#data$republican<-recode(data$pid*6+1, "1:2=0; 4=0; 5:7=1" )
#data$democrat<-recode(data$pid*6+1, "1:3=1; 4=0; 5:7=0" )
#data$independent<-recode(data$pid*6+1, "1:3=0; 4=1; 4:7=0" )
data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$participation<-rowSums(cbind(data$p1, data$p2, data$p3, data$p4, data$p5), na.rm=T)
data$AuthXRep<-data$authoritarianism*data$republican
data$AuthXInd<-data$authoritarianism*data$independent
psych::alpha(with(data, cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data, cbind(moral1, moral2, moral3, moral4)))

data$egalitarianism<-zero.one(rowMeans(with(data, cbind(egal1, egal2, egal3, egal4)),
                                       na.rm=T))
data$moral.traditionalism<-zero.one(rowMeans(with(data, cbind(moral1, moral2, moral3, moral4)),
                                             na.rm=T))

### Construct Emotion Measures ###
data$rep.neg<-rowSums(cbind(data$fear.rep, data$anger.rep), na.rm=T)
data$dem.neg<-rowSums(cbind(data$fear.dem, data$anger.dem), na.rm=T)
data$rep.pos<-rowSums(cbind(data$proud.rep, data$hope.rep), na.rm=T)
data$dem.pos<-rowSums(cbind(data$proud.dem, data$hope.dem), na.rm=T)
cor.test(data$fear.rep, data$anger.rep)
cor.test(data$fear.dem, data$anger.dem)
cor.test(data$hope.rep, data$proud.rep)
cor.test(data$hope.dem, data$proud.dem)
white.data<-subset(data, white==1)
black.data<-subset(data, black==1)
hispanic.data<-subset(data, hispanic==1)
nonwhite.data<-subset(data, nonwhite==1)
tapply(white.data$authoritarianism[white.data$year==1992], white.data$party3[white.data$year==1992],  mean, na.rm=T)
tapply(white.data$authoritarianism[white.data$year==2016], white.data$party3[white.data$year==2016],  mean, na.rm=T)
tapply(white.data$republican[white.data$year==1992], ifelse(white.data$authoritarianism[white.data$year==1992]>0.56, 1, 0),  mean, na.rm=T)
tapply(white.data$republican[white.data$year==2016], ifelse(white.data$authoritarianism[white.data$year==2016]>0.56, 1, 0),  mean, na.rm=T)
tapply(white.data$democrat[white.data$year==1992], ifelse(white.data$authoritarianism[white.data$year==1992]>0.56, 1, 0),  mean, na.rm=T)
tapply(white.data$democrat[white.data$year==2016], ifelse(white.data$authoritarianism[white.data$year==2016]>0.56, 1, 0),  mean, na.rm=T)
white.data$auth.high<-ifelse(white.data$authoritarianism>quantile(white.data$authoritarianism, 0.75, na.rm=T), 1, NA)
white.data$auth.high<-ifelse(white.data$authoritarianism<quantile(white.data$authoritarianism, 0.25, na.rm=T), 0, white.data$auth.high)

white.data$auth.high<-ifelse(white.data$authoritarianism>=quantile(white.data$authoritarianism, 0.5, na.rm=T), 1, 0)

nonwhite.data$auth.high<-ifelse(nonwhite.data$authoritarianism>=quantile(nonwhite.data$authoritarianism, 0.5, na.rm=T), 1, 0)

#### Some questions
t<-table(white.data$auth.high[white.data$year==1992], white.data$party3[white.data$year==1992])
prop.table(t, 1)

#### Some questions
t<-table(white.data$auth.high[white.data$year==1992], white.data$party3[white.data$year==1992])
prop.table(t, 1)

t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t, 1)

t<-table(white.data$auth.high[white.data$year==1992], white.data$party3[white.data$year==1992])
prop.table(t, 2)

t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t, 2)

t<-table(white.data$party3[white.data$year==1992])
prop.table(t)

t<-table(white.data$party3[white.data$year==2016])
prop.table(t)

t<-table(white.data$auth.high[white.data$year==1992])
prop.table(t)

t<-table(white.data$auth.high[white.data$year==2016])
prop.table(t)




t<-table(nonwhite.data$auth.high[nonwhite.data$year==1992], nonwhite.data$party3[nonwhite.data$year==1992])
prop.table(t, 1)

t<-table(nonwhite.data$auth.high[nonwhite.data$year==2016], nonwhite.data$party3[nonwhite.data$year==2016])
prop.table(t, 1)

t<-table(nonwhite.data$auth.high[nonwhite.data$year==1992], nonwhite.data$party3[nonwhite.data$year==1992])
prop.table(t, 2)

t<-table(nonwhite.data$auth.high[nonwhite.data$year==2016], nonwhite.data$party3[nonwhite.data$year==2016])
prop.table(t, 2)

t<-table(nonwhite.data$auth.high[nonwhite.data$year==2016])
prop.table(t)

t<-table(nonwhite.data$auth.high[nonwhite.data$year==1992])
prop.table(t)



#### Some questions
t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t, 2)



t<-table(white.data$auth.high[white.data$year==1992], white.data$party3[white.data$year==1992])
prop.table(t)
t<-table(white.data$auth.high[white.data$year==2000], white.data$party3[white.data$year==2000])
prop.table(t)
t<-table(white.data$auth.high[white.data$year==2004], white.data$party3[white.data$year==2004])
prop.table(t)
t<-table(white.data$auth.high[white.data$year==2008], white.data$party3[white.data$year==2008])
prop.table(t)
t<-table(white.data$auth.high[white.data$year==2012], white.data$party3[white.data$year==2012])
prop.table(t)
t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t)



t<-table(white.data$auth.high[white.data$year==1992], white.data$party3[white.data$year==1992])
prop.table(t, 2)

t<-table(white.data$auth.high[white.data$year==1992])

t<-table(white.data$auth.high[white.data$year==1992])

t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t, 1)


###### Why? Use Bayes Law ######
# So, what is the probability of identifying as authoritarian given one is a partisan #
t<-table(white.data$auth.high[white.data$year==1992], white.data$party3[white.data$year==1992])
prop.table(t, 2)

t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t, 2)

t<-table(white.data$auth.high[white.data$year==1992], white.data$party3[white.data$year==1992])
prop.table(t, 2)

t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t, 2)



t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t, 2)

t<-table(white.data$auth.high[white.data$year==1992])
prop.table(t)

t<-table(white.data$auth.high[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(t)
t<-table(white.data$auth.high[white.data$year==2016])
prop.table(t)

t<-table(white.data$party3[white.data$year==1992])
prop.table(t)



t<-table(white.data$party3[white.data$year==2016])
prop.table(t)

{
### Authoritarianism maps onto vote choice; this represents the total marginal effect
## Figure 1
##### Figure 1
detach("package:dplyr")
tt<-as.formula(vote~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))


### Predictions for Vote ###########################

spaghetti.plot<-rbind(spaghetti(a),
                      spaghetti(b),
                      spaghetti(c),
                      spaghetti(d),
                      spaghetti(e),
                      spaghetti(f)
)
spaghetti.plot$Authoritarianism<-rep(seq(0,100)/100, times=6)
spaghetti.plot$Year<-rep(c(1992,2000,2004,2008,2012,2016), each=101)
melted<-reshape2::melt(spaghetti.plot, id=952:953)
#melted$Year<-rep(c(1992,2000,2004,2008,2012,2016), each=dim(melted)[1]/6)

#melted<-subset(melted, Year==2016)
plot1<- ggplot(data =melted,
               aes(x = Authoritarianism, 
                   y = value, group=variable))+
  geom_line(colour="lightgray", alpha=0.05)+guides(colour=FALSE) +
  geom_smooth(aes(group=1), se=FALSE, colour="black", size =0.5)+
  facet_wrap(~ Year)+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism and Presidential Vote. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=9,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=9, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability of Republican Vote", limits=c(0,1))+
  scale_x_continuous("Authoritarianism") 
plot1

dev.copy(png,'ch5_1.jpg',
         width = 750, height = 500,)
dev.off()


##### Figure 2 and Probs ###
detach("package:dplyr")
tt<-as.formula(vote~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))

logitmfx(formula=tt, atmean=FALSE, data=subset(white.data, year==2016))

### Predictions for Vote ###########################
cov<-c(0,1)
vote.margin<-function(a){
  temp<-model.prediction(a, 
                         design.matrix.marginal(a, "authoritarianism"), "Marginal",
                         "Binary.Logit",1)
return(temp)
}

plot.data<-rbind(vote.margin(a), vote.margin(b),
                 vote.margin(c), vote.margin(d),
                 vote.margin(e), vote.margin(f)
)  
plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996

plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5))+
  geom_point(size=1.5) +
  geom_line(data = plot.data,
            aes(x = as.factor(Year), 
                y = mean.score, group=1),
            colour="#535353")+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Republican Presidential Vote. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(0,0.7))+
  scale_x_discrete("Year")
plot1

dev.copy(png,'ch5_2.jpg',
         width = 750, height = 500)
dev.off()



### Feelings towards the Political Candidates #####
#### Point estimate towards teh Parties
# Republicans
detach("package:dplyr")
tt<-as.formula(I(feeling.rep/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_1<-lm(tt, data=subset(white.data, year==1992))
b_1<-lm(tt, data=subset(white.data, year==2000))
c_1<-lm(tt, data=subset(white.data, year==2004))
d_1<-lm(tt, data=subset(white.data, year==2008))
e_1<-lm(tt, data=subset(white.data, year==2012))
f_1<-lm(tt, data=subset(white.data, year==2016))

#summary(margins(a_1,  vce="simulation", iterations = 100L))
pred.feeling.noint(a_1)

pred.feeling(a_1,1)
pred.feeling(f_1,1)
# Republican Candidate
tt<-as.formula(I(feeling.repc/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_2<-lm(tt, data=subset(white.data, year==1992))
b_2<-lm(tt, data=subset(white.data, year==2000))
c_2<-lm(tt, data=subset(white.data, year==2004))
d_2<-lm(tt, data=subset(white.data, year==2008))
e_2<-lm(tt, data=subset(white.data, year==2012))
f_2<-lm(tt, data=subset(white.data, year==2016))

pred.feeling(a_2,1)
pred.feeling(f_2,1)

# Democrats
tt<-as.formula(I(feeling.dem/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_3<-lm(tt, data=subset(white.data, year==1992))
b_3<-lm(tt, data=subset(white.data, year==2000))
c_3<-lm(tt, data=subset(white.data, year==2004))
d_3<-lm(tt, data=subset(white.data, year==2008))
e_3<-lm(tt, data=subset(white.data, year==2012))
f_3<-lm(tt, data=subset(white.data, year==2016))

pred.feeling(a_3,1)
pred.feeling(f_3,1)


# Democratic Candidate
tt<-as.formula(I(feeling.demc/100)~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_4<-lm(tt, data=subset(white.data, year==1992))
b_4<-lm(tt, data=subset(white.data, year==2000))
c_4<-lm(tt, data=subset(white.data, year==2004))
d_4<-lm(tt, data=subset(white.data, year==2008))
e_4<-lm(tt, data=subset(white.data, year==2012))
f_4<-lm(tt, data=subset(white.data, year==2016))

pred.feeling(a_4,1)
pred.feeling(f_4,1)

auth.feelingP<-rbind(
  pred.feeling(a_1,1),
  pred.feeling(b_1, 1),
  pred.feeling(c_1, 1),
  pred.feeling(d_1, 1),
  pred.feeling(e_1, 1),
  pred.feeling(f_1, 1),
  pred.feeling(a_3,1),
  pred.feeling(b_3, 1),
  pred.feeling(c_3, 1),
  pred.feeling(d_3, 1),
  pred.feeling(e_3, 1),
  pred.feeling(f_3, 1)
)
Nauth.feelingP<-rbind(
  pred.feeling(a_1,0),
  pred.feeling(b_1, 0),
  pred.feeling(c_1, 0),
  pred.feeling(d_1, 0),
  pred.feeling(e_1, 0),
  pred.feeling(f_1, 0),
  pred.feeling(a_3, 0),
  pred.feeling(b_3, 0),
  pred.feeling(c_3, 0),
  pred.feeling(d_3, 0),
  pred.feeling(e_3, 0),
  pred.feeling(f_3, 0)
)

auth.feelingC<-rbind(
  pred.feeling(a_2,1),
  pred.feeling(b_2, 1),
  pred.feeling(c_2, 1),
  pred.feeling(d_2, 1),
  pred.feeling(e_2, 1),
  pred.feeling(f_2, 1),
  pred.feeling(a_4,1),
  pred.feeling(b_4, 1),
  pred.feeling(c_4, 1),
  pred.feeling(d_4, 1),
  pred.feeling(e_4, 1),
  pred.feeling(f_4, 1)
)
Nauth.feelingC<-rbind(
  pred.feeling(a_2,0),
  pred.feeling(b_2, 0),
  pred.feeling(c_2, 0),
  pred.feeling(d_2, 0),
  pred.feeling(e_2, 0),
  pred.feeling(f_2, 0),
  pred.feeling(a_2, 0),
  pred.feeling(b_4, 0),
  pred.feeling(c_4, 0),
  pred.feeling(d_4, 0),
  pred.feeling(e_4, 0),
  pred.feeling(f_4, 0)
)
plot.label<-function(plot.feeling){
  names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean")
  plot.feeling$year<-rep(c("1992","2000", "2004", "2008", "2012", "2016"), times=2)
  plot.feeling$Party<-rep(c("Republican", "Democrat"), each=6)
  return(plot.feeling)
}
feelingP<-rbind(plot.label(auth.feelingP),
                plot.label(Nauth.feelingP))
feelingP$Group<-rep(c("Authoritarian", "Non-Authoritarian"), each=12)


feelingC<-rbind(plot.label(auth.feelingC),
                plot.label(Nauth.feelingC))
feelingC$Group<-rep(c("Authoritarian", "Non-Authoritarian"), each=12)

plot1<-ggplot(data=feelingP) + 
  geom_bar(aes(x=year, y=mean, fill=Party, group=Party), stat="identity", position=position_dodge())+
  scale_fill_manual(name="Party", values=c("gray", "lightgray"))+
  geom_errorbar(aes(x=year, ymin=min1, ymax=max1, group=Party),  width=.2,                    # Width of the error bars
                position=position_dodge(0.9)) +
  facet_wrap(~Group)+
  ggtitle("Feelings towards parties") +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5))+
  scale_y_continuous("Predicted Score", limits=c(-0.1,1))

plot1
dev.copy(png,'ch5_3.jpg',
         width = 750, height = 500)
dev.off()


plot1<-ggplot(data=feelingC) + 
  geom_bar(aes(x=year, y=mean, fill=Party, group=Party), stat="identity", position=position_dodge())+
  scale_fill_manual(name="Party", values=c("gray", "lightgray"))+
  geom_errorbar(aes(x=year, ymin=min1, ymax=max1, group=Party),  width=.2,                    # Width of the error bars
                position=position_dodge(0.9)) +
  facet_wrap(~Group)+
  ggtitle("Feelings towards candidates") +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5))+
  scale_y_continuous("Predicted Score", limits=c(-0.1,1))
plot1
dev.copy(png,'ch5_4.jpg',
         width = 750, height = 500)
dev.off()



plot.feeling<-rbind(
  pred.feeling.noint(a_1),
  mean(c(as.numeric(pred.feeling.noint(a_1)[5]), as.numeric(pred.feeling.noint(b_1)[5]))),
  pred.feeling.noint(b_1),
  pred.feeling.noint(c_1),
  pred.feeling.noint(d_1),
  pred.feeling.noint(e_1),
  pred.feeling.noint(f_1)
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp1<-plot.feeling
temp1$Question<-"Feelings Toward Republican Party"

plot.feeling<-rbind(
  pred.feeling.noint(a_2),
  mean(c(as.numeric(pred.feeling.noint(a_2)[5]), as.numeric(pred.feeling.noint(b_2)[5]))),
  pred.feeling.noint(b_2),
  pred.feeling.noint(c_2),
  pred.feeling.noint(d_2),
  pred.feeling.noint(e_2),
  pred.feeling.noint(f_2)
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp2<-plot.feeling
temp2$Question<-"Feelings Toward Republican Candidate"

plot.feeling<-rbind(
  pred.feeling.noint(a_3),
  mean(c(as.numeric(pred.feeling.noint(a_3)[5]), as.numeric(pred.feeling.noint(b_3)[5]))),
  pred.feeling.noint(b_3),
  pred.feeling.noint(c_3),
  pred.feeling.noint(d_3),
  pred.feeling.noint(e_3),
  pred.feeling.noint(f_3)
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp3<-plot.feeling
temp3$Question<-"Feelings Toward Democratic Party"

plot.feeling<-rbind(
  pred.feeling.noint(a_4),
  mean(c(as.numeric(pred.feeling.noint(a_4)[5]), as.numeric(pred.feeling.noint(b_4)[5]))),
  pred.feeling.noint(b_4),
  pred.feeling.noint(c_4),
  pred.feeling.noint(d_4),
  pred.feeling.noint(e_4),
  pred.feeling.noint(f_4)
)
plot.feeling$year<-rep(c("1992","1996", "2000", "2004", "2008", "2012", "2016"))
names(plot.feeling)<-c("min1", "min2", "max2", "max1", "mean", "Year")
temp4<-plot.feeling
temp4$Question<-"Feelings Toward Democratic Candidate"

plot.data<-rbind(temp1, temp2, temp3, temp4)

plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean, ymin=min1, 
                  ymax=max1))+
  facet_wrap(~Question)+
  geom_point(position = position_dodge(width = 0.2)) +
  geom_line(data = plot.data,
            aes(x = Year, 
                y = mean, group=1))+
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
  geom_errorbar(aes(ymin=min2, 
                    ymax=max2),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism and Party Affect. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.5,0.5))+
  scale_x_discrete("Year")+    
  geom_hline(yintercept=0, linetype="dashed" )
plot1

dev.copy(png,'ch5_5.jpg',
         width = 750, height = 500)
dev.off()

### Show how authoritarianism maps onto PID
require(nnet)
### (2) Alignment with authoritarianism and PID
#a. Multinomial Logit. Present Figure
tt<-as.formula(party3~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
require(nnet)
a<-multinom(tt, data=subset(white.data, year==1992))
b<-multinom(tt, data=subset(white.data, year==2000))
c<-multinom(tt, data=subset(white.data, year==2004))
d<-multinom(tt, data=subset(white.data, year==2008))
e<-multinom(tt, data=subset(white.data, year==2012))
f<-multinom(tt, data=subset(white.data, year==2016))

###
plot.pid<-rbind(
  cbind(pred.pid.noint(a), year=1992),
  cbind(pred.pid.noint(b), year=2000),
  cbind(pred.pid.noint(c), year=2004),
  cbind(pred.pid.noint(d), year=2008),
  cbind(pred.pid.noint(e), year=2012),
  cbind(pred.pid.noint(f), year=2016)
)

detach("package:car")
detach("package:foreign")
library(tidyverse)
library(readr)
library(ggjoy)
library(ggplot2)

rep.plot<-data.frame(Probability=c(plot.pid$rep.h, plot.pid$rep.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
                     Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))

rep.plot$Year<-factor(rep.plot$Year, levels=c("2016", "2012", "2008", "2004", "2000",  "1992"))

dem.plot<-data.frame(Probability=c(plot.pid$dem.h, plot.pid$dem.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
                     Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))

dem.plot$Year<-factor(dem.plot$Year, levels=c("2016", "2012", "2008", "2004", "2000",  "1992"))

ind.plot<-data.frame(Probability=c(plot.pid$ind.h, plot.pid$ind.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
                     Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))

ind.plot$Year<-factor(dem.plot$Year, levels=c("2016", "2012", "2008", "2004", "2000",  "1992"))

total.plot<-rbind(
  cbind(dem.plot, Group="Democrat"),
  cbind(ind.plot, Group="Independent"),
  cbind(rep.plot, Group="Republican")
  )
  

plot1<-ggplot(total.plot, aes(x=Probability,
                       y=as.factor(Year),
                       fill=Authoritarianism))+
       facet_wrap(~Group, nrow=3)+
       stat_density_ridges(geom="density_ridges", alpha=0.5, 
                           quantile_lines=TRUE, quantiles = 2)+
       xlab('Value') +
       theme_joy() +
       theme(axis.title.y = element_blank())+
       scale_fill_manual(name = "Authoritarianism", 
                         values = c("#D3D3D3", "#686868"))+
  ylab("Year") +
  xlab("Simulated Probability Distribution")+
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.65, hjust=0.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5, hjust=0.5))+
  ggtitle("Party Identification and Authoritarianism. White Respondents") 
plot1     

dev.copy(png,'ch5_6.jpg',
         width = 750, height = 500)
dev.off()

plot1<-ggplot(total.plot, aes(x=Probability,
                              y=as.factor(Year),
                              fill=Group))+
  facet_wrap(~Authoritarianism, nrow=3)+
  stat_density_ridges(geom="density_ridges", alpha=0.3,
                      quantile_lines=TRUE, quantiles = 2)+
  xlab('Value') +
  theme_joy() +
  theme(axis.title.y = element_blank())+
  scale_fill_manual(name = "Group", 
                    values = c("blue", "purple", "red"))+
  ylab("Year") +
  xlab("Simulated Probability Distribution")+
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.65, hjust=0.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5, hjust=0.5))+
  ggtitle("Party Identification and Authoritarianism. White Respondents") 
plot1

# dev.copy(png,'ch5_7.jpg',
#          width = 750, height = 500)
# dev.off()

##### Are authoritarians better sorted than non-authoritarians ######
### Sorted Analysis ###
white.data$auth.binary<-ifelse(white.data$authoritarianism<quantile(white.data$authoritarianism, 0.25, na.rm=T),
                               0, 
                               NA
)
white.data$auth.binary<-ifelse(white.data$authoritarianism>quantile(white.data$authoritarianism, 0.75, na.rm=T),
                               1, 
                               white.data$auth.binary
)
                          
my.table1<-table(white.data$auth.binary[white.data$year==1992], white.data$party3[white.data$year==1992])
my.table2<-table(white.data$auth.binary[white.data$year==2000], white.data$party3[white.data$year==2000])
prop.table(my.table, 2)
my.table3<-table(white.data$auth.binary[white.data$year==2004], white.data$party3[white.data$year==2004])
prop.table(my.table, 2)
my.table4<-table(white.data$auth.binary[white.data$year==2008], white.data$party3[white.data$year==2008])
prop.table(my.table, 2)
prop.table(my.table, 2)
my.table5<-table(white.data$auth.binary[white.data$year==2012], white.data$party3[white.data$year==2012])
prop.table(my.table, 2)
my.table6<-table(white.data$auth.binary[white.data$year==2016], white.data$party3[white.data$year==2016])
prop.table(my.table, 2)

a<-cbind(prop.table(my.table1, 2))
b<-cbind(prop.table(my.table2, 2))
c<-cbind(prop.table(my.table3, 2))
d<-cbind(prop.table(my.table4, 2))
e<-cbind(prop.table(my.table5, 2))
f<-cbind(prop.table(my.table6, 2))
rownames(a)<-rownames(b)<-rownames(c)<-rownames(d)<-rownames(e)<-rownames(f)<-c("Non-Authoritarian", "Authoritarian")
require(xtable)
xtable(rbind(a,b,c, d, e, f))



### What are the medians ####
a<-tapply(white.data$authoritarianism[white.data$year==1992], white.data$party3[white.data$year==1992], quantile, 0.5, na.rm=T)
b<-tapply(white.data$authoritarianism[white.data$year==2000], white.data$party3[white.data$year==2000], quantile, 0.5, na.rm=T)
c<-tapply(white.data$authoritarianism[white.data$year==2004], white.data$party3[white.data$year==2004], quantile, 0.5, na.rm=T)
d<-tapply(white.data$authoritarianism[white.data$year==2008], white.data$party3[white.data$year==2008], quantile, 0.5, na.rm=T)
e<-tapply(white.data$authoritarianism[white.data$year==2012], white.data$party3[white.data$year==2012], quantile, 0.5, na.rm=T)
f<-tapply(white.data$authoritarianism[white.data$year==2016], white.data$party3[white.data$year==2016], quantile, 0.5, na.rm=T)
# 
# median.table<-rbind(a,b,c,d,e,f)
# rownames(median.table)<-c(1992, 2000, 2004, 2008, 2012, 2016)
# require(xtable)
# xtable(median.table)
# 
# white.data$sorted<-NA
# white.data$sorted<-ifelse(white.data$auth.binary==1 & white.data$party3=="Republican", "Authoritarian Republican", white.data$sorted )
# white.data$sorted<-ifelse(white.data$auth.binary==1 & white.data$party3=="Democrat", "Authoritarian Democrat", white.data$sorted )
# white.data$sorted<-ifelse(white.data$auth.binary==0 & white.data$party3=="Republican", "Non-Authoritarian Republican", white.data$sorted )
# white.data$sorted<-ifelse(white.data$auth.binary==0 & white.data$party3=="Democrat", "Non-Authoritarian Democrat", white.data$sorted )
# 
# 
# tt<-as.formula(sorted~
#                  female+age+college+income+
#                  catholic+other+ideology)
# require(nnet)
# a<-multinom(tt, data=subset(white.data, year==1992))
# b<-multinom(tt, data=subset(white.data, year==2000))
# c<-multinom(tt, data=subset(white.data, year==2004))
# d<-multinom(tt, data=subset(white.data, year==2008))
# e<-multinom(tt, data=subset(white.data, year==2012))
# f<-multinom(tt, data=subset(white.data, year==2016))
# 
# ###
# plot.pid<-rbind(
#   cbind(pred.pid.noint(a), year=1992),
#   cbind(pred.pid.noint(b), year=2000),
#   cbind(pred.pid.noint(c), year=2004),
#   cbind(pred.pid.noint(d), year=2008),
#   cbind(pred.pid.noint(e), year=2012),
#   cbind(pred.pid.noint(f), year=2016)
# )
# 
# detach("package:car")
# detach("package:foreign")
# library(tidyverse)
# library(readr)
# library(ggjoy)
# library(ggplot2)
# 
# rep.plot<-data.frame(Probability=c(plot.pid$rep.h, plot.pid$rep.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
#                      Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))
# 
# rep.plot$Year<-factor(rep.plot$Year, levels=c("2016", "2012", "2008", "2004", "2000",  "1992"))
# 
# dem.plot<-data.frame(Probability=c(plot.pid$dem.h, plot.pid$dem.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
#                      Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))
# 
# dem.plot$Year<-factor(dem.plot$Year, levels=c("2016", "2012", "2008", "2004", "2000",  "1992"))
# 
# ind.plot<-data.frame(Probability=c(plot.pid$ind.h, plot.pid$ind.l), Year=as.integer(c(plot.pid$year, plot.pid$year)), 
#                      Authoritarianism=as.character(rep(c("High", "Low"), each=length(plot.pid$rep.h))))
# 
# ind.plot$Year<-factor(dem.plot$Year, levels=c("2016", "2012", "2008", "2004", "2000",  "1992"))
# 
# total.plot<-rbind(
#   cbind(dem.plot, Group="Democrat"),
#   cbind(ind.plot, Group="Independent"),
#   cbind(rep.plot, Group="Republican")
# )
# 
# 
# plot1<-ggplot(total.plot, aes(x=Probability,
#                               y=as.factor(Year),
#                               fill=Authoritarianism))+
#   facet_wrap(~Group, nrow=3)+
#   stat_density_ridges(geom="density_ridges", 
#                       quantile_lines=TRUE, quantiles = 2)+
#   xlab('Value') +
#   theme_joy() +
#   theme(axis.title.y = element_blank())+
#   scale_fill_manual(name = "Authoritarianism", 
#                     values = c("#D3D3D3", "#686868"))+
#   ylab("Year") +
#   xlab("Simulated Probability Distribution")+
#   theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
#   theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
#   theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
#   theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.65, hjust=0.5)) +
#   theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5, hjust=0.5))+
#   ggtitle("Party Identification and Authoritarianism. White Respondents") 
# 
# pdf("ch5_6.pdf", width=8, height=5)
# plot1
# dev.off() 

## Simple model with Direct Effect ##
tt<-as.formula(vote~
                 authoritarianism+republican+independent+
                 female+age+college+
                 income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))

cov<-c(0,1)
vote.margin<-function(a){
  temp<-model.prediction(a, 
                         design.matrix.marginal(a, "authoritarianism", FALSE), "Marginal",
                         "Binary.Logit",1)
  return(temp)
}

plot.data<-rbind(vote.margin(a), vote.margin(b),
                 vote.margin(c), vote.margin(d),
                 vote.margin(e), vote.margin(f)
)  
plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996

plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5))+
  geom_point(size=1.5) +
  geom_line(data = plot.data,
            aes(x = as.factor(Year), 
                y = mean.score, group=1),
            colour="#535353")+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Republican Presidential Vote. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism, Net PID", limits=c(0,0.6))+
  scale_x_discrete("Year")+
  geom_hline(yintercept=0.0, linetype="dashed", color="gray", size=0.8)+
  geom_hline(aes(yintercept=0), linetype="dashed")
plot1

dev.copy(png,'ch5_6.jpg',
         width = 750, height = 500)
dev.off()


###### Conditional Marginal Effect ######
## Simple model with Direct Effect ##
  
tt<-as.formula(vote~
                 authoritarianism+
                 republican+independent+
                 AuthXRep+
                 AuthXInd+
                 female+age+college+income+
                 catholic)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))


i.margin<-function(a){
  temp<-model.prediction(a, 
                         design.matrix.int.marginal.2(a, 
                                                      "authoritarianism", "republican", "AuthXRep", 
                                                      control=c("independent", "AuthXInd"), 1, 
                                                      ordinal=FALSE), "Marginal", "Binary.Logit")
  
  for(i in seq_along(cov)){
    temp<-rbind(temp, model.prediction(a, 
                                       design.matrix.int.marginal.2(a, "authoritarianism", 
                                                                    "republican", "AuthXRep", 
                                                                    control=c("independent", "AuthXInd"), cov[i], 
                                                                    ordinal=FALSE), "Marginal", "Binary.Logit"))
  }
  return(temp[-1,])
}

plot.data<-rbind(i.margin(a), i.margin(b),
                 i.margin(c), i.margin(d),
                 i.margin(e), i.margin(f)
)  
plot.data$PID<-rep(c("Democrat", "Republican"))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$PID=="Democrat"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$PID=="Democrat"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$PID[nrow(plot.data)]<-"Democrat"

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$PID=="Republican"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$PID=="Republican"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$PID[nrow(plot.data)]<-"Republican"

plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5))+
  facet_wrap(~PID)+
  geom_point(size=1.5) +
  geom_line(data = plot.data,
            aes(x = as.factor(Year), 
                y = mean.score),
            colour="#535353", group=1)+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Republican Presidential Vote. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism, Net PID", limits=c(-1,1))+
  scale_x_discrete("Year")+
geom_hline(yintercept=0.0, linetype="dashed", color="gray", size=0.8)
  
plot1

dev.copy(png,'ch5_7.jpg',
         width = 750, height = 500)
dev.off()

}
######### Egalitarainism Analayis ####
range(zero.one(white.data$egalitarianism), na.rm=T)
white.data$egalitarianism<-zero.one(white.data$egalitarianism)
white.data$AuthXEgal<-white.data$egalitarianism*white.data$authoritarianism
tt<-as.formula(as.factor(party3)~
                 authoritarianism+
                 egalitarianism+
                 AuthXEgal+
                 female+age+college+income+
                 jewish+catholic)
a<-multinom(tt, data=subset(white.data, year==1992))
b<-multinom(tt, data=subset(white.data, year==2000))
c<-multinom(tt, data=subset(white.data, year==2004))
d<-multinom(tt, data=subset(white.data, year==2008))
e<-multinom(tt, data=subset(white.data, year==2012))
f<-multinom(tt, data=subset(white.data, year==2016))

temp.f<-function(output){
  egal<-seq(0,1,by=0.2)
  auth<-seq(0,1,by=0.2)
  e.data<-expand.grid(auth=auth, egal=egal)
  e.data$int<-e.data[,1]*e.data[,2]  
  e.data<-as.data.frame(e.data)
  temp<-model.prediction(output, 
                         design.matrix.int.predictive.3(output, names=c("authoritarianism", 
                                                                        "egalitarianism", "AuthXEgal"),
                                                        values=c(0,0,0)), 
                         type="Predictive", model.type="Multinomial.Logit"
  )[[1]]
  for(i in 1:nrow(e.data)){
    temp<-rbind(temp, model.prediction(output, 
                                       design.matrix.int.predictive.3(output, 
                                                                      names=c("authoritarianism", 
                                                                              "egalitarianism", "AuthXEgal"), 
                                                                      values=c(e.data$auth[i], e.data$egal[i], 
                                                                               e.data$int[i]), FALSE),
                                       "Predictive", "Multinomial.Logit")[[1]],
                model.prediction(output, 
                                 design.matrix.int.predictive.3(output, 
                                                                names=c("authoritarianism", 
                                                                        "egalitarianism", "AuthXEgal"), 
                                                                values=c(e.data$auth[i], e.data$egal[i], 
                                                                         e.data$int[i]), FALSE),
                                 "Predictive", "Multinomial.Logit")[[2]],
                model.prediction(output, 
                                 design.matrix.int.predictive.3(output, 
                                                                names=c("authoritarianism", 
                                                                        "egalitarianism", "AuthXEgal"), 
                                                                values=c(e.data$auth[i], e.data$egal[i], 
                                                                         e.data$int[i]), FALSE),
                                 "Predictive", "Multinomial.Logit")[[3]])
    
  }
  temp<-temp[-1,]
  return(temp)
}

aa<-rbind(temp.f(a), temp.f(b), temp.f(c), temp.f(d), temp.f(e), temp.f(f)) 

plot.label<-function(aa){
  egal<-seq(0,1,by=0.2)
  auth<-seq(0,1,by=0.2)
  e.data<-expand.grid(auth=auth, egal=egal)
  names(aa)<-c("min1", "min2", "max2", "max1", "mean")
  aa$year<-rep(c("1992","2000", "2004", "2008", "2012", "2016"), each=length(aa[,1])/6)
  aa$Authoritarianism<-rep(e.data[,1], each=3)
  aa$AE<-rep(e.data[,2], each=3)
  aa$Group<-rep(c("Democrat", "Independent", 
                  "Republican"))
  return(aa)
}
plot<-plot.label(aa)


require(akima)
par(mfcol=c(2,3)) ## create outer margin
data<-subset(plot, Group=="Democrat" & year==1992)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="1992"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Democrat" & year==2000)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2000"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Democrat" & year==2004)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2004"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Democrat" & year==2008)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2008"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Democrat" & year==2012)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2012"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Democrat" & year==2016)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2016"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

par(mfcol=c(2,3)) ## create outer margin
data<-subset(plot, Group=="Republican" & year==1992)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="1992"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Republican" & year==2000)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2000"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Republican" & year==2004)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2004"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Republican" & year==2008)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2008"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Republican" & year==2012)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2012"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

data<-subset(plot, Group=="Republican" & year==2016)
z1<-interp(data$Authoritarianism, data$AE, data$mean)
image(z1,  col=gray((400:0)/400),   xlab="Authoritarianism", ylab="AE", plot.title=title(main="2016"), 
      useRaster=FALSE, cex.axis=0.6, cex.lab=0.6, cex.main=0.3)
contour(z1, add=T, col = "black", nlevels=10, labcex=0.8)

temp.f<-function(output){
  egal<-c(0,1)
  auth<-seq(0,1,by=0.1)
  e.data<-expand.grid(auth=auth, egal=egal)
  e.data$int<-e.data[,1]*e.data[,2]  
  e.data<-as.data.frame(e.data)
  temp<-model.prediction(output, 
                         design.matrix.int.predictive.3(output, names=c("authoritarianism", 
                                                                        "egalitarianism", "AuthXEgal"),
                                                        values=c(0,0,0)), 
                         type="Predictive", model.type="Multinomial.Logit"
  )[[1]]
  for(i in 1:nrow(e.data)){
    temp<-rbind(temp, model.prediction(output, 
                                       design.matrix.int.predictive.3(output, 
                                                                      names=c("authoritarianism", 
                                                                              "egalitarianism", "AuthXEgal"), 
                                                                      values=c(e.data$auth[i], e.data$egal[i], 
                                                                               e.data$int[i]), FALSE),
                                       "Predictive", "Multinomial.Logit")[[1]],
                model.prediction(output, 
                                 design.matrix.int.predictive.3(output, 
                                                                names=c("authoritarianism", 
                                                                        "egalitarianism", "AuthXEgal"), 
                                                                values=c(e.data$auth[i], e.data$egal[i], 
                                                                         e.data$int[i]), FALSE),
                                 "Predictive", "Multinomial.Logit")[[2]],
                model.prediction(output, 
                                 design.matrix.int.predictive.3(output, 
                                                                names=c("authoritarianism", 
                                                                        "egalitarianism", "AuthXEgal"), 
                                                                values=c(e.data$auth[i], e.data$egal[i], 
                                                                         e.data$int[i]), FALSE),
                                 "Predictive", "Multinomial.Logit")[[3]])
    
  }
  temp<-temp[-1,]
  return(temp)
}

aa<-rbind(temp.f(a), temp.f(b), temp.f(c), temp.f(d), temp.f(e), temp.f(f)) 

plot.label<-function(aa){
  egal<-c(0,1)
  auth<-seq(0,1,by=0.1)
  e.data<-expand.grid(auth=auth, egal=egal)
  names(aa)<-c("min1", "min2", "max2", "max1", "mean")
  aa$year<-rep(c("1992","2000", "2004", "2008", "2012", "2016"), each=length(aa[,1])/6)
  aa$Authoritarianism<-rep(e.data[,1], each=3)
  aa$AE<-rep(e.data[,2], each=3)
  aa$AE<-ifelse(aa$AE==1, "Anti-Egalitarian", "Egalitarian")
  aa$Group<-rep(c("Democrat", "Independent", 
                  "Republican"))
  return(aa)
}
plot<-plot.label(aa)
plot<-subset(plot, Group!="Independent")

plot1<-ggplot(data = subset(plot, Group=="Democrat"),
              aes(x = Authoritarianism, 
                  y = mean, ymin=min1, 
                  ymax=max1, group=AE))+
  facet_wrap(~year)+
  geom_line(aes(x=Authoritarianism, y=mean, colour=AE))+
  geom_ribbon(fill="grey79", alpha=0.5)+
  geom_ribbon(aes( ymin=min2, 
                   ymax=max2), fill="grey79", alpha=0.5)+
  scale_colour_manual(name="Anti-Egalitarianism", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Probability of Identifying as Democrat. White Respondents") +
  theme(plot.title=element_text(face="bold", hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=9,colour="#535353", angle=0)) +
  theme(axis.text.y=element_text(size=10, colour="#535353")) +
  theme(axis.title.y=element_text(size=9,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Authoritarianism", limits=c(0,1))+
  geom_hline(aes(yintercept=.5), linetype="dashed", colour="grey")
plot1

dev.copy(png,'ch5_9.jpg',
         width = 750, height = 500)
dev.off()


plot1<-ggplot(data = subset(plot, Group=="Republican"),
              aes(x = Authoritarianism, 
                  y = mean, ymin=min1, 
                  ymax=max1, group=AE))+
  facet_wrap(~year)+
  geom_line(aes(x=Authoritarianism, y=mean, colour=AE))+
  geom_ribbon(fill="grey79", alpha=0.5)+
  geom_ribbon(aes( ymin=min2, 
                   ymax=max2), fill="grey79", alpha=0.5)+
  scale_colour_manual(name="Anti-Egalitarianism", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Probability of Identifying as Republican. White Respondents") +
  theme(plot.title=element_text(face="bold", hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=9,colour="#535353", angle=0)) +
  theme(axis.text.y=element_text(size=10, colour="#535353")) +
  theme(axis.title.y=element_text(size=9,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Authoritarianism", limits=c(0,1))+
  geom_hline(aes(yintercept=.5), linetype="dashed", colour="grey")
plot1

dev.copy(png,'ch5_10.jpg',
         width = 750, height = 500)
dev.off()
#### Over time Analysis ####
# Plot marginal effect alongside the egalitarianism effect, over time #
detach("package:dplyr")
tt<-as.formula(vote~
                 egalitarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))

### Predictions for Vote ###########################
cov<-c(0,1)
vote.margin<-function(a){
  temp<-model.prediction(a, 
                         design.matrix.marginal(a, "egalitarianism"), "Marginal",
                         "Binary.Logit",1)
  return(temp)
}

plot.data<-rbind(vote.margin(a), vote.margin(b),
                 vote.margin(c), vote.margin(d),
                 vote.margin(e), vote.margin(f)
)  
plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Question<-"Voting"
p1<-plot.data

detach("package:dplyr")
tt<-as.formula(I(feeling.rep/100)~
                 egalitarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_1<-lm(tt, data=subset(white.data, year==1992))
b_1<-lm(tt, data=subset(white.data, year==2000))
c_1<-lm(tt, data=subset(white.data, year==2004))
d_1<-lm(tt, data=subset(white.data, year==2008))
e_1<-lm(tt, data=subset(white.data, year==2012))
f_1<-lm(tt, data=subset(white.data, year==2016))

#summary(margins(a_1,  vce="simulation", iterations = 100L))
pred.feeling.noint(a_1)

pred.feeling(a_1,1)
pred.feeling(f_1,1)
# Republican Candidate
tt<-as.formula(I(feeling.repc/100)~
                 egalitarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_2<-lm(tt, data=subset(white.data, year==1992))
b_2<-lm(tt, data=subset(white.data, year==2000))
c_2<-lm(tt, data=subset(white.data, year==2004))
d_2<-lm(tt, data=subset(white.data, year==2008))
e_2<-lm(tt, data=subset(white.data, year==2012))
f_2<-lm(tt, data=subset(white.data, year==2016))

pred.feeling(a_2,1)
pred.feeling(f_2,1)

# Democrats
tt<-as.formula(I(feeling.dem/100)~
                 egalitarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_3<-lm(tt, data=subset(white.data, year==1992))
b_3<-lm(tt, data=subset(white.data, year==2000))
c_3<-lm(tt, data=subset(white.data, year==2004))
d_3<-lm(tt, data=subset(white.data, year==2008))
e_3<-lm(tt, data=subset(white.data, year==2012))
f_3<-lm(tt, data=subset(white.data, year==2016))

pred.feeling(a_3,1)
pred.feeling(f_3,1)


# Democratic Candidate
tt<-as.formula(I(feeling.demc/100)~
                 egalitarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a_4<-lm(tt, data=subset(white.data, year==1992))
b_4<-lm(tt, data=subset(white.data, year==2000))
c_4<-lm(tt, data=subset(white.data, year==2004))
d_4<-lm(tt, data=subset(white.data, year==2008))
e_4<-lm(tt, data=subset(white.data, year==2012))
f_4<-lm(tt, data=subset(white.data, year==2016))



temp.f<-function(output){
  return(model.prediction(output, 
                          design.matrix.marginal(output, "egalitarianism"), 
                          type="Marginal", model.type="OLS"))
}

plot.data<-data.frame(rbind(temp.f(a_1), temp.f(b_1), temp.f(c_1), 
                            temp.f(d_1), temp.f(e_1), temp.f(f_1)))
plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Question<-"Republican Party Feelings"
p2<-plot.data

temp.f<-function(output){
  return(model.prediction(output, 
                          design.matrix.marginal(output, "egalitarianism"), 
                          type="Marginal", model.type="OLS"))
}

plot.data<-data.frame(rbind(temp.f(a_2), temp.f(b_2), temp.f(c_2), 
                            temp.f(d_2), temp.f(e_2), temp.f(f_2)))
plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Question<-"Republican Candidate Feelings"
p3<-plot.data


plot.data<-data.frame(rbind(temp.f(a_3), temp.f(b_3), temp.f(c_3), 
                            temp.f(d_3), temp.f(e_3), temp.f(f_3)))
plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Question<-"Democrat Party Feelings"
p4<-plot.data

plot.data<-data.frame(rbind(temp.f(a_4), temp.f(b_4), temp.f(c_4), 
                            temp.f(d_4), temp.f(e_4), temp.f(f_4)))
plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992],
                                        plot.data$mean.score[plot.data$Year==2000])))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Question<-"Democrat Candidate Feelings"
p5<-plot.data

##### Now, finish with Party Identification ####
tt<-as.formula(party3~
                 egalitarianism+
                 female+age+college+income+
                 jewish+catholic)
a<-multinom(tt, data=subset(white.data, year==1992))
b<-multinom(tt, data=subset(white.data, year==2000))
c<-multinom(tt, data=subset(white.data, year==2004))
d<-multinom(tt, data=subset(white.data, year==2008))
e<-multinom(tt, data=subset(white.data, year==2012))
f<-multinom(tt, data=subset(white.data, year==2016))

temp.f<-function(output){
  return(
    model.prediction(output, 
                     design.matrix.marginal(output, "egalitarianism", FALSE), 
                     type="Marginal", model.type="Multinomial.Logit")
  )
}


aa<-rbind(temp.f(a)[[1]], temp.f(a)[[3]], 
          temp.f(b)[[1]], temp.f(b)[[3]], 
          temp.f(c)[[1]], temp.f(c)[[3]], 
          temp.f(d)[[1]], temp.f(d)[[3]], 
          temp.f(e)[[1]], temp.f(e)[[3]], 
          temp.f(f)[[1]], temp.f(f)[[3]])



aa$Year<-rep(c("1992","2000", "2004", "2008", "2012", "2016"), each=2)
aa$Question<-rep(c("Democrat ID", 
                   "Republican ID"))
plot.data<-aa
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Question=="Democrat ID"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Question=="Democrat ID"])))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Question[nrow(plot.data)]<-"Democrat ID"
p6<-plot.data

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Question=="Republican ID"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Question=="Republican ID"])))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Question[nrow(plot.data)]<-"Republican ID"
p7<-plot.data

plot<-rbind(p1, p2, p3, p4, p5, p6, p7)
names(plot)<-c("min1", "min2", "max2", "max1", "mean", "Year", "Question")

plot$Question <- factor(plot$Question,
                                  levels = c('Democrat ID', 'Republican ID', 
                                             'Democrat Party Feelings', 'Republican Party Feelings',
                                             'Democrat Candidate Feelings', 'Republican Candidate Feelings',
                                             'Voting'))
plot1<-ggplot(data = plot,
              aes(x = factor(Year), 
                  y = mean, ymin=min1, 
                  ymax=max1))+
  facet_wrap(~Question, ncol=2)+
  geom_point(size=1.5) +
  geom_line(data = plot,
            aes(x = as.factor(Year), 
                y = mean, group=1),
            colour="#535353")+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min2, 
                    ymax=max2),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#FAF4F3",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Marginal Effect of Anti-Egalitarianism. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=9,colour="#535353")) +
  theme(axis.text.y=element_text(size=9, colour="#535353")) +
  theme(axis.title.y=element_text(size=9,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=9,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of AE", limits=c(-1.5,1))+
  scale_x_discrete("Year")+
  geom_hline(aes(yintercept=0), linetype="dashed")
plot1
dev.copy(png,'ch5_11.jpg',
         width = 750, height = 500)
dev.off()
#### Disaggregvated Plot #####

temp.f<-function(output){
  e.data<-seq(0,1,by=0.1)
  e.data<-as.data.frame(e.data)
  temp<-model.prediction(output, 
                         design.matrix.predictive(output, "egalitarianism",
                                                  covariate.value=1), 
                         type="Predictive", model.type="Multinomial.Logit"
  )[[1]]
  for(i in 1:nrow(e.data)){
    temp<-rbind(temp, 
                model.prediction(output, 
                                 design.matrix.predictive(output, "egalitarianism",
                                                          covariate.value=e.data[i,]), 
                                 type="Predictive", model.type="Multinomial.Logit"
                )[[1]],
                model.prediction(output, 
                                 design.matrix.predictive(output, "egalitarianism",
                                                          covariate.value=e.data[i,]), 
                                 type="Predictive", model.type="Multinomial.Logit"
                )[[2]],
                model.prediction(output, 
                                 design.matrix.predictive(output, "egalitarianism",
                                                          covariate.value=e.data[i,]), 
                                 type="Predictive", model.type="Multinomial.Logit"
                )[[3]]
    )
  }
  temp<-temp[-1,]
  return(temp)
}
aa<-rbind(temp.f(a), temp.f(b), temp.f(c), temp.f(d), temp.f(e), temp.f(f)) 
plot.label<-function(aa){
  names(aa)<-c("min1", "min2", "max2", "max1", "mean")
  aa$year<-rep(c("1992","2000", "2004", "2008", "2012", "2016"), each=length(aa[,1])/6)
  aa$AE<-rep(seq(0,1, by=0.1), each=3)
  aa$Group<-rep(c("Democrat", "Independent", 
                  "Republican"))
  return(aa)
}
plot<-plot.label(aa)
plot<-subset(plot, Group!="Independent")

plot1<-ggplot(data = plot,
              aes(x = AE, 
                  y = mean, ymin=min1, 
                  ymax=max1, group=Group))+
  facet_wrap(~year)+
  geom_line(aes(x=AE, y=mean, colour=Group))+
  geom_ribbon(fill="grey79", alpha=0.5)+
  geom_ribbon(aes( ymin=min2, 
                   ymax=max2), fill="grey79", alpha=0.5)+
  scale_colour_manual(name="Party Identification", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Party Identification Anti-Egalitarianism. White Respondents") +
  theme(plot.title=element_text(face="bold", hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=9,colour="#535353", angle=0)) +
  theme(axis.text.y=element_text(size=10, colour="#535353")) +
  theme(axis.title.y=element_text(size=9,colour="#535353",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Anti-Egalitarianism", limits=c(0,1))+
  geom_hline(aes(yintercept=.5), linetype="dashed", colour="grey")

plot1
dev.copy(png,'ch5_12.jpg',
         width = 750, height = 500)
dev.off()

###### Vote, Moderated by Education ######
white.data$collegeXauthoritarianism<-white.data$college*white.data$authoritarianism
tt<-as.formula(vote~
                 authoritarianism+college+
                 collegeXauthoritarianism+
                 female+age+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))

marginal.ed<-function(a){
  temp<-rbind(
    model.prediction(a, 
                         design.matrix.int.marginal(a, 
                                                      iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                      value.moderator=1, FALSE), 
                                                      "Marginal", "Binary.Logit"),
    model.prediction(a, 
                     design.matrix.int.marginal(a, 
                                                iv.name="authoritarianism", mod.name="college", int.name="collegeXauthoritarianism", 
                                                value.moderator=0, FALSE), 
                     "Marginal", "Binary.Logit")
  )
  return(temp)
}
    
plot.data<-rbind(marginal.ed(a), marginal.ed(b), marginal.ed(c),
        marginal.ed(d), marginal.ed(e), marginal.ed(f))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
plot.data$Education<-rep(c("College Degree or Greater", "Less than College Degree"))

#### Year Imputation ###
plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="Less than College Degree"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="Less than College Degree"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"Less than College Degree"

plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$Year==1992 & plot.data$Education=="College Degree or Greater"],
                                        plot.data$mean.score[plot.data$Year==2000 & plot.data$Education=="College Degree or Greater"] )))
plot.data$Year[nrow(plot.data)]<-1996
plot.data$Education[nrow(plot.data)]<-"College Degree or Greater"


plot1<-ggplot(data = plot.data,
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5))+
  facet_wrap(~Education)+
  geom_point(position = position_dodge(width = .01)) +
  geom_line(data = plot.data,
            aes(x = as.factor(Year), 
                y = mean.score, group=1))+
  geom_errorbar(width = 0.10, alpha=0.5) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                width = 0.01, size=0.5, alpha=0.7)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Authoritarianism, Education, and Voting") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-.1,1))+
  scale_x_discrete("Year")+
  geom_hline(aes(yintercept=0), linetype="dashed")
plot1
dev.copy(png,'ch5_13.jpg',
         width = 750, height = 500)
dev.off()
 
logit.change<-function(output){
  require(MASS)
  set.seed(123)
  t<-model.matrix(output)
  p1<-cbind(
    1,
    1,  #High Authoritarianism
    1,
    1*1,
    t[,5:dim(t)[2]])
  o1<-cbind(
    1,
    0,  #Low Authoritarianism
    1,
    1*0,
    t[,5:dim(t)[2]])
  p2<-cbind(
    1,
    1,  #High Authoritarianism
    0,
    1*0,
    t[,5:dim(t)[2]])
  o2<-cbind(
    1,
    0,  #High Authoritarianism
    0,
    0*0,
    t[,5:dim(t)[2]])
  beta.sim<-mvrnorm(1000, c(output$coefficients), vcov(output)) ##Draw samples from multivariate distrbution
  h1<-apply(plogis(p1%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  h2<-apply(plogis(o1%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  pp1<-h1-h2
  i1<-apply(plogis(p2%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  i2<-apply(plogis(o2%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  pp2<-i1-i2
  m1<-pp1-pp2
  return(list(marginal=data.frame(min.2.5=quantile(m1, 0.025),
                                  min.25=quantile(m1, 0.25),
                                  max.75= quantile(m1, 0.75),
                                  max.97.5=quantile(m1, 0.975),
                                  mean.score=quantile(m1, 0.5))          
  ))
}

logit.change(a)
logit.change(b)
logit.change(c)
logit.change(d)
logit.change(e)
logit.change(f)


#### Counterfactual Prediction #####

### Show how authoritarianism maps onto PID
require(nnet)
### (2) Alignment with authoritarianism and PID
#a. Multinomial Logit. Present Figure
tt<-as.formula(party3~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
require(nnet)
a<-multinom(tt, data=subset(white.data, year==1992))
model.matrix(a)
dat<- na.omit(subset(white.data, year==2016, select=c("party3", "authoritarianism", "female", "age", "college", "income", "jewish", "catholic", "other")))
y.comp<-dat$party3
dat<- subset(white.data, year==2016, select=c("authoritarianism", "female", "age", "college", "income", "jewish", "catholic", "other"))
out<-predict(a, dat)            
table(y.comp)/sum(table(y.comp))  ### This is the observed 2016 data
table(out)/sum(table(out))  ### This is the observed 2016 data
tt<-as.formula(party3~
                 female+age+college+income+
                 jewish+catholic+other)
a<-multinom(tt, data=subset(white.data, year==2016))
model.matrix(a)
dat<- na.omit(subset(white.data, year==2016, select=c("party3", "authoritarianism", "female", "age", "college", "income", "jewish", "catholic", "other")))
y.comp<-dat$party3
dat<- subset(dat,  select=c( "female", "age", "college", "income", "jewish", "catholic", "other"))
out<-predict(a)            
table(y.comp)/sum(table(y.comp))  ### This is the observed 2016 data
table(out)/sum(table(out))  ### This is the observed 2016 data

##### Or, we could do this for voting, which may be better ###
tt<-as.formula(vote~
                 authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
model.matrix(a)
dat<- na.omit(subset(white.data, year==2008, select=c("vote", "authoritarianism", "female", "age", "college", "income", "jewish", "catholic", "other")))
dat.m<- subset(dat, select=c("authoritarianism", "female", "age", "college", "income", "jewish", "catholic", "other"))
out<-rbinom(length(dat.m[,1]), 1, plogis(predict(a, dat.m)))           
table(dat$vote)/sum(table(dat$vote))  ### This is the observed 2016 data
table(out)/sum(table(out))  ### This is the observed 2016 data

### Simulate each election year #####

### Full model, white and non white ###  
  set.seed(1234)
  tt<-as.formula(vote~authoritarianism+
                   female+age+college+income+
                   jewish+catholic+other)
  a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
  year=c(2000, 2004, 2008, 2012, 2016)
  ### Generate each design matrix 
  design.list<-list()
  for(i in 1:5){
    design.list[[i]]<-subset(white.data, year==year[i],  select=c("vote", "authoritarianism", 
                                                                "female", "age", "college",   
                                                                "income", "jewish", "catholic", 
                                                                "other")) %>% na.omit()
  }
  actual<-NA
  for(i in 1:5){
  actual=rbind(actual, as.numeric(table(design.list[[i]]["vote"])/sum(table(design.list[[i]]["vote"]))))
  }
  actual<-actual[-1,2]
  #####predicated values####
  pred.list<-list() ### List of predictions
  for(i in 1:5){
    pred.list[[i]]<-rbinom(dim(design.list[[i]])[1], 1, plogis(predict(a, design.list[[i]][,2:9])))
  }                         
  ### Retrieve prediction ###
  predictions<-unlist(lapply(pred.list, mean))
  data.frame(actual=actual, prediction=predictions)
                           
                           
  
  
  





