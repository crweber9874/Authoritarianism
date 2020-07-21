####-------------------------------------------------------------------------------------####
# Chapter 7: Spatial and Operational Analysis. 
# This file generates an operational data factor model, which separates economic and fiscal ideology
# It also includes a spatial model showing the ideological perception of the parties.

####-------------------------------------------------------------------------------------####
require(car)
require(ggplot2)
require(rstan)
require(semPlot)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


#####################################################################################################################################
#####################################################################################################################################
#                                           Part I: Alignment, Measurement Stuff                                                                    #
#####################################################################################################################################
#####################################################################################################################################
rm(list=ls())
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
#Figure working directory#
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
### Load measurement models ###
outfile<-"/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Analysis/Measurement Models/Chapter 7 Measurement Model_M1.out"

semPlot::semPaths(outfile, what="paths", "est", style="lisrel", rotation=1,
                  thresholds=FALSE, residuals=FALSE, intercepts=FALSE, 
                  sizeMan2=2, sizeMan=7, sizeLat2=7, sizeLat=6, 
                  label.prop=0.5, label.cex=0.5, title=TRUE, node.width=1,
                  edge.label.cex = 0.5, 
                  nCharNodes=0, label.font=3, label.scale=FALSE)
dev.copy(png,'ch6_F1.jpg',
         width = 750, height = 500)
dev.off()


detach("package:dplyr")
##### Estimate a varying loading model ####
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Recoded Data/pooled.auth.Rdata")
factor.scores<-read.delim2("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/factor.scores.txt", header = FALSE, sep="",
                           stringsAsFactors=FALSE)
factor.scores<-factor.scores[,9:ncol(factor.scores)]
names(factor.scores)<-c("rid2", "f1", "sd1", "f2", "sd2")
factor.scores$rid2<-as.numeric(factor.scores$rid2)
factor.scores$f1<-as.numeric(factor.scores$f1)
factor.scores$f2<-as.numeric(factor.scores$f2)
### Data Merge ###
data<-merge(factor.scores, data, by="rid2")
data$racial.resentment<-(rowMeans(cbind(data$rr1, data$rr2, data$rr3, data$rr4), na.rm=T)-1)/4
data$authoritarianism<-(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T)-1)/2
data$split.ticket<-recode(as.character(data$split.house), "'DP-DC'=0; 'DP-RC'=1; 'RP-DC'=1; 'RP-RC'=0; else=NA")
data$efficacy<-rowMeans(cbind(data$efficacy1, data$efficacy2, data$efficacy3, data$efficacy5, data$efficacy6, data$efficacy7), na.rm=T)
data$knowledge<-((rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4)
data$moral.traditionalism<-(rowMeans(cbind(data$moral1, data$moral2, data$moral3, data$moral4), na.rm=T)-1)/4
data$interest.in.politics<-recode(data$interest.politics, "1=1; 0=0; else=NA")
data$interest.in.elections<-recode(data$interest.elections, "1=0; 2:3=1; else=NA")
data$age<-(data$age-17)/80
data$knowXauthoritarianism<-data$knowledge*data$authoritarianism
data$interestXauthoritarianism<-data$knowledge*data$authoritarianism
data$collegeXauthoritarianism<-data$college*data$authoritarianism
data$mediaXauthoritarianism<-data$media*data$authoritarianism
data$interaction<-data$authoritarianism*data$pid
data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$republican<-recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
data$democrat<-recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
data$independent<-recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )


data$participation<-rowSums(cbind(data$p1, data$p2, data$p3, data$p4, data$p5), na.rm=T)
data$AuthXRep<-data$authoritarianism*data$republican
data$AuthXInd<-data$authoritarianism*data$independent
### Display trends over time ####
tapply(data$f1, list(data$party3, data$year),mean, na.rm=T)

#### Replicate Federico et al ####
year.list<-c(1992, 2000, 2004, 2008, 2012, 2016)
for(i in 1:6){
  year<-year.list[i]
  print(cor.test(data$f1[data$year==year], data$ideology[data$year==year]))
}

for(i in 1:6){
  year<-year.list[i]
  print(cor.test(data$f2[data$year==year], data$ideology[data$year==year]))
}
### How have the parties "shifted" over time ####
# Simple plot showing over time means
# Simple plot showing correlations over time
#
### Data truncation to be consistent with previous chapters #####
data$mode<-as.character(data$mode)
data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
y<-subset(data, white==1)
#### Figure 1: Display the social fiscal means for each party over time
########  Mean Predictions for Ideology Types  ########
# Parties diverge on both dimensions -- Dems leftward swing
a<- as.formula(f1~republican+independent+
                 female+age+college+income+
                 jewish+catholic+other)
m1a<-lm(a, subset(y, year==1992))
m2a<-lm(a, subset(y, year==2000))
m3a<-lm(a, subset(y, year==2004))
m4a<-lm(a, subset(y, year==2008))
m5a<-lm(a, subset(y, year==2012))
m6a<-lm(a, subset(y, year==2016))

a<- as.formula(f2~republican+independent+
                 female+age+college+income+
                 jewish+catholic+other)
m1b<-lm(a, subset(y, year==1992))
m2b<-lm(a, subset(y, year==2000))
m3b<-lm(a, subset(y, year==2004))
m4b<-lm(a, subset(y, year==2008))
m5b<-lm(a, subset(y, year==2012))
m6b<-lm(a, subset(y, year==2016))

data1<-rbind(pred.ols.ideo(m1a)[[1]],
             mean(c(as.numeric(pred.ols.ideo(m1a)[[1]][5]), as.numeric(pred.ols.ideo(m2a)[[1]][5]))),
             pred.ols.ideo(m2a)[[1]],
             pred.ols.ideo(m3a)[[1]],pred.ols.ideo(m4a)[[1]],
             pred.ols.ideo(m5a)[[1]],pred.ols.ideo(m6a)[[1]],
             pred.ols.ideo(m1a)[[2]],
             mean(c(as.numeric(pred.ols.ideo(m1a)[[2]][5]), as.numeric(pred.ols.ideo(m2a)[[2]][5]))),
             pred.ols.ideo(m2a)[[2]],
             pred.ols.ideo(m3a)[[2]],pred.ols.ideo(m4a)[[2]],
             pred.ols.ideo(m5a)[[2]],pred.ols.ideo(m6a)[[2]],
             pred.ols.ideo(m1a)[[3]],
             mean(c(as.numeric(pred.ols.ideo(m1a)[[3]][5]), as.numeric(pred.ols.ideo(m2a)[[3]][5]))),
             pred.ols.ideo(m2a)[[3]],
             pred.ols.ideo(m3a)[[3]],pred.ols.ideo(m4a)[[3]],
             pred.ols.ideo(m5a)[[3]],pred.ols.ideo(m6a)[[3]]
             
)
names(data1)<-c("min1", "min2", "max2", "max1", "mean")
data1$year<-rep(c(1992,1996,2000,2004, 2008, 2012, 2016), times=3)
data1$Party<-rep(c("Republican", "Independent", "Democrat"), each=7)
data1$Ideology<-"Social"

data2<-rbind(pred.ols.ideo(m1b)[[1]],
             mean(c(as.numeric(pred.ols.ideo(m1b)[[1]][5]), as.numeric(pred.ols.ideo(m2b)[[1]][5]))),
             pred.ols.ideo(m2b)[[1]],
             pred.ols.ideo(m3b)[[1]],pred.ols.ideo(m4b)[[1]],
             pred.ols.ideo(m5b)[[1]],pred.ols.ideo(m6b)[[1]],
             pred.ols.ideo(m1b)[[2]],
             mean(c(as.numeric(pred.ols.ideo(m1b)[[2]][5]), as.numeric(pred.ols.ideo(m2b)[[2]][5]))),
             pred.ols.ideo(m2b)[[2]],
             pred.ols.ideo(m3b)[[2]],pred.ols.ideo(m4b)[[2]],
             pred.ols.ideo(m5b)[[2]],pred.ols.ideo(m6b)[[2]],
             pred.ols.ideo(m1b)[[3]],
             mean(c(as.numeric(pred.ols.ideo(m1b)[[3]][5]), as.numeric(pred.ols.ideo(m2b)[[3]][5]))),
             pred.ols.ideo(m2b)[[3]],
             pred.ols.ideo(m3b)[[3]],pred.ols.ideo(m4b)[[3]],
             pred.ols.ideo(m5b)[[3]],pred.ols.ideo(m6b)[[3]]
             
)
names(data2)<-c("min1", "min2", "max2", "max1", "mean")
data2$year<-rep(c(1992,1996, 2000,2004, 2008, 2012, 2016), times=3)
data2$Party<-rep(c("Republican", "Independent", "Democrat"), each=7)
data2$Ideology<-"Fiscal"
data<-rbind(data1, data2)


plot1<-ggplot(data = data,
              aes(x = factor(year), 
                  y = mean, ymin=min1, 
                  ymax=max1, group=Party, colour=Party, linetype=Party))+
  geom_point(aes(x = factor(year), 
                 y = mean, shape=Party), 
             size=2, alpha=0.75) +
  scale_shape(solid=TRUE)+
  geom_line(data = data,
            aes(x = factor(year), 
                y = mean, group=Party))+
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
  geom_errorbar(aes(ymin=min2, 
                    ymax=max2),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  facet_wrap(~Ideology)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  scale_linetype_manual(name="Party", values=c("solid", "solid", "solid"))+
  scale_colour_manual(name="Party", values=c("grey47", "black", "grey79"))+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Fiscal and Social Ideology, by Party. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Latent Conservatism", limits=c(-0.75,0.75))+
  scale_x_discrete("Year")
plot1
subset(data, data$year==1992 & data$Party=="Republican")

subset(data, data$year==1992 & data$Party=="Democrat")

subset(data, data$year==2016 & data$Party=="Republican")

subset(data, data$year==2016 & data$Party=="Democrat")

subset(data, data$year==1992 & data$Party=="Democrat" & data$Ideology=="Social")[5]
subset(data, data$year==2016 & data$Party=="Democrat" & data$Ideology=="Social")[5]
(subset(data, data$year==1992 & data$Party=="Democrat" & data$Ideology=="Social")[5]-
    subset(data, data$year==2016 & data$Party=="Democrat" & data$Ideology=="Social")[5])/
  subset(data, data$year==1992 & data$Party=="Democrat" & data$Ideology=="Social")[5]

plot1


################################################################################################################
################################################################################################################

dev.copy(png,'ch7_2.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################

#### Figure 2: Display the social fiscal means for authoritarianism over time
a<- as.formula(f1~authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other+income:bible)
m1a<-lm(a, subset(y, year==1992))
m2a<-lm(a, subset(y, year==2000))
m3a<-lm(a, subset(y, year==2004))
m4a<-lm(a, subset(y, year==2008))
m5a<-lm(a, subset(y, year==2012))
m6a<-lm(a, subset(y, year==2016))

a<- as.formula(f2~authoritarianism+
                 female+age+college+income+
                 jewish+catholic+other)
m1b<-lm(a, subset(y, year==1992))
m2b<-lm(a, subset(y, year==2000))
m3b<-lm(a, subset(y, year==2004))
m4b<-lm(a, subset(y, year==2008))
m5b<-lm(a, subset(y, year==2012))
m6b<-lm(a, subset(y, year==2016))

data1<-rbind(pred.ols.authoritarianism(m1a)[[1]],
             mean(c(as.numeric(pred.ols.authoritarianism(m1a)[[1]][5]), as.numeric(pred.ols.authoritarianism(m2a)[[1]][5]))),
             pred.ols.authoritarianism(m2a)[[1]],
             pred.ols.authoritarianism(m3a)[[1]],pred.ols.authoritarianism(m4a)[[1]],
             pred.ols.authoritarianism(m5a)[[1]],pred.ols.authoritarianism(m6a)[[1]],
             pred.ols.authoritarianism(m1a)[[2]],
             mean(c(as.numeric(pred.ols.authoritarianism(m1a)[[2]][5]), as.numeric(pred.ols.authoritarianism(m2a)[[2]][5]))),
             pred.ols.authoritarianism(m2a)[[2]],
             pred.ols.authoritarianism(m3a)[[2]],pred.ols.authoritarianism(m4a)[[2]],
             pred.ols.authoritarianism(m5a)[[2]],pred.ols.authoritarianism(m6a)[[2]]
             
)

names(data1)<-c("min1", "min2", "max2", "max1", "mean")
data1$year<-rep(c(1992,1996,2000,2004, 2008, 2012, 2016), times=2)
data1$Authoritarianism<-rep(c("Authoritarian", "Non-Authoritarian"), each=7)
data1$Ideology<-"Social"

data2<-rbind(pred.ols.authoritarianism(m1b)[[1]],
             mean(c(as.numeric(pred.ols.authoritarianism(m1b)[[1]][5]), as.numeric(pred.ols.authoritarianism(m2b)[[1]][5]))),
             pred.ols.authoritarianism(m2b)[[1]],
             pred.ols.authoritarianism(m3b)[[1]],pred.ols.authoritarianism(m4b)[[1]],
             pred.ols.authoritarianism(m5b)[[1]],pred.ols.authoritarianism(m6b)[[1]],
             pred.ols.authoritarianism(m1b)[[2]],
             mean(c(as.numeric(pred.ols.authoritarianism(m1b)[[2]][5]), as.numeric(pred.ols.authoritarianism(m2b)[[2]][5]))),
             pred.ols.authoritarianism(m2b)[[2]],
             pred.ols.authoritarianism(m3b)[[2]],pred.ols.authoritarianism(m4b)[[2]],
             pred.ols.authoritarianism(m5b)[[2]],pred.ols.authoritarianism(m6b)[[2]]
             
)

names(data2)<-c("min1", "min2", "max2", "max1", "mean")
data2$year<-rep(c(1992,1996, 2000,2004, 2008, 2012, 2016), times=2)
data2$Authoritarianism<-rep(c("Authoritarian", "Non-Authoritarian"), each=7)
data2$Ideology<-"Fiscal"
data<-rbind(data1, data2)



plot1<-ggplot(data = data,
              aes(x = factor(year), 
                  y = mean, ymin=min1, 
                  ymax=max1, group=Authoritarianism, colour=Authoritarianism))+
  facet_wrap(~Ideology)+
  geom_point(aes(x = factor(year), 
                 y = mean, shape=Authoritarianism), 
             size=2, alpha=0.75) +
  scale_shape(solid=TRUE)+
  geom_line(data = data,
            aes(x = factor(year), 
                y = mean, group=Authoritarianism))+
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
  geom_errorbar(aes(ymin=min2, 
                    ymax=max2),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  scale_colour_manual(name="Authoritarianism", values=c("black", "grey79"))+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Fiscal and Social Ideology, by Authoritarianism. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Latent Conservatism", limits=c(-1,1))+
  scale_x_discrete("Year")
plot1

################################################################################################################

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_3.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################

# Figure 3: Show the correlations over time.	
### Correlations over time #####
#### Replicate Federico et al ####
year.list<-c(1992, 2000, 2004, 2008, 2012, 2016)
# Social Ideology
a<-matrix(rep(NA, 3), ncol=3)
for(i in 1:6){
  year<-year.list[i]
  temp<-cor.test(y$f1[y$year==year], y$ideology[y$year==year])
  a<-rbind(a, cbind(temp$conf.int[1], temp$conf.int[2], temp$estimate))
}
social.ideology<-data.frame(a[-1,])
names(social.ideology)<-c("min1", "max1", "mean")
social.ideology$year<-c(1992,2000,2004, 2008, 2012, 2016)
social.ideology<-rbind(social.ideology,
                       c(rep(mean(c(social.ideology$mean[social.ideology$year==1992], social.ideology$mean[social.ideology$year==2000])), 3), 1996))
social.ideology$Ideology<-"Social/Self-Placement"

### Fiscal
year.list<-c(1992, 2000, 2004, 2008, 2012, 2016)
a<-matrix(rep(NA, 3), ncol=3)
for(i in 1:6){
  year<-year.list[i]
  temp<-cor.test(y$f2[y$year==year], y$ideology[y$year==year])
  a<-rbind(a, cbind(temp$conf.int[1], temp$conf.int[2], temp$estimate))
}
fiscal.ideology<-data.frame(a[-1,])
names(fiscal.ideology)<-c("min1", "max1", "mean")
fiscal.ideology$year<-c(1992,2000,2004, 2008, 2012, 2016)
fiscal.ideology<-rbind(fiscal.ideology,
                       c(rep(c(mean(fiscal.ideology$mean[fiscal.ideology$year==1992], fiscal.ideology$mean[fiscal.ideology$year==2000])), 3), 1996))
fiscal.ideology$Ideology<-"Fiscal/Self-Placement"


### Combined
year.list<-c(1992, 2000, 2004, 2008, 2012, 2016)
a<-matrix(rep(NA, 3), ncol=3)
for(i in 1:6){
  year<-year.list[i]
  temp<-cor.test(y$f2[y$year==year], y$f1[y$year==year])
  a<-rbind(a, cbind(temp$conf.int[1], temp$conf.int[2], temp$estimate))
}
combined<-data.frame(a[-1,])
names(combined)<-c("min1", "max1", "mean")
combined$year<-c(1992,2000,2004, 2008, 2012, 2016)
combined<-rbind(combined,
                c(rep(c(mean(combined$mean[combined$year==1992], combined$mean[combined$year==2000])), 3), 1996))
combined$Ideology<-"Fiscal/Social"


data<-rbind(fiscal.ideology, social.ideology, combined)


plot1<-ggplot(data = data,
              aes(x = factor(year), 
                  y = mean, ymin=min1, 
                  ymax=max1, group=Ideology, linetype=Ideology, colour=Ideology))+
  geom_point(aes(x = factor(year), 
                 y = mean, shape=Ideology), 
             size=2, alpha=0.75) +
  scale_shape(solid=TRUE)+
  geom_line(data = data,
            aes(x = factor(year), 
                y = mean, group=Ideology))+
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  scale_linetype_manual(name="Ideology", values=c("solid", "solid", "solid"))+
  scale_colour_manual(name="Ideology", values=c("grey47", "black", "grey79"))+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Correlations between Fisal, Social, and Symbolic Ideology. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Correlation Coefficient", limits=c(0,1))+
  scale_x_discrete("Year")
plot1
################################################################################################################
################################################################################################################
# Correlation of ideology types

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_4.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################

################################################################################################################
################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#                                           Media Analysis                                                                     #
#####################################################################################################################################
#####################################################################################################################################

### Media Effects ####

load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Voter2016.RData")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")

require(ggfortify)
full.data$authoritarianism<-rowMeans(cbind(full.data$auth1, full.data$auth2, full.data$auth3, full.data$auth4), na.rm=T)
full.data<-subset(full.data, white.2011==1)
tv.data<-data.frame(with(full.data, cbind(watch.abc , watch.abclate , 
                                          watch.ammorning , 
                                          watch.andersoncooper , watch.cbs , watch.cnn , watch.colbert , 
                                          watch.conan , watch.dailyshow , watch.dateline , 
                                          watch.early , watch.early2 , watch.edshow , watch.ff ,
                                          watch.fortyeight , watch.fox , watch.foxsunday , watch.ftn , 
                                          watch.gma , watch.hannity , watch.hardball , watch.larryo , 
                                          watch.leno , watch.letterman , watch.maddow , watch.msnbc , 
                                          watch.mtp , watch.nbc , watch.nightline , watch.oreilly , 
                                          watch.outfront , watch.pbs , watch.sixtyminutes , 
                                          watch.stateunion , watch.thisweek , watch.today , 
                                          watch.vansustern, authoritarianism, female.2016, age.2016, college.2011, income.2011,
                                          jewish.2011, catholic.2011, other.2011)))

names(tv.data)<-c("ABC.Primetime", "ABC.Late", "American.Morning.CNN",
                  "Anderson.Cooper.CNN", "CBS.Primetime", "CNN.Primetime", "Colbert.CC",
                  "Conan.TBS", "Daily.Show.CC", "Dateline.NBC", "Early.Show.CBS",
                  "Early.Today.NBC", "Ed.Show.MSNBC", "Fox.and.Friends.Fox",
                  "Forty.Eight.Hours.CBS", "Fox.Primetime", "Fox.Sunday",
                  "FTN.CBS", "GMA.ABC","Hannity.Fox", "Hardball.MSNBC", "Lawrence.O.Donnell.MSNBC",
                  "Leno.NBC", "Letterman.CBS", "Maddow.MSNBC", "MSNBC.Primetime",
                  "MTP.NBC", "NBC.Primetime", "Nightline.ABC", "O.Reilly.Factor.Fox", "Out.Front.CNN",
                  "PBS.News.Hour", "Sixty.Minutes.CBS", "State.of.the.Union.CNN", "This.Week.ABC",
                  "Today.Show.NBC", "Great.Van.Sustern.Fox",
                  "Authoritarianism", "Female", "Age", "College", "Income",
                  "Jewish", "Catholic", "Other.Religion")


television.data<-subset(tv.data, select=c(Fox.and.Friends.Fox, 
                                          Fox.Primetime, O.Reilly.Factor.Fox, Great.Van.Sustern.Fox,
                                          Fox.Sunday, Hannity.Fox,ABC.Primetime, ABC.Late,
                                          Forty.Eight.Hours.CBS, CBS.Primetime, Early.Show.CBS, 
                                          Early.Today.NBC, FTN.CBS, GMA.ABC, MTP.NBC, NBC.Primetime, 
                                          Nightline.ABC, PBS.News.Hour, Sixty.Minutes.CBS, This.Week.ABC,
                                          Today.Show.NBC, Ed.Show.MSNBC, American.Morning.CNN,
                                          Anderson.Cooper.CNN, CNN.Primetime, Hardball.MSNBC ,
                                          Lawrence.O.Donnell.MSNBC, Out.Front.CNN,State.of.the.Union.CNN
))
require(ggrepel)
require(smacof)
adj.matrix<-(t(as.matrix(television.data))%*%as.matrix(television.data))
for(i in 1:dim(adj.matrix)[1]){
  adj.matrix[,i]<-adj.matrix[,i]/diag(adj.matrix)[i] 
}
adj.matrix[upper.tri(adj.matrix)]<-adj.matrix[lower.tri(adj.matrix)]
dd<-as.dist(1-adj.matrix)

plot(hclust(dd))
mds.data= mds(delta = dd , ndim = 2 , type = "interval")
plot<-ggplot() + 
  geom_point(data = as.data.frame(mds.data$conf) , 
             mapping = aes(x = D1, y = D2), color = "darkgray", alpha = 0.5)+
  ggrepel::geom_text_repel(data =data.frame(row.names(adj.matrix), 
                                            mds.data$conf), mapping = aes(x=D1, y=D2 , 
                                                                          label = row.names(adj.matrix))  ) + 
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Media Consumption. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  scale_colour_manual(name="Ideology", values=c("grey47", "black", "grey79"))+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Correlations between Fisal, Social, and Symbolic Ideology. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) 
  scale_y_continuous("Dimension 2")+
  scale_x_continuous("Dimension 1")

  
  dev.copy(png,'ch7_media.jpg',
           width = 750, height = 500,)
  dev.off()
  
  
tv.data$Fox.News=rowSums(cbind(tv.data$Fox.and.Friends.Fox, tv.data$Fox.Primetime, tv.data$O.Reilly.Factor.Fox,
                               tv.data$Great.Van.Sustern.Fox, tv.data$Fox.Sunday, tv.data$Hannity.Fox), na.rm=T)
tv.data$Network=rowSums(cbind(tv.data$ABC.Primetime+tv.data$ABC.Late+tv.data$Forty.Eight.Hours.CBS, tv.data$CBS.Primetime, tv.data$Early.Show.CBS,
                              tv.data$Early.Today.NBC, tv.data$FTN.CBS, tv.data$GMA.ABC, tv.data$MTP.NBC, tv.data$NBC.Primetime, tv.data$Nightline.ABC), na.rm=T)
tv.data$Cable=rowSums(cbind(tv.data$Ed.Show.MSNBC, tv.data$American.Morning.CNN, tv.data$Anderson.Cooper.CNN, tv.data$CNN.Primetime, tv.data$Hardball.MSNBC,
                            tv.data$Lawrence.O.Donnell.MSNBC, tv.data$Out.Front.CNN, tv.data$State.of.the.Union.CNN), na.rm=T)

aa<-as.formula(Fox.News~
                 Authoritarianism+
                 Female+Age+College+
                 Income+
                 Jewish+Catholic+
                 Other.Religion)

bb<-as.formula(Network~
                 Authoritarianism+
                 Female+Age+College+
                 Income+
                 Jewish+Catholic+Other.Religion)
cc<-as.formula(Cable~
                 Authoritarianism+
                 Female+Age+College+
                 Income+
                 Jewish+Catholic+Other.Religion)

require(pscl)
require(boot)
a<-zeroinfl(Fox.News~
              Authoritarianism+
              Female+Age+College+
              Income+
              Jewish+Catholic+
              Other.Religion, data=tv.data)
b<-zeroinfl(Network~
              Authoritarianism+
              Female+Age+College+
              Income+
              Jewish+Catholic+
              Other.Religion, data=tv.data)
c<-zeroinfl(Cable~
              Authoritarianism+
              Female+Age+College+
              Income+
              Jewish+Catholic+
              Other.Religion, data=tv.data)

table(tv.data$Network)/sum(table(tv.data$Network))
table(tv.data$Fox.News)/sum(table(tv.data$Fox.News))
table(tv.data$Cable)/sum(table(tv.data$Cable))

### Expected number of Counts, across authoritarianism ####/
count.model<-rbind(predict.zinb(a, "count"),
                   predict.zinb(b, "count"),
                   predict.zinb(c, "count"),
                   predict.zinb(a, "zero"),
                   predict.zinb(b, "zero"),
                   predict.zinb(c, "zero")
                   
)
count.model$model<-rep(c("Number of Programs", "Some versus No Program"), each=3)
count.model$Media<-rep(c("Fox", "Network", "Cable (CNN, MSNBC)"), times=2)
names(count.model)<-c("min1", "min2", "max2", "max1", "mean", "Model", "Media")


plot1<-ggplot(data = count.model,
              aes(x = factor(Media), 
                  y = mean, ymin=min1, ymax=max1))+
  facet_wrap(~Model)+
  geom_point(position = position_dodge(width = 0.01))+
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.01) +
  geom_errorbar(aes(x = factor(Media), 
                    y = mean, ymin=min2, ymax=max2), 
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  coord_flip()+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Marginal Effect of Authoritarianism on Media Consumption") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=8,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=8, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=8,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=8,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-1,1))+
  scale_x_discrete("Media")+
  geom_hline(yintercept=0, linetype="dashed")
plot1

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_6.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################



### Generate the predictions from the model ###

temp1<-predict.zinb2(a,1)
temp2<-predict.zinb2(b,1)
temp3<-predict.zinb2(c,1)

score<-seq(0,1, 0.01)
for(i in 1:length(score)){
  temp1<-rbind(temp1, predict.zinb2(a,score[i]))
  temp2<-rbind(temp2, predict.zinb2(b,score[i]))
  temp3<-rbind(temp3, predict.zinb2(c,score[i]))
}
temp1<-temp1[-1,]
temp1$Media<-"Fox"
temp2<-temp2[-1,]
temp2$Media<-"Network"
temp3<-temp3[-1,]
temp3$Media<-"Cable (CNN, MSNBC)"
plot.data<-data.frame(cbind(rbind(temp1, temp2, temp3), 
                            rep(score, times=3)))

names(plot.data)<-c("min1", "min2", "max2", "max1", "mean", "Media", "Authoritarianism")


plot1<-ggplot(data = plot.data,
              aes(x = Authoritarianism, 
                  y = mean, ymin=min1, 
                  ymax=max1))+
  facet_wrap(~Media)+
  geom_ribbon(aes(x=Authoritarianism, ymin=min2, ymax=max2), fill="grey79", alpha=0.5)+
  geom_line(aes(x=Authoritarianism, y=mean))+
  #scale_linetype_manual(name="Ideology", values=c("solid", "solid"))+
  #scale_colour_manual(name="Ideology", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Media Consumption and Authoritarianism. White Respondents.") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=45)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Number of Programs/Week", limits=c(1,4))+
  scale_x_continuous("Authoritarianism", limits=c(0,1))

plot1

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_7.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################


#####################################################################################################################################
###---------------------------------------------------------------------------------------------------------------# 
###                                          Part II: Panel Analysis
#####################################################################################################################################
#####################################################################################################################################
#Figure 5:

require(nnet)
require(car)
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel2000.RData")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
y<-subset(d2, white.2000==1)
y<-subset(y, select=c("abortion.2000", "fem.2000",
                      "fund.2000",
                      "gay1.2000", "gay2.2000",
                      "insurance.2000", "services.2000", "jobs.2000",
                      "big.business", "unions", "womens.role", 
                      "partial.birth", "parental.consent",
                      "gay.adopt", "gay.military", "guns", "protect.ss", "tax.cuts",
                      "vouchers", "english"))


y$rid<-seq(1:nrow(y))
#write.csv(y, file="/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/mplus_ch7_panel.csv")
### Load measurement models ###
outfile<-"/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Analysis/Measurement Models/Chapter 7 Panel_M3.out"
labels<-c("Feminists", "Gays", "Fundamentalists", "Abortion", "Marriage",
          "Insurance", "Services", "Jobs", "Social", "Fiscal")
semPlot::semPaths(outfile, what="paths", "est", style="lisrel", rotation=2,
                  thresholds=FALSE, residuals=FALSE, intercepts=FALSE,
                  sizeMan2=2, sizeMan=7, sizeLat2=7, sizeLat=6, 
                  label.prop=0.5, label.cex=0.5, title=TRUE, node.width=1,
                  edge.label.cex = 0.5, 
                  nCharNodes=0, label.font=3, label.scale=FALSE, nodeLabels=labels)


dev.copy(png,'ch7_f2.jpg',
         width = 750, height = 500,)
dev.off()

detach("package:dplyr")
factor.scores<-read.delim2("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/factor.scores2.txt", header = FALSE, sep="",
                           stringsAsFactors=FALSE)
factor.scores<-factor.scores[,9:ncol(factor.scores)]
names(factor.scores)<-c("rid", "f1", "sd1", "f2", "sd2")
factor.scores$rid<-as.numeric(factor.scores$rid)
factor.scores$f1<-as.numeric(factor.scores$f1)
factor.scores$f2<-as.numeric(factor.scores$f2)
### Data Merge ###
y2<-subset(d2, white.2000==1)
y2$rid<-seq(1:nrow(y2))
data<-merge(factor.scores, y2, by="rid")
y<-data
y$party1<-recode(as.numeric(y$pid.2000), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$party2<-recode(as.numeric(y$pid.2002), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$party3<-recode(as.numeric(y$pid.2004), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$authoritarianism<-(rowMeans(cbind(y$auth1.2000, y$auth2.2000, y$auth3.2000, y$auth4.2000), na.rm=T)-1)/2
y$f1<-zero.one(y$f1)
y$f2<-zero.one(y$f2)
################################################################################################################
################################################################################################################

### Do authoritarians variably rely on this construct?	

################################################################################################################
################################################################################################################
y$interaction1<-y$authoritarianism*y$f1
y$interaction2<-y$authoritarianism*y$f2

tt<-as.formula(party1~
                 f1+f2+
                 authoritarianism+
                 interaction1+
                 interaction2+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
a<-multinom(tt, data=y)
tt<-as.formula(party2~
                 f1+f2+
                 authoritarianism+
                 interaction1+
                 interaction2+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)

b<-multinom(tt, data=y)
tt<-as.formula(party3~
                 f1+f2+
                 authoritarianism+
                 interaction1+
                 interaction2+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
c<-multinom(tt, data=y)

three.year.plot<-function(year){
  val<-seq(0,1, by=0.01)  
  if(year==2000){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(a)[1,], coef(a)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(a, "fiscal", "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure$PID<-"Democrat"
    
    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(a, "fiscal", "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure2$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=8)
    plot.data$Year<-2000
  } 
  
  if(year==2002){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(b)[1,], coef(b)[2,]), vcov(b)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(b, "fiscal", "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure$PID<-"Democrat"
    
    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(b, "fiscal", "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure2$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=8)
    plot.data$Year<-2002
    
  }
  
  if(year==2004){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(c)[1,], coef(c)[2,]), vcov(c)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(c, "fiscal", "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure$PID<-"Democrat"
    
    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(c, "fiscal", "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure2$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=8)
    plot.data$Year<-2004
    
  }
  return(plot.data)
}

plot.data<-rbind(three.year.plot(2000), three.year.plot(2002), three.year.plot(2004))

plot1<-ggplot(data = subset(plot.data, Group=="Authoritarian"),
              aes(x = ideo, 
                  y = mean, ymin=min1, 
                  ymax=max1,  linetype=Ideology, group=Ideology))+
  facet_wrap(~PID+Year)+
  geom_ribbon(aes(x=ideo, ymin=min2, ymax=max2, group=Ideology), fill="grey79", alpha=0.5)+
  geom_line(aes(x=ideo, y=mean, linetype=Ideology, colour=Ideology))+
  scale_linetype_manual(name="Ideology", values=c("solid", "solid"))+
  scale_colour_manual(name="Ideology", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=0))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Party Identification by Social and Fiscal Ideology. White Authoritarians") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=45)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Ideology", limits=c(0,1))

plot1

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_9.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################




plot1<-ggplot(data = subset(plot.data, Group=="Non-Authoritarian"),
              aes(x = ideo, 
                  y = mean, ymin=min1, 
                  ymax=max1,  linetype=Ideology, group=Ideology))+
  facet_wrap(~PID+Year)+
  geom_ribbon(aes(x=ideo, ymin=min2, ymax=max2, group=Ideology), fill="grey79", alpha=0.5)+
  geom_line(aes(x=ideo, y=mean, linetype=Ideology, colour=Ideology))+
  scale_linetype_manual(name="Ideology", values=c("solid", "solid"))+
  scale_colour_manual(name="Ideology", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Party Identification by Social and Fiscal Ideology. White Non-Authoritarians") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Ideology", limits=c(0,1))

plot1

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_10.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################


#####################################################################################################################################
#####                                       Part II. Spatial Model ####
#####################################################################################################################################
#####################################################################################################################################

rm(list=ls())
require(foreign)
require(mirt)
require(ggplot2)
require(rstan)
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Recoded Data/spatial.Rdata")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
spatial.model$auth<-(rowMeans(cbind(spatial.model$auth.1, spatial.model$auth.2,spatial.model$auth.3, spatial.model$auth.4), na.rm=T)-1)/2
data<-spatial.model
data<-subset(data, year>1999| year==1992)
data$party3<-car::recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$republican<-car::recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
data$democrat<-car::recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
data$independent<-car::recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
cor.test(data$know.interview.pre, data$know.interview.post)
data$knowledge<-(rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4
yeart<-c(1992,2000,2004, 2008, 2012, 2016)  ### One problem is that there is data sparsity in some years
#data$ideology<-(data$ideology-1)/6
#data$ideologyD<-(data$ideologyD-1)/6
#data$ideologyR<-(data$ideologyR-1)/6
#### Difference Measure ####
tapply(data$ideologyR, data$year, mean, na.rm=T)
tapply(data$ideologyCR, data$year, mean, na.rm=T)
tapply(data$ideologyCD, data$year, mean, na.rm=T)
tapply(data$ideologyCR, data$year, mean, na.rm=T)
tapply(data$jobsD, data$year, mean, na.rm=T)
tapply(data$jobsR, data$year, mean, na.rm=T)
tapply(data$jobs, data$year, mean, na.rm=T)
tapply(data$jobsCR, data$year, mean, na.rm=T)
tapply(data$jobsCD, data$year, mean, na.rm=T)

tapply(data$gov.servicesCR, data$year, mean, na.rm=T)
tapply(data$gov.servicesCD, data$year, mean, na.rm=T)
tapply(data$gov.services, data$year, mean, na.rm=T)
#### Basic measurement properties
psych::alpha(cbind(data$ideologyR, data$ideologyCR,
                   data$jobsR, data$jobsCR,
                   data$gov.servicesCR ,data$gov.servicesR)) 
psych::alpha(cbind(data$ideologyD, data$ideologyCD,
                   data$jobsD, data$jobsCD,
                   data$gov.servicesCD ,data$gov.servicesD))
psych::alpha(cbind(data$ideology,
                   data$jobs,
                   data$gov.services))

# # data$ideologyR<-(rowMeans(cbind(data$ideologyR, data$ideologyCR,
#                                 data$jobsR, data$jobsCR,
#                                 data$gov.servicesCR ,data$gov.servicesR), na.rm=T)-1)/6
# 
# # data$ideologyD<-(rowMeans(cbind(data$ideologyD, data$ideologyCD,
#                                  data$jobsD, data$jobsCD,
#                                  data$gov.servicesCD , data$gov.servicesD), na.rm=T)-1)/6
# # data$ideology<-(rowMeans(cbind(data$ideology,
#                                  data$jobs,
#                                  data$gov.services), na.rm=T)-1)/6
# data$ideologyR<-(data$ideologyR-1)/6
# data$ideologyD<-(data$ideologyD-1)/6
# data$ideology<-(data$ideology-1)/6
# 
#
data$ideologyR<-(rowMeans(cbind(data$ideologyR, data$ideologyCR), na.rm=T)-1)/6

data$ideologyD<-(rowMeans(cbind(data$ideologyD, data$ideologyCD) 
                          , na.rm=T)-1)/6
data$ideology<-(data$ideology-1)/6

###Estimate ###
data$d.score<-mean(data$ideologyD, na.rm=T)
data$r.score<-mean(data$ideologyR, na.rm=T)

data$difference<-data$ideologyR-data$ideologyD
data$differenceR1<-data$ideology-data$ideologyR
data$differenceD1<-data$ideology-data$ideologyD
data$proximity<-(data$ideology-data$ideologyD)^2-(data$ideology-data$ideologyR)^2
data$differenceR2<-(data$ideology-data$ideologyR)^2
data$differenceD2<-(data$ideology-data$ideologyD)^2

#   ### Mean for each year ####
#   mean.year1<-tapply(data$ideologyR, data$year, mean, na.rm=T)
#   mean.year2<-tapply(data$ideologyD, data$year, mean, na.rm=T)
#   year<-c(1992, 2000, 2004, 2008, 2012, 2016)
#   data$differenceR2<-NA
#   data$differenceD2<-NA
# for(i in 1:6){
#   data$differenceR2[data$year==year[i]]<-data$ideology[data$year==year[i]]-mean.year1[i]  #Self - mean party year
#   data$differenceD2[data$year==year[i]]<-data$ideology[data$year==year[i]]-mean.year2[i]
# }  
### Data truncation to be consistent with previous chapters #####
data$mode<-as.character(data$mode)
data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
y<-subset(data, white==1)
#  y<-subset(data, !(ideologyD>ideologyR))
## Diagnose common items
for(i in 1:30){
  print(names(y)[i])
  print(table(y[y$year==2016,i]))
}

#### Estimate the Effect of Authoritarianism on Party Placement ###
y$diffR<-y$ideology-y$ideologyR
y$diffD<-y$ideology-y$ideologyD
a<- as.formula(diffD~auth+
                 female+age+college+income+
                 jewish+catholic+other)
m1a<-lm(a, subset(y, year==1992))
m2a<-lm(a, subset(y, year==2000))
m3a<-lm(a, subset(y, year==2004))
m4a<-lm(a, subset(y, year==2008))
m5a<-lm(a, subset(y, year==2012))
m6a<-lm(a, subset(y, year==2016))
a<- as.formula(diffR~auth+
                 female+age+college+
                 income+
                 jewish+catholic+
                 other)
m1b<-lm(a, subset(y, year==1992))
m2b<-lm(a, subset(y, year==2000))
m3b<-lm(a, subset(y, year==2004))
m4b<-lm(a, subset(y, year==2008))
m5b<-lm(a, subset(y, year==2012))
m6b<-lm(a, subset(y, year==2016))


data1<-rbind(pred.ols.authoritarianism(m1a)[[1]],pred.ols.authoritarianism(m2a)[[1]],
             pred.ols.authoritarianism(m3a)[[1]],pred.ols.authoritarianism(m4a)[[1]],
             pred.ols.authoritarianism(m5a)[[1]],pred.ols.authoritarianism(m6a)[[1]],
             pred.ols.authoritarianism(m1a)[[2]],pred.ols.authoritarianism(m2a)[[2]],
             pred.ols.authoritarianism(m3a)[[2]],pred.ols.authoritarianism(m4a)[[2]],
             pred.ols.authoritarianism(m5a)[[2]],pred.ols.authoritarianism(m6a)[[2]]
)
names(data1)<-c("min1", "min2", "max2", "max1", "mean")
data1$year<-rep(c(1992,2000,2004, 2008, 2012, 2016), times=2)   
data1$authoritarianism<-rep(c("High", "Low"), each=6)      

data2<-rbind(pred.ols.authoritarianism(m1b)[[1]],pred.ols.authoritarianism(m2b)[[1]],
             pred.ols.authoritarianism(m3b)[[1]],pred.ols.authoritarianism(m4b)[[1]],
             pred.ols.authoritarianism(m5b)[[1]],pred.ols.authoritarianism(m6b)[[1]],
             pred.ols.authoritarianism(m1b)[[2]],pred.ols.authoritarianism(m2b)[[2]],
             pred.ols.authoritarianism(m3b)[[2]],pred.ols.authoritarianism(m4b)[[2]],
             pred.ols.authoritarianism(m5b)[[2]],pred.ols.authoritarianism(m6b)[[2]]
)
names(data2)<-c("min1", "min2", "max2", "max1", "mean")
data2$year<-rep(c(1992,2000,2004, 2008, 2012, 2016), times=2)   
data2$authoritarianism<-rep(c("High", "Low"), each=6)      

plot1<-ggplot(data = data1,
              aes(x = factor(year), 
                  y = mean, ymin=min1, ymax=max1, 
                  linetype=authoritarianism, colour=authoritarianism))+
  geom_point(position = position_dodge(width = 0.01)) +
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.01) +
  geom_errorbar(aes(x = as.factor(year), 
                    y = mean, ymin=min2, 
                    ymax=max2, linetype=authoritarianism),
                position = position_dodge(width = 0.01), width = 0.01, size=0.7)+
  coord_flip()+
  scale_linetype_manual(name="Authoritarianism", values=c("solid", "solid"))+
  scale_colour_manual(name="Authoritarianism", values=c("black", "grey79"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=0))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Ideological Distance from Democratic Party") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=8,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=8, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=8,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=8,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Distance", limits=c(0,1))+
  scale_x_discrete("Year")+
  geom_hline(yintercept=0)

plot1

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_11.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################

plot2<-ggplot(data = data2,
              aes(x = factor(year), 
                  y = mean, ymin=min1, ymax=max1, 
                  linetype=authoritarianism, colour=authoritarianism))+
  geom_point(position = position_dodge(width = 0.01)) +
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.01) +
  geom_errorbar(aes(x = as.factor(year), 
                    y = mean, ymin=min2, 
                    ymax=max2, linetype=authoritarianism),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  coord_flip()+
  scale_linetype_manual(name="Authoritarianism", values=c("solid", "solid"))+
  scale_colour_manual(name="Authoritarianism", values=c("black", "grey79"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Ideological Distance from Republican Party. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=8,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=8, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=8,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=8,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Distance", limits=c(-0.6,0.2))+
  scale_x_discrete("Year")+
  geom_hline(yintercept=0)
plot2

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_12.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Ideological Polarization ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
y$diff<-y$ideologyR-y$ideologyD
a<- as.formula(diff~auth+
                 female+age+college+income+
                 jewish+catholic+other)
m1a<-lm(a, subset(y, year==1992))
m2a<-lm(a, subset(y, year==2000))
m3a<-lm(a, subset(y, year==2004))
m4a<-lm(a, subset(y, year==2008))
m5a<-lm(a, subset(y, year==2012))
m6a<-lm(a, subset(y, year==2016))

data1<-rbind(pred.ols(m1a, 1000)[[1]],pred.ols(m2a, 1000)[[1]],
             pred.ols(m3a, 1000)[[1]],pred.ols(m4a, 1000)[[1]],
             pred.ols(m5a, 1000)[[1]],pred.ols(m6a, 1000)[[1]],
             pred.ols(m1a, 1000)[[2]],pred.ols(m2a, 1000)[[2]],
             pred.ols(m3a, 1000)[[2]],pred.ols(m4a, 1000)[[2]],
             pred.ols(m5a, 1000)[[2]],pred.ols(m6a, 1000)[[2]]
)
names(data1)<-c("min1", "min2", "max2", "max1", "mean")
data1$year<-rep(c(1992,2000,2004, 2008, 2012, 2016), times=2)   
data1$authoritarianism<-rep(c("High", "Low"), each=6)      


plot1<-ggplot(data = data1,
              aes(x = factor(year), 
                  y = mean, ymin=min1, ymax=max1, 
                  linetype=authoritarianism, colour=authoritarianism))+
  geom_point(position = position_dodge(width = 0.01)) +
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.01) +
  geom_errorbar(aes(x = as.factor(year), 
                    y = mean, ymin=min2, 
                    ymax=max2, linetype=authoritarianism),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  coord_flip()+
  scale_linetype_manual(name="Authoritarianism", values=c("solid", "solid"))+
  scale_colour_manual(name="Authoritarianism", values=c("black", "grey79"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=0))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Perceived Polarization (Republican Conservatism-Democrat Conservatism). White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=8,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=8, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=8,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=8,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Distance", limits=c(0,1))+
  scale_x_discrete("Year")+
  geom_hline(yintercept=0)




### Distance Analysis ###
detach("package:dplyr")
library(dplyr)
library(rsample)
library(rstan)
#> Warning: package 'rsample' was built under R version 3.5.1
#> Warning: package 'tidyr' was built under R version 3.5.1
library(broom)
library(purrr)
set.seed(27)
#https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
require(splines)
y$d1992<-ifelse(y$year==1992, 1, 0)
y$d2000<-ifelse(y$year==2000, 1, 0)
y$d2004<-ifelse(y$year==2004, 1, 0)
y$d2008<-ifelse(y$year==2008, 1, 0)
y$d2012<-ifelse(y$year==2012, 1, 0)
y$d2016<-ifelse(y$year==2016, 1, 0)


################################################################################################################

################################################################################################################
################################################################################################################
#####   General Difference Analysis #####
BOOT<-500
boots<-bootstraps(y, times = BOOT)
### Apply the spline function to each bootstrap sample ####
fit_bootstrap <- function(split) {
  glm(vote~ns(differenceR1, 3)*auth+
        ns(differenceD1, 3)*auth+female+age+
        jewish+catholic+other+income+d2000+d2004+
        d2008+d2012+d2016,
      family="binomial", data=analysis(split))
}
boot_models <- boots %>% 
  mutate(model = map(splits, fit_bootstrap),
         coef_info = map(model, tidy))
boot_coefs <- boot_models %>% 
  unnest(coef_info)
alpha <- .05
boot_coefs %>% 
  group_by(term) %>%
  summarize(low = quantile(estimate, alpha / 2),
            high = quantile(estimate, 1 - alpha / 2))
model_matrix <- boot_models %>%
  mutate(models=map(model, model.matrix)) 
mod.matrix<-model_matrix$models
#### Generate the design matrix, for APR Effects ######
design.matrix<-as.data.frame(expand.grid(differenceD1=seq(-1,1, length=100),
                                         auth=c(1,0)))
design.matrix$female<-1
design.matrix$age<-mean(y$age, na.rm=T)
design.matrix$catholic<-0
design.matrix$jewish<-0
design.matrix$other<-0
design.matrix$income<-0
design.matrix$d2000<-0
design.matrix$d2004<-0
design.matrix$d2008<-0
design.matrix$d2012<-0
design.matrix$d2016<-1
design.matrix$differenceR1<-mean(y$differenceR1, na.rm=T)

my.models<-boot_models$model
plot.data<-data.frame(pr= unlist(lapply(my.models, predict, newdata=design.matrix, type="response")))
plot.data$proximity<-design.matrix[,1]
plot.data$authoritarianism<-design.matrix[,2]
plot.data$id<-rep(c(1:BOOT), each=nrow(design.matrix))
structure.plot<-as.data.frame(
  aggregate(plot.data$pr, by=list(plot.data$authoritarianism, 
                                  plot.data$proximity), quantile, prob=0.025))
names(structure.plot)<-c("authoritarianism", "proximity", "min")
structure.plot$max<-aggregate(plot.data$pr, 
                              by=list(plot.data$authoritarianism, 
                                      plot.data$proximity), 
                              quantile, prob=0.975)[,3]
structure.plot$mean<-aggregate(plot.data$pr, 
                               by=list(plot.data$authoritarianism, 
                                       plot.data$proximity), 
                               quantile, prob=0.5)[,3]

structure.plot$authoritarianism<-as.character(car::recode(structure.plot$authoritarianism, "1='Authoritarian'; 0='Non-Authoritarian'"))

plot1<-structure.plot


### And Republicans ####
design.matrix<-as.data.frame(expand.grid(differenceR1=seq(-1,1, length=100),
                                         auth=c(1,0)))
design.matrix$female<-1
design.matrix$age<-mean(y$age, na.rm=T)
design.matrix$catholic<-0
design.matrix$jewish<-0
design.matrix$other<-0
design.matrix$income<-mean(y$income, na.rm=T)
design.matrix$d2000<-0
design.matrix$d2004<-0
design.matrix$d2008<-0
design.matrix$d2012<-0
design.matrix$d2016<-1
design.matrix$differenceD1<-mean(y$differenceD1, na.rm=T)
my.models<-boot_models$model
plot.data<-data.frame(pr= unlist(lapply(my.models, predict, newdata=design.matrix, type="response")))
plot.data$proximity<-design.matrix[,1]
plot.data$authoritarianism<-design.matrix[,2]
plot.data$id<-rep(c(1:BOOT), each=nrow(design.matrix))
structure.plot<-as.data.frame(
  aggregate(plot.data$pr, by=list(plot.data$authoritarianism, 
                                  plot.data$proximity), quantile, prob=0.025))
names(structure.plot)<-c("authoritarianism", "proximity", "min")
structure.plot$max<-aggregate(plot.data$pr, 
                              by=list(plot.data$authoritarianism, 
                                      plot.data$proximity), 
                              quantile, prob=0.975)[,3]
structure.plot$mean<-aggregate(plot.data$pr, 
                               by=list(plot.data$authoritarianism, 
                                       plot.data$proximity), 
                               quantile, prob=0.5)[,3]

structure.plot$authoritarianism<-as.character(car::recode(structure.plot$authoritarianism, "1='Authoritarian'; 0='Non-Authoritarian'"))

structure.plot$Party<-"Republican Party"
plot1$Party<-"Democratic Party"
structure.plot<-rbind(structure.plot, plot1)

plot2<-ggplot(data = structure.plot,
              aes(x = proximity, 
                  y = mean, ymin=min, 
                  ymax=max,  linetype=Party, group=Party))+
  facet_wrap(~authoritarianism)+
  geom_ribbon(fill="lightgray", alpha=0.5)+
  geom_line(aes(x=proximity, y=mean, linetype=Party, colour=Party))+
  scale_linetype_manual(name="Partisan Evaluation", values=c("solid", "solid"))+
  scale_colour_manual(name="Partisan Evaluation", values=c("black", "grey79"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=0))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Probability of Republican Vote, by Ideological Proximity to Parties. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Proximity (Self Conservatism-Party Conservatism)", limits=c(-1,1))
plot2

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_14x.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################


### Figure 11: Different Measure of Ideology ####
rm(list=ls())
require(foreign)
require(mirt)
require(ggplot2)
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Recoded Data/spatial.Rdata")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")

spatial.model$auth<-(rowMeans(cbind(spatial.model$auth.1, spatial.model$auth.2,spatial.model$auth.3, spatial.model$auth.4), na.rm=T)-1)/2
data<-spatial.model
data<-subset(data, year>1999| year==1992)
data$party3<-car::recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$republican<-car::recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
data$democrat<-car::recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
data$independent<-car::recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
cor.test(data$know.interview.pre, data$know.interview.post)
data$knowledge<-(rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4
yeart<-c(1992,2000,2004, 2008, 2012, 2016)  ### One problem is that there is data sparsity in some years
#### Basic measurement properties
psych::alpha(cbind(data$ideologyR, data$ideologyCR,
                   data$jobsR, data$jobsCR,
                   data$gov.servicesCR ,data$gov.servicesR)) 
psych::alpha(cbind(data$ideologyD, data$ideologyCD,
                   data$jobsD, data$jobsCD,
                   data$gov.servicesCD ,data$gov.servicesD))
psych::alpha(cbind(data$ideology,
                   data$jobs,
                   data$gov.services))

data$ideologyR<-(rowMeans(cbind(
  data$jobsR, data$jobsCR,
  data$gov.servicesCR ,
  data$gov.servicesR), na.rm=T)-1)/6


data$ideologyD<-(rowMeans(cbind(
  data$jobsD, data$jobsCD,
  data$gov.servicesCD , 
  data$gov.servicesD), na.rm=T)-1)/6
data$ideology<-(rowMeans(cbind(
  data$jobs,
  data$gov.services), na.rm=T)-1)/6

data$difference<-data$ideologyR-data$ideologyD
data$differenceR1<-data$ideology-data$ideologyR
data$differenceD1<-data$ideology-data$ideologyD
data$proximity<-(data$ideology-data$ideologyD)^2-(data$ideology-data$ideologyR)^2
data$differenceR2<-(data$ideology-data$ideologyR)^2
data$differenceD2<-(data$ideology-data$ideologyD)^2

data$mode<-as.character(data$mode)
data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
y<-subset(data, white==1)
#  y<-subset(data, !(ideologyD>ideologyR))
## Diagnose common items
for(i in 1:30){
  print(names(y)[i])
  print(table(y[y$year==2016,i]))
}

#### Estimate the Effect of Authoritarianism on Party Placement ###
y$diffR<-y$ideology-y$ideologyR
y$diffD<-y$ideology-y$ideologyD

### Distance Analysis ###
detach("package:dplyr")
library(dplyr)
library(rsample)
#> Warning: package 'rsample' was built under R version 3.5.1
#> Warning: package 'tidyr' was built under R version 3.5.1
library(broom)
library(purrr)
set.seed(27)
#https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
require(splines)
y$d1992<-ifelse(y$year==1992, 1, 0)
y$d2000<-ifelse(y$year==2000, 1, 0)
y$d2004<-ifelse(y$year==2004, 1, 0)
y$d2008<-ifelse(y$year==2008, 1, 0)
y$d2012<-ifelse(y$year==2012, 1, 0)
y$d2016<-ifelse(y$year==2016, 1, 0)

BOOT<-500
boots<-bootstraps(y, times = BOOT)
### Apply the spline function to each bootstrap sample ####
fit_bootstrap <- function(split) {
  glm(vote~ns(differenceR1, 3)*auth+
        ns(differenceD1, 3)*auth+female+age+
        jewish+catholic+other+income+d2000+d2004+
        d2008+d2012+d2016,
      family="binomial", data=analysis(split))
}
boot_models <- boots %>% 
  mutate(model = map(splits, fit_bootstrap),
         coef_info = map(model, tidy))
boot_coefs <- boot_models %>% 
  unnest(coef_info)
alpha <- .05
boot_coefs %>% 
  group_by(term) %>%
  summarize(low = quantile(estimate, alpha / 2),
            high = quantile(estimate, 1 - alpha / 2))
model_matrix <- boot_models %>%
  mutate(models=map(model, model.matrix)) 
mod.matrix<-model_matrix$models
#### Generate the design matrix, for APR Effects ######
design.matrix<-as.data.frame(expand.grid(differenceD1=seq(-1,1, length=100),
                                         auth=c(1,0)))
design.matrix$female<-1
design.matrix$age<-mean(y$age, na.rm=T)
design.matrix$catholic<-0
design.matrix$jewish<-0
design.matrix$other<-0
design.matrix$income<-0
design.matrix$d2000<-0
design.matrix$d2004<-0
design.matrix$d2008<-0
design.matrix$d2012<-0
design.matrix$d2016<-1
design.matrix$differenceR1<-mean(y$differenceR1, na.rm=T)

my.models<-boot_models$model
plot.data<-data.frame(pr= unlist(lapply(my.models, predict, newdata=design.matrix, type="response")))
plot.data$proximity<-design.matrix[,1]
plot.data$authoritarianism<-design.matrix[,2]
plot.data$id<-rep(c(1:BOOT), each=nrow(design.matrix))
structure.plot<-as.data.frame(
  aggregate(plot.data$pr, by=list(plot.data$authoritarianism, 
                                  plot.data$proximity), quantile, prob=0.025))
names(structure.plot)<-c("authoritarianism", "proximity", "min")
structure.plot$max<-aggregate(plot.data$pr, 
                              by=list(plot.data$authoritarianism, 
                                      plot.data$proximity), 
                              quantile, prob=0.975)[,3]
structure.plot$mean<-aggregate(plot.data$pr, 
                               by=list(plot.data$authoritarianism, 
                                       plot.data$proximity), 
                               quantile, prob=0.5)[,3]

structure.plot$authoritarianism<-as.character(car::recode(structure.plot$authoritarianism, "1='Authoritarian'; 0='Non-Authoritarian'"))

plot1<-structure.plot


### And Republicans ####
design.matrix<-as.data.frame(expand.grid(differenceR1=seq(-1,1, length=100),
                                         auth=c(1,0)))
design.matrix$female<-1
design.matrix$age<-mean(y$age, na.rm=T)
design.matrix$catholic<-0
design.matrix$jewish<-0
design.matrix$other<-0
design.matrix$income<-mean(y$income, na.rm=T)
design.matrix$d2000<-0
design.matrix$d2004<-0
design.matrix$d2008<-0
design.matrix$d2012<-0
design.matrix$d2016<-1
design.matrix$differenceD1<-mean(y$differenceD1, na.rm=T)
my.models<-boot_models$model
plot.data<-data.frame(pr= unlist(lapply(my.models, predict, newdata=design.matrix, type="response")))
plot.data$proximity<-design.matrix[,1]
plot.data$authoritarianism<-design.matrix[,2]
plot.data$id<-rep(c(1:BOOT), each=nrow(design.matrix))
structure.plot<-as.data.frame(
  aggregate(plot.data$pr, by=list(plot.data$authoritarianism, 
                                  plot.data$proximity), quantile, prob=0.025))
names(structure.plot)<-c("authoritarianism", "proximity", "min")
structure.plot$max<-aggregate(plot.data$pr, 
                              by=list(plot.data$authoritarianism, 
                                      plot.data$proximity), 
                              quantile, prob=0.975)[,3]
structure.plot$mean<-aggregate(plot.data$pr, 
                               by=list(plot.data$authoritarianism, 
                                       plot.data$proximity), 
                               quantile, prob=0.5)[,3]

structure.plot$authoritarianism<-as.character(car::recode(structure.plot$authoritarianism, "1='Authoritarian'; 0='Non-Authoritarian'"))

structure.plot$Party<-"Republican Party"
plot1$Party<-"Democratic Party"
structure.plot<-rbind(structure.plot, plot1)








plot2<-ggplot(data = structure.plot,
              
              aes(x = proximity, 
                  y = mean, ymin=min, 
                  ymax=max,  linetype=Party, group=Party))+
  facet_wrap(~authoritarianism)+
  geom_ribbon(fill="lightgray", alpha=0.5)+
  geom_line(aes(x=proximity, y=mean, linetype=Party, colour=Party))+
  scale_linetype_manual(name="Partisan Evaluation", values=c("solid", "solid"))+
  scale_colour_manual(name="Partisan Evaluation", values=c("black", "grey79"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Probability of Republican Vote, by Ideological Proximity to Parties. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Proximity (Self Conservatism-Party Conservatism)", limits=c(-1,1))
plot2
################################################################################################################
################################################################################################################

dev.copy(png,'ch7_15.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################

### Finally, show the impact of polarization ####
rm(list=ls())
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")

source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Recoded Data/pooled.auth.Rdata")
data<-subset(data, year>1999| year==1992)
data$party3<-car::recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
data$knowledge<-(rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4
yeart<-c(1992,2000,2004, 2008, 2012, 2016)  ### One problem is that there is data sparsity in some years
data$auth<-(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T)-1)/2
#data$ideology<-(data$ideology-1)/6
#data$ideologyD<-(data$ideologyD-1)/6
#data$ideologyR<-(data$ideologyR-1)/6
#### Difference Measure ####
tapply(data$ideologyR, data$year, mean, na.rm=T)
tapply(data$ideologyCR, data$year, mean, na.rm=T)
tapply(data$ideologyCD, data$year, mean, na.rm=T)
tapply(data$ideologyCR, data$year, mean, na.rm=T)

tapply(data$jobsD, data$year, mean, na.rm=T)
tapply(data$jobsR, data$year, mean, na.rm=T)
tapply(data$jobs, data$year, mean, na.rm=T)
tapply(data$jobsCR, data$year, mean, na.rm=T)
tapply(data$jobsCD, data$year, mean, na.rm=T)

tapply(data$gov.servicesCR, data$year, mean, na.rm=T)
tapply(data$gov.servicesCD, data$year, mean, na.rm=T)
tapply(data$gov.services, data$year, mean, na.rm=T)

data$d1992<-ifelse(data$year==1992, 1, 0)
data$d2000<-ifelse(data$year==2000, 1, 0)
data$d2004<-ifelse(data$year==2004, 1, 0)
data$d2008<-ifelse(data$year==2008, 1, 0)
data$d2012<-ifelse(data$year==2012, 1, 0)
data$d2016<-ifelse(data$year==2016, 1, 0)


#### Basic measurement properties
psych::alpha(cbind(data$ideologyR, data$ideologyCR,
                   data$jobsR, data$jobsCR,
                   data$gov.servicesCR ,data$gov.servicesR)) 
psych::alpha(cbind(data$ideologyD, data$ideologyCD,
                   data$jobsD, data$jobsCD,
                   data$gov.servicesCD ,data$gov.servicesD))
psych::alpha(cbind(data$ideology,
                   data$jobs,
                   data$gov.services))

# data$ideologyR<-(rowMeans(cbind(data$ideologyR, data$ideologyCR,
#                                 data$jobsR, data$jobsCR,
#                                 data$gov.servicesCR ,data$gov.servicesR), na.rm=T)-1)/6
#
# data$ideologyD<-(rowMeans(cbind(data$ideologyD, data$ideologyCD,
#                                 data$jobsD, data$jobsCD,
#                                 data$gov.servicesCD , data$gov.servicesD), na.rm=T)-1)/6
# data$ideology<-(rowMeans(cbind(data$ideology,
#                                data$jobs,
#                                data$gov.services), na.rm=T)-1)/6

data$ideologyR<-(rowMeans(cbind(data$ideologyR, data$ideologyCR), na.rm=T)-1)/6

data$ideologyD<-(rowMeans(cbind(data$ideologyD, data$ideologyCD) 
                          , na.rm=T)-1)/6
data$ideology<-(data$ideology-1)/6

#data$ideologyR<-(data$ideologyR-1)/6
#data$ideologyD<-(data$ideologyD-1)/6
#data$ideology<-(data$ideology-1)/6

data$difference<-data$ideologyR-data$ideologyD
data$differenceR1<-data$ideology-data$ideologyR
data$differenceD1<-data$ideology-data$ideologyD
data$proximity<-(data$ideologyR-data$ideologyD)^2-(data$ideology-data$ideologyR)^2
data$differenceR2<-(data$ideology-data$ideologyR)^2
data$differenceD2<-(data$ideology-data$ideologyD)^2
data$knowledge<-(rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm=T)-1)/4
#   ### Mean for each year ####
#   mean.year1<-tapply(data$ideologyR, data$year, mean, na.rm=T)
#   mean.year2<-tapply(data$ideologyD, data$year, mean, na.rm=T)
#   year<-c(1992, 2000, 2004, 2008, 2012, 2016)
#   data$differenceR2<-NA
#   data$differenceD2<-NA
# for(i in 1:6){
#   data$differenceR2[data$year==year[i]]<-data$ideology[data$year==year[i]]-mean.year1[i]  #Self - mean party year
#   data$differenceD2[data$year==year[i]]<-data$ideology[data$year==year[i]]-mean.year2[i]
# }  
### Data truncation to be consistent with previous chapters #####
data$mode<-as.character(data$mode)
data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
y<-subset(data, white==1)
y<-y
# difference Rep Ideology - Dem Ideology
table(y$difference<0)/sum(table(y$difference<0)) ### 11% have negative scores
y$difference[y$difference<0]<-NA
y$interaction1<-y$auth*y$difference
y$interaction2<-y$auth*y$knowledge 
## Interest doesnt work -- very few not at all or very
m1<- as.formula(vote~auth+difference+interaction1+
                  interaction2+knowledge+
                  female+age+college+income+
                  jewish+catholic+other+d2000+d2004+
                  d2008+d2012+d2016)
a<-glm(m1, data=y, family=binomial("logit"))

mean(data$difference>0, na.rm=T)
mean(data$difference==0, na.rm=T)


### Predictions for Vote ###########################

### Marginal Effect of Authoritarianism Across Levels of Polarization
m.effect<-c(NA, NA, NA, NA, NA)
names(m.effect)<-c(names(logit.difference(a, 0)[[1]]))  
for(i in seq(0,1, 0.1)){
  m.effect<-rbind(m.effect, logit.difference(a, i)[[1]])
}
plot.vote<-m.effect[-1,]
plot.vote$Polarization<-seq(0,1, 0.1)
names(plot.vote)<-c("min1", "min2", "max2", "max1", "mean", "Polarization")
plot.vote$Type<-"Marginal Effect (Authoritarianism)"

m.effect<-c(NA, NA, NA, NA, NA)
names(m.effect)<-c(names(logit.difference(a, 0)[[2]]))  
for(i in seq(0,1, 0.1)){
  m.effect<-rbind(m.effect, logit.difference(a, i)[[2]])
}
plot.vote2<-m.effect[-1,]
plot.vote2$Polarization<-seq(0,1, 0.1)
names(plot.vote2)<-c("min1", "min2", "max2", "max1", "mean", "Polarization")
plot.vote2$Type<-"Authoritarian (Probability)"

m.effect<-c(NA, NA, NA, NA, NA)
names(m.effect)<-c(names(logit.difference(a, 0)[[3]]))  
for(i in seq(0,1, 0.1)){
  m.effect<-rbind(m.effect, logit.difference(a, i)[[3]])
}
plot.vote3<-m.effect[-1,]
plot.vote3$Polarization<-seq(0,1, 0.1)
names(plot.vote3)<-c("min1", "min2", "max2", "max1", "mean", "Polarization")
plot.vote3$Type<-"Non-Authoritarian (Probability)"
plot.vote<-rbind(plot.vote, plot.vote2, plot.vote3)

plot.vote$Type <- factor(plot.vote$Type, levels = c("Authoritarian (Probability)", 
                                                    "Non-Authoritarian (Probability)",
                                                    "Marginal Effect (Authoritarianism)")) 

plot1<-ggplot(data = subset(plot.vote),
              aes(x = Polarization, 
                  y = mean, ymin=min1, 
                  ymax=max1))+
  facet_wrap(~Type)+
  geom_ribbon(fill="lightgray", alpha=0.75)+
  geom_ribbon(aes(ymin=min2, 
                  ymax=max2), fill="black", alpha=0.05)+
  geom_line(aes(x=Polarization, y=mean))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=0))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Polarization, Authoritarianism, and Voting. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(0,1))+
  scale_x_continuous("Polarization", limits=c(0,1))
plot1
################################################################################################################
################################################################################################################

dev.copy(png,'ch7_16.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################




### END ####

##   Appendix Information. Be sure to rerun the appendix measurement model
	#Figure 5:
	require(nnet)
	require(car)
	load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel2000.RData")
	source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
	y<-subset(d2, white.2000==1)
	y<-subset(y, select=c("abortion.2000", "fem.2000",
	                      "fund.2000",
	                      "gay1.2000", "gay2.2000",
	                     "insurance.2000", "services.2000", "jobs.2000",
	                      "big.business", "unions", "womens.role",
	                     "partial.birth", "parental.consent",
	                      "gay.adopt", "gay.military", "guns", "protect.ss", "tax.cuts",
	                     "vouchers", "english"))

	### Need to adjust the measurement model
	y$rid<-seq(1:nrow(y))
	#write.csv(y, file="/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/mplus_ch7_panel.csv")
	### Load measurement models ###
	outfile<-"/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Analysis/Measurement Models/Chapter 7 Appendix Models/Chapter 7 Panel_M3.out"
	labels<-c("Feminists", "Gays", "Fundament", "Abortion", "Marriage",
	  "Adoption",
	  "Partial Birth", "Consent", "English", "Military", "Women",
	  "Insurance", "Services", "Jobs", "Vouchers", "Social", "Fiscal")
	
dev.copy(png,'ch7_A1.jpg',
         	         width = 750, height = 500,)

semPlot::semPaths(outfile, what="paths", "est", style="lisrel", rotation=2,
	                  thresholds=FALSE, residuals=FALSE, intercepts=FALSE,
	                  sizeMan2=2, sizeMan=7, sizeLat2=7, sizeLat=6,
	                  label.prop=0.5, label.cex=0.5, title=TRUE, node.width=1,
	                  edge.label.cex = 0.5,
	                  nCharNodes=0, label.font=3, label.scale=FALSE, nodeLabels=labels)
	dev.off()

factor.scores<-read.delim2("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/factor.scores2.txt", header = FALSE, sep="",
	                           stringsAsFactors=FALSE)
factor.scores<-factor.scores[,16:ncol(factor.scores)]
names(factor.scores)<-c("rid", "f1", "sd1", "f2", "sd2")
factor.scores$rid<-as.numeric(factor.scores$rid)
factor.scores$f1<-as.numeric(factor.scores$f1)
factor.scores$f2<-as.numeric(factor.scores$f2)
### Data Merge ###
y2<-subset(d2, white.2000==1)
y2$rid<-seq(1:nrow(y2))
data<-merge(factor.scores, y2, by="rid")
y<-data
y$party1<-recode(as.numeric(y$pid.2000), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$party2<-recode(as.numeric(y$pid.2002), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$party3<-recode(as.numeric(y$pid.2004), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$authoritarianism<-(rowMeans(cbind(y$auth1.2000, y$auth2.2000, y$auth3.2000, y$auth4.2000), na.rm=T)-1)/2
y$f1<-zero.one(y$f1)
y$f2<-zero.one(y$f2)
################################################################################################################
################################################################################################################

### Do authoritarians variably rely on this construct?	

################################################################################################################
################################################################################################################
y$interaction1<-y$authoritarianism*y$f1
y$interaction2<-y$authoritarianism*y$f2

tt<-as.formula(party1~
                 f1+f2+
                 authoritarianism+
                 interaction1+
                 interaction2+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
a<-multinom(tt, data=y)
tt<-as.formula(party2~
                 f1+f2+
                 authoritarianism+
                 interaction1+
                 interaction2+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)

b<-multinom(tt, data=y)
tt<-as.formula(party3~
                 f1+f2+
                 authoritarianism+
                 interaction1+
                 interaction2+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
c<-multinom(tt, data=y)

three.year.plot<-function(year){
  val<-seq(0,1, by=0.01)  
  if(year==2000){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(a)[1,], coef(a)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(a, "fiscal", "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure$PID<-"Democrat"
    
    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(a, "fiscal", "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "fiscal", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(a, "social", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure2$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=8)
    plot.data$Year<-2000
  } 
  
  if(year==2002){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(b)[1,], coef(b)[2,]), vcov(b)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(b, "fiscal", "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure$PID<-"Democrat"
    
    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(b, "fiscal", "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "fiscal", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(b, "social", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure2$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=8)
    plot.data$Year<-2002
    
  }
  
  if(year==2004){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(c)[1,], coef(c)[2,]), vcov(c)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(c, "fiscal", "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure$PID<-"Democrat"
    
    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth2(c, "fiscal", "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "fiscal", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth2(c, "social", "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val), times=2)
    plot.figure2$Ideology<-rep(c("Fiscal","Social"), each=length(val)*2)
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=8)
    plot.data$Year<-2004
    
  }
  return(plot.data)
}

plot.data<-rbind(three.year.plot(2000), three.year.plot(2002), three.year.plot(2004))

plot1<-ggplot(data = subset(plot.data, Group=="Authoritarian"),
              aes(x = ideo, 
                  y = mean, ymin=min1, 
                  ymax=max1,  linetype=Ideology, group=Ideology))+
  facet_wrap(~PID+Year)+
  geom_ribbon(aes(x=ideo, ymin=min2, ymax=max2, group=Ideology), fill="grey79", alpha=0.5)+
  geom_line(aes(x=ideo, y=mean, linetype=Ideology, colour=Ideology))+
  scale_linetype_manual(name="Ideology", values=c("solid", "solid"))+
  scale_colour_manual(name="Ideology", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=0))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Party Identification by Social and Fiscal Ideology. White Authoritarians") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=45)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Ideology", limits=c(0,1))

plot1

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_AF1.jpg',
         width = 750, height = 500,)
dev.off()

################################################################################################################

plot1<-ggplot(data = subset(plot.data, Group=="Non-Authoritarian"),
              aes(x = ideo, 
                  y = mean, ymin=min1, 
                  ymax=max1,  linetype=Ideology, group=Ideology))+
  facet_wrap(~PID+Year)+
  geom_ribbon(aes(x=ideo, ymin=min2, ymax=max2, group=Ideology), fill="grey79", alpha=0.5)+
  geom_line(aes(x=ideo, y=mean, linetype=Ideology, colour=Ideology))+
  scale_linetype_manual(name="Ideology", values=c("solid", "solid"))+
  scale_colour_manual(name="Ideology", values=c("grey79", "black"))+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Party Identification by Social and Fiscal Ideology. White Non-Authoritarians") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Ideology", limits=c(0,1))

plot1

################################################################################################################
################################################################################################################

dev.copy(png,'ch7_AF2.jpg',
         width = 750, height = 500,)
dev.off()

	
##############################################################################################################
## DISAGGREGATED EFFECTS PER COAUTHORS SUGGESTION
# 
# ################################################################################################################
# ################################################################################################################

require(nnet)
require(car)
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel2000.RData")
source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
y<-subset(d2, white.2000==1)
y<-subset(y, select=c("abortion.2000", "fem.2000",
                      "fund.2000",
                      "gay1.2000", "gay2.2000",
                      "insurance.2000", "services.2000", "jobs.2000",
                      "big.business", "unions", "womens.role", 
                      "partial.birth", "parental.consent",
                      "gay.adopt", "gay.military", "guns", "protect.ss", "tax.cuts",
                      "vouchers", "english"))


y$rid<-seq(1:nrow(y))
#write.csv(y, file="/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/mplus_ch7_panel.csv")
### Load measurement models ###
outfile<-"/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Analysis/Measurement Models/Chapter 7 Panel_M3.out"
labels<-c("Feminists", "Gays", "Fundamentalists", "Abortion", "Marriage",
          "Insurance", "Services", "Jobs", "Social", "Fiscal")
semPlot::semPaths(outfile, what="paths", "est", style="lisrel", rotation=2,
                  thresholds=FALSE, residuals=FALSE, intercepts=FALSE,
                  sizeMan2=2, sizeMan=7, sizeLat2=7, sizeLat=6, 
                  label.prop=0.5, label.cex=0.5, title=TRUE, node.width=1,
                  edge.label.cex = 0.5, 
                  nCharNodes=0, label.font=3, label.scale=FALSE, nodeLabels=labels)

detach("package:dplyr")
factor.scores<-read.delim2("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/factor.scores2.txt", header = FALSE, sep="",
                           stringsAsFactors=FALSE)
factor.scores<-factor.scores[,9:ncol(factor.scores)]
names(factor.scores)<-c("rid", "f1", "sd1", "f2", "sd2")
factor.scores$rid<-as.numeric(factor.scores$rid)
factor.scores$f1<-as.numeric(factor.scores$f1)
factor.scores$f2<-as.numeric(factor.scores$f2)
### Data Merge ###
y2<-subset(d2, white.2000==1)
y2$rid<-seq(1:nrow(y2))
data<-merge(factor.scores, y2, by="rid")
y<-data
y$party1<-recode(as.numeric(y$pid.2000), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$party2<-recode(as.numeric(y$pid.2002), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$party3<-recode(as.numeric(y$pid.2004), "1:3='Democrat'; 4='Independent'; 5:7='Republican'; else=NA" )
y$authoritarianism<-(rowMeans(cbind(y$auth1.2000, y$auth2.2000, y$auth3.2000, y$auth4.2000), na.rm=T)-1)/2
y$f1<-zero.one(y$f1)
y$f2<-zero.one(y$f2)
################################################################################################################
################################################################################################################

### Do authoritarians variably rely on this construct?	

################################################################################################################
################################################################################################################
y$interaction1<-y$authoritarianism*y$f1
tt<-as.formula(party1~
                 f1+
                 authoritarianism+
                 interaction1+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
a<-multinom(tt, data=y)
tt<-as.formula(party2~
                 f1+
                 authoritarianism+
                 interaction1+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)

b<-multinom(tt, data=y)
tt<-as.formula(party3~
                 f1+
                 authoritarianism+
                 interaction1+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
c<-multinom(tt, data=y)


three.year.plot<-function(year){
    val<-seq(0,1, by=0.01)
    if(year==2000){
      ### Display the Predictive Effects ####
      beta.sim<-mvrnorm(1000, c(coef(a)[1,], coef(a)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
      aa<-as.data.frame(matrix(ncol=5))
      names(aa)<-c(names(pid.alignment.auth3(a,  "authoritarian", 0, beta.sim)[[1]]))
      aa<-aa[-1,]
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(a, "authoritarian", val[i], beta.sim)[[1]])
      }
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(a, "non-authoritarian", val[i], beta.sim)[[1]])
      }
      plot.figure<-aa
      names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
      plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
      plot.figure$PID<-"Democrat"

      ### Display the Predictive Effects ####
      aa<-as.data.frame(matrix(ncol=5))
      names(aa)<-c(names(pid.alignment.auth3(a,  "authoritarian", 0, beta.sim)[[3]]))
      aa<-aa[-1,]
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(a, "authoritarian", val[i], beta.sim)[[3]])
      }
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(a, "non-authoritarian", val[i], beta.sim)[[3]])
      }
      plot.figure2<-aa
      names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
      plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
      plot.figure2$PID<-"Republican"
      plot.data<-rbind(plot.figure, plot.figure2)
      plot.data$ideo<-rep(val, times=4)
      plot.data$Year<-2000
    }
    if(year==2002){
      ### Display the Predictive Effects ####
      beta.sim<-mvrnorm(1000, c(coef(b)[1,], coef(b)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
      aa<-as.data.frame(matrix(ncol=5))
      names(aa)<-c(names(pid.alignment.auth3(b,  "authoritarian", 0, beta.sim)[[1]]))
      aa<-aa[-1,]
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(b, "authoritarian", val[i], beta.sim)[[1]])
      }
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(b, "non-authoritarian", val[i], beta.sim)[[1]])
      }
      plot.figure<-aa
      names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
      plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
      plot.figure$PID<-"Democrat"

      ### Display the Predictive Effects ####
      aa<-as.data.frame(matrix(ncol=5))
      names(aa)<-c(names(pid.alignment.auth3(b,  "authoritarian", 0, beta.sim)[[3]]))
      aa<-aa[-1,]
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(b, "authoritarian", val[i], beta.sim)[[3]])
      }
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(b, "non-authoritarian", val[i], beta.sim)[[3]])
      }
      plot.figure2<-aa
      names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
      plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
      plot.figure2$PID<-"Republican"
      plot.data<-rbind(plot.figure, plot.figure2)
      plot.data$ideo<-rep(val, times=4)
      plot.data$Year<-2002
    }
    if(year==2004){
      ### Display the Predictive Effects ####
      beta.sim<-mvrnorm(1000, c(coef(a)[1,], coef(a)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
      aa<-as.data.frame(matrix(ncol=5))
      names(aa)<-c(names(pid.alignment.auth3(c,  "authoritarian", 0, beta.sim)[[1]]))
      aa<-aa[-1,]
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(c, "authoritarian", val[i], beta.sim)[[1]])
      }
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(c, "non-authoritarian", val[i], beta.sim)[[1]])
      }
      plot.figure<-aa
      names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
      plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
      plot.figure$PID<-"Democrat"

      ### Display the Predictive Effects ####
      aa<-as.data.frame(matrix(ncol=5))
      names(aa)<-c(names(pid.alignment.auth3(c,  "authoritarian", 0, beta.sim)[[3]]))
      aa<-aa[-1,]
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(c, "authoritarian", val[i], beta.sim)[[3]])
      }
      for(i in 1:length(val)){
        aa<-rbind(aa, pid.alignment.auth3(c, "non-authoritarian", val[i], beta.sim)[[3]])
      }
      plot.figure2<-aa
      names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
      plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
      plot.figure2$PID<-"Republican"
      plot.data<-rbind(plot.figure, plot.figure2)
      plot.data$ideo<-rep(val, times=4)
      plot.data$Year<-2004
    }
    return(plot.data)
  }
plot.data1<-rbind(three.year.plot(2000), three.year.plot(2002), three.year.plot(2004))
plot.data1$Ideology<-"Social"

cor.test(y$f1, y$f2) #0.67
## Social Analysis ##
y$interaction1<-y$authoritarianism*y$f2
tt<-as.formula(party1~
                 f2+
                 authoritarianism+
                 interaction1+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
a<-multinom(tt, data=y)
tt<-as.formula(party2~
                 f2+
                 authoritarianism+
                 interaction1+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)

b<-multinom(tt, data=y)
tt<-as.formula(party3~
                 f2+
                 authoritarianism+
                 interaction1+
                 college.2000+
                 sex.2000+age.2000+income.2000+
                 jewish.2000+catholic.2000+other.2000)
c<-multinom(tt, data=y)

three.year.plot<-function(year){
  val<-seq(0,1, by=0.01)
  if(year==2000){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(a)[1,], coef(a)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth3(a,  "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(a, "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(a, "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
    plot.figure$PID<-"Democrat"

    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth3(a,  "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(a, "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(a, "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=4)
    plot.data$Year<-2000
  }
  if(year==2002){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(b)[1,], coef(b)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth3(b,  "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(b, "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(b, "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
    plot.figure$PID<-"Democrat"

    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth3(b,  "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(b, "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(b, "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=4)
    plot.data$Year<-2002
  }
  if(year==2004){
    ### Display the Predictive Effects ####
    beta.sim<-mvrnorm(1000, c(coef(a)[1,], coef(a)[2,]), vcov(a)) ##Draw samples from multivariate distrbution
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth3(c,  "authoritarian", 0, beta.sim)[[1]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(c, "authoritarian", val[i], beta.sim)[[1]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(c, "non-authoritarian", val[i], beta.sim)[[1]])
    }
    plot.figure<-aa
    names(plot.figure)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
    plot.figure$PID<-"Democrat"

    ### Display the Predictive Effects ####
    aa<-as.data.frame(matrix(ncol=5))
    names(aa)<-c(names(pid.alignment.auth3(c,  "authoritarian", 0, beta.sim)[[3]]))
    aa<-aa[-1,]
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(c, "authoritarian", val[i], beta.sim)[[3]])
    }
    for(i in 1:length(val)){
      aa<-rbind(aa, pid.alignment.auth3(c, "non-authoritarian", val[i], beta.sim)[[3]])
    }
    plot.figure2<-aa
    names(plot.figure2)<-c("min1", "min2", "max2", "max1", "mean")
    plot.figure2$Group<-rep(c("Authoritarian","Non-Authoritarian"), each=length(val))
    plot.figure2$PID<-"Republican"
    plot.data<-rbind(plot.figure, plot.figure2)
    plot.data$ideo<-rep(val, times=4)
    plot.data$Year<-2004
  }
  return(plot.data)
}

plot.data2<-rbind(three.year.plot(2000), three.year.plot(2002), three.year.plot(2004))
plot.data2$Ideology<-"Fiscal"
plot.data<-rbind(plot.data1, plot.data2)

plot1<-ggplot(data = subset(plot.data, Group=="Authoritarian"),
              aes(x = ideo,
                  y = mean, ymin=min1,
                  ymax=max1,  group=Ideology))+
  facet_wrap(~PID+Year)+
  geom_ribbon(aes(x=ideo, ymin=min2, ymax=max2, group=Ideology), fill="darkgrey", alpha=0.5)+
  geom_line(aes(x=ideo, y=mean, colour=Ideology))+
  scale_colour_manual("Ideology", values = c("darkgrey", "lightgrey")) +
  theme(text=element_text(size=10),
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Party Identification by Social and Fiscal Ideology. White Authoritarians") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=45)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Ideology", limits=c(0,1))

plot1

dev.copy(png,'ch7_AF3.jpg',
         width = 750, height = 500,)
dev.off()



plot1<-ggplot(data = subset(plot.data, Group=="Non-Authoritarian"),
              aes(x = ideo,
                  y = mean, ymin=min1,
                  ymax=max1,  group=Ideology))+
  facet_wrap(~PID+Year)+
  geom_ribbon(aes(x=ideo, ymin=min2, ymax=max2, group=Ideology), fill="darkgrey", alpha=0.5)+
  geom_line(aes(x=ideo, y=mean, colour=Ideology))+
  scale_colour_manual("Ideology", values = c("darkgrey", "lightgrey")) +
  theme(text=element_text(size=10),
        axis.text.y=element_text(angle=45))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Party Identification by Social and Fiscal Ideology. White Non-Authoritarians") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=45)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Probability", limits=c(0,1))+
  scale_x_continuous("Ideology", limits=c(0,1))

plot1

dev.copy(png,'ch7_AF4.jpg',
         width = 750, height = 500)
dev.off()

