####-------------------------------------------------------------------------------------####
# Chapter 8 and 9: Defection Chapter/Authoritarian Rule
# This chapter uses both the VSG and ANES data ##
####-------------------------------------------------------------------------------------####
### Recode Block ###
  rm(list=ls())
  #Figure working directory#
  setwd("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Book/Chapters/Edited Chapters_Weber/Figures/")
    detach("package:dplyr")
  require(car)
  require(foreign)
  require(ggplot2)
  load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Recoded Data/pooled.auth.Rdata")
  source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
  data<-subset(data, year!=1990) ## Drop 1990
  data<-subset(data, year!=1994) ## Drop 1990
  data$mode<-as.character(data$mode)
  data$authoritarianism<-(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T)-1)/2
  data$participation<-rowSums(cbind(data$p1, data$p2, data$p3, data$p4,data$p5), na.rm=T)
  data$republican<-recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
  data$democrat<-recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
  data$independent<-recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
  data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
  data$split<-recode(as.numeric(data$split.house),"1=0; 4=0; 2=1; 3=1; else=NA")
  data$AuthXRep<-data$authoritarianism*data$republican
  data$AuthXInd<-data$authoritarianism*data$independent
  data$efficacy<-rowMeans(with(data, cbind(efficacy1, efficacy2, efficacy3, efficacy4)),
      na.rm=T)
  psych::alpha(with(data, cbind(efficacy1, efficacy2, efficacy3, efficacy4)))

  ### Measure of ambivalence ###
  data$in.party<-NA
  data$in.party[which(data$party3=="Democrat")]<-data$feeling.dem[which(data$party3=="Democrat")]
  data$in.party[which(data$party3=="Republican")]<-data$feeling.rep[which(data$party3=="Republican")]
  
  
  data$out.party<-NA
  data$out.party[which(data$party3=="Democrat")]<-data$feeling.rep[which(data$party3=="Democrat")]
  data$out.party[which(data$party3=="Republican")]<-data$feeling.dem[which(data$party3=="Republican")]
  
  data$conflict1<-(100-data$in.party)+data$out.party

  data$in.party<-NA
  data$in.party[which(data$party3=="Democrat")]<-data$feeling.demc[which(data$party3=="Democrat")]
  data$in.party[which(data$party3=="Republican")]<-data$feeling.repc[which(data$party3=="Republican")]
  
  data$out.party<-NA
  data$out.party[which(data$party3=="Democrat")]<-data$feeling.repc[which(data$party3=="Democrat")]
  data$out.party[which(data$party3=="Republican")]<-data$feeling.demc[which(data$party3=="Republican")]
  
  data$conflict2<-(100-data$in.party)+data$out.party
  
  ### Measures of egalitarianism ###
  psych::alpha(with(data, cbind(egal1, egal2, egal3, egal4)))
  psych::alpha(with(data, cbind(moral1, moral2, moral3, moral4)))
  
  data$egalitarianism<-zero.one(rowMeans(with(data, cbind(egal1, egal2, egal3, egal4)),
                          na.rm=T))
  data$moral.traditionalism<-zero.one(rowMeans(with(data, cbind(moral1, moral2, moral3, moral4)),
                                         na.rm=T))
  ### Code Mismatch ###
  
  ## A high score is negativity toward one's own party and positivity towards the competing
  #white.data<-subset(data, white==1 & year>=2008)
  white.data<-subset(data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
  white.data<-subset(white.data, white==1) ## Drop 1990
  black.data<-subset(data, black==1 & year>=2008)
  hispanic.data<-subset(data, hispanic==1 & year>=2008)
  nonwhite.data<-subset(data, ((hispanic==1 | black==1) & year>=2008))

### Part I: Context###
  
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
    
total.plot<-subset(total.plot, Group!="Independent")
    
    
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
    dev.copy(png,'ch8_1.jpg',
             width = 750, height = 500)
    dev.off()
    
    
    plot.data<-na.omit(data.frame(party3=white.data$party3, authoritarianism=white.data$authoritarianism, year=white.data$year))
    plot.data$year <- factor(plot.data$year, c("2016", "2012", "2008", "2004", "2000", "1992"))
    plot.data<-subset(plot.data, party3!="Independent")
    plot1<-ggplot(plot.data, aes(x=authoritarianism,
                                 y=year,
                                 fill=party3))+
      stat_density_ridges(geom="density_ridges", 
                          bandwidth=.12,
                          alpha = .50, color = "black",
                          quantile_lines=FALSE, quantiles = 2)+
      scale_fill_cyclical(
        values = c("lightgrey", "black"),
        name = "Party", guide = "legend")+
      xlab('Value') +
      theme_joy()+
      theme(axis.title.y = element_blank())+
      xlab("Density Plots")+
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
      theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.65, hjust=0.5)) +
      theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5, hjust=0.5))+
      geom_vline(xintercept=0.5, linetype="dashed")+
      ggtitle("Authoritarianism within the Parties. White Respondents") 
    
    plot1
    dev.copy(png,'ch8_2.jpg',
             width = 750, height = 500)
    dev.off()
    
  ### Party Mismatch Analysis ####
 #white.data$match<-with(white.data, ifelse(party3=="Democrat" & ideology<4, 1, NA))    
 #white.data$match<-with(white.data, ifelse(party3=="Republican" & ideology>4, 1, match))    
 #white.data$match<-with(white.data, ifelse(party3=="Republican" & ideology<4, 0, match))    
 #white.data$match<-with(white.data, ifelse(party3=="Democrat" & ideology>4, 0, match))    
 #white.data$ideology<-zero.one(white.data$ideology)
white.data$dif<-abs(white.data$pid-zero.one(white.data$ideology))    
tt<-as.formula(dif~
                 authoritarianism+
                 republican+independent+
                 AuthXRep+
                 AuthXInd+
                 female+age+college+income+jewish+catholic)
a<-lm(tt, data=subset(white.data, year==1992))
b<-lm(tt, data=subset(white.data, year==2000))
c<-lm(tt, data=subset(white.data, year==2004))
d<-lm(tt, data=subset(white.data, year==2008))
e<-lm(tt, data=subset(white.data, year==2012))
f<-lm(tt, data=subset(white.data, year==2016))

#### Generate Predictive Effects #####
cov<-c(0,1)
### Generate Probability of being in most extreme category
temp.f<-function(output, rep, ind){
  rep<-rep
  ind<-ind
  auth<-c(0,1)
  e.data<-expand.grid(rep=rep, ind=ind, auth=auth)
  e.data$int1<-rep*auth  
  e.data$int2<-ind*auth  
  e.data<-as.data.frame(e.data)
  temp<-model.prediction(output, design.matrix.int.predictive.2(output, names=c("authoritarianism", "republican", 
                                                                                "AuthXRep", "independent", "AuthXInd"), 
                                                                values=c(1,1,1,1,1), FALSE),
                         "Predictive", "OLS",3)
  for(i in 1:nrow(e.data)){
    temp<-rbind(temp, model.prediction(output, 
                                       design.matrix.int.predictive.2(output, 
                                                                      names=c("authoritarianism", "republican", 
                                                                              "AuthXRep", "independent", "AuthXInd"), 
                                                                      values=c(e.data$auth[i], e.data$rep[i], 
                                                                               e.data$int1[i], e.data$ind[i],
                                                                               e.data$int2[i]), FALSE),
                                       "Predictive", "OLS", 3))
  }
  temp<-temp[-1,]
  return(temp)
}

plot.data<-rbind(temp.f(a, 1, 0),
                 temp.f(a, 0, 0),
                 temp.f(a, 0, 1),
                 temp.f(b, 1, 0),
                 temp.f(b, 0, 0),
                 temp.f(b, 0, 1),
                 temp.f(c, 1, 0),
                 temp.f(c, 0, 0),
                 temp.f(c, 0, 1),
                 temp.f(d, 1, 0),
                 temp.f(d, 0, 0),
                 temp.f(d, 0, 1),
                 temp.f(e, 1, 0),
                 temp.f(e, 0, 0),
                 temp.f(e, 0, 1),
                 temp.f(f, 1, 0),
                 temp.f(f, 0, 0),
                 temp.f(f, 0, 1))
plot.data$Group<-rep(c("Non-Authoritarian", "Authoritarian"))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=3*2)
plot.data$R.Party<-rep(c("Republican Respondent", "Democratic Respondent", "Independent Respondent"), each=2)
plot.data

plot.data<-plot.data[order(plot.data$Year),]

plot1<-ggplot(data =subset(plot.data, R.Party!="Independent Respondent"),
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5, group=Group, colour=Group))+
  facet_wrap(~R.Party, nrow=3)+
  geom_point(position = position_dodge(width = 0)) +
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  scale_colour_manual(name="Group", values=c("gray", "black"))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+ # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Ideological-Partisan Mismatch. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Prediction", limits=c(0,1))+
  scale_x_discrete("Year")
plot1
dev.copy(png,'ch8_3.jpg',
         width = 750, height = 500)
dev.off()
tt<-as.formula(zero.one(ideology)~
                 authoritarianism+
                 republican+independent+
                 AuthXRep+
                 AuthXInd+
                 female+age+college+income+jewish+catholic)
a<-lm(tt, data=subset(white.data, year==1992))
b<-lm(tt, data=subset(white.data, year==2000))
c<-lm(tt, data=subset(white.data, year==2004))
d<-lm(tt, data=subset(white.data, year==2008))
e<-lm(tt, data=subset(white.data, year==2012))
f<-lm(tt, data=subset(white.data, year==2016))

#### Generate Predictive Effects #####
cov<-c(0,1)
### Generate Probability of being in most extreme category
temp.f<-function(output, rep, ind){
  rep<-rep
  ind<-ind
  auth<-c(0,1)
  e.data<-expand.grid(rep=rep, ind=ind, auth=auth)
  e.data$int1<-rep*auth  
  e.data$int2<-ind*auth  
  e.data<-as.data.frame(e.data)
  temp<-model.prediction(output, design.matrix.int.predictive.2(output, names=c("authoritarianism", "republican", 
                                                                                "AuthXRep", "independent", "AuthXInd"), 
                                                                values=c(1,1,1,1,1), FALSE),
                         "Predictive", "OLS",3)
  for(i in 1:nrow(e.data)){
    temp<-rbind(temp, model.prediction(output, 
                                       design.matrix.int.predictive.2(output, 
                                                                      names=c("authoritarianism", "republican", 
                                                                              "AuthXRep", "independent", "AuthXInd"), 
                                                                      values=c(e.data$auth[i], e.data$rep[i], 
                                                                               e.data$int1[i], e.data$ind[i],
                                                                               e.data$int2[i]), FALSE),
                                       "Predictive", "OLS", 3))
  }
  temp<-temp[-1,]
  return(temp)
}

plot.data<-rbind(temp.f(a, 1, 0),
                 temp.f(a, 0, 0),
                 temp.f(a, 0, 1),
                 temp.f(b, 1, 0),
                 temp.f(b, 0, 0),
                 temp.f(b, 0, 1),
                 temp.f(c, 1, 0),
                 temp.f(c, 0, 0),
                 temp.f(c, 0, 1),
                 temp.f(d, 1, 0),
                 temp.f(d, 0, 0),
                 temp.f(d, 0, 1),
                 temp.f(e, 1, 0),
                 temp.f(e, 0, 0),
                 temp.f(e, 0, 1),
                 temp.f(f, 1, 0),
                 temp.f(f, 0, 0),
                 temp.f(f, 0, 1))
plot.data$Group<-rep(c("Non-Authoritarian", "Authoritarian"))
plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=3*2)
plot.data$R.Party<-rep(c("Republican Respondent", "Democratic Respondent", "Independent Respondent"), each=2)
plot.data

plot.data<-plot.data[order(plot.data$Year),]

plot1<-ggplot(data =subset(plot.data, R.Party!="Independent Respondent"),
              aes(x = factor(Year), 
                  y = mean.score, ymin=min.2.5, 
                  ymax=max.97.5, group=Group, colour=Group))+
  facet_wrap(~R.Party, nrow=3)+
  geom_point(position = position_dodge(width = 0)) +
  geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
  geom_errorbar(aes(ymin=min.25, 
                    ymax=max.75),
                position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
  theme(text=element_text(size=10), 
        axis.text.y=element_text(angle=45))+
  scale_colour_manual(name="Group", values=c("gray", "black"))+
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  theme_bw()+ # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(axis.ticks=element_blank())+
  ggtitle("Ideology and Authoritarianism. White Respondents") +
  theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
  theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  scale_y_continuous("Conservatism", limits=c(0,1))+
  scale_x_discrete("Year")
plot1

dev.copy(png,'ch8_4.jpg',
         width = 750, height = 500)
dev.off()

  
####### Part II. Defection  ######

    rm(list=ls())
    detach("package:dplyr")
    require(car)
    require(foreign)
    require(ggplot2)
    load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/Recoded Data/pooled.auth.Rdata")
    source("/Users/chrisweber/dropbox/working projects/Authoritarianism_BookProject/analysis/functions/BookFunctions.R")
    data<-subset(data, year!=1990) ## Drop 1990
    data<-subset(data, year!=1994) ## Drop 1990
    data$mode<-as.character(data$mode)
    data$authoritarianism<-(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm=T)-1)/2
    data$participation<-rowSums(cbind(data$p1, data$p2, data$p3, data$p4,data$p5), na.rm=T)
    data$republican<-recode(data$pid*6+1, "1:2=0; 3:5=0; 6:7=1" )
    data$democrat<-recode(data$pid*6+1, "1:2=1; 3:5=0; 6:7=0" )
    data$independent<-recode(data$pid*6+1, "1:2=0; 3:5=1; 6:7=0" )
    data$party3<-recode(data$pid*6+1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA" )
    data$split<-recode(as.numeric(data$split.house),"1=0; 4=0; 2=1; 3=1; else=NA")
    data$AuthXRep<-data$authoritarianism*data$republican
    data$AuthXInd<-data$authoritarianism*data$independent
    data$efficacy<-rowMeans(with(data, cbind(efficacy1, efficacy2, efficacy3, efficacy4)),
                            na.rm=T)
    
    psych::alpha(with(data, cbind(efficacy1, efficacy2, efficacy3, efficacy4)))
    ### Measure of ambivalence ###
    data$in.party<-NA
    data$in.party[which(data$party3=="Democrat")]<-data$feeling.dem[which(data$party3=="Democrat")]
    data$in.party[which(data$party3=="Republican")]<-data$feeling.rep[which(data$party3=="Republican")]
    
    
    data$out.party<-NA
    data$out.party[which(data$party3=="Democrat")]<-data$feeling.rep[which(data$party3=="Democrat")]
    data$out.party[which(data$party3=="Republican")]<-data$feeling.dem[which(data$party3=="Republican")]
    
    data$conflict1<-(100-data$in.party)+data$out.party
    
    data$in.party<-NA
    data$in.party[which(data$party3=="Democrat")]<-data$feeling.demc[which(data$party3=="Democrat")]
    data$in.party[which(data$party3=="Republican")]<-data$feeling.repc[which(data$party3=="Republican")]
    
    data$out.party<-NA
    data$out.party[which(data$party3=="Democrat")]<-data$feeling.repc[which(data$party3=="Democrat")]
    data$out.party[which(data$party3=="Republican")]<-data$feeling.demc[which(data$party3=="Republican")]
    
    data$conflict2<-(100-data$in.party)+data$out.party
    
    ## A high score is negativity toward one's own party and positivity towards the competing
    white.data<-subset(data, white==1)
    white.data<-subset(white.data, mode=="FTF"|mode=="FTC/CASI" ) ## Drop 1990
    black.data<-subset(data, black==1)
    hispanic.data<-subset(data, hispanic==1)
    nonwhite.data<-subset(data, nonwhite==1)
    auth.split<-ifelse(white.data$authoritarianism<0.5, 0, 1)
    
    ### Weight these estimates. Include raw counts, and row and column percentages ###  
    ## For white respondents, only use FTF weights
    mytable <- questionr::wtd.table(white.data$party3, auth.split, white.data$weights.ftf)
    margin.table(mytable, 1) # A frequencies (summed over B) 
    margin.table(mytable, 2) # B frequencies (summed over A)
    prop.table(mytable) # cell percentages
    prop.table(mytable, 1) # row percentages 
    prop.table(mytable, 2) # column percentages
    
    auth.split<-ifelse(black.data$authoritarianism<0.5, 0, 1)
    mytable <- questionr::wtd.table(black.data$party3, auth.split, black.data$weights.all)
    margin.table(mytable, 1) # A frequencies (summed over B) 
    margin.table(mytable, 2) # B frequencies (summed over A)
    prop.table(mytable) # cell percentages
    prop.table(mytable, 1) # row percentages 
    prop.table(mytable, 2) # column percentages
    
    auth.split<-ifelse(hispanic.data$authoritarianism<0.5, 0, 1)
    mytable <- questionr::wtd.table(hispanic.data$party3, auth.split, hispanic.data$weights.all)
    margin.table(mytable, 1) # A frequencies (summed over B) 
    margin.table(mytable, 2) # B frequencies (summed over A)
    prop.table(mytable) # cell percentages
    prop.table(mytable, 1) # row percentages 
    prop.table(mytable, 2) # column percentages
    
    
    # White analysis #
    ### Presidential Voting, Whites
    tt<-as.formula(voted~
                     authoritarianism+
                     republican+independent+
                     AuthXRep+
                     AuthXInd+
                     female+age+college+income+
                     jewish+catholic+other)
    a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
    b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
    c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
    d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
    e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
    f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
    cov<-c(0,1)
    defect.margin<-function(a){
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
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    plot.data$PID<-rep(c("Democrat", "Republican"))
    plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
    
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Democrat"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==2000])))
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data$PID[nrow(plot.data)]<-"Republican"
    plot.data1<-plot.data
    ### Code for Independents
    cov<-c(1)
    defect.margin<-function(a){
      temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                             control=c("republican", "AuthXRep"), 1, 
                                                             ordinal=FALSE), "Marginal", "Binary.Logit")
      return(temp)
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    
    plot.data$PID<-"Independent"
    plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Independent"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data1, plot.data)
    plot.data<-plot.data[order(plot.data$Year),]
    
    plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
    
    plot1<- ggplot(data = subset(plot.data, Year!=1996 & PID!="Independent"),
                   aes(x = factor(party), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~Year)+
      geom_point(position = position_dodge(width = 0)) +
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Voting. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.75,.75))+
      scale_x_discrete("PID")
    plot1
    dev.copy(png,'ch8_5.jpg',
             width = 750, height = 500)
    dev.off()
    
    plot2<- ggplot(data = subset(plot.data, PID!="Independent"),
                   aes(x = factor(Year), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~PID)+
      geom_point(position = position_dodge(width = 0)) +
      geom_line(data = subset(plot.data, PID!="Independent"),
                aes(x = factor(Year), 
                    y = mean.score, group=1))+
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Voting. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353", face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.75, 0.75))+
      scale_x_discrete("Year")
    plot2
    dev.copy(png,'ch8_6.jpg',
             width = 750, height = 500)
    dev.off()
    
    #### Estimate the Voting Model ############3
    #### Marginal Effect ####
    tt<-as.formula(participation~
                     authoritarianism+
                     republican+independent+
                     AuthXRep+
                     AuthXInd+
                     female+age+college+income+
                     jewish+catholic+other)
    a<-glm(tt, data=subset(white.data, year==1992), family=poisson())
    b<-glm(tt, data=subset(white.data, year==2000), family=poisson())
    c<-glm(tt, data=subset(white.data, year==2004), family=poisson())
    d<-glm(tt, data=subset(white.data, year==2008), family=poisson())
    e<-glm(tt, data=subset(white.data, year==2012), family=poisson())
    f<-glm(tt, data=subset(white.data, year==2016), family=poisson())
    
    
    cov<-c(0,1)
    defect.margin<-function(a){
      temp<-model.prediction(a, 
                             design.matrix.int.marginal.2(a, 
                                                          "authoritarianism", "republican", "AuthXRep", 
                                                          control=c("independent", "AuthXInd"), 1, 
                                                          ordinal=FALSE), "Marginal", "Count")
      
      for(i in seq_along(cov)){
        temp<-rbind(temp, model.prediction(a, 
                                           design.matrix.int.marginal.2(a, "authoritarianism", 
                                                                        "republican", "AuthXRep", 
                                                                        control=c("independent", "AuthXInd"), cov[i], 
                                                                        ordinal=FALSE), "Marginal", "Count"))
      }
      return(temp[-1,])
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    plot.data$PID<-rep(c("Democrat", "Republican"))
    plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
    
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Democrat"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==2000])))
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data$PID[nrow(plot.data)]<-"Republican"
    plot.data1<-plot.data
    ### Code for Independents
    cov<-c(1)
    defect.margin<-function(a){
      temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                             control=c("republican", "AuthXRep"), 1, 
                                                             ordinal=FALSE), "Marginal", "Count")
      return(temp)
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    
    plot.data$PID<-"Independent"
    plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Independent"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data1, plot.data)
    plot.data<-plot.data[order(plot.data$Year),]
    
    plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
    
  plot1<- ggplot(data = subset(plot.data, Year!=1996 & PID!="Independent"),
                   aes(x = factor(party), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~Year)+
      geom_point(position = position_dodge(width = 0)) +
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Participation. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism on Participation", limits=c(-2,2))+
      scale_x_discrete("PID")
  plot1
  dev.copy(png,'ch8_7.jpg',
           width = 750, height = 500)
  dev.off()
    
    plot2<- ggplot(data = subset(plot.data, Year!=1996 & PID!="Independent" ),
                   aes(x = factor(Year), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~PID)+
      geom_point(position = position_dodge(width = 0)) +
      geom_line(data = subset(plot.data, Year!=1996 & PID!="Independent" ),
                aes(x = factor(Year), 
                    y = mean.score, group=1))+
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Participation. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353", face="bold", angle=45)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-2, 2))+
      scale_x_discrete("Year")
    
    
    plot2
    dev.copy(png,'ch8_8.jpg',
             width = 750, height = 500)
    dev.off()

        ##### Split Ballot Voting #######
    tt<-as.formula(split~
                     authoritarianism+
                     republican+independent+
                     AuthXRep+
                     AuthXInd+
                     female+age+college+income+
                     jewish+catholic+other)
    a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
    b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
    c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
    d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
    e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
    f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
    
    cov<-c(0,1)
    defect.margin<-function(a){
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
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    plot.data$PID<-rep(c("Democrat", "Republican"))
    plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
    
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Democrat"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==2000])))
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data$PID[nrow(plot.data)]<-"Republican"
    plot.data1<-plot.data
    ### Code for Independents
    cov<-c(1)
    defect.margin<-function(a){
      temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                             control=c("republican", "AuthXRep"), 1, 
                                                             ordinal=FALSE), "Marginal", "Binary.Logit")
      return(temp)
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    
    plot.data$PID<-"Independent"
    plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Independent"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data1, plot.data)
    plot.data<-plot.data[order(plot.data$Year),]
    
    plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
    
    plot1<- ggplot(data = subset(plot.data, Year!=1996 & PID!="Independent"),
                   aes(x = factor(party), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~Year)+
      geom_point(position = position_dodge(width = 0)) +
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Split Ticket Voting. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.75,.75))+
      scale_x_discrete("PID")
    plot1
    dev.copy(png,'ch8_9.jpg',
             width = 750, height = 500)
    dev.off()
    
    plot1<- ggplot(data = subset(plot.data,  PID!="Independent"),
                   aes(x = factor(Year), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~PID)+
      geom_point(position = position_dodge(width = 0)) +
      geom_line(data = subset(plot.data,  PID!="Independent", ),
                aes(x = factor(Year), 
                    y = mean.score, group=1))+
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Split Ticket Voting. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353", face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.75, 0.75))+
      scale_x_discrete("Year")
    
    plot1
    dev.copy(png,'ch8_10.jpg',
             width = 750, height = 500)
    dev.off()
    
    #####  Vote  #######
    tt<-as.formula(vote~
                     authoritarianism+
                     republican+independent+
                     AuthXRep+
                     AuthXInd+
                     female+age+college+income+
                     jewish+catholic+other)
    a<-glm(tt, data=subset(white.data, year==1992), family=binomial("logit"))
    b<-glm(tt, data=subset(white.data, year==2000), family=binomial("logit"))
    c<-glm(tt, data=subset(white.data, year==2004), family=binomial("logit"))
    d<-glm(tt, data=subset(white.data, year==2008), family=binomial("logit"))
    e<-glm(tt, data=subset(white.data, year==2012), family=binomial("logit"))
    f<-glm(tt, data=subset(white.data, year==2016), family=binomial("logit"))
    
    cov<-c(0,1)
    defect.margin<-function(a){
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
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    plot.data$PID<-rep(c("Democrat", "Republican"))
    plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
    
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Democrat"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==2000])))
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data$PID[nrow(plot.data)]<-"Republican"
    plot.data1<-plot.data
    ### Code for Independents
    cov<-c(1)
    defect.margin<-function(a){
      temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                             control=c("republican", "AuthXRep"), 1, 
                                                             ordinal=FALSE), "Marginal", "Binary.Logit")
      return(temp)
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    
    plot.data$PID<-"Independent"
    plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Independent"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data1, plot.data)
    plot.data<-plot.data[order(plot.data$Year),]
    
    plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
    
    plot1<- ggplot(data = subset(plot.data, Year!=1996 & PID!="Independent"),
                   aes(x = factor(party), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~Year)+
      geom_point(position = position_dodge(width = 0)) +
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Republican Presidential Vote. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.75,.75))+
      scale_x_discrete("PID")
    
    plot1
    dev.copy(png,'ch8_11.jpg',
             width = 750, height = 500)
    dev.off()
    
    plot2<- ggplot(data = subset(plot.data, PID!="Independent"),
                   aes(x = factor(Year), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~PID)+
      geom_point(position = position_dodge(width = 0)) +
      geom_line(data = subset(plot.data, PID!="Independent"),
                aes(x = factor(Year), 
                    y = mean.score, group=1))+
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Republican Vote. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353", face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.75, 0.75))+
      scale_x_discrete("Year")
    
    plot2
    dev.copy(png,'ch8_12.jpg',
             width = 750, height = 500)
    dev.off()
    

    ### Efficacy ######
    tt<-as.formula(zero.one(efficacy)~
                     authoritarianism+
                     republican+independent+
                     AuthXRep+
                     AuthXInd+
                     female+age+college+income+
                     jewish+catholic+other)
    a<-lm(tt, data=subset(white.data, year==1992))
    b<-lm(tt, data=subset(white.data, year==2000))
    c<-lm(tt, data=subset(white.data, year==2004))
    d<-lm(tt, data=subset(white.data, year==2008))
    e<-lm(tt, data=subset(white.data, year==2012))
    f<-lm(tt, data=subset(white.data, year==2016))
    
    cov<-c(0,1)
    defect.margin<-function(a){
      temp<-model.prediction(a, 
                             design.matrix.int.marginal.2(a, 
                                                          "authoritarianism", "republican", "AuthXRep", 
                                                          control=c("independent", "AuthXInd"), 1, 
                                                          ordinal=FALSE), "Marginal", "OLS")
      
      for(i in seq_along(cov)){
        temp<-rbind(temp, model.prediction(a, 
                                           design.matrix.int.marginal.2(a, "authoritarianism", 
                                                                        "republican", "AuthXRep", 
                                                                        control=c("independent", "AuthXInd"), cov[i], 
                                                                        ordinal=FALSE), "Marginal", "OLS"))
      }
      return(temp[-1,])
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    plot.data$PID<-rep(c("Democrat", "Republican"))
    plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
    
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Democrat" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Democrat"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Republican" & plot.data$Year==2000])))
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data$PID[nrow(plot.data)]<-"Republican"
    plot.data1<-plot.data
    ### Code for Independents
    cov<-c(1)
    defect.margin<-function(a){
      temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                             control=c("republican", "AuthXRep"), 1, 
                                                             ordinal=FALSE), "Marginal", "OLS")
      return(temp)
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    
    plot.data$PID<-"Independent"
    plot.data$Year<-c(1992, 2000, 2004, 2008, 2012, 2016)
    #### Year Imputation ###
    plot.data<-rbind(plot.data,  mean(cbind(plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==1992],
                                            plot.data$mean.score[plot.data$PID=="Independent" & plot.data$Year==2000])))
    plot.data$PID[nrow(plot.data)]<-"Independent"
    plot.data$Year[nrow(plot.data)]<-1996
    plot.data<-rbind(plot.data1, plot.data)
    plot.data<-plot.data[order(plot.data$Year),]
    
    plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
    
    plot1<- ggplot(data = subset(plot.data, Year!=1996 & PID!="Independent"),
                   aes(x = factor(party), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~Year)+
      geom_point(position = position_dodge(width = 0)) +
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Efficacy. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.30,.30))+
      scale_x_discrete("PID")
    
    
    plot1
    dev.copy(png,'ch8_13.jpg',
             width = 750, height = 500)
    dev.off()
    
    plot2<- ggplot(data = subset(plot.data, PID!="Independent"),
                   aes(x = factor(Year), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5))+
      facet_wrap(~PID)+
      geom_point(position = position_dodge(width = 0)) +
      geom_line(data = subset(plot.data, PID!="Independent"),
                aes(x = factor(Year), 
                    y = mean.score, group=1))+
      geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Efficacy. Whites Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353", face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.30, 0.30))+
      scale_x_discrete("Year")
    
    plot2
    dev.copy(png,'ch8_14.jpg',
             width = 750, height = 500)
    dev.off()
    
    # Conflict
    tt<-as.formula(zero.one(conflict1)~
                     authoritarianism+
                     republican+
                     AuthXRep+
                     female+age+college+income+
                     jewish+catholic+other)
    a<-lm(tt, data=subset(white.data, year==1992))
    b<-lm(tt, data=subset(white.data, year==2000))
    c<-lm(tt, data=subset(white.data, year==2004))
    d<-lm(tt, data=subset(white.data, year==2008))
    e<-lm(tt, data=subset(white.data, year==2012))
    f<-lm(tt, data=subset(white.data, year==2016))
    
    cov<-c(0,1)
    defect.margin<-function(a){
      temp<-model.prediction(a, 
                             design.matrix.int.marginal.3(a, 
                                                          "authoritarianism", "republican", "AuthXRep", 
                                                          1, 
                                                          ordinal=FALSE), "Marginal", "OLS")
      
      for(i in seq_along(cov)){
        temp<-rbind(temp, model.prediction(a, 
                                           design.matrix.int.marginal.3(a, "authoritarianism", 
                                                                        "republican", "AuthXRep", 
                                                                        cov[i], 
                                                                        ordinal=FALSE), "Marginal", "OLS"))
      }
      return(temp[-1,])
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    plot.data$PID<-rep(c("Democrat", "Republican"))
    plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
    
    
    plot.data<-plot.data[order(plot.data$Year),]
    plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
    p1<-plot.data
    p1$Conflict<-"Party Internal Conflict"
    
    tt<-as.formula(zero.one(conflict2)~
                     authoritarianism+
                     republican+
                     AuthXRep+
                     female+age+college+income+
                     jewish+catholic+other)
    a<-lm(tt, data=subset(white.data, year==1992))
    b<-lm(tt, data=subset(white.data, year==2000))
    c<-lm(tt, data=subset(white.data, year==2004))
    d<-lm(tt, data=subset(white.data, year==2008))
    e<-lm(tt, data=subset(white.data, year==2012))
    f<-lm(tt, data=subset(white.data, year==2016))
    
    cov<-c(0,1)
    defect.margin<-function(a){
      temp<-model.prediction(a, 
                             design.matrix.int.marginal.3(a, 
                                                          "authoritarianism", "republican", "AuthXRep", 
                                                          1, 
                                                          ordinal=FALSE), "Marginal", "OLS")
      
      for(i in seq_along(cov)){
        temp<-rbind(temp, model.prediction(a, 
                                           design.matrix.int.marginal.3(a, "authoritarianism", 
                                                                        "republican", "AuthXRep", 
                                                                        cov[i], 
                                                                        ordinal=FALSE), "Marginal", "OLS"))
      }
      return(temp[-1,])
    }
    
    plot.data<-rbind(defect.margin(a), defect.margin(b),
                     defect.margin(c), defect.margin(d),
                     defect.margin(e), defect.margin(f)
    )  
    plot.data$PID<-rep(c("Democrat", "Republican"))
    plot.data$Year<-rep(c(1992, 2000, 2004, 2008, 2012, 2016), each=2)
    
    
    plot.data<-plot.data[order(plot.data$Year),]
    plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
    p2<-plot.data
    p2$Conflict<-"Candidate Internal Conflict"
    plot.data<-rbind(p1, p2)
    plot1<- ggplot(data =plot.data,
                   aes(x = factor(party), 
                       y = mean.score, ymin=min.2.5, 
                       ymax=max.97.5, colour=Conflict))+
      facet_wrap(~Year)+
      geom_point(position = position_dodge(width = .5)) +
      geom_errorbar(position = position_dodge(width = 0.5), width = 0.1) +
      geom_errorbar(aes(ymin=min.25, 
                        ymax=max.75),
                    position = position_dodge(width = 0.5), width = 0.01, size=0.9)+
      theme(text=element_text(size=10), 
            axis.text.y=element_text(angle=45))+
      geom_hline(yintercept=0, linetype="dashed" )+
      theme(panel.background=element_rect(fill="white")) +
      theme(plot.background=element_rect(fill="white")) +
      scale_colour_manual(name="Internal Conflict", values=c("black", "grey79"))+
      theme_bw()+
      # Format the grid
      theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
      theme(axis.ticks=element_blank())+
      ggtitle("Marginal Effect of Authoritarianism on Conflict. White Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.40,.40))+
      scale_x_discrete("PID")
    
    plot1
    dev.copy(png,'ch8_15.jpg',
             width = 750, height = 500)
    dev.off()

# Hispanic analysis, parallel to blacks
    # 4) All analyses are pooled, with a fixed effect for the survey year. 
hispanic.data<-nonwhite.data
  
  tt<-as.formula(voted~
                   authoritarianism+
                   republican+independent+
                   AuthXRep+
                   AuthXInd+
                   female+age+college+income+
                   catholic+as.factor(year)+other)
  a<-glm(tt, data=hispanic.data, family=binomial("logit"))
  
  
  cov<-c(0,1)
  defect.margin<-function(a){
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
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-rep(c("Democrat", "Republican"))
  temp<-plot.data
  ### Code for Independents
  cov<-c(1)
  defect.margin<-function(a){
    temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                           control=c("republican", "AuthXRep"), 1, 
                                                           ordinal=FALSE), "Marginal", "Binary.Logit")
    return(temp)
  }
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-"Independent"
  plot.data<-rbind(temp, plot.data)
  plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
  p1<-plot.data
  
  ################################################################################################################
  tt<-as.formula(participation~
                   authoritarianism+
                   republican+independent+
                   AuthXRep+
                   AuthXInd+
                   female+age+college+income+
                   catholic+as.factor(year)+other)
  a<-glm(tt, data=hispanic.data, family=poisson())
  cov<-c(0,1)
  defect.margin<-function(a){
    temp<-model.prediction(a, 
                           design.matrix.int.marginal.2(a, 
                                                        "authoritarianism", "republican", "AuthXRep", 
                                                        control=c("independent", "AuthXInd"), 1, 
                                                        ordinal=FALSE), "Marginal", "Count")
    
    for(i in seq_along(cov)){
      temp<-rbind(temp, model.prediction(a, 
                                         design.matrix.int.marginal.2(a, "authoritarianism", 
                                                                      "republican", "AuthXRep", 
                                                                      control=c("independent", "AuthXInd"), cov[i], 
                                                                      ordinal=FALSE), "Marginal", "Count"))
    }
    return(temp[-1,])
  }
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-rep(c("Democrat", "Republican"))
  temp<-plot.data
  ### Code for Independents
  cov<-c(1)
  defect.margin<-function(a){
    temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                           control=c("republican", "AuthXRep"), 1, 
                                                           ordinal=FALSE), "Marginal", "Count")
    return(temp)
  }
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-"Independent"
  plot.data<-rbind(temp, plot.data)
  plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
  p2<-plot.data
  
  ################################################################################################################
  #### Estimate the Efficacy Model ############3
  tt<-as.formula(zero.one(efficacy)~
                   authoritarianism+
                   republican+independent+
                   AuthXRep+
                   AuthXInd+
                   female+age+college+income+
                   catholic+as.factor(year)+other)
  a<-lm(tt, data=hispanic.data)
  cov<-c(0,1)
  defect.margin<-function(a){
    temp<-model.prediction(a, 
                           design.matrix.int.marginal.2(a, 
                                                        "authoritarianism", "republican", "AuthXRep", 
                                                        control=c("independent", "AuthXInd"), 1, 
                                                        ordinal=FALSE), "Marginal", "OLS")
    
    for(i in seq_along(cov)){
      temp<-rbind(temp, model.prediction(a, 
                                         design.matrix.int.marginal.2(a, "authoritarianism", 
                                                                      "republican", "AuthXRep", 
                                                                      control=c("independent", "AuthXInd"), cov[i], 
                                                                      ordinal=FALSE), "Marginal", "OLS"))
    }
    return(temp[-1,])
  }
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-rep(c("Democrat", "Republican"))
  temp<-plot.data
  ### Code for Independents
  cov<-c(1)
  defect.margin<-function(a){
    temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                           control=c("republican", "AuthXRep"), 1, 
                                                           ordinal=FALSE), "Marginal", "OLS")
    return(temp)
  }
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-"Independent"
  plot.data<-rbind(temp, plot.data)
  plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
  p3<-plot.data
  
  ## Split voting and directional voting ##
  tt<-as.formula(split~
                   authoritarianism+
                   republican+independent+
                   AuthXRep+
                   AuthXInd+
                   female+age+college+income+
                   catholic+as.factor(year)+other)
  a<-glm(tt, data=hispanic.data, family=binomial("logit"))
  cov<-c(0,1)
  defect.margin<-function(a){
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
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-rep(c("Democrat", "Republican"))
  temp<-plot.data
  ### Code for Independents
  cov<-c(1)
  defect.margin<-function(a){
    temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                           control=c("republican", "AuthXRep"), 1, 
                                                           ordinal=FALSE), "Marginal", "Binary.Logit")
    return(temp)
  }
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-"Independent"
  plot.data<-rbind(temp, plot.data)
  plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
  p4<-plot.data
  ## Directional Voting
  tt<-as.formula(vote~
                   authoritarianism+
                   republican+independent+
                   AuthXRep+
                   AuthXInd+
                   female+age+college+income+
                   catholic+as.factor(year)+other)
  a<-glm(tt, data=hispanic.data, family=binomial("logit"))
  cov<-c(0,1)
  defect.margin<-function(a){
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
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-rep(c("Democrat", "Republican"))
  temp<-plot.data
  ### Code for Independents
  cov<-c(1)
  defect.margin<-function(a){
    temp<-model.prediction(a, design.matrix.int.marginal.2(a, "authoritarianism", "independent", "AuthXInd", 
                                                           control=c("republican", "AuthXRep"), 1, 
                                                           ordinal=FALSE), "Marginal", "Binary.Logit")
    return(temp)
  }
  
  plot.data<-rbind(defect.margin(a))  
  plot.data$PID<-"Independent"
  plot.data<-rbind(temp, plot.data)
  plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
  p5<-plot.data
  ################################################################################################################
  plot.data<-data.frame(rbind(p1, p2, p3, p4, p5))
  plot.data$Behavior<-rep(c("Voted", "Participation",
                            "Efficacy", "Split Ticket Voting",
                            "Republican Vote"), each=3)
  temp<-plot.data
  
  tt<-as.formula(zero.one(conflict1)~
                   authoritarianism+
                   republican+
                   AuthXRep+
                   female+age+college+income+
                   catholic+as.factor(year)+other)
  a<-lm(tt, data=nonwhite.data)
  cov<-c(0,1)
  defect.margin<-function(a){
    temp<-model.prediction(a, 
                           design.matrix.int.marginal.3(a, 
                                                        "authoritarianism", "republican", "AuthXRep", 
                                                        1, 
                                                        ordinal=FALSE), "Marginal", "OLS")
    
    for(i in seq_along(cov)){
      temp<-rbind(temp, model.prediction(a, 
                                         design.matrix.int.marginal.3(a, "authoritarianism", 
                                                                      "republican", "AuthXRep", 
                                                                      cov[i], 
                                                                      ordinal=FALSE), "Marginal", "OLS"))
    }
    return(temp[-1,])
  }
  
  plot.data<-defect.margin(a)
  plot.data$PID<-rep(c("Democrat", "Republican"))
  plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
  p1<-plot.data
  p1$Behavior<-"Party Internal Conflict"
  
  tt<-as.formula(zero.one(conflict2)~
                   authoritarianism+
                   republican+
                   AuthXRep+
                   female+age+college+income+
                   catholic+as.factor(year)+other)
  a<-lm(tt, data=hispanic.data)
  
  cov<-c(0,1)
  defect.margin<-function(a){
    temp<-model.prediction(a, 
                           design.matrix.int.marginal.3(a, 
                                                        "authoritarianism", "republican", "AuthXRep", 
                                                        1, 
                                                        ordinal=FALSE), "Marginal", "OLS")
    
    for(i in seq_along(cov)){
      temp<-rbind(temp, model.prediction(a, 
                                         design.matrix.int.marginal.3(a, "authoritarianism", 
                                                                      "republican", "AuthXRep", 
                                                                      cov[i], 
                                                                      ordinal=FALSE), "Marginal", "OLS"))
    }
    return(temp[-1,])
  }
  
  plot.data<-defect.margin(a)  
  plot.data$PID<-rep(c("Democrat", "Republican"))
  plot.data$party<-recode(plot.data$PID, "'Democrat'='D'; 'Republican'='R'; 'Independent'='I'")
  p2<-plot.data
  p2$Behavior<-"Candidate Internal Conflict"
  plot.data<-rbind(p1, p2)
  plot.data<-rbind(temp, plot.data)
  
  
  plot.data$Behavior <- factor(plot.data$Behavior, c("Republican Vote", "Split Ticket Voting",
                                                     "Candidate Internal Conflict", "Party Internal Conflict",
                                                     "Voted", "Participation", "Efficacy"))
  
  
  plot1<- ggplot(data = subset(plot.data, PID!="Independent"), 
                 aes(x = factor(party), 
                     y = mean.score, ymin=min.2.5, 
                     ymax=max.97.5))+
    facet_wrap(~Behavior, scales="free")+
    geom_point(position = position_dodge(width = 0)) +
    geom_errorbar(position = position_dodge(width = 0.01), width = 0.1) +
    geom_errorbar(aes(ymin=min.25, 
                      ymax=max.75),
                  position = position_dodge(width = 0.01), width = 0.01, size=0.9)+
    theme(text=element_text(size=10), 
          axis.text.y=element_text(angle=45))+
    geom_hline(yintercept=0, linetype="dashed" )+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    theme_bw()+
    # Format the grid
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    ggtitle("Marginal Effect of Authoritarianism. Latino/a and African-American Respondents") +
    theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
    theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
    theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
    scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-1,1))+
    scale_x_discrete("PID")
  
  plot1
  dev.copy(png,'ch8_Pooled.jpg',
           width = 750, height = 500)
  dev.off()
  
  plot1<- ggplot(data =plot.data,
                 aes(x = factor(party), 
                     y = mean.score, ymin=min.2.5, 
                     ymax=max.97.5, colour=Conflict))+
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(position = position_dodge(width = 0.5), width = 0.1) +
    geom_errorbar(aes(ymin=min.25, 
                      ymax=max.75),
                  position = position_dodge(width = 0.5), width = 0.01, size=0.9)+
    theme(text=element_text(size=10), 
          axis.text.y=element_text(angle=45))+
    geom_hline(yintercept=0, linetype="dashed" )+
    theme(panel.background=element_rect(fill="white")) +
    theme(plot.background=element_rect(fill="white")) +
    scale_colour_manual(name="Conflict", values=c("black", "grey79"))+
    theme_bw()+
    # Format the grid
    theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
    theme(axis.ticks=element_blank())+
    ggtitle("Marginal Effect of Authoritarianism. Latino/a and African-American Respondents") +
    theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=11)) +
    theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=0)) +
    theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
    scale_y_continuous("Marginal Effect of Authoritarianism", limits=c(-0.40,.40))+
    scale_x_discrete("PID")
  
  plot1
  dev.copy(png,'ch8_Pooled2.jpg',
           width = 750, height = 500)
  dev.off()
}   






########### END    ######







{    
    #### Replicate the analysis for Latinos/Hispanics ####
    # 1) Jewish=1 is sparse, so drop from analysis 
    # 2) For Hispanics, estimate a demobilization effect
    # 3) For African-Americans, only show the main effect of authoritarianism, since so few Republicans.
    # 4) All analyses are pooled, with a fixed effect for the survey year. 
    #### African American Results ###
    ## Stack results because of no interaction ###
    tt<-as.formula(voted~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=hispanic.data, family=binomial("logit"))
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Binary.Logit")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Binary.Logit"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p1<-defect.prediction(a)
    p1$Behavior<-"Vote"
    
    
    tt<-as.formula(participation~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=hispanic.data, family=poisson())
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Count")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Count"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p2<-defect.prediction(a)
    p2$Behavior<-"Participation"
    
    
    tt<-as.formula(zero.one(efficacy)~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-lm(tt, data=hispanic.data)
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "OLS")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "OLS"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p3<-defect.prediction(a)
    p3$Behavior<-"Efficacy"
    
    tt<-as.formula(split~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=hispanic.data, family=binomial("logit"))
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Binary.Logit")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Binary.Logit"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p4<-defect.prediction(a)
    p4$Behavior<-"Split Ticket Voting"
    
    
    tt<-as.formula(vote~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=hispanic.data, family=binomial("logit"))
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Binary.Logit")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Binary.Logit"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p5<-defect.prediction(a)
    p5$Behavior<-"Republican Voting"
    
    
    tt<-as.formula(zero.one(conflict1)~
                     authoritarianism+
                     republican+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-lm(tt, data=hispanic.data)
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "OLS")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "OLS"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p6<-defect.prediction(a)
    p6$Behavior<-"Partisan Conflict"
    
    
    tt<-as.formula(zero.one(conflict2)~
                     authoritarianism+
                     republican+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-lm(tt, data=hispanic.data)
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "OLS")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "OLS"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p7<-defect.prediction(a)
    p7$Behavior<-"Candidate Conflict"
    
    
    
    plot.data<-rbind(p1, p2, p3, p4, p5, p6, p7)
    
    plot.data$Behavior <- factor(plot.data$Behavior, c("Vote", "Participation",
                                                       "Efficacy", "Split Ticket Voting",
                                                       "Republican Voting", "Partisan Conflict",
                                                       "Candidate Conflict"))
    
    
    
    plot1<-ggplot(data = plot.data,
                  aes(x = authoritarianism, 
                      y = mean.score, ymin=min.2.5, 
                      ymax=max.97.5))+
      facet_wrap(~Behavior, scales="fixed")+
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
      ggtitle("Authoritarianism, Engagement and Defection. Latino/a Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=90)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Predicted Value")+
      scale_x_continuous("Authoritarianism", limits=c(0,1))
    plot1
    
    plot1
    dev.copy(png,'ch8_16.jpg',
             width = 750, height = 500)
    dev.off()
    
    
    
    
    
      }   
#### African American Results ###
{
    tt<-as.formula(voted~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=black.data, family=binomial("logit"))
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Binary.Logit")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Binary.Logit"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p1<-defect.prediction(a)
    p1$Behavior<-"Vote"
    
    
    tt<-as.formula(participation~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=black.data, family=poisson())
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Count")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Count"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p2<-defect.prediction(a)
    p2$Behavior<-"Participation"
    
    
    tt<-as.formula(zero.one(efficacy)~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-lm(tt, data=black.data)
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "OLS")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "OLS"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p3<-defect.prediction(a)
    p3$Behavior<-"Efficacy"
    
    tt<-as.formula(split~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=black.data, family=binomial("logit"))
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Binary.Logit")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Binary.Logit"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p4<-defect.prediction(a)
    p4$Behavior<-"Split Ticket Voting"
    
    
    tt<-as.formula(vote~
                     authoritarianism+
                     republican+independent+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-glm(tt, data=black.data, family=binomial("logit"))
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "Binary.Logit")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "Binary.Logit"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p5<-defect.prediction(a)
    p5$Behavior<-"Republican Voting"
    
    
    tt<-as.formula(zero.one(conflict1)~
                     authoritarianism+
                     republican+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-lm(tt, data=black.data)
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "OLS")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "OLS"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p6<-defect.prediction(a)
    p6$Behavior<-"Partisan Conflict"
    
    
    tt<-as.formula(zero.one(conflict2)~
                     authoritarianism+
                     republican+
                     female+age+college+income+
                     catholic+as.factor(year)+other)
    a<-lm(tt, data=black.data)
    
    defect.prediction<-function(a){ 
      data<-model.matrix(a)
      tt3<-seq(min(data[,"authoritarianism"], na.rm=T), max(data[,"authoritarianism"], na.rm=T), by=0.01) 
      ## List of design matrices
      temp<-model.prediction(a, design.matrix.predictive(a, "authoritarianism", 1, FALSE),
                             "Predictive", "OLS")
      for(i in 1:length(tt3)){
        temp<-rbind(temp, model.prediction(a, design.matrix.predictive(a, "authoritarianism", 
                                                                       tt3[i], FALSE),
                                           "Predictive", "OLS"))
      }
      temp<-temp[-1,]
      temp$authoritarianism<-tt3
      return(as.data.frame(temp))
    }
    p7<-defect.prediction(a)
    p7$Behavior<-"Candidate Conflict"
    
    
    
    plot.data<-rbind(p1, p2, p3, p4, p5, p6, p7)
    
    plot.data$Behavior <- factor(plot.data$Behavior, c("Vote", "Participation",
                                                       "Efficacy", "Split Ticket Voting",
                                                       "Republican Voting", "Partisan Conflict",
                                                       "Candidate Conflict"))
    
    
    
    plot1<-ggplot(data = plot.data,
                  aes(x = authoritarianism, 
                      y = mean.score, ymin=min.2.5, 
                      ymax=max.97.5))+
      facet_wrap(~Behavior, scales="fixed")+
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
      ggtitle("Authoritarianism, Engagement and Defection. African-American Respondents") +
      theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
      theme(axis.text.x=element_text(size=11,colour="#535353",face="bold", angle=90)) +
      theme(axis.text.y=element_text(size=11, colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
      scale_y_continuous("Predicted Value")+
      scale_x_continuous("Authoritarianism", limits=c(0,1))
    plot1
    
    plot1
    dev.copy(png,'ch8_17.jpg',
             width = 750, height = 500)
    dev.off()
}
####### Chapter 9  ######

    
    
