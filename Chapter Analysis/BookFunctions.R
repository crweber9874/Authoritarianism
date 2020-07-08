
zero.one<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}

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

plot.f2<-function(lab1, lab2, lab3){
  p1<-ggplot(p3,aes(x=authoritarianism,y=cat1.mean.score
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
  
  p2<-ggplot(p4,aes(x=authoritarianism,y=cat1.mean.score,
                    fill=Category)) + 
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

plot.f3<-function(lab1, lab2, lab3){
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


# Start with a matrix with no interactions
# Function for both ordinal and non-ordinal data. Need to specify model object, covariate name, value, and whether intercept/ordinal is specified
design.matrix.predictive<-function(output, covariate.name, covariate.value, ordinal=FALSE){
  if(ordinal==FALSE){
    temp1<-model.matrix(output)
    temp1[,covariate.name]<-covariate.value
  }
  if(ordinal==TRUE){
    temp1<-model.matrix(output)[,-1]
    temp1[,covariate.name]<-covariate.value
  }
  return(temp1)
}
# Create a Marginal Effect Design Matrix for a "Main Effect" Model
# 2 Discrete Marginal Effects: Max-Min and 75 to 25. 
# Marginal for both ordinal and non-ordinal data. Need to specify model object, covariate name, value, and whether intercept/ordinal is specified
design.matrix.marginal<-function(output, covariate.name, ordinal=FALSE){
  if(ordinal==FALSE){
    temp1<-model.matrix(output)
    temp1[,covariate.name]<-max(temp1[,covariate.name], na.rm=T)
    temp2<-model.matrix(output)
    temp2[,covariate.name]<-min(temp2[,covariate.name], na.rm=T)
    p1<-temp1
    o1<-temp2
    temp1<-model.matrix(output)
    temp1[,covariate.name]<-quantile(temp1[,covariate.name], probs=0.75, na.rm=T)
    temp2<-model.matrix(output)
    temp2[,covariate.name]<-quantile(temp2[,covariate.name], probs=0.25, na.rm=T)
    p2<-temp1
    o2<-temp2
  }
  if(ordinal==TRUE){
    temp1<-model.matrix(output)[,-1]
    temp1[,covariate.name]<-max(temp1[,covariate.name], na.rm=T)
    temp2<-model.matrix(output)[,-1]
    temp2[,covariate.name]<-min(temp2[,covariate.name], na.rm=T)
    p1<-temp1
    o1<-temp2
    temp1<-model.matrix(output)[,-1]
    temp1[,covariate.name]<-quantile(temp1[,covariate.name], probs=0.75, na.rm=T)
    temp2<-model.matrix(output)[,-1]
    temp2[,covariate.name]<-quantile(temp2[,covariate.name], probs=0.25, na.rm=T)
    p2<-temp1
    o2<-temp2
  }
  
  return(list(p1=p1, o1=o1, p2=p2, o2=o2))
}
## This works and can send the design matrix model.prediction
# Key: n1:n3, names of intervention variables
# n1=IV1, n2=IV1xModerator, n3=Interaction; n4=
# Marginal needs to specify value of the moderator
# I've created different predictive and marginal interaction models. This was a bit more involved.
design.matrix.int.predictive<-function(output, names=c(n1, n2, n3), values=c(v1, v2, v3),
                                       ordinal=FALSE){
  if(ordinal==FALSE){
    temp1<-model.matrix(output)
    temp1[,names[1]]<-values[1]
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[3]
  }
  
  if(ordinal==TRUE){
    temp1<-model.matrix(output)[,-1]
    temp1[,names[1]]<-values[1]
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[3]
  }
  return(temp1)
}
design.matrix.int.marginal<-function(output, iv.name, mod.name, int.name,  
                                     value.moderator,ordinal=FALSE){
  paste("The independent variable is", iv.name)
  paste("The moderator variable is", mod.name)
  if(ordinal==FALSE){
    temp<-model.matrix(output)
    temp1<-model.matrix(output)
    temp1[,iv.name]<-max(temp[,iv.name], na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*max(temp[,iv.name], na.rm=T)
    temp2<-model.matrix(output)
    temp2[,iv.name]<-min(temp[,iv.name], na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*min(temp[,iv.name], na.rm=T)
    p1<-temp1
    o1<-temp2
    temp1<-model.matrix(output)
    temp1[,iv.name]<-quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp2<-model.matrix(output)
    temp2[,iv.name]<-quantile(temp[,iv.name], probs=0.25, na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.25, na.rm=T)
    p2<-temp1
    o2<-temp2
  }
  if(ordinal==TRUE){
    temp<-model.matrix(output)[,-1]
    temp1<-model.matrix(output)[,-1]
    temp1[,iv.name]<-max(temp[,iv.name], na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*max(temp[,iv.name], na.rm=T)
    temp2<-model.matrix(output)[,-1]
    temp2[,iv.name]<-min(temp[,iv.name], na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*min(temp[,iv.name], na.rm=T)
    p1<-temp1
    o1<-temp2
    temp1<-model.matrix(output)[,-1]
    temp1[,iv.name]<-quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp2<-model.matrix(output)[,-1]
    temp2[,iv.name]<-quantile(temp[,iv.name], probs=0.25, na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.25, na.rm=T)
    p2<-temp1
    o2<-temp2
  }
  
  return(list(p1=p1, o1=o1, p2=p2, o2=o2))
}

### This too has to be set to a value. It's an interaction with dummies!
design.matrix.int.predictive.2<-function(output, names=c(n1, n2, n3, n4, n5), 
                                         values=c(v1, v2, v3, v4, v5),
                                         ordinal=FALSE){
  if(ordinal==FALSE){
    temp1<-model.matrix(output)
    temp1[,names[1]]<-values[1]
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[3]
    temp1[,names[4]]<-values[4]
    temp1[,names[5]]<-values[5]
  }
  
  if(ordinal==TRUE){
    temp1<-model.matrix(output)[,-1]
    temp1[,names[1]]<-values[1]
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[3]
    temp1[,names[4]]<-values[4]
    temp1[,names[5]]<-values[5]
  }
  return(as.matrix(temp1))
}
beta.design.matrix.int.marginal.2<-function(output,  names=c(n1, n2, n3, n4, n5), 
                                            values=c(v1, v2, v3, v4,v5),
                                            ordinal=FALSE){
  if(ordinal==FALSE){
    temp1<-model.matrix(output)
    temp1[,names[1]]<-1
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[3]
    temp1[,names[4]]<-values[4]
    temp1[,names[5]]<-values[5]
    
    temp2<-model.matrix(output)
    temp2[,names[1]]<-0
    temp2[,names[2]]<-values[2]
    temp2[,names[3]]<-values[3]
    temp2[,names[4]]<-values[4]
    temp2[,names[5]]<-values[5]
    
    p1<-temp1
    o1<-temp2
    
  }
  if(ordinal==TRUE){
    temp1<-model.matrix(output)[,-1]
    temp1[,names[1]]<-1
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[3]
    temp1[,names[4]]<-values[4]
    temp1[,names[5]]<-values[5]
    
    temp2<-model.matrix(output)[,11]
    temp2[,names[1]]<-0
    temp2[,names[2]]<-values[2]
    temp2[,names[3]]<-values[3]
    temp2[,names[4]]<-values[4]
    temp2[,names[5]]<-values[5]
    
    
    
    p1<-temp1
    o1<-temp2
    
    
  }
  
  return(list(p1=p1, o1=o1))
}

design.matrix.int.marginal.2<-function(output, iv.name, mod.name, int.name, 
                                       control=c(c.name, c.int.name),
                                       value.moderator,ordinal=FALSE){
  #print("The independent variable is", iv.name, "")
  #print("The moderator variable is", mod.name, "")
  #print("The controlxModerator", control[2], "")
  #print("The control name", control[1], "")
  if(ordinal==FALSE){
    temp1<-model.matrix(output)
    temp1[,iv.name]<-max(temp1[,iv.name], na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*max(temp1[,iv.name], na.rm=T)
    temp1[, control[2]]<-max(temp1[,iv.name], na.rm=T)*temp1[,control[1]] ### This is the only addition
    
    temp2<-model.matrix(output)
    temp2[,iv.name]<-min(temp2[,iv.name], na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*min(temp2[,iv.name], na.rm=T)
    temp2[,control[2]]<-min(temp2[,iv.name], na.rm=T)*temp2[,control[1]] ### This is the only addition
    
    
    p1<-temp1
    o1<-temp2
    
  }
  if(ordinal==TRUE){
    temp1<-model.matrix(output)[,-1]
    temp1[,iv.name]<-max(temp1[,iv.name], na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*max(temp1[,iv.name], na.rm=T)
    temp1[, control[2]]<-max(temp1[,iv.name], na.rm=T)*temp1[,control[1]] ### This is the only addition
    
    temp2<-model.matrix(output)[,-1]
    temp2[,iv.name]<-min(temp2[,iv.name], na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*min(temp2[,iv.name], na.rm=T)
    temp2[,control[2]]<-min(temp2[,iv.name], na.rm=T)*temp2[,control[1]] ### This is the only addition
    
    
    
  }
  
  return(list(p1=p1, o1=o1))
}

design.matrix.int.predictive.3<-function(output, names=c(n1, n2, n3), 
                                         values=c(v1, v2, v3),
                                         ordinal=FALSE){
  if(ordinal==FALSE){
    temp1<-model.matrix(output)
    temp1[,names[1]]<-values[1]
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[1]*values[2]
  }
  
  if(ordinal==TRUE){
    temp1<-model.matrix(output)[,-1]
    temp1[,names[1]]<-values[1]
    temp1[,names[2]]<-values[2]
    temp1[,names[3]]<-values[1]*values[2]
  }
  return(as.matrix(temp1))  ## Delete if problematic
}
design.matrix.int.marginal.3<-function(output, iv.name, mod.name, int.name, 
                                       value.moderator,ordinal=FALSE){
  #print("The independent variable is", iv.name, "")
  #print("The moderator variable is", mod.name, "")
  #print("The controlxModerator", control[2], "")
  #print("The control name", control[1], "")
  if(ordinal==FALSE){
    temp<-model.matrix(output)
    temp1<-model.matrix(output)
    temp1[,iv.name]<-max(temp[,iv.name], na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*max(temp[,iv.name], na.rm=T)
    
    temp2<-model.matrix(output)
    temp2[,iv.name]<-min(temp[,iv.name], na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*min(temp[,iv.name], na.rm=T)
    
    
    p1<-temp1
    o1<-temp2
    
    temp1<-model.matrix(output)
    temp1[,iv.name]<-quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp2<-model.matrix(output)
    temp2[,iv.name]<-quantile(temp[,iv.name], probs=0.25,na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.25, na.rm=T)
    p2<-temp1
    o2<-temp2
  }
  if(ordinal==TRUE){
    temp<-model.matrix(output)[,-1]
    temp1<-model.matrix(output)[,-1]
    temp1[,iv.name]<-max(temp[,iv.name], na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*max(temp[,iv.name], na.rm=T)
    temp2<-model.matrix(output)
    temp2[,iv.name]<-min(temp[,iv.name], na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*min(temp[,iv.name], na.rm=T)
    p1<-temp1
    o1<-temp2
    
    temp1<-model.matrix(output)[,-1]
    temp1[,iv.name]<-quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp1[,mod.name]<-value.moderator
    temp1[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.75, na.rm=T)
    temp2<-model.matrix(output)
    temp2[,iv.name]<-quantile(temp[,iv.name], probs=0.25,na.rm=T)
    temp2[,mod.name]<-value.moderator
    temp2[,int.name]<-value.moderator*quantile(temp[,iv.name], probs=0.25, na.rm=T)
    p2<-temp1
    o2<-temp2
  }
  
  return(list(p1=p1, o1=o1, p2=p2, o2=o2))
}

## Chapter 5 Functions ###
pred.pid.noint<-function(output){
  require(MASS)
  t<-model.matrix(output)
  p<-cbind(
    1,
    1,  #High Authoritarianism
    t[,3:dim(t)[2]]
  )
  o<-cbind(
    1,
    0,  #Low Authoritarianism
    t[,3:dim(t)[2]]
  )
  beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
  ###Generate the Predictions###
  g1.ind.p<-apply(p%*%t(beta.sim[,1:9]), 2, mean)
  g1.rep.p<-apply(p%*%t(beta.sim[,10:18]), 2, mean)
  g1.ind.o<-apply(o%*%t(beta.sim[,1:9]), 2, mean)
  g1.rep.o<-apply(o%*%t(beta.sim[,10:18]), 2, mean)
  
  pred.dem.p<-1/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.ind.p<-exp(g1.ind.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.rep.p<-exp(g1.rep.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  
  pred.dem.o<-1/(1+exp(g1.ind.o)+exp(g1.rep.o))
  pred.ind.o<-exp(g1.ind.o)/(1+exp(g1.ind.o)+exp(g1.rep.o))
  pred.rep.o<-exp(g1.rep.o)/(1+exp(g1.ind.o)+exp(g1.rep.o))
  return(data.frame(dem.h=pred.dem.p, ind.h=pred.ind.p, rep.h=pred.rep.p,
                    dem.l=pred.dem.o, ind.l=pred.ind.o, rep.l=pred.rep.o))
}
### Chapter 7 Functions
#### Zero one recode 
### Function to create Figure 1, Plot mean of factor models across groups
pred.ols.ideo<-function(output){
  require(MASS)
  t<-model.matrix(output)
  rep<-cbind(
    t[,1],
    1,
    0,
    t[,4:ncol(t)])
  ind<-cbind(
    t[,1],
    0,
    1,
    t[,4:ncol(t)])
  dem<-cbind(
    t[,1],
    0,
    0,
    t[,4:ncol(t)])
  sim.beta <- mvrnorm(1000,coef(output),vcov(output)) ##Simulate Draws from a multivariate normal
  pp<-apply(rep%*%t(sim.beta), 2, mean) ### The average effect. Simulate b and auth
  oo<-apply(ind%*%t(sim.beta), 2, mean) ### The average effect. Simulate b and auth
  tt<-apply(dem%*%t(sim.beta), 2, mean) ### The average effect. Simulate b and auth
  return(list(data.frame(min.2.5=quantile(pp, 0.025),
                         min.25=quantile(pp, 0.25),
                         max.75= quantile(pp, 0.75),
                         max.97.5=quantile(pp, 0.975),
                         mean.score=quantile(pp, 0.5)),
              data.frame(min.2.5=quantile(oo, 0.025),
                         min.25=quantile(oo, 0.25),
                         max.75= quantile(oo, 0.75),
                         max.97.5=quantile(oo, 0.975),
                         mean.score=quantile(oo, 0.5)),
              data.frame(min.2.5=quantile(tt, 0.025),
                         min.25=quantile(tt, 0.25),
                         max.75= quantile(tt, 0.75),
                         max.97.5=quantile(tt, 0.975),
                         mean.score=quantile(tt, 0.5))
              
  ))
}
### Function to create Figure 1, Plot mean of factor models across groups
pred.ols.authoritarianism<-function(output){
  require(MASS)
  t<-model.matrix(output)
  p<-cbind(
    t[,1],
    1,
    t[,3:ncol(t)])
  o<-cbind(
    t[,1],
    0,
    t[,3:ncol(t)])
  sim.beta <- mvrnorm(1000,coef(output),vcov(output)) ##Simulate Draws from a multivariate normal
  pp<-apply(p%*%t(sim.beta), 2, mean) ### The average effect. Simulate b and auth
  oo<-apply(o%*%t(sim.beta), 2, mean) ### The average effect. Simulate b and auth
  return(list(data.frame(min.2.5=quantile(pp, 0.025),
                         min.25=quantile(pp, 0.25),
                         max.75= quantile(pp, 0.75),
                         max.97.5=quantile(pp, 0.975),
                         mean.score=quantile(pp, 0.5)),
              data.frame(min.2.5=quantile(oo, 0.025),
                         min.25=quantile(oo, 0.25),
                         max.75= quantile(oo, 0.75),
                         max.97.5=quantile(oo, 0.975),
                         mean.score=quantile(oo, 0.5))
              
  ))
}
####Function to show different reliance on social fiscal issues over time ####
pid.alignment<-function(output, type="social"){
  require(MASS)
  t<-model.matrix(output)
  if(type=="social"){
    p<-cbind(
      1,
      1,  #High Social
      t[,3:dim(t)[2]]
    )
    o<-cbind(
      1,
      0,  #Low Social
      t[,3:dim(t)[2]]
    )
  }
  if(type=="fiscal"){
    p<-cbind(
      1,
      t[,2],
      1,
      t[,4:dim(t)[2]]
    )
    o<-cbind(
      1,
      t[,2],
      0,
      t[,4:dim(t)[2]]
    )
  }
  beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
  ###Generate the Predictions###
  g1.ind.p<-apply(p%*%t(beta.sim[,1:10]), 2, mean)
  g1.rep.p<-apply(p%*%t(beta.sim[,11:20]), 2, mean)
  g1.ind.o<-apply(o%*%t(beta.sim[,1:10]), 2, mean)
  g1.rep.o<-apply(o%*%t(beta.sim[,11:20]), 2, mean)
  pred.dem.p<-1/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.ind.p<-exp(g1.ind.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.rep.p<-exp(g1.rep.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.dem.o<-1/(1+exp(g1.ind.o)+exp(g1.rep.o))
  pred.ind.o<-exp(g1.ind.o)/(1+exp(g1.ind.o)+exp(g1.rep.o))
  pred.rep.o<-exp(g1.rep.o)/(1+exp(g1.ind.o)+exp(g1.rep.o))
  
  g1<-pred.dem.p-pred.dem.o
  g2<-pred.ind.p-pred.ind.o
  g3<-pred.rep.p-pred.rep.o
  
  return(list(    dem.margin=data.frame(min.2.5=quantile(g1, 0.025),
                                        min.25=quantile(g1, 0.25),
                                        max.75=quantile(g1, 0.75),
                                        max.97.5=quantile(g1, 0.975),
                                        mean.score=quantile(g1, 0.5)),
                  ind.margin=data.frame(min.2.5=quantile(g2, 0.025),
                                        min.25=quantile(g2, 0.25),
                                        max.75=quantile(g2, 0.75),
                                        max.97.5=quantile(g2, 0.975),
                                        mean.score=quantile(g2, 0.5)),
                  rep.margin=data.frame(min.2.5=quantile(g3, 0.025),
                                        min.25=quantile(g3, 0.25),
                                        max.75=quantile(g3, 0.75),
                                        max.97.5=quantile(g3, 0.975),
                                        mean.score=quantile(g3, 0.5))
  ))
}
####Function to show different reliance on social fiscal issues x authoritarianism ####
pid.alignment.auth2<-function(output, type="social", authoritarian="authoritarian", val, beta.sim){
  require(MASS)
  t<-model.matrix(output)
  if(type=="social" & authoritarian=="authoritarian"){
    p<-cbind(
      1,
      val,      #High Social
      t[,3],
      1,      #Authoritarian
      val * 1,  #High social x Authoritarian
      t[,3] * 1, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }
  if(type=="social" & authoritarian=="non-authoritarian"){
    p<-cbind(
      1,
      val,      #High Social
      t[,3],
      0,      #Authoritarian
      val * 0,  #High social x Authoritarian
      t[,3] * 0, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }
  if(type=="fiscal" & authoritarian=="authoritarian"){
    p<-cbind(
      1,
      t[,2],      # Social
      val,      # High Fiscal
      1,      #Authoritarian
      t[,2] * 1,  # social x Authoritarian
      val * 1, # High Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }  
  if(type=="fiscal" & authoritarian=="non-authoritarian"){
    p<-cbind(
      1,
      t[,2],      # Social
      val,      # High Fiscal
      0,      #Non Authoritarian
      t[,2] * 0,  # social x Authoritarian
      val * 0, # High Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }  
  ###Generate the Predictions###
  g1.ind.p<-apply(p%*%t(beta.sim[,1:13]), 2, mean)
  g1.rep.p<-apply(p%*%t(beta.sim[,14:26]), 2, mean)
  pred.dem.p<-1/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.ind.p<-exp(g1.ind.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.rep.p<-exp(g1.rep.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  g1<-pred.dem.p
  g2<-pred.ind.p
  g3<-pred.rep.p
  
  return(list(    dem.pred=data.frame(min.2.5=quantile(g1, 0.025),
                                      min.25=quantile(g1, 0.25),
                                      max.75=quantile(g1, 0.75),
                                      max.97.5=quantile(g1, 0.975),
                                      mean.score=quantile(g1, 0.5)),
                  ind.pred=data.frame(min.2.5=quantile(g2, 0.025),
                                      min.25=quantile(g2, 0.25),
                                      max.75=quantile(g2, 0.75),
                                      max.97.5=quantile(g2, 0.975),
                                      mean.score=quantile(g2, 0.5)),
                  rep.pred=data.frame(min.2.5=quantile(g3, 0.025),
                                      min.25=quantile(g3, 0.25),
                                      max.75=quantile(g3, 0.75),
                                      max.97.5=quantile(g3, 0.975),
                                      mean.score=quantile(g3, 0.5))
  ))
}
####Function to show different reliance on social fiscal issues x authoritarianism ####
pid.alignment.auth3<-function(output, authoritarian="authoritarian", val, beta.sim){
  require(MASS)
  t<-model.matrix(output)
  if(authoritarian=="authoritarian"){
    p<-cbind(
      1,
      val,     
      1,      #Authoritarian
      val * 1,  #High social x Authoritarian
      t[,5:dim(t)[2]]
    )
  }
  if(authoritarian=="non-authoritarian"){
    p<-cbind(
      1,
      val,     
      0,      #Authoritarian
      val * 0,  #High social x Authoritarian
      t[,5:dim(t)[2]]
      
    )
  }
  ###Generate the Predictions###
  g1.ind.p<-apply(p%*%t(beta.sim[,1:11]), 2, mean)
  g1.rep.p<-apply(p%*%t(beta.sim[,12:22]), 2, mean)
  pred.dem.p<-1/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.ind.p<-exp(g1.ind.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.rep.p<-exp(g1.rep.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  g1<-pred.dem.p
  g2<-pred.ind.p
  g3<-pred.rep.p
  
  
  
  return(list(    dem.pred=data.frame(min.2.5=quantile(g1, 0.025),
                                      min.25=quantile(g1, 0.25),
                                      max.75=quantile(g1, 0.75),
                                      max.97.5=quantile(g1, 0.975),
                                      mean.score=quantile(g1, 0.5)),
                  ind.pred=data.frame(min.2.5=quantile(g2, 0.025),
                                      min.25=quantile(g2, 0.25),
                                      max.75=quantile(g2, 0.75),
                                      max.97.5=quantile(g2, 0.975),
                                      mean.score=quantile(g2, 0.5)),
                  rep.pred=data.frame(min.2.5=quantile(g3, 0.025),
                                      min.25=quantile(g3, 0.25),
                                      max.75=quantile(g3, 0.75),
                                      max.97.5=quantile(g3, 0.975),
                                      mean.score=quantile(g3, 0.5))
  ))
}
####Function to show different reliance on social fiscal issues x authoritarianism ####
pid.alignment.auth<-function(output, type="social", authoritarian="authoritarian"){
  require(MASS)
  t<-model.matrix(output)
  if(type=="social" & authoritarian=="authoritarian"){
    p<-cbind(
      1,
      1,      #High Social
      t[,3],
      1,      #Authoritarian
      1 * 1,  #High social x Authoritarian
      t[,3] * 1, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
    o<-cbind(
      1,
      0,      #Low Social
      t[,3],
      1,      #Authoritarian
      0 * 1,  #Low social x Authoritarian
      t[,3] * 1, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }
  if(type=="social" & authoritarian=="non-authoritarian"){
    p<-cbind(
      1,
      1,      #High Social
      t[,3],
      0,      #Authoritarian
      1 * 0,  #High social x Authoritarian
      t[,3] * 0, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
    o<-cbind(
      1,
      0,      #Low Social
      t[,3],
      0,      #Authoritarian
      0 * 0,  #Low social x Authoritarian
      t[,3] * 0, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }
  if(type=="fiscal" & authoritarian=="authoritarian"){
    p<-cbind(
      1,
      t[,2],      # Social
      1,      # High Fiscal
      1,      #Authoritarian
      t[,2] * 1,  # social x Authoritarian
      1 * 1, # High Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
    o<-cbind(
      1,
      t[,2],      #Low Social
      0,      #Low Fiscal
      1,      #Authoritarian
      t[,2] * 1,  # social x Authoritarian
      0 * 1, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }  
  if(type=="fiscal" & authoritarian=="non-authoritarian"){
    p<-cbind(
      1,
      1,      # Social
      1,      # High Fiscal
      0,      #Authoritarian
      t[,2] * 0,  # social x Authoritarian
      1 * 0, # High Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
    o<-cbind(
      1,
      t[,2],      #Low Social
      0,      #Low Fiscal
      0,      #Authoritarian
      t[,2] * 0,  # social x Authoritarian
      0 * 0, #Fiscal x authoritarian
      t[,7:dim(t)[2]]
    )
  }  
  beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
  ###Generate the Predictions###
  g1.ind.p<-apply(p%*%t(beta.sim[,1:13]), 2, mean)
  g1.rep.p<-apply(p%*%t(beta.sim[,14:26]), 2, mean)
  g1.ind.o<-apply(o%*%t(beta.sim[,1:13]), 2, mean)
  g1.rep.o<-apply(o%*%t(beta.sim[,14:26]), 2, mean)
  pred.dem.p<-1/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.ind.p<-exp(g1.ind.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.rep.p<-exp(g1.rep.p)/(1+exp(g1.ind.p)+exp(g1.rep.p))
  pred.dem.o<-1/(1+exp(g1.ind.o)+exp(g1.rep.o))
  pred.ind.o<-exp(g1.ind.o)/(1+exp(g1.ind.o)+exp(g1.rep.o))
  pred.rep.o<-exp(g1.rep.o)/(1+exp(g1.ind.o)+exp(g1.rep.o))
  
  g1<-pred.dem.p-pred.dem.o
  g2<-pred.ind.p-pred.ind.o
  g3<-pred.rep.p-pred.rep.o
  
  return(list(    dem.margin=data.frame(min.2.5=quantile(g1, 0.025),
                                        min.25=quantile(g1, 0.25),
                                        max.75=quantile(g1, 0.75),
                                        max.97.5=quantile(g1, 0.975),
                                        mean.score=quantile(g1, 0.5)),
                  ind.margin=data.frame(min.2.5=quantile(g2, 0.025),
                                        min.25=quantile(g2, 0.25),
                                        max.75=quantile(g2, 0.75),
                                        max.97.5=quantile(g2, 0.975),
                                        mean.score=quantile(g2, 0.5)),
                  rep.margin=data.frame(min.2.5=quantile(g3, 0.025),
                                        min.25=quantile(g3, 0.25),
                                        max.75=quantile(g3, 0.75),
                                        max.97.5=quantile(g3, 0.975),
                                        mean.score=quantile(g3, 0.5))
  ))
}
### Plot marginal effects at max differences and no difference ###
logit.marginal.pid<-function(output, group="Republican", eval.cov="Republican"){
  require(MASS)
  t<-model.matrix(output)
  if(group=="Independent" & eval.cov=="Republican"){
    p<-cbind(
      1,
      t[,2],
      0, # Republican
      1, # Independent
      t[,5],  # Ideology D
      1,  # Ideology R
      t[,5]*0,# Ideology D x Rep
      t[,5]*1,# Ideology D x Ind 
      1*0,# Ideology R x Rep 
      1*1,# Ideology R x Ind
      t[,11:dim(t)[2]])
    o<-cbind(
      1,
      t[,2],
      0, # Republican
      1, # Independent
      t[,5],  # Ideology D
      0,  # Ideology R
      t[,5]*0,# Ideology D x Rep
      t[,5]*1,# Ideology D x Ind 
      0*0,# Ideology R x Rep 
      0*1,# Ideology R x Ind
      t[,11:dim(t)[2]])
  }    
  if(group=="Independent" & eval.cov=="Democrat"){
    p<-cbind(
      1,
      t[,2],
      0, # Republican
      1, # Independent
      1,  # Ideology D
      t[,6],  # Ideology R
      1*0,# Ideology D x Rep
      1*1,# Ideology D x Ind 
      t[,6]*0,# Ideology R x Rep 
      t[,6]*1,# Ideology R x Ind
      t[,11:dim(t)[2]])
    o<-cbind(
      1,
      t[,2],
      0, # Republican
      1, # Independent
      0,  # Ideology D
      t[,6],  # Ideology R
      0*0,# Ideology D x Rep
      0*1,# Ideology D x Ind 
      t[,6]*0,# Ideology R x Rep 
      t[,6]*1,# Ideology R x Ind
      t[,11:dim(t)[2]])
  }  
  if(group=="Republican" & eval.cov=="Republican"){
    p<-cbind(
      1,
      t[,2],
      1, # Republican
      0, # Independent
      t[,5],  # Ideology D
      1,  # Ideology R
      t[,5]*1,# Ideology D x Rep
      t[,5]*0,# Ideology D x Ind 
      1*1,# Ideology R x Rep 
      1*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
    o<-cbind(
      1,
      t[,2],
      1, # Republican
      0, # Independent
      t[,5],  # Ideology D
      0,  # Ideology R
      t[,5]*1,# Ideology D x Rep
      t[,5]*0,# Ideology D x Ind 
      0*1,# Ideology R x Rep 
      0*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
  }    
  if(group=="Republican" & eval.cov=="Democrat"){
    p<-cbind(
      1,
      t[,2],
      1, # Republican
      0, # Independent
      1,  # Ideology D
      t[,6],  # Ideology R
      1*1,# Ideology D x Rep
      1*0,# Ideology D x Ind 
      t[,6]*1,# Ideology R x Rep 
      t[,6]*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
    o<-cbind(
      1,
      t[,2],
      1, # Republican
      0, # Independent
      0,  # Ideology D
      t[,6],  # Ideology R
      0*1,# Ideology D x Rep
      0*0,# Ideology D x Ind 
      t[,6]*1,# Ideology R x Rep 
      t[,6]*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
  }    
  if(group=="Democrat" & eval.cov=="Republican"){
    p<-cbind(
      1,
      t[,2],
      0, # Republican
      0, # Independent
      t[,5],  # Ideology D
      1,  # Ideology R
      t[,5]*0,# Ideology D x Rep
      t[,5]*0,# Ideology D x Ind 
      1*0,# Ideology R x Rep 
      1*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
    o<-cbind(
      1,
      t[,2],
      0, # Republican
      0, # Independent
      t[,5],  # Ideology D
      0,  # Ideology R
      t[,5]*0,# Ideology D x Rep
      t[,5]*0,# Ideology D x Ind 
      0*0,# Ideology R x Rep 
      0*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
  }    
  if(group=="Democrat" & eval.cov=="Democrat"){
    p<-cbind(
      1,
      t[,2],
      0, # Republican
      0, # Independent
      1,  # Ideology D
      t[,6],  # Ideology R
      1*0,# Ideology D x Rep
      1*0,# Ideology D x Ind 
      t[,6]*0,# Ideology R x Rep 
      t[,6]*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
    o<-cbind(
      1,
      t[,2],
      0, # Republican
      0, # Independent
      0,  # Ideology D
      t[,6],  # Ideology R
      0*0,# Ideology D x Rep
      0*0,# Ideology D x Ind 
      t[,6]*0,# Ideology R x Rep 
      t[,6]*0,# Ideology R x Ind
      t[,11:dim(t)[2]])
  }    
  beta.sim<-mvrnorm(1000, c(output$coefficients), vcov(output)) ##Draw samples from multivariate distrbution
  g1<-apply(plogis(p%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  g2<-apply(plogis(o%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  p1<-g1-g2
  return(list(marginal=data.frame(min.2.5=quantile(p1, 0.025),
                                  min.25=quantile(p1, 0.25),
                                  max.75= quantile(p1, 0.75),
                                  max.97.5=quantile(p1, 0.975),
                                  mean.score=quantile(p1, 0.5)),
              high=data.frame(min.2.5=quantile(g1, 0.025),
                              min.25=quantile(g1, 0.25),
                              max.75= quantile(g1, 0.75),
                              max.97.5=quantile(g1, 0.975),
                              mean.score=quantile(g1, 0.5)),
              low=data.frame(min.2.5=quantile(g2, 0.025),
                             min.25=quantile(g2, 0.25),
                             max.75= quantile(g2, 0.75),
                             max.97.5=quantile(g2, 0.975),
                             mean.score=quantile(g2, 0.5))
              
  ))
}
### Poisson Model ###
predict.zinb<-function(output, level="zero"){
  require(MASS)
  if(level=="count"){
    t<-model.matrix(output)
    p<-cbind(
      1,
      1,  #High Authoritarianism
      t[,3:dim(t)[2]])
    o<-cbind(
      1,
      0,  #low Authoritarianism
      t[,3:dim(t)[2]])
    beta.sim<-mvrnorm(1000, output$coefficients$count, vcov(output)[c(1:9),c(1:9)]) ##Draw samples from multivariate distrbution
    g1<-apply(exp(p%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    g2<-apply(exp(o%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    p1<-g1-g2

  }
  if(level=="zero"){
    t<-model.matrix(output)
    p<-cbind(
      1,
      1,  #High Authoritarianism
      t[,3:dim(t)[2]])
    o<-cbind(
      1,
      0,  #low Authoritarianism
      t[,3:dim(t)[2]])
    beta.sim<-mvrnorm(1000, output$coefficients$zero, vcov(output)[c(10:18),c(10:18)]) ##Draw samples from multivariate distrbution
    g1<-1-apply(plogis(p%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    g2<-1-apply(plogis(o%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    p1<-g1-g2
  }
  return(data.frame(min.2.5=quantile(p1, 0.025),
                    min.25=quantile(p1, 0.25),
                    max.75= quantile(p1, 0.75),
                    max.97.5=quantile(p1, 0.975),
                    mean.score=quantile(p1, 0.5)))
}  
### Poisson Model ###
predict.zinb2<-function(output, auth){
  require(MASS)
    set.seed(123)
    t<-model.matrix(output)
    p<-cbind(
      1,
      auth,  #High Authoritarianism
      t[,3:dim(t)[2]])
    beta.sim<-mvrnorm(1000, output$coefficients$count, vcov(output)[c(1:9),c(1:9)]) ##Draw samples from multivariate distrbution
    p1<-apply(exp(p%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  return(data.frame(min.2.5=quantile(p1, 0.025),
                    min.25=quantile(p1, 0.25),
                    max.75= quantile(p1, 0.75),
                    max.97.5=quantile(p1, 0.975),
                    mean.score=quantile(p1, 0.5))
              )
}

# Create a design matrix for different effect types,
# Here, several are specified, corresponding to whether there are interactive effects and
# whether dummy variables are involved. ordinal refers to whether shall be used in ordered logit 




# design.matrix.oneway<-function(output, predictor, predictor.value, 
#                                model.type=c("Binary.Logit", "OLS", "Multinomial.Logit", "Ordered.Logit"),
#                                prediction.type=c("Predictive", "Marginal.Max")){
#   ordinal<-ifelse(model.type=="Ordered.Logit", TRUE, FALSE)
#   if(prediction.type=="Predictive"){
#     return(design.matrix.predictive(output, predictor, predictor.value, ordinal))
#   }
#   if(prediction.type=="Marginal.Max"){
#     return(design.matrix.marginal(output, predictor, ordinal))
#   }
# }
# 
# 
# Through simulation predict outcomes, using a linear model 
model.prediction.linear<-function(output, design, type=c("Predictive", "Marginal.Max", "Marginal.75")){
if(type=="Predictive"){
  require(MASS)
  set.seed(27)
  beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
  g1<-apply(design%*%t(beta.sim), 2, mean) ### The average effect. Simulate b and auth
  return(data.frame(min.2.5=quantile(g1, 0.025),
                    min.25=quantile(g1, 0.25),
                    max.75= quantile(g1, 0.75),
                    max.97.5=quantile(g1, 0.975),
                    mean.score=quantile(g1, 0.5)
  ))
}
if(type=="Marginal.Max"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply(design$p1%*%t(beta.sim), 2, mean) ### The average effect. Simulate b and auth
    g2<-apply(design$o1%*%t(beta.sim), 2, mean) ### The average effect. Simulate b and auth
}
  if(type=="Marginal.75"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply(design$p2%*%t(beta.sim), 2, mean) ### The average effect. Simulate b and auth
    g2<-apply(design$o2%*%t(beta.sim), 2, mean) ### The average effect. Simulate b and auth
  }
    gg1<-g1-g2
    return(data.frame(min.2.5=quantile(gg1, 0.025),
                      min.25=quantile(gg1, 0.25),
                      max.75= quantile(gg1, 0.75),
                      max.97.5=quantile(gg1, 0.975),
                      mean.score=quantile(gg1, 0.5)

    ))
}
# Through Simulation predict outcomes, using a binary regression
model.prediction.count<-function(output, design, type=c("Predictive", "Marginal.Max", "Marginal.75")){
  if(type=="Predictive"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply(exp(design%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    return(data.frame(min.2.5=quantile(g1, 0.025),
                      min.25=quantile(g1, 0.25),
                      max.75= quantile(g1, 0.75),
                      max.97.5=quantile(g1, 0.975),
                      mean.score=quantile(g1, 0.5)
    ))
  }
  if(type=="Marginal.Max"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply( exp(design$p1%*%t(beta.sim)), 2, mean)### The average effect. Simulate b and auth
    g2<-apply( exp(design$o1%*% t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  }
  if(type=="Marginal.75"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply( exp(design$p1%*%t(beta.sim)), 2, mean)### The average effect. Simulate b and auth
    g2<-apply( exp(design$o1%*% t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  }
    gg1<-g1-g2
 return(data.frame(min.2.5=quantile(gg1, 0.025),
                      min.25=quantile(gg1, 0.25),
                      max.75= quantile(gg1, 0.75),
                      max.97.5=quantile(gg1, 0.975),
                      mean.score=quantile(gg1, 0.5)
                   ))
}

model.prediction.logit<-function(output, design, type=c("Predictive", "Marginal.Max", "Marginal.75")){
  if(type=="Predictive"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply(plogis(design %*% t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    return(data.frame(min.2.5=quantile(g1, 0.025),
                      min.25=quantile(g1, 0.25),
                      max.75= quantile(g1, 0.75),
                      max.97.5=quantile(g1, 0.975),
                      mean.score=quantile(g1, 0.5)
    ))
  }
  if(type=="Marginal.Max"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply(plogis(design$p1%*%t(beta.sim)), 2, mean)### The average effect. Simulate b and auth
    g2<-apply(plogis(design$o1%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  }
  if(type=="Marginal.75"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, coef(output), vcov(output)) 
    g1<-apply(plogis(design$p2%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    g2<-apply(plogis(design$o2%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  }
  gg1<-g1-g2
  return(data.frame(min.2.5=quantile(gg1, 0.025),
                    min.25=quantile(gg1, 0.25),
                    max.75= quantile(gg1, 0.75),
                    max.97.5=quantile(gg1, 0.975),
                    mean.score=quantile(gg1, 0.5)
  ))
}

# Through Simulation predict ordinal, using MASS::polr package 
model.prediction.ordinal<-function(output, design, type=c("Predictive", "Marginal.Max", "Marginal.75"), cat=3){
  if(type=="Predictive"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, c(output$coefficients, output$zeta), vcov(output)) ##Draw samples from multivariate distrbution
    k=max(as.numeric(output$lev))-1
    temp1<-matrix(0, nrow=nrow(design), ncol=k)
    temp1[,cat-1]<--1
    g1<-apply(plogis(cbind(design, temp1)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    return(data.frame(min.2.5=quantile(g1, 0.025),
                      min.25=quantile(g1, 0.25),
                      max.75= quantile(g1, 0.75),
                      max.97.5=quantile(g1, 0.975),
                      mean.score=quantile(g1, 0.5) 
                      ))
    }
  if(type=="Marginal.Max"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, c(output$coefficients, output$zeta), vcov(output)) ##Draw samples from multivariate distrbution
    k=max(as.numeric(output$lev))-1
    temp1<-matrix(0, nrow=nrow(design$p1), ncol=k)
    temp1[,cat-1]<--1
    g1<-apply(plogis(cbind(design$p1, temp1) %*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    g2<-apply(plogis(cbind(design$o1, temp1) %*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    gg1<-g1-g2
    return(data.frame(min.2.5=quantile(gg1, 0.025),
                      min.25=quantile(gg1, 0.25),
                      max.75= quantile(gg1, 0.75),
                      max.97.5=quantile(gg1, 0.975),
                      mean.score=quantile(gg1, 0.5)))
  }
  if(type=="Marginal.75"){
    require(MASS)
    set.seed(27)
    beta.sim<-mvrnorm(1000, c(output$coefficients, output$zeta), vcov(output)) ##Draw samples from multivariate distrbution
    k=max(as.numeric(output$lev))-1
    temp1<-matrix(0, nrow=nrow(design$p1), ncol=k)
    temp1[,cat-1]<--1
    g1<-apply(plogis(cbind(design$p2, temp1) %*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    g2<-apply(plogis(cbind(design$o2, temp1) %*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    gg1<-g1-g2
    return(data.frame(min.2.5=quantile(gg1, 0.025),
                      min.25=quantile(gg1, 0.25),
                      max.75= quantile(gg1, 0.75),
                      max.97.5=quantile(gg1, 0.975),
                      mean.score=quantile(gg1, 0.5)))
  }
}  
# Through Simulation predict ordinal, using MASS::polr package:each cat
model.prediction.ordinal2<-function(output, design){
    require(MASS)
    set.seed(27)
    p<-design
    beta.sim<-mvrnorm(1000, c(output$coefficients, output$zeta), vcov(output)) ##Draw samples from multivariate distrbution
    g1<-apply(plogis(cbind(p, -1, 0, 0)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    g2<-apply(plogis(cbind(p, 0, -1, 0)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    g3<-apply(plogis(cbind(p, 0, 0, -1)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
    pr.1<-1-g1
    pr.2<-g1-g2
    pr.3<-g2-g3
    pr.4<-g3
    return(list(ONE=data.frame(min.2.5=quantile(pr.1, 0.025),
                                   min.25=quantile(pr.1, 0.25),
                                   max.75= quantile(pr.1, 0.75),
                                   max.97.5=quantile(pr.1, 0.975),
                                   mean.score=quantile(pr.1, 0.5)),
                TWO=data.frame(min.2.5=quantile(pr.2, 0.025),
                                    min.25=quantile(pr.2, 0.25),
                                    max.75= quantile(pr.2, 0.75),
                                    max.97.5=quantile(pr.2, 0.975),
                                    mean.score=quantile(pr.2, 0.5)),
                THREE= data.frame(min.2.5=quantile(pr.3, 0.025),
                                         min.25=quantile(pr.3, 0.25),
                                         max.75= quantile(pr.3, 0.75),
                                         max.97.5=quantile(pr.3, 0.975),
                                         mean.score=quantile(pr.3, 0.5)),
                FOUR= data.frame(min.2.5=quantile(pr.4, 0.025),
                                         min.25=quantile(pr.4, 0.25),
                                         max.75= quantile(pr.4, 0.75),
                                         max.97.5=quantile(pr.4, 0.975),
                                         mean.score=quantile(pr.4, 0.5))
    ))
  }

model.prediction.ordinal3<-function(output, design){
  require(MASS)
  set.seed(27)
  p<-design
  beta.sim<-mvrnorm(1000, c(output$coefficients, output$zeta), vcov(output)) ##Draw samples from multivariate distrbution
  g1<-apply(plogis(cbind(p, -1, 0)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  pr.1<-1-g1
  pr.3<-g1
  return(list(ONE=data.frame(min.2.5=quantile(pr.1, 0.025),
                             min.25=quantile(pr.1, 0.25),
                             max.75= quantile(pr.1, 0.75),
                             max.97.5=quantile(pr.1, 0.975),
                             mean.score=quantile(pr.1, 0.5)),
              TWO=data.frame(min.2.5=quantile(pr.2, 0.025),
                             min.25=quantile(pr.2, 0.25),
                             max.75= quantile(pr.2, 0.75),
                             max.97.5=quantile(pr.2, 0.975),
                             mean.score=quantile(pr.2, 0.5))
  ))
}

model.prediction.ordinal.ch9<-function(output, design){
  require(MASS)
  set.seed(27)
  p<-design
  beta.sim<-mvrnorm(1000, c(output$coefficients, output$zeta), vcov(output)) ##Draw samples from multivariate distrbution
  g1<-apply(plogis(cbind(p, -1, 0, 0)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  g2<-apply(plogis(cbind(p, 0, -1, 0)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  g3<-apply(plogis(cbind(p, 0, 0, -1)%*%t(beta.sim)), 2, mean) ### The average effect. Simulate b and auth
  pr.3<-g2
  return( data.frame(min.2.5=quantile(pr.3, 0.025),
                                min.25=quantile(pr.3, 0.25),
                                max.75= quantile(pr.3, 0.75),
                                max.97.5=quantile(pr.3, 0.975),
                                mean.score=quantile(pr.3, 0.5)
             
  ))
}


# Multinomial 
model.prediction.multi.2<-function(output, design){
  require(MASS)
  set.seed(27)
  p<-design
  k<-max(as.numeric(as.factor(output$lev)))-1
  j<-length(coef(output))/2
  beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
  c2<-p%*%t(beta.sim[,1:j])
  c3<-p%*%t(beta.sim[,(j+1):(j*2)])
  p.c1<-1/(1+exp(c2)+exp(c3))
  p.c2<-exp(c2)/(1+exp(c2)+exp(c3))
  p.c3<-exp(c3)/(1+exp(c2)+exp(c3))
  g1<-apply(p.c1, 2, mean)
  g2<-apply(p.c2, 2, mean)
  g3<-apply(p.c3, 2, mean)
  return(list(ONE=data.frame(min.2.5=quantile(pr.1, 0.025),
                             min.25=quantile(pr.1, 0.25),
                             max.75= quantile(pr.1, 0.75),
                             max.97.5=quantile(pr.1, 0.975),
                             mean.score=quantile(pr.1, 0.5)),
              TWO=data.frame(min.2.5=quantile(pr.2, 0.025),
                             min.25=quantile(pr.2, 0.25),
                             max.75= quantile(pr.2, 0.75),
                             max.97.5=quantile(pr.2, 0.975),
                             mean.score=quantile(pr.2, 0.5)),
              THREE= data.frame(min.2.5=quantile(pr.3, 0.025),
                                min.25=quantile(pr.3, 0.25),
                                max.75= quantile(pr.3, 0.75),
                                max.97.5=quantile(pr.3, 0.975),
                                mean.score=quantile(pr.3, 0.5))
  ))
}

    
## Prediction for Multinomial Model. Marginal Max=maximum discrete effect, 75 is 25 to 75 effect
model.prediction.multinomial<-function(output, design, type=c("Predictive", "Marginal.Max")){
  if(type=="Predictive"){
    print("Average Predictive Effect for Multinomial Model")
    require(MASS)
    set.seed(27)
    k<-max(as.numeric(as.factor(output$lev)))-1
    j<-length(coef(output))/2
    beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
    c2<-design%*%t(beta.sim[,1:j])
    c3<-design%*%t(beta.sim[,(j+1):(j*2)])
    p.c1<-1/(1+exp(c2)+exp(c3))
    p.c2<-exp(c2)/(1+exp(c2)+exp(c3))
    p.c3<-exp(c3)/(1+exp(c2)+exp(c3))
    g1<-apply(p.c1, 2, mean)
    g2<-apply(p.c2, 2, mean)
    g3<-apply(p.c3, 2, mean)
    return(list(cat1=data.frame(min.2.5=quantile(g1, 0.025),
                      min.25=quantile(g1, 0.25),
                      max.75= quantile(g1, 0.75),
                      max.97.5=quantile(g1, 0.975),
                      mean.score=quantile(g1, 0.5)),
                cat2=data.frame(min.2.5=quantile(g2, 0.025),
                           min.25=quantile(g2, 0.25),
                           max.75= quantile(g2, 0.75),
                           max.97.5=quantile(g2, 0.975),
                           mean.score=quantile(g2, 0.5)),
                cat3=data.frame(min.2.5=quantile(g3, 0.025),
                           min.25=quantile(g3, 0.25),
                           max.75= quantile(g3, 0.75),
                           max.97.5=quantile(g3, 0.975),
                           mean.score=quantile(g3, 0.5))
                      
    ))
  }
  if(type=="Marginal.Max"){
    require(MASS)
    print("Average Marginal Effect for Multinomial Model, Min to Max")
    set.seed(27)
    k<-max(as.numeric(as.factor(output$lev)))-1
    j<-length(coef(output))/2
    beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
    c2a<-design$p1%*%t(beta.sim[,1:j])
    c3a<-design$p1%*%t(beta.sim[,(j+1):(j*2)])
    c2b<-design$o1%*%t(beta.sim[,1:j])
    c3b<-design$o1%*%t(beta.sim[,(j+1):(j*2)])
    p.c1a<-1/(1+exp(c2a)+exp(c3a))-1/(1+exp(c2b)+exp(c3b))
    p.c2a<-exp(c2a)/(1+exp(c2a)+exp(c3a))
    p.c3a<-exp(c3a)/(1+exp(c2a)+exp(c3a))
    p.c1b<-1/(1+exp(c2b)+exp(c3b))
    p.c2b<-exp(c2b)/(1+exp(c2b)+exp(c3b))
    p.c3b<-exp(c3b)/(1+exp(c2b)+exp(c3b))
    g1<-apply(p.c1a, 2, mean)-apply(p.c1b, 2, mean)
    g2<-apply(p.c2a, 2, mean)-apply(p.c2b, 2, mean)
    g3<-apply(p.c3a, 2, mean)-apply(p.c3b, 2, mean)
    return(list(cat1=data.frame(min.2.5=quantile(g1, 0.025),
                                min.25=quantile(g1, 0.25),
                                max.75= quantile(g1, 0.75),
                                max.97.5=quantile(g1, 0.975),
                                mean.score=quantile(g1, 0.5)),
                cat2=data.frame(min.2.5=quantile(g2, 0.025),
                                min.25=quantile(g2, 0.25),
                                max.75= quantile(g2, 0.75),
                                max.97.5=quantile(g2, 0.975),
                                mean.score=quantile(g2, 0.5)),
                cat3=data.frame(min.2.5=quantile(g3, 0.025),
                                min.25=quantile(g3, 0.25),
                                max.75= quantile(g3, 0.75),
                                max.97.5=quantile(g3, 0.975),
                                mean.score=quantile(g3, 0.5))
                
    ))
  }
}

## Prediction for multinomial, each category.
model.prediction.m2<-function(output, design){
    require(MASS)
    set.seed(27)
    k<-max(as.numeric(as.factor(output$lev)))-1
    j<-length(coef(output))/2
    beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
    c2<-design%*%t(beta.sim[,1:j])
    c3<-design%*%t(beta.sim[,(j+1):(j*2)])
    p.c1<-1/(1+exp(c2)+exp(c3))
    p.c2<-exp(c2)/(1+exp(c2)+exp(c3))
    p.c3<-exp(c3)/(1+exp(c2)+exp(c3))
    g1<-apply(p.c1, 2, mean)
    g2<-apply(p.c2, 2, mean)
    g3<-apply(p.c3, 2, mean)
    return(list(cat1=data.frame(min.2.5=quantile(g1, 0.025),
                                min.25=quantile(g1, 0.25),
                                max.75= quantile(g1, 0.75),
                                max.97.5=quantile(g1, 0.975),
                                mean.score=quantile(g1, 0.5)),
                cat2=data.frame(min.2.5=quantile(g2, 0.025),
                                min.25=quantile(g2, 0.25),
                                max.75= quantile(g2, 0.75),
                                max.97.5=quantile(g2, 0.975),
                                mean.score=quantile(g2, 0.5)),
                cat3=data.frame(min.2.5=quantile(g3, 0.025),
                                min.25=quantile(g3, 0.25),
                                max.75= quantile(g3, 0.75),
                                max.97.5=quantile(g3, 0.975),
                                mean.score=quantile(g3, 0.5))
                
    ))
  }

## Prediction for Multinomial Model. Marginal Max=maximum discrete effect, 75 is 25 to 75 effect
model.prediction.multinomial.ch9<-function(output, design){
    require(MASS)
    set.seed(27)
    k<-max(as.numeric(as.factor(output$lev)))-1
    j<-length(coef(output))/2
    beta.sim<-mvrnorm(1000, c(coef(output)[1,], coef(output)[2,]), vcov(output)) ##Draw samples from multivariate distrbution
    c2<-design%*%t(beta.sim[,1:j])
    c3<-design%*%t(beta.sim[,(j+1):(j*2)])
    p.c1<-1/(1+exp(c2)+exp(c3))
    p.c2<-exp(c2)/(1+exp(c2)+exp(c3))
    p.c3<-exp(c3)/(1+exp(c2)+exp(c3))
    g1<-apply(p.c1, 2, mean)
    g2<-apply(p.c2, 2, mean)
    g3<-apply(p.c3, 2, mean)
    d<-list(cat1=data.frame(min.2.5=quantile(g1, 0.025),
                                min.25=quantile(g1, 0.25),
                                max.75= quantile(g1, 0.75),
                                max.97.5=quantile(g1, 0.975),
                                mean.score=quantile(g1, 0.5)),
                cat2=data.frame(min.2.5=quantile(g2, 0.025),
                                min.25=quantile(g2, 0.25),
                                max.75= quantile(g2, 0.75),
                                max.97.5=quantile(g2, 0.975),
                                mean.score=quantile(g2, 0.5)),
                cat3=data.frame(min.2.5=quantile(g3, 0.025),
                                min.25=quantile(g3, 0.25),
                                max.75= quantile(g3, 0.75),
                                max.97.5=quantile(g3, 0.975),
                                mean.score=quantile(g3, 0.5))
    )
    return(d$cat3)

  }


# Wrapper function
model.prediction<-function(output, design, type=c("Predictive", "Marginal"),
                           model.type=c("Binary.Logit", "OLS", "Multinomial.Logit", "Ordered.Logit", "Count"), 
                           cat=1){
  if(model.type=="Binary.Logit" & type=="Predictive")
    return(model.prediction.logit(output, design, "Predictive"))
  if(model.type=="Binary.Logit" & type=="Marginal")
    return(model.prediction.logit(output, design, "Marginal.Max"))
  if(model.type=="Count" & type=="Predictive")
    return(model.prediction.count(output, design, "Predictive"))
  if(model.type=="Count" & type=="Marginal")
    return(model.prediction.count(output, design, "Marginal.Max"))
    if(model.type=="OLS" & type=="Predictive")
    return(model.prediction.linear(output, design, "Predictive"))
  if(model.type=="OLS" & type=="Marginal")
    return(model.prediction.linear(output, design, "Marginal.Max"))
  if(model.type=="Multinomial.Logit" & type=="Predictive")
    return(model.prediction.multinomial(output, design, "Predictive"))
  if(model.type=="Multinomial.Logit" & type=="Marginal")
    return(model.prediction.multinomial(output, design, "Marginal.Max"))
  if(model.type=="Ordered.Logit" & type=="Predictive")
    return(model.prediction.ordinal(output, design, "Predictive", cat))
  if(model.type=="Ordered.Logit" & type=="Marginal")
    return(model.prediction.ordinal(output, design, "Marginal.Max", cat))
}
###Utilities
zero.one<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}

### Spaghetti Plot ###
spaghetti<-function(output){
  require(MASS)
  t<-model.matrix(output)
  p<-cbind(
    1,
    seq(0,100)/100,
    1,
    mean(t[,4]),
    0,
    0,
    0,
    0,
    0)
  
  sim.beta <- mvrnorm(1000,coef(output),vcov(output)) ##Simulate Draws from a multivariate normal
  p1<-t(plogis(p%*%t(sim.beta)))
  sorted.data <- apply(p1,2,sort)
  cib <- 0.05	
  lb <- round((dim(sorted.data)[1] * cib)/2)	
  ub <- dim(sorted.data)[1] - lb 
  return(data.frame(t(sorted.data[lb:ub,])))
}

### Chapter Specific Simulation Effects

pred.feeling.noint<-function(output){
  require(MASS)
  t<-model.matrix(output)
  p<-cbind(
    1,
    1,  #High Authoritarianism
    t[,3:dim(t)[2]]
  )
  o<-cbind(
    1,
    0,  #High Authoritarianism
    t[,3:dim(t)[2]]
  )
  beta.sim<-mvrnorm(1000, c(output$coefficients), vcov(output)) ##Draw samples from multivariate distrbution
  g1<-apply(p%*%t(beta.sim), 2, mean) ### The average effect. Simulate b and auth
  g2<-apply(o%*%t(beta.sim), 2, mean) ### The average effect. Simulate b and auth
  p1<-g1-g2
  return(data.frame(min.2.5=quantile(p1, 0.025),
                    min.25=quantile(p1, 0.25),
                    max.75= quantile(p1, 0.75),
                    max.97.5=quantile(p1, 0.975),
                    mean.score=quantile(p1, 0.5)))
}  

pred.feeling<-function(output, auth){
  require(MASS)
  t<-model.matrix(output)
  p<-cbind(
    1,
    auth,
    1,
    mean(t[,4]),
    0,
    0,
    0,
    0,
    0)
  # sim.beta<-matrix(NA, nrow=1000, ncol=dim(t)[2])
  # sigma<-matrix(NA, nrow=1000)
  # for(i in 1:1000){
  # sigma[i]<-summary(output)$sigma*sqrt((dim(t)[1]-(dim(t)[2]-1))/rchisq(1,dim(t)[1]-(dim(t)[2]-1)))
  # sim.beta[i,] <- mvrnorm(1,coef(output),vcov(output)*sigma[i]^2) ##Simulate Draws from a multivariate normal
  # }
  sim.beta <- mvrnorm(1000,coef(output),vcov(output)) ##Simulate Draws from a multivariate normal
  p1<-p%*%t(sim.beta)
  return(data.frame(pred=data.frame(min.2.5=quantile(p1, 0.025),
                                    min.25=quantile(p1, 0.25),
                                    max.75= quantile(p1, 0.75),
                                    max.97.5=quantile(p1, 0.975),
                                    mean.score=quantile(p1, 0.5))
                    
  ))
}

# Chapter 9 Functions
## Two most scores
main.effect<-function(m1a){
  t1<-design.matrix.predictive(m1a, "authoritarianism", 1, ordinal=TRUE) ## List of design matrices
  model.prediction.ordinal.ch9(m1a, t1)
  
  design.matrix.predictive(m1a, "authoritarianism", 0, ordinal=TRUE) ## List of design matrices
  
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.predictive(m1a, "authoritarianism", x, ordinal=TRUE)) ## List of design matrices
  
  temp<-model.prediction.ordinal.ch9(m1a, 
                                     design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.ordinal.ch9(m1a, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  p1<-temp
  p1$PID<-"Average Effect"
  
  return(p1)
}
interactive.effect<-function(m1b){
  t2<-design.matrix.int.predictive.2(m1b, 
                                     names=c("authoritarianism", names(m1b$model)[2],
                                             names(m1b$model)[3], names(m1b$model)[4],
                                             names(m1b$model)[5]),
                                     values=c(1, 1, 0, 0, 1*1),
                                     ordinal=TRUE) ## List of design matrices
  model.prediction.ordinal.ch9(m1b, t2)
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$model)[3],
                                           names(m1b$model)[4], names(m1b$model)[5],
                                           names(m1b$model)[6]),
                                   values=c(x, 1, 0, 1*x, x*0),
                                   ordinal=TRUE)) ## List of design matrices
  
  temp<-model.prediction.ordinal.ch9(m1b,design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.ordinal.ch9(m1b, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  
  rep<-temp
  rep$PID<-"Republican"
  
  
  #Democrats
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$model)[3],
                                           names(m1b$model)[4], names(m1b$model)[5],
                                           names(m1b$model)[6]),
                                   values=c(x, 0, 0, 0*x, x*0),
                                   ordinal=TRUE)) ## List of design matrices
  
  temp<-model.prediction.ordinal.ch9(m1b, 
                                     design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.ordinal.ch9(m1b, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  temp$PID<-"Democrat"
  dem<-temp
  
  #Inds
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$model)[3],
                                           names(m1b$model)[4], names(m1b$model)[5],
                                           names(m1b$model)[6]),
                                   values=c(x, 0, 1, 0*x, x*1),
                                   ordinal=TRUE)) ## List of design matrices
  
  temp<-model.prediction.ordinal.ch9(m1b, 
                                     design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.ordinal.ch9(m1b, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  temp$PID<-"Independent"
  return(rbind(temp,dem,rep))
}
main.effect2<-function(m1a){
  t1<-design.matrix.predictive(m1a, "authoritarianism", 1, ordinal=FALSE) ## List of design matrices
  model.prediction.multinomial.ch9(m1a, t1)
  
  design.matrix.predictive(m1a, "authoritarianism", 0, ordinal=FALSE) ## List of design matrices
  
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.predictive(m1a, "authoritarianism", x, ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.multinomial.ch9(m1a, 
                                         design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.multinomial.ch9(m1a, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  p1<-temp
  p1$PID<-"Average Effect"
  
  return(p1)
}
interactive.effect2<-function(m1b){
  t2<-design.matrix.int.predictive.2(m1b, 
                                     names=c("authoritarianism", m1b$coefnames[3],
                                             m1b$coefnames[4], m1b$coefnames[5],
                                             m1b$coefnames[6]),
                                     values=c(1, 1, 0, 0, 1*1),
                                     ordinal=FALSE) ## List of design matrices
  model.prediction.multinomial.ch9(m1b, t2)
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", m1b$coefnames[3],
                                           m1b$coefnames[4], m1b$coefnames[5],
                                           m1b$coefnames[6]),
                                   values=c(x, 1, 0, 1*x, x*0),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.multinomial.ch9(m1b,design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.multinomial.ch9(m1b, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  
  rep<-temp
  rep$PID<-"Republican"
  
  
  #Democrats
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", m1b$coefnames[3],
                                           m1b$coefnames[4], m1b$coefnames[5],
                                           m1b$coefnames[6]),
                                   values=c(x, 0, 0, 0*x, x*0),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.multinomial.ch9(m1b, 
                                         design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.multinomial.ch9(m1b, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  temp$PID<-"Democrat"
  dem<-temp
  
  #Inds
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", m1b$coefnames[3],
                                           m1b$coefnames[4], m1b$coefnames[5],
                                           m1b$coefnames[6]),
                                   values=c(x, 0, 1, 0*x, x*1),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.multinomial.ch9(m1b, 
                                         design.temp[[1]])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.multinomial.ch9(m1b, design.temp[[i]])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  temp$PID<-"Independent"
  d<-rbind(temp,dem,rep)
  return(d)
}
main.effect.logit<-function(m1a){
  t1<-design.matrix.predictive(m1a, "authoritarianism", 1, ordinal=FALSE) ## List of design matrices
  model.prediction.logit(m1a, t1, "Predictive")
  
  design.matrix.predictive(m1a, "authoritarianism", 0, ordinal=FALSE) ## List of design matrices
  
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.predictive(m1a, "authoritarianism", x, ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1a, 
                               design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1a, design.temp[[i]], "Predictive"),
                1- model.prediction.logit(m1a, design.temp[[i]], "Predictive")
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-rep(seq(0,1,1), each=2)
  p1<-temp
  p1<-p1[,c(2,4)]
  return(p1)
}
interactive.effect.logit<-function(m1b){
  t2<-design.matrix.int.predictive.2(m1b, 
                                     names=c("authoritarianism", names(m1b$coefficients)[3],
                                             names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                             names(m1b$coefficients)[6]),
                                     values=c(1, 1, 0, 0, 1*1),
                                     ordinal=FALSE) ## List of design matrices
  model.prediction.logit(m1b, t2, "Predictive")
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$coefficients)[3],
                                           names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                           names(m1b$coefficients)[6]),
                                   values=c(x, 1, 0, 1*x, x*0),
                                   
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1b,  design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1b, design.temp[[i]], "Predictive")    
    )
  }
  
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  rep<-temp
  rep$PID<-"Republican"
  
  
  #Democrats
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$coefficients)[3],
                                           names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                           names(m1b$coefficients)[6]),
                                   values=c(x, 0, 0, 0*x, x*0),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1b, design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1b, design.temp[[i]], "Predictive")
    )
  }
  
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  
  dem<-temp
  dem$PID<-"Democrat"
  
  #Inds
  design.temp<-lapply(seq(0,1, 1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$coefficients)[3],
                                           names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                           names(m1b$coefficients)[6]),
                                   values=c(x, 0, 1, 0*x, x*1),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1b, design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1b, design.temp[[i]], "Predictive")
    )
  }
  
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$authoritarianism<-c(0,1)
  
  temp$PID<-"Independent"
  return(rbind(temp,dem,rep))
}
### each category ###
main.effectV1<-function(m1a, year="2017", 
                        ti=c("Strongly Unfavorable", "Somewhat Unfavorable", 
                             "Somewhat Favorable", "Strongly Favorable")){
  t1<-design.matrix.predictive(m1a, "authoritarianism", 1, ordinal=TRUE) ## List of design matrices
  model.prediction.ordinal2(m1a, t1)
  
  design.matrix.predictive(m1a, "authoritarianism", 0, ordinal=TRUE) ## List of design matrices
  
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.predictive(m1a, "authoritarianism", x, ordinal=TRUE)) ## List of design matrices
  
  temp<-unlist(model.prediction.ordinal2(m1a, 
                                         design.temp[[1]])[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.ordinal2(m1a, design.temp[[i]])[1]),
                unlist(model.prediction.ordinal2(m1a, design.temp[[i]])[2]),
                unlist(model.prediction.ordinal2(m1a, design.temp[[i]])[3]),
                unlist(model.prediction.ordinal2(m1a, design.temp[[i]])[4])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=4)
  temp$Category <- factor(temp$category, ti)
  
  p1<-temp
  p1$year<-year
  return(p1)
}
interactive.effectV1<-function(m1b, year="2017", ti=c("Strongly Unfavorable", "Somewhat Unfavorable", 
                                                      "Somewhat Favorable", "Strongly Favorable")){
  t2<-design.matrix.int.predictive.2(m1b, 
                                     names=c("authoritarianism", names(m1b$model)[2],
                                             names(m1b$model)[3], names(m1b$model)[4],
                                             names(m1b$model)[5]),
                                     values=c(1, 1, 0, 0, 1*1),
                                     ordinal=TRUE) ## List of design matrices
  model.prediction.ordinal2(m1b, t2)
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$model)[3],
                                           names(m1b$model)[4], names(m1b$model)[5],
                                           names(m1b$model)[6]),
                                   values=c(x, 1, 0, 1*x, x*0),
                                   ordinal=TRUE)) ## List of design matrices
  
  temp<-unlist(model.prediction.ordinal2(m1b, 
                                         design.temp[[1]])[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[1]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[2]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[3]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[4])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=4)
  temp$Category <- factor(temp$category, ti)
  
  rep<-temp
  rep$PID<-"Republican"
  rep$year<-year
  
  
  #Democrats
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$model)[3],
                                           names(m1b$model)[4], names(m1b$model)[5],
                                           names(m1b$model)[6]),
                                   values=c(x, 0, 0, 0*x, x*0),
                                   ordinal=TRUE)) ## List of design matrices
  
  temp<-unlist(model.prediction.ordinal2(m1b, 
                                         design.temp[[1]])[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[1]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[2]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[3]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[4])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=4)
  temp$Category <- factor(temp$category, ti)
  temp$PID<-"Democrat"
  temp$year<-year
  dem<-temp
  
  #Inds
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$model)[3],
                                           names(m1b$model)[4], names(m1b$model)[5],
                                           names(m1b$model)[6]),
                                   values=c(x, 0, 1, 0*x, x*1),
                                   ordinal=TRUE)) ## List of design matrices
  
  temp<-unlist(model.prediction.ordinal2(m1b, 
                                         design.temp[[1]])[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[1]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[2]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[3]),
                unlist(model.prediction.ordinal2(m1b, design.temp[[i]])[4])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=4)
  temp$Category <- factor(temp$category,ti)
  temp$PID<-"Independent"
  temp$year<-year
  return(rbind(temp,dem,rep))
}
main.effect.V2<-function(m1a, year="2017", 
                         ti=c("Strongly Unfavorable", "Somewhat Unfavorable", 
                              "Somewhat Favorable", "Strongly Favorable")){
  t1<-design.matrix.predictive(m1a, "authoritarianism", 1, ordinal=FALSE) ## List of design matrices
  model.prediction.multinomial(m1a, t1, "Predictive")
  
  design.matrix.predictive(m1a, "authoritarianism", 0, ordinal=FALSE) ## List of design matrices
  
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.predictive(m1a, "authoritarianism", x, ordinal=FALSE)) ## List of design matrices
  
  temp<-unlist(model.prediction.multinomial(m1a, 
                                            design.temp[[1]], "Predictive")[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.multinomial(m1a, design.temp[[i]], "Predictive")[1]),
                unlist(model.prediction.multinomial(m1a, design.temp[[i]], "Predictive")[2]),
                unlist(model.prediction.multinomial(m1a, design.temp[[i]], "Predictive")[3])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=3)
  temp$Category <- factor(temp$category, ti)
  
  p1<-temp
  p1$year<-year
  return(p1)
}
interactive.effect.V2<-function(m1b, year="2017", ti=c("Strongly Unfavorable", "Somewhat Unfavorable", 
                                                       "Somewhat Favorable", "Strongly Favorable")){
  t2<-design.matrix.int.predictive.2(m1b, 
                                     names=c("authoritarianism", m1b$coefnames[3],
                                             m1b$coefnames[4], m1b$coefnames[5],
                                             m1b$coefnames[6]),
                                     values=c(1, 1, 0, 0, 1*1),
                                     ordinal=FALSE) ## List of design matrices
  model.prediction.multinomial(m1b, t2, "Predictive")
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", m1b$coefnames[3],
                                           m1b$coefnames[4], m1b$coefnames[5],
                                           m1b$coefnames[6]),
                                   values=c(x, 1, 0, 1*x, x*0),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-unlist(model.prediction.multinomial(m1b, 
                                            design.temp[[1]], "Predictive")[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[1]),
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[2]),
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[3])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=3)
  temp$Category <- factor(temp$category, ti)
  
  rep<-temp
  rep$PID<-"Republican"
  rep$year<-year
  
  
  #Democrats
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", m1b$coefnames[3],
                                           m1b$coefnames[4], m1b$coefnames[5],
                                           m1b$coefnames[6]),
                                   values=c(x, 0, 0, 0*x, x*0),
                                   ordinal=FALSE)) ## List of design matrices
  
  
  temp<-unlist(model.prediction.multinomial(m1b, 
                                            design.temp[[1]], "Predictive")[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[1]),
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[2]),
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[3])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=3)
  temp$Category <- factor(temp$category, ti)
  temp$PID<-"Democrat"
  temp$year<-year
  dem<-temp
  
  #Inds
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", m1b$coefnames[3],
                                           m1b$coefnames[4], m1b$coefnames[5],
                                           m1b$coefnames[6]),
                                   values=c(x, 0, 1, 0*x, x*1),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-unlist(model.prediction.multinomial(m1b, 
                                            design.temp[[1]], "Predictive")[1])
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[1]),
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[2]),
                unlist(model.prediction.multinomial(m1b, design.temp[[i]], "Predictive")[3])
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=3)
  temp$Category <- factor(temp$category,ti)
  temp$PID<-"Independent"
  temp$year<-year
  return(rbind(temp,dem,rep))
}
main.effect.logit.V1<-function(m1a, year="2017", 
                               ti=c("1", "0")){
  t1<-design.matrix.predictive(m1a, "authoritarianism", 1, ordinal=FALSE) ## List of design matrices
  model.prediction.logit(m1a, t1, "Predictive")
  
  design.matrix.predictive(m1a, "authoritarianism", 0, ordinal=FALSE) ## List of design matrices
  
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.predictive(m1a, "authoritarianism", x, ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1a, 
                               design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1a, design.temp[[i]], "Predictive"),
                1- model.prediction.logit(m1a, design.temp[[i]], "Predictive")
    )
  }
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=2)
  temp$Category <- factor(temp$category, ti)
  p1<-temp
  p1$year<-year
  return(p1)
}
interactive.effect.logit.V1<-function(m1b, year="2017", ti=c("Strongly Unfavorable", "Somewhat Unfavorable")){
  t2<-design.matrix.int.predictive.2(m1b, 
                                     names=c("authoritarianism", names(m1b$coefficients)[3],
                                             names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                             names(m1b$coefficients)[6]),
                                     values=c(1, 1, 0, 0, 1*1),
                                     ordinal=FALSE) ## List of design matrices
  model.prediction.logit(m1b, t2, "Predictive")
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$coefficients)[3],
                                           names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                           names(m1b$coefficients)[6]),
                                   values=c(x, 1, 0, 1*x, x*0),
                                   
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1b,  design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1b, design.temp[[i]], "Predictive"),
                1- model.prediction.logit(m1b, design.temp[[i]], "Predictive")
    )
  }
  
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=2)
  temp$Category <- factor(temp$category, ti)
  
  rep<-temp
  rep$PID<-"Republican"
  rep$year<-year
  
  
  #Democrats
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$coefficients)[3],
                                           names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                           names(m1b$coefficients)[6]),
                                   values=c(x, 0, 0, 0*x, x*0),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1b, design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1b, design.temp[[i]], "Predictive"),
                1- model.prediction.logit(m1b, design.temp[[i]], "Predictive")
    )
  }
  
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=2)
  temp$Category <- factor(temp$category, ti)
  
  dem<-temp
  dem$PID<-"Democrat"
  dem$year<-year
  
  #Inds
  design.temp<-lapply(seq(0,1, 0.1), function(x) 
    design.matrix.int.predictive.2(m1b, 
                                   names=c("authoritarianism", names(m1b$coefficients)[3],
                                           names(m1b$coefficients)[4], names(m1b$coefficients)[5],
                                           names(m1b$coefficients)[6]),
                                   values=c(x, 0, 1, 0*x, x*1),
                                   ordinal=FALSE)) ## List of design matrices
  
  temp<-model.prediction.logit(m1b, design.temp[[1]], "Predictive")
  
  for(i in 1:length(design.temp)){
    temp<-rbind(temp, 
                model.prediction.logit(m1b, design.temp[[i]], "Predictive"),
                1- model.prediction.logit(m1b, design.temp[[i]], "Predictive")
    )
  }
  
  temp<-temp[-1,]
  temp<-data.frame(temp)
  temp$category<-rep(ti)
  temp$authoritarianism<-rep(seq(0,1,0.1), each=2)
  temp$Category <- factor(temp$category, ti)
  
  temp$PID<-"Independent"
  temp$year<-year
  return(rbind(temp,dem,rep))
}





