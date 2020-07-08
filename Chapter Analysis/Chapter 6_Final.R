####-------------------------------------------------------------------------------------####
# This File Analyzes Four panel Datasets which include Authoritarianism 
# Several panels, 1990-1992 panel; 1992-1997 panel, 2000-2004, 2008-2009; 2012-2013, 2012-2017 (YouGov)
####-------------------------------------------------------------------------------------####
rm(list=ls())
detach("package:dplyr")
require(car)
require(foreign)

##### Data Recodes #####
### 1990 ANES ####
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel1990.RData")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel2000.RData")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel2008.RData")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel2012.RData")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel1992.RData")
load("/Users/chrisweber/Dropbox/Working Projects/Authoritarianism_BookProject/Data/panel2014.RData")
## Authoritarianism 
d1$authoritarianism<-(rowMeans(cbind(d1$auth1.1992, d1$auth2.1992, d1$auth3.1992, d1$auth4.1992), na.rm=T)-1)/2
d2$authoritarianism<-(rowMeans(cbind(d2$auth1.2000, d2$auth2.2000, d2$auth3.2000, d2$auth4.2000), na.rm=T)-1)/2
d3$authoritarianism<-rowMeans(cbind(d3$auth1.2008, d3$auth2.2008, d3$auth3.2008, d3$auth4.2008), na.rm=T)
d4$authoritarianism<-rowMeans(cbind(d4$auth1.2016, d4$auth2.2016, d4$auth3.2016, d4$auth4.2016), na.rm=T)
d5$authoritarianism<-(rowMeans(cbind(d5$auth1.1992, d5$auth2.1992, d5$auth3.1992, d5$auth4.1992), na.rm=T)-1)/2
d6$authoritarianism1<-(rowMeans(cbind(d6$auth1.w1, d6$auth2.w1, d6$auth3.w1, d6$auth4.w1), na.rm=T)-1)/2
d6$authoritarianism2<-rowMeans(cbind(d6$auth1.w2, d6$auth2.w2, d6$auth3.w2, d6$auth4.w2), na.rm=T)
d6$rwa<-(rowMeans(cbind(d6$rwa1.w2, d6$rwa2.w2, d6$rwa3.w2, d6$rwa4.w2, d6$rwa5.w2), na.rm=T)-1)/4

##Party Identification
pid.r<-function(x){
  return(recode(x, "1:2=1; 3=2; 4=3; 5=4; 6:7=5"))
}

d1$pid3.1<-pid.r(d1$pid.1990)
d1$pid3.2<-pid.r(d1$pid.1991)
d1$pid3.3<-pid.r(d1$pid.1992)

d2$pid3.1<-pid.r(d2$pid.2000)
d2$pid3.2<-pid.r(d2$pid.2002)
d2$pid3.3<-pid.r(d2$pid.2004)

d3$pid3.1<-pid.r(d3$pid.w1)
d3$pid3.2<-pid.r(d3$pid.w11)
d3$pid3.3<-pid.r(d3$pid.w17)
d3$pid3.4<-pid.r(d3$pid.w19)

d4$pid3.1<-pid.r(d4$pid.2012a)
d4$pid3.2<-pid.r(d4$pid.2012b)
d4$pid3.3<-pid.r(d4$pid.2016)
d4$pid3.4<-pid.r(d4$pid.2017)
d4$pid3.5<-pid.r(d4$pid.2018)
d4$pid3.6<-pid.r(d4$pid.2019)


d5$pid3.1<-pid.r(d5$pid.1992)
d5$pid3.2<-pid.r(d5$pid.1993)
d5$pid3.3<-pid.r(d5$pid.1994)
d5$pid3.4<-pid.r(d5$pid.1995)
d5$pid3.5<-pid.r(d5$pid.1996)
d5$pid3.6<-pid.r(d5$pid.1997)

d6$pid3.1<-pid.r(d6$pid.w1)
d6$pid3.2<-pid.r(d6$pid.w2)


d1<-subset(d1, white.1990==1)
d1$Independent<-recode(d1$pid3.1, "1=0; 2=1; 3=0; else=NA")
d1$Republican<-recode(d1$pid3.1, "1=0; 2=0; 3=1; else=NA")
d1$IndependentXauthoritarianism<-d1$Independent*d1$authoritarianism
d1$RepublicanXauthoritarianism<-d1$Republican*d1$authoritarianism

d2<-subset(d2, white.2000==1)
d2$Independent<-recode(d2$pid3.1, "1=0; 2=1; 3=0; else=NA")
d2$Republican<-recode(d2$pid3.1, "1=0; 2=0; 3=1; else=NA")
d2$IndependentXauthoritarianism<-d2$Independent*d2$authoritarianism
d2$RepublicanXauthoritarianism<-d2$Republican*d2$authoritarianism

d2$gay1<-recode(d2$gay1.2000, "1:2=1; 3:4=2; else=NA")
d2$gay2<-recode(d2$gay1.2004, "1:2=1; 3:4=2; else=NA")


d3<-subset(d3, white.2008==1)
d3$Independent<-recode(d3$pid3.1, "1=0; 2=1; 3=0; else=NA")
d3$Republican<-recode(d3$pid3.1, "1=0; 2=0; 3=1; else=NA")
d3$IndependentXauthoritarianism<-d3$Independent*d3$authoritarianism
d3$RepublicanXauthoritarianism<-d3$Republican*d3$authoritarianism


d4<-subset(d4, white.2012==1)
d4$Independent<-recode(d4$pid3.1, "1=0; 2=1; 3=0; else=NA")
d4$Republican<-recode(d4$pid3.1, "1=0; 2=0; 3=1; else=NA")
d4$IndependentXauthoritarianism<-d4$Independent*d4$authoritarianism
d4$RepublicanXauthoritarianism<-d4$Republican*d4$authoritarianism

d5<-subset(d5, white.1992==1)
d5$Independent<-recode(d5$pid3.1, "1=0; 2=1; 3=0; else=NA")
d5$Republican<-recode(d5$pid3.1, "1=0; 2=0; 3=1; else=NA")
d5$IndependentXauthoritarianism<-d5$Independent*d5$authoritarianism
d5$RepublicanXauthoritarianism<-d5$Republican*d5$authoritarianism

d2$voteXauthoritarianism<-d2$authoritarianism*d2$vote.2000
d4$voteXauthoritarianism<-d4$authoritarianism*d4$vote.2012

d6<-subset(d6, white==1)
d6$Independent<-recode(d6$pid3.1, "1=0; 2=1; 3=0; else=NA")
d6$Republican<-recode(d6$pid3.1, "1=0; 2=0; 3=1; else=NA")
d6$IndependentXauthoritarianism<-d6$Independent*d6$authoritarianism1
d6$RepublicanXauthoritarianism<-d6$Republican*d6$authoritarianism1

#### Party Identification
#### Stability of authoritarianism ####
cor.test(d6$authoritarianism1, d6$authoritarianism2)

###### 1992 -1997 #########
# PID models # 
require(msm)
require(dplyr)
##Construct 1992-1996 data frame
## There is an error in the coding for 1995. I have respondents, but all the race items are missing
prop.table(table(d5$pid3.1, d5$pid3.5), 1)
dim(model.matrix(lm(d5$pid3.1~d5$pid3.5)))

data<-data.frame(id=rep(1:length(d5$id), times=6), state=c(d5$pid3.1, d5$pid3.2, d5$pid3.3, d5$pid3.4, d5$pid3.5, d5$pid3.6 ),
                 authoritarianism=rep(d5$authoritarianism, times=6),
                 sex=rep(d5$sex.1992, times=6),
                 college=rep(d5$college.1992, times=6),
                 income=rep(d5$income.1992, times=6),
                 age=(rep(d5$age.1992, times=6)-18)/72,
                 jewish=rep(d5$jewish.1992, times=6),
                 catholic=rep(d5$catholic.1992, times=6),
                 other=rep(d5$other.1992, times=6),
                 time=c(rep(c(1:6), each=length(d5$id) )))
data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)


twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
              )
twoway3.q
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age, 
           subject=id, data=data,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))

options(digits=3)
prevalence.msm(model, times=seq(0,6,1))
plot.prevalence.msm(model, mintime=0, maxtime=6,
                    legend.pos=c(0,10))

## The Hazard Ration, exp(b_k).
## A one unit increase in authoritarianism increases the rate of D->I 12 times
## An increase of D-R nearly 3 times. Slightly increases D-I, but only weakly effects
## moving to left.
hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth

table(data$college)
table(data$income)
table(data$sex)
table(data$catholic)

t<-pmatrix.msm(model, t=4, ci="none", covariates=list(authoritarianism=0, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=4, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))

#Low authoritarian
t
o
auth<-
grViz("
digraph  {
      graph [layout = neato,
      overlap = FALSE,
      forcelabels=TRUE]
      node [shape = rectangle]
     
      dt1 [pos =  '0,10!', label = 'Democrat\n(t-1)',  fontsize=14]
      rt1 [pos =  '0,-2!', label = 'Republican\n(t-1)',  fontsize=14]
      ldt1 [pos =  '0, 7!', label = 'Lean Democrat\n(t-1)',  fontsize=14]
      lrt1 [pos =  '0,1!', label = 'Lean Republican\n(t-1)',  fontsize=14]
      it1 [pos =  '0, 4!', label = 'Pure Independent\n(t-1)', fontsize=14]
#Dem
      dd [pos = '-4,8!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.75</b> <br/> <font color='black'> High=<i>0.60</i> </font> >]
      dld [pos = '-1.85,8!', label = 'LD',  fontsize=14, xlabel=<<b>0.17</b> <br/> <font color='black'> <i>0.18</i> </font> >]
      di [pos = '0, 8!',  label = 'Ind',  fontsize=14,  xlabel=<<b>0.03</b> <br/> <font color='black'> <i>0.06</i> </font> >]
      dlr [pos = '1.85, 8!', label = 'LR',  fontsize=14,  xlabel=<<b>0.03</b> <br/> <font color='black'> <i>0.09</i> </font> >]
      dr [pos = '4, 8!', label = 'Rep',  fontsize=14, xlabel=<<b>0.03</b> <br/> <font color='black'> <i>0.08</i> </font> >]

#Lean Dem
      ldd [pos = '-4,5!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.42</b> <br/> <font color='black'> High=<i>0.45</i> </font> >]
      ldld [pos = '-1.85,5!', label = 'LD',  fontsize=14, xlabel=<<b>0.30</b> <br/> <font color='black'> <i>0.17</i> </font> >]
      ldi [pos = '0,5!', label = 'Ind',  fontsize=14, xlabel=<<b>0.07</b> <br/> <font color='black'> <i>0.06</i> </font> >]
      ldlr [pos = '1.85,5!', label = 'LR',  fontsize=14, xlabel=<<b>0.09</b> <br/> <font color='black'> <i>0.12</i> </font> >]
      ldr [pos = '4,5!', label = 'Rep',  fontsize=14, xlabel=<<b>0.12</b> <br/> <font color='black'> <i>0.19</i> </font> >]
      
#Independent

      id [pos = '-4,2!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.26</b> <br/> <font color='black'> High=<i>0.40</i> </font> >]
      ild [pos = '-1.85,2!', label = 'LD',  fontsize=14, xlabel=<<b>0.27</b> <br/> <font color='black'> <i>0.17</i> </font> >]
      ii [pos = '0, 2!',  label = 'Ind',  fontsize=14, xlabel=<<b>0.09</b> <br/> <font color='black'> <i>0.06</i> </font> >]
      ilr [pos = '1.85, 2!',  label = 'LR',  fontsize=14, xlabel=<<b>0.12</b> <br/> <font color='black'> <i>0.13</i> </font> >]
      ir [pos = '4, 2!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.25</b> <br/> <font color='black'> <i>0.24</i> </font> >]
      
#LR

      lrd [pos = '-4,-1!', label = 'Dem',  fontsize=14,xlabel=<Low=<b>0.19</b> <br/> <font color='black'> High=<i>0.22</i> </font> >]
      lrld [pos = '-1.85,-1!', label = 'LD',  fontsize=14, xlabel=<<b>0.25</b> <br/> <font color='black'> <i>0.12</i> </font> >]
      lri [pos = '0, -1!',  label = 'Ind',  fontsize=14, xlabel=<<b>0.09</b> <br/> <font color='black'> <i>0.05</i> </font> >]
      lrlr [pos = '1.85, -1!',  label = 'LR',  fontsize=14, xlabel=<<b>0.14</b> <br/> <font color='black'> <i>0.14</i> </font> >]
      lrr [pos = '4, -1!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.33</b> <br/> <font color='black'> <i>0.47</i> </font> >]
      
#Rep

      rd [pos = '-4,-4!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.06</b> <br/> <font color='black'> High=<i>0.04</i> </font> >]
      rld [pos = '-1.85,-4!', label = 'LD',  fontsize=14, xlabel=<<b>0.15</b> <br/> <font color='black'> <i>0.04</i> </font> >]
      ri [pos = '0, -4!',  label = 'Ind',  fontsize=14,xlabel=<<b>0.08</b> <br/> <font color='black'> <i>0.02</i> </font> >]
      rlr [pos = '1.85, -4!',  label = 'LR',  fontsize=14,xlabel=<<b>0.15</b> <br/> <font color='black'> <i>0.11</i> </font> >]
      rr [pos = '4, -4!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.57</b> <br/> <font color='black'> <i>0.78</i> </font> >]
      
dt1->dd  [ headport=nw]
dt1->dld  [ headport=n]
dt1->di 
dt1->dlr  [ headport=n]
dt1->dr  [ headport=ne]

ldt1->ldd  [ headport=nw]
ldt1->ldld  [ headport=n]
ldt1->ldi 
ldt1->ldlr  [ headport=n]
ldt1->ldr  [ headport=ne]
      
it1->id  [ headport=nw]
it1->ild  [ headport=n]
it1->ii 
it1->ilr  [ headport=n]
it1->ir  [ headport=ne]
      
lrt1->lrd  [ headport=nw]
lrt1->lrld  [ headport=n]
lrt1->lri 
lrt1->lrlr [ headport=n]
lrt1->lrr  [ headport=ne]

      
rt1->rd [ headport=nw]
rt1->rld [ headport=n] 
rt1->ri
rt1->rlr [ headport=n]
rt1->rr [ headport=ne]
            
}")

auth

### Gingrich Effect ###

twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age,
          subject=id, data=data, pci=3,
          qmatrix=twoway3.q, obstype=1,
          method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))

printold.msm(model)

pmatrix.msm(model, t=2, 3,  covariates=list(authoritarianism=0, college=0, income=0,
                                            sex=0, catholic=0, age=mean(data$age, na.rm=T)))

pmatrix.msm(model, t=2, 3, ci="normal", cl=0.95, covariates=list(authoritarianism=0, college=0, income=0,
                                                                 sex=0, catholic=0, age=mean(data$age, na.rm=T)))
pmatrix.msm(model, t=2, 3, ci="normal", cl=0.95, covariates=list(authoritarianism=0, college=0, income=0,
                                                                 sex=0, catholic=0, age=mean(data$age, na.rm=T)))

### Gingrich INfluence ###
hazard.msm(model)

pmatrix.msm(model, t=2, 1, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
pmatrix.msm(model, t=2, 1, ci="none", covariates=list(authoritarianism=0, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
pmatrix.msm(model, t=2, 3, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
pmatrix.msm(model, t=2, 3, ci="none",  covariates=list(authoritarianism=0, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))

################################################################################################

##############################
### 2000 Panel Analysis ####
#############################

################################################################################################
d2$id<-c(1:dim(d2)[1])

prop.table(table(d2$pid3.1, d2$pid3.3), 1)
dim(model.matrix(lm(d2$pid3.1~d2$pid3.3)))

data<-data.frame(id=rep(1:length(d2$id), times=3), state=c(d2$pid3.1, d2$pid3.2, d2$pid3.3),
                 authoritarianism=rep(d2$authoritarianism, times=3),
                 sex=rep(d2$sex.2000, times=3),
                 college=rep(d2$college.2000, times=3),
                 income=rep(d2$income.2000, times=3),
                 age=(rep(d2$age.2000, times=3)-18)/79,
                 jewish=rep(d2$jewish.2000, times=3),
                 catholic=rep(d2$catholic.2000, times=3),
                 other=rep(d2$other.2000, times=3),
                 time=c(rep(c(1:3), each=length(d2$id) )))
require(dplyr)
data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                c(0.1,0, 0.10, 0, 0),
                c(0, 0.10, 0, 0.1, 0),
                c(0 ,0, 0.10, 0, 0.1),
                c(0 ,0, 0, 0.1, 0)
)
#rownames(twoway3.q)<-colnames(twoway3.q)<-c("Democrat", "Independent", "Republican")
twoway3.q
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age, 
           subject=id, data=data,
           qmatrix=twoway3.q, gen.inits=TRUE, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1, fnscale=2000, maxit=20000, reltol=1e-8))
options(digits=3)
prevalence.msm(model, times=seq(0,3,1))
plot.prevalence.msm(model, mintime=0, maxtime=3,
                    legend.pos=c(0,10))


table(data$college)
table(data$income)
table(data$sex)
table(data$catholic)
hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,  other=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0, other=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth

t<-pmatrix.msm(model, t=2, ci="none", covariates=list(authoritarianism=0, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=2, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
print(t)
print(o)
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> 291e8b4bc26160f1d3c52b723655023ac79c9e0c

### Stanley's question about heterogeneous transitions ####
# 
# model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age,
#            subject=id, data=data, pci=2,
#            qmatrix=twoway3.q, obstype=1,
#            method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))
# 
# pmatrix.msm(model, t=1, 1, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
#                                                       sex=0, catholic=0, age=mean(data$age, na.rm=T)))
# pmatrix.msm(model, t=1, 1, ci="none", covariates=list(authoritarianism=0, college=0, income=0,
#                                                       sex=0, catholic=0, age=mean(data$age, na.rm=T)))
# pmatrix.msm(model, t=1, 2, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
#                                                       sex=0, catholic=0, age=mean(data$age, na.rm=T)))
# pmatrix.msm(model, t=1, 2, ci="none",  covariates=list(authoritarianism=0, college=0, income=0,
#                                                        sex=0, catholic=0, age=mean(data$age, na.rm=T)))
# 
<<<<<<< HEAD
>>>>>>> 291e8b4bc26160f1d3c52b723655023ac79c9e0c
=======
>>>>>>> 291e8b4bc26160f1d3c52b723655023ac79c9e0c
table(data$college)
table(data$income)
table(data$sex)
table(data$catholic)


t
o
auth<-
  grViz("
        digraph  {
        graph [layout = neato,
        overlap = FALSE,
        forcelabels=TRUE]
        node [shape = rectangle]
        
        dt1 [pos =  '0,10!', label = 'Democrat\n(t-1)',  fontsize=14]
        rt1 [pos =  '0,-2!', label = 'Republican\n(t-1)',  fontsize=14]
        ldt1 [pos =  '0, 7!', label = 'Lean Democrat\n(t-1)',  fontsize=14]
        lrt1 [pos =  '0,1!', label = 'Lean Republican\n(t-1)',  fontsize=14]
        it1 [pos =  '0, 4!', label = 'Pure Independent\n(t-1)', fontsize=14]
        #Dem
        dd [pos = '-4,8!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.85</b> <br/> <font color='black'> High=<i>0.59</i> </font> >]
        dld [pos = '-1.85,8!', label = 'LD',  fontsize=14, xlabel=<<b>0.14</b> <br/> <font color='black'> <i>0.07</i> </font> >]
        di [pos = '0, 8!',  label = 'Ind',  fontsize=14,  xlabel=<<b>0.01</b> <br/> <font color='black'> <i>0.05</i> </font> >]
        dlr [pos = '1.85, 8!', label = 'LR',  fontsize=14,  xlabel=<<b>0.01</b> <br/> <font color='black'> <i>0.18</i> </font> >]
        dr [pos = '4, 8!', label = 'Rep',  fontsize=14, xlabel=<<b>0.01</b> <br/> <font color='black'> <i>0.12</i> </font> >]
        
        #Lean Dem
        ldd [pos = '-4,5!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.26</b> <br/> <font color='black'> High=<i>0.43</i> </font> >]
        ldld [pos = '-1.85,5!', label = 'LD',  fontsize=14, xlabel=<<b>0.57</b> <br/> <font color='black'> <i>0.06</i> </font> >]
        ldi [pos = '0,5!', label = 'Ind',  fontsize=14, xlabel=<<b>0.04</b> <br/> <font color='black'> <i>0.05</i> </font> >]
        ldlr [pos = '1.85,5!', label = 'LR',  fontsize=14, xlabel=<<b>0.09</b> <br/> <font color='black'> <i>0.21</i> </font> >]
        ldr [pos = '4,5!', label = 'Rep',  fontsize=14, xlabel=<<b>0.04</b> <br/> <font color='black'> <i>0.25</i> </font> >]
        
        #Independent
        
        id [pos = '-4,2!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.03</b> <br/> <font color='black'> High=<i>0.21</i> </font> >]
        ild [pos = '-1.85,2!', label = 'LD',  fontsize=14, xlabel=<<b>0.12</b> <br/> <font color='black'> <i>0.04</i> </font> >]
        ii [pos = '0, 2!',  label = 'Ind',  fontsize=14, xlabel=<<b>0.14</b> <br/> <font color='black'> <i>0.06</i> </font> >]
        ilr [pos = '1.85, 2!',  label = 'LR',  fontsize=14, xlabel=<<b>0.39</b> <br/> <font color='black'> <i>0.25</i> </font> >]
        ir [pos = '4, 2!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.32</b> <br/> <font color='black'> <i>0.44</i> </font> >]
        
        #LR
        
        lrd [pos = '-4,-1!', label = 'Dem',  fontsize=14,xlabel=<Low=<b>0.03</b> <br/> <font color='black'> High=<i>0.20</i> </font> >]
        lrld [pos = '-1.85,-1!', label = 'LD',  fontsize=14, xlabel=<<b>0.11</b> <br/> <font color='black'> <i>0.04</i> </font> >]
        lri [pos = '0, -1!',  label = 'Ind',  fontsize=14, xlabel=<<b>0.14</b> <br/> <font color='black'> <i>0.06</i> </font> >]
        lrlr [pos = '1.85, -1!',  label = 'LR',  fontsize=14, xlabel=<<b>0.39</b> <br/> <font color='black'> <i>0.25</i> </font> >]
        lrr [pos = '4, -1!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.34</b> <br/> <font color='black'> <i>0.45</i> </font> >]
        
        #Rep
        
        rd [pos = '-4,-4!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.01</b> <br/> <font color='black'> High=<i>0.03</i> </font> >]
        rld [pos = '-1.85,-4!', label = 'LD',  fontsize=14, xlabel=<<b>0.02</b> <br/> <font color='black'> <i>0.01</i> </font> >]
        ri [pos = '0, -4!',  label = 'Ind',  fontsize=14,xlabel=<<b>0.06</b> <br/> <font color='black'> <i>0.02</i> </font> >]
        rlr [pos = '1.85, -4!',  label = 'LR',  fontsize=14,xlabel=<<b>0.17</b> <br/> <font color='black'> <i>0.08</i> </font> >]
        rr [pos = '4, -4!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.74</b> <br/> <font color='black'> <i>0.86</i> </font> >]
        
        dt1->dd  [ headport=nw]
        dt1->dld  [ headport=n]
        dt1->di 
        dt1->dlr  [ headport=n]
        dt1->dr  [ headport=ne]
        
        ldt1->ldd  [ headport=nw]
        ldt1->ldld  [ headport=n]
        ldt1->ldi 
        ldt1->ldlr  [ headport=n]
        ldt1->ldr  [ headport=ne]
        
        it1->id  [ headport=nw]
        it1->ild  [ headport=n]
        it1->ii 
        it1->ilr  [ headport=n]
        it1->ir  [ headport=ne]
        
        lrt1->lrd  [ headport=nw]
        lrt1->lrld  [ headport=n]
        lrt1->lri 
        lrt1->lrlr [ headport=n]
        lrt1->lrr  [ headport=ne]
        
        
        rt1->rd [ headport=nw]
        rt1->rld [ headport=n] 
        rt1->ri
        rt1->rlr [ headport=n]
        rt1->rr [ headport=ne]
        
        }")

auth




##### You Gov Data ##########
#### 2012-2017
head(d4)
d4$id<-c(1:length(d4$pid3.1))
prop.table(table(d4$pid3.1, d4$pid3.3), 1)
dim(model.matrix(lm(d4$pid3.1~d4$pid3.3)))


<<<<<<< HEAD
<<<<<<< HEAD
data<-data.frame(id=rep(1:length(d4$id), times=5), state=c(d4$pid3.1, d4$pid3.3, d4$pid3.4, 
                                                           d4$pid3.5, d4$pid3.6),
                 authoritarianism=rep(d4$authoritarianism, times=5),
                 sex=rep(d4$sex.2012, times=5),
                 college=rep(d4$college.2012, times=5),
                 income=rep(d4$income.2012, times=5),
                 age=(rep(d4$age.2012, times=5)-22),
                 jewish=rep(d4$jewish.2012, times=5),
                 catholic=rep(d4$catholic.2012, times=5),
                 other=rep(d4$other.2012, times=5),
                 time=c(rep(c(1:5), each=length(d4$id) )))
data<-data.frame(id=rep(1:length(d4$id), times=2), state=c(d4$pid3.1, d4$pid3.3),
                 authoritarianism=rep(d4$authoritarianism, times=2),
                 sex=rep(d4$sex.2012, times=2),
                 college=rep(d4$college.2012, times=2),
                 income=rep(d4$income.2012, times=2),
                 age=(rep(d4$age.2012, times=2)-22)/73,
                 jewish=rep(d4$jewish.2012, times=2),
                 catholic=rep(d4$catholic.2012, times=2),
                 other=rep(d4$other.2012, times=2),
                 time=c(rep(c(1:2), each=length(d4$id) )))

require(dplyr)
data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)
twoway3.q


model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic, 
           subject=id, data=data,pci=2,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))


# Non-authoritarians: Democrats shift to the right not as much as Republicans shift to the
# left; Non-authoritarians are disenchanted with Trump. 

# Authoritarians: There isn't much of a change over time. Defecting 

require(diagram)
t1na<-pmatrix.msm(model, t=1, 1 , covariates=list(authoritarianism=0, college=0, income=1,
                                                      sex=1, catholic=0))

t2na<-pmatrix.msm(model, t=3, 3, covariates=list(authoritarianism=0, college=0, income=1,
                                           sex=1, catholic=0))

t1nb<-pmatrix.msm(model, t=1, 1 , covariates=list(authoritarianism=1, college=0, income=1,
                                                  sex=1, catholic=0))

t2nb<-pmatrix.msm(model, t=3, 3, covariates=list(authoritarianism=1, college=0, income=1,
                                                 sex=1, catholic=0))

p1<-matrix(t1na, ncol=5)
p2<-matrix(t2na, ncol=5)
p3<-p1-p2

p4<-matrix(t1nb, ncol=5)
p5<-matrix(t2nb, ncol=5)
p6<-p4-p5

rownames(p1)<-rownames(p4)<-c("D(2011)", "LD(2011)", "I(2011)", "LR(2011)", "R(2011)")
colnames(p1)<-colnames(p4)<-c("Democrat(2016)", "Lean Democrat(2016)", "Independent(2016)", "Lean Republican(2016)", "Republican(2016)")

rownames(p2)<-rownames(p5)<-c("D(2017)", "LD(2017)", "I(2017)", "LR(2017)", "R(2017)")
colnames(p2)<-colnames(p5)<-c("Democrat(2019)", "Lean Democrat(2019)", "Independent(2019)", "Lean Republican(2019)", "Republican(2019)")



a<-ggcorrplot(p1, lab=TRUE, type="full", method="circle", tl.col="blue",
           ggtheme = ggplot2::theme_gray, show.legend = FALSE,
           title="Non-Authoritarian", tl.srt = 45,  tl.cex=8, lab_size=3)
b<-ggcorrplot(p2, lab=TRUE, type="full", method="circle",
              ggtheme = ggplot2::theme_gray, show.legend = FALSE,
              title="Non-Authoritarian", tl.srt = 45,   tl.cex=8, lab_size=3)
c<-ggcorrplot(p4, lab=TRUE, type="full", method="circle",
              ggtheme = ggplot2::theme_gray, show.legend = FALSE,
              title="Authoritarian", tl.srt = 45 ,  tl.cex=8, lab_size=3)
d<-ggcorrplot(p5, lab=TRUE, type="full", method="circle",
              ggtheme = ggplot2::theme_gray, show.legend = FALSE,
              title="Authoritarian", tl.srt = 45, tl.cex=8, lab_size=3)

ggarrange(a,b,c,d)

dev.copy(png,'ch6_A2.jpg',
         width = 750, height = 500)
dev.off()
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age, 
           subject=id, data=data,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))
prevalence.msm(model, times=seq(0,3,1))
plot.prevalence.msm(model, mintime=0, maxtime=3,
                    legend.pos=c(0,10))

## The Hazard Ration, exp(b_k).
## A one unit increase in authoritarianism increases the rate of D->I 12 times
## An increase of D-R nearly 3 times. Slightly increases D-I, but only weakly effects
## moving to left.
hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=1,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=1,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth
t<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0, college=0, income=1,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1, college=0, income=1,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
t
o
auth<-
  grViz("
        digraph  {
        graph [layout = neato,
        overlap = FALSE,
        forcelabels=TRUE]
        node [shape = rectangle]
        
        dt1 [pos =  '0,10!', label = 'Democrat\n(t-1)',  fontsize=14]
        rt1 [pos =  '0,-2!', label = 'Republican\n(t-1)',  fontsize=14]
        ldt1 [pos =  '0, 7!', label = 'Lean Democrat\n(t-1)',  fontsize=14]
        lrt1 [pos =  '0,1!', label = 'Lean Republican\n(t-1)',  fontsize=14]
        it1 [pos =  '0, 4!', label = 'Pure Independent\n(t-1)', fontsize=14]
        #Dem
        dd [pos = '-4,8!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.88</b> <br/> <font color='black'> High=<i>0.74</i> </font> >]
        dld [pos = '-1.85,8!', label = 'LD',  fontsize=14, xlabel=<<b>0.08</b> <br/> <font color='black'> <i>0.06</i> </font> >]
        di [pos = '0, 8!',  label = 'Ind',  fontsize=14,  xlabel=<<b>0.03</b> <br/> <font color='black'> <i>0.12</i> </font> >]
        dlr [pos = '1.85, 8!', label = 'LR',  fontsize=14,  xlabel=<<b>0.01</b> <br/> <font color='black'> <i>0.06</i> </font> >]
        dr [pos = '4, 8!', label = 'Rep',  fontsize=14, xlabel=<<b>0.01</b> <br/> <font color='black'> <i>0.02</i> </font> >]
        
        #Lean Dem
        ldd [pos = '-4,5!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.42</b> <br/> <font color='black'> High=<i>0.23</i> </font> >]
        ldld [pos = '-1.85,5!', label = 'LD',  fontsize=14, xlabel=<<b>0.26</b> <br/> <font color='black'> <i>0.05</i> </font> >]
        ldi [pos = '0,5!', label = 'Ind',  fontsize=14, xlabel=<<b>0.25</b> <br/> <font color='black'> <i>0.32</i> </font> >]
        ldlr [pos = '1.85,5!', label = 'LR',  fontsize=14, xlabel=<<b>0.06</b> <br/> <font color='black'> <i>0.27</i> </font> >]
        ldr [pos = '4,5!', label = 'Rep',  fontsize=14, xlabel=<<b>0.07</b> <br/> <font color='black'> <i>0.14</i> </font> >]
        
        #Independent
        
        id [pos = '-4,2!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.10</b> <br/> <font color='black'> High=<i>0.05</i> </font> >]
        ild [pos = '-1.85,2!', label = 'LD',  fontsize=14, xlabel=<<b>0.16</b> <br/> <font color='black'> <i>0.04</i> </font> >]
        ii [pos = '0, 2!',  label = 'Ind',  fontsize=14, xlabel=<<b>0.48</b> <br/> <font color='black'> <i>0.34</i> </font> >]
        ilr [pos = '1.85, 2!',  label = 'LR',  fontsize=14, xlabel=<<b>0.19</b> <br/> <font color='black'> <i>0.35</i> </font> >]
        ir [pos = '4, 2!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.08</b> <br/> <font color='black'> <i>0.22</i> </font> >]
        
        #LR
        
        lrd [pos = '-4,-1!', label = 'Dem',  fontsize=14,xlabel=<Low=<b>0.02</b> <br/> <font color='black'> High=<i>0.01</i> </font> >]
        lrld [pos = '-1.85,-1!', label = 'LD',  fontsize=14, xlabel=<<b>0.05</b> <br/> <font color='black'> <i>0.02</i> </font> >]
        lri [pos = '0, -1!',  label = 'Ind',  fontsize=14, xlabel=<<b>0.25</b> <br/> <font color='black'> <i>0.18</i> </font> >]
        lrlr [pos = '1.85, -1!',  label = 'LR',  fontsize=14, xlabel=<<b>0.35</b> <br/> <font color='black'> <i>0.35</i> </font> >]
        lrr [pos = '4, -1!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.32</b> <br/> <font color='black'> <i>0.44</i> </font> >]
        
        #Rep
        
        rd [pos = '-4,-4!', label = 'Dem',  fontsize=14, xlabel=<Low=<b>0.01</b> <br/> <font color='black'> High=<i>0.01</i> </font> >]
        rld [pos = '-1.85,-4!', label = 'LD',  fontsize=14, xlabel=<<b>0.01</b> <br/> <font color='black'> <i>0.01</i> </font> >]
        ri [pos = '0, -4!',  label = 'Ind',  fontsize=14,xlabel=<<b>0.04</b> <br/> <font color='black'> <i>0.02</i> </font> >]
        rlr [pos = '1.85, -4!',  label = 'LR',  fontsize=14,xlabel=<<b>0.13</b> <br/> <font color='black'> <i>0.06</i> </font> >]
        rr [pos = '4, -4!',  label = 'Rep',  fontsize=14, xlabel=<<b>0.82</b> <br/> <font color='black'> <i>0.92</i> </font> >]
        
        dt1->dd  [ headport=nw]
        dt1->dld  [ headport=n]
        dt1->di 
        dt1->dlr  [ headport=n]
        dt1->dr  [ headport=ne]
        
        ldt1->ldd  [ headport=nw]
        ldt1->ldld  [ headport=n]
        ldt1->ldi 
        ldt1->ldlr  [ headport=n]
        ldt1->ldr  [ headport=ne]
        
        it1->id  [ headport=nw]
        it1->ild  [ headport=n]
        it1->ii 
        it1->ilr  [ headport=n]
        it1->ir  [ headport=ne]
        
        lrt1->lrd  [ headport=nw]
        lrt1->lrld  [ headport=n]
        lrt1->lri 
        lrt1->lrlr [ headport=n]
        lrt1->lrr  [ headport=ne]
        
        
        rt1->rd [ headport=nw]
        rt1->rld [ headport=n] 
        rt1->ri
        rt1->rlr [ headport=n]
        rt1->rr [ headport=ne]
        
        }")

auth




################################################################################################
###### 2016-2017                    ######################################################
################################################################################################
#### 2012-2017
head(d4)
d4$id<-c(1:length(d4$pid3.3))
prop.table(table(d4$pid3.3, d4$pid3.4), 1)
dim(model.matrix(lm(d4$pid3.3~d4$pid3.4)))


data<-data.frame(id=rep(1:length(d4$id), times=2), state=c(d4$pid3.3, d4$pid3.4),
                 authoritarianism=rep(d4$authoritarianism, times=2),
                 sex=rep(d4$sex.2012, times=2),
                 college=rep(d4$college.2012, times=2),
                 income=rep(d4$income.2012, times=2),
                 age=(rep(d4$age.2012, times=2)-22)/73,
                 jewish=rep(d4$jewish.2012, times=2),
                 catholic=rep(d4$catholic.2012, times=2),
                 other=rep(d4$other.2012, times=2),
                 time=c(rep(c(1:2), each=length(d4$id) )))

require(dplyr)
data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)
twoway3.q

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age, 
           subject=id, data=data,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000))
prevalence.msm(model, times=seq(0,3,1))
plot.prevalence.msm(model, mintime=0, maxtime=3,
                    legend.pos=c(0,10))

## The Hazard Ration, exp(b_k).
## A one unit increase in authoritarianism increases the rate of D->I 12 times
## An increase of D-R nearly 3 times. Slightly increases D-I, but only weakly effects
## moving to left.
hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth
t<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0, college=0, income=0,
                                                      sex=0, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
                                                      sex=0, catholic=0, age=mean(data$age, na.rm=T)))

options("scipen"=10, "digits"=2)
t
o
auth<-grViz("
            digraph SEM {
            graph [layout = neato,
            overlap = TRUE,
            outputorder = edgesfirst]
            node [shape = rectangle]
            
            dt1 [pos =  '0,10!', label = 'Democrat\n(t-1)',  fontsize=12]
            rt1 [pos =  '0,-2!', label = 'Republican\n(t-1)',  fontsize=12]
            ldt1 [pos =  '0, 7!', label = 'Lean Democrat\n(t-1)',  fontsize=12]
            lrt1 [pos =  '0,1!', label = 'Lean Republican\n(t-1)',  fontsize=12]
            it1 [pos =  '0, 4!', label = 'Pure Independent\n(t-1)',  fontsize=12]
            #Dem
            dd [pos = '-3.5,8!', label = 'Dem',  fontsize=12]
            dld [pos = '-1.5,8!', label = 'LD',  fontsize=12]
            di [pos = '0, 8!',  label = 'Ind',  fontsize=12]
            dlr [pos = '1.5, 8!', label = 'LR',  fontsize=12]
            dr [pos = '3.5, 8!', label = 'Rep', fontsize=12]
            
            #Lean Dem
            ldd [pos = '-3.5,5!', label = 'Dem',  fontsize=12]
            ldld [pos = '-1.5,5!', label = 'LD',  fontsize=12]
            ldi [pos = '0,5!', label = 'Ind',  fontsize=12]
            ldlr [pos = '1.5,5!', label = 'LR',  fontsize=12]
            ldr [pos = '3.5,5!', label = 'Rep',  fontsize=12]
            
            #Independent
            
            id [pos = '-3.5,2!', label = 'Dem',  fontsize=12]
            ild [pos = '-1.5,2!', label = 'LD',  fontsize=12]
            ii [pos = '0, 2!',  label = 'Ind',  fontsize=12]
            ilr [pos = '1.5, 2!',  label = 'LR',  fontsize=12]
            ir [pos = '3.5, 2!',  label = 'Rep',  fontsize=12]
            
            #LR
            
            lrd [pos = '-3.5,-1!', label = 'Dem',  fontsize=12]
            lrld [pos = '-1.5,-1!', label = 'LD',  fontsize=12]
            lri [pos = '0, -1!',  label = 'Ind',  fontsize=12]
            lrlr [pos = '1.5, -1!',  label = 'LR',  fontsize=12]
            lrr [pos = '3.5, -1!',  label = 'Rep',  fontsize=12]
            
            #Rep
            
            rd [pos = '-3.5,-4!', label = 'Dem',  fontsize=12]
            rld [pos = '-1.5,-4!', label = 'LD',  fontsize=12]
            ri [pos = '0, -4!',  label = 'Ind',  fontsize=12]
            rlr [pos = '1.5, -4!',  label = 'LR',  fontsize=12]
            rr [pos = '3.5, -4!',  label = 'Rep',  fontsize=12]
            
            dt1->dd [label = '0.90\n[0.77]',  fontsize=12]
            dt1->dld [label = '0.08 \n[0.08]',  fontsize=12]
            dt1->di [label = '0.02\n[0.11]',  fontsize=12]
            dt1->dlr [label = '0.01\n[0.03]',  fontsize=12]
            dt1->dr [label = '0.01\n[0.01]',  fontsize=12]
            
            ldt1->ldd [label = '0.40\n[0.27]',  fontsize=12]
            ldt1->ldld [label = '0.36\n[0.08]',  fontsize=12]
            ldt1->ldi [label = '0.20\n[0.37]',  fontsize=12]
            ldt1->ldlr [label = '0.03\n[0.20]',  fontsize=12]
            ldt1->ldr [label = '0.01\n[0.08]',  fontsize=12]
            
            it1->id [label = '0.07\n[0.05]',  fontsize=12]
            it1->ild [label = '0.15\n[0.05]',  fontsize=12]
            it1->ii [label = '0.56\n[0.43]',  fontsize=12]
            it1->ilr [label = '0.16\n[0.30]',  fontsize=12]
            it1->ir [label = '0.06\n[0.17]',  fontsize=12]
            
            lrt1->lrd [label = '0.01\n[0.01]',  fontsize=12]
            lrt1->lrld [label = '0.05\n[0.02]',  fontsize=12]
            lrt1->lri [label = '0.27\n[0.21]',  fontsize=12]
            lrt1->lrlr [label = '0.36\n[0.34]',  fontsize=12]
            lrt1->lrr [label = '0.30\n[0.42]',  fontsize=12]
            
            
            rt1->rd [label = '0.01\n[0.02]',  fontsize=12]
            rt1->rld [label = '0.01\n[0.02]',  fontsize=12]
            rt1->ri [label = '0.03\n[0.01]',  fontsize=12]
            rt1->rlr [label = '0.09\n[0.13]',  fontsize=12]
            rt1->rr [label = '0.88\n[0.83]',  fontsize=12]
            
            }")





################################################################################################
###### Voting, Presidential and Otherwise ######################################################
################################################################################################

#### Vote Switching 92-94 ####
require(dplyr)
data<-data.frame(id=rep(1:length(d5$id), times=3), state=c(d5$vote.1992H, d5$vote.1994H, d5$vote.1996H),
                 authoritarianism=rep(d5$authoritarianism, times=3),
                 sex=rep(d5$sex.1992, times=3),
                 college=rep(d5$college.1992, times=3),
                 income=rep(d5$income.1992, times=3),
                 age=(rep(d5$age.1992, times=3)-18)/72,
                 jewish=rep(d5$jewish.1992, times=3),
                 catholic=rep(d5$catholic.1992, times=3),
                 other=rep(d5$other.1992, times=3),
                 time=c(rep(c(1:3), each=length(d5$id) )))
data<-data%>%
  arrange(id, time)%>%
  na.omit

statetable.msm(state, id, data)
twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0))
data$state<-1+data$state
rownames(twoway2.q)<-colnames(twoway2.q)<-c("Democrat", "Republican")
twoway2.q
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1))

t<-pmatrix.msm(model, t=1, 1, covariates=list(authoritarianism=0, college=0, income=0,
                                                           sex=1, catholic=0, age=mean(data$age, na.rm=T)))
t<-pmatrix.msm(model, t=1, 1, ci="normal", covariates=list(authoritarianism=0, college=0, income=0,
                                                           sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, 1, ci="normal", covariates=list(authoritarianism=1, college=0, income=0,
                                                           sex=1, catholic=0, age=mean(data$age, na.rm=T)))
t<-pmatrix.msm(model, t=1, 1, ci="normal", covariates=list(authoritarianism=0, college=0, income=0,
                                                           sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, 1, ci="normal", covariates=list(authoritarianism=1, college=0, income=0,
                                                           sex=1, catholic=0, age=mean(data$age, na.rm=T)))



#Dem Dem l
#Dem Rep l

#Dem Dem h
#Dem Rep h

#Rep Dem l
#Rep Rep l

#Rep Dem h
#Rep Rep h


t
o



require(DiagrammeR)
plot.1992<-
  grViz("
        digraph  {
        graph [layout = neato,
        overlap = FALSE,
        forcelabels=TRUE,
        compound = true, nodesep = .5, ranksep = .25,
        color = crimson]
        node [shape = rectangle]
        
        dt1 [pos =  '0,3!', label = 'Democratic Candidate\n(t-1)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Republican Candidate\n(t-1)',  fontsize=14]
        
        #Dem
<<<<<<< HEAD
<<<<<<< HEAD
        dd [pos = '-2,1!', label = 'Democrat',  fontsize=14, xlabel=<Low=<b>0.82</b> <br/> <font color='black'> High=<i>0.76</i> </font> >]
        dr [pos = '2, 1!', label = 'Republican',  fontsize=14, xlabel=<<b>0.18</b> <br/> <font color='black'> <i>0.23</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Democratic',  fontsize=14, xlabel=<Low=<b>0.11</b> <br/> <font color='black'> High=<i>0.16</i> </font> >]
        rr [pos = '2, -2!',  label = 'Republican',  fontsize=14, xlabel=<<b>0.89</b> <br/> <font color='black'> <i>0.84</i> </font> >]
        dd [pos = '-2,1!', label = 'Democrat',  fontsize=14, xlabel=<Low=<b>0.84</b> <br/> <font color='black'> High=<i>0.83</i> </font> >]
        dr [pos = '2, 1!', label = 'Republican',  fontsize=14, xlabel=<<b>0.16</b> <br/> <font color='black'> <i>0.17</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Democratic',  fontsize=14, xlabel=<Low=<b>0.13</b> <br/> <font color='black'> High=<i>0.22</i> </font> >]
        rr [pos = '2, -2!',  label = 'Republican',  fontsize=14, xlabel=<<b>0.87</b> <br/> <font color='black'> <i>0.78</i> </font> >]

        dt1->dd  [ headport=n]
        dt1->dr  [ headport=n]
        
        rt1->rd [ headport=n]
        rt1->rr [ headport=n]
        
        }")

plot.1992

#### 2000-2004
##### Vote in House Election ###
d2$id<-c(1:dim(d2)[1])
data<-data.frame(id=rep(1:length(d2$id), times=3), state=c(d2$vote.2000H, d2$vote.2002H, d2$vote.2004H),
                 authoritarianism=rep(d2$authoritarianism, times=3),
                 sex=rep(d2$sex.2000, times=3),
                 college=rep(d2$college.2000, times=3),
                 income=rep(d2$income.2000, times=3),
                 age=(rep(d2$age.2000, times=3)-18)/79,
                 jewish=rep(d2$jewish.2000, times=3),
                 catholic=rep(d2$catholic.2000, times=3),
                 other=rep(d2$other.2000, times=3),
                 time=c(rep(c(1:3), each=length(d2$id) )))
data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)
twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0.))
data$state<-1+data$state
rownames(twoway2.q)<-colnames(twoway2.q)<-c("Democrat", "Republican")
twoway2.q

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1))

## The Hazard Ration, exp(b_k).
## A one unit increase in authoritarianism increases the rate of D->I 12 times
## An increase of D-R nearly 3 times. Slightly increases D-I, but only weakly effects
## moving to left.
hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,  other=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0, other=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth
t<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0, college=0, income=0, other=0,
                                                      sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1, college=0, income=0, other=0,
                                                      sex=1, catholic=0, age=mean(data$age, na.rm=T)))



t
o

plot.2000<-
  grViz("
        digraph  {
        graph [layout = neato,
        overlap = FALSE,
        forcelabels=TRUE,
        compound = true, nodesep = .5, ranksep = .25,
        color = crimson]
        node [shape = rectangle]
        
        dt1 [pos =  '0,3!', label = 'Democratic Candidate\n(t-1)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Republican Candidate\n(t-1)',  fontsize=14]
        
        #Dem
        dd [pos = '-2,1!', label = 'Democrat',  fontsize=14, xlabel=<Low=<b>0.92</b> <br/> <font color='black'> High=<i>0.63</i> </font> >]
        dr [pos = '2, 1!', label = 'Republican',  fontsize=14, xlabel=<<b>0.07</b> <br/> <font color='black'> <i>0.37</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Democratic',  fontsize=14, xlabel=<Low=<b>0.15</b> <br/> <font color='black'> High=<i>0.12</i> </font> >]
        rr [pos = '2, -2!',  label = 'Republican',  fontsize=14, xlabel=<<b>0.85</b> <br/> <font color='black'> <i>0.88</i> </font> >]
        dd [pos = '-2,1!', label = 'Democrat',  fontsize=14, xlabel=<Low=<b>0.94</b> <br/> <font color='black'> High=<i>0.75</i> </font> >]
        dr [pos = '2, 1!', label = 'Republican',  fontsize=14, xlabel=<<b>0.06</b> <br/> <font color='black'> <i>0.25</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Democratic',  fontsize=14, xlabel=<Low=<b>0.15</b> <br/> <font color='black'> High=<i>0.13</i> </font> >]
        rr [pos = '2, -2!',  label = 'Republican',  fontsize=14, xlabel=<<b>0.85</b> <br/> <font color='black'> <i>0.86</i> </font> >]

        dt1->dd  [ headport=n]
        dt1->dr  [ headport=n]
        
        rt1->rd [ headport=n]
        rt1->rr [ headport=n]
        
        }")

plot.2000

#########################################################################################################
### Presidential Voting ###
#########################################################################################################

data<-data.frame(id=rep(1:length(d5$id), times=2), state=c(d5$vote.1992, d5$vote.1996),
                 authoritarianism=rep(d5$authoritarianism, times=2),
                 sex=rep(d5$sex.1992, times=2),
                 college=rep(d5$college.1992, times=2),
                 income=rep(d5$income.1992, times=2),
                 age=(rep(d5$age.1992, times=2)-18)/72,
                 jewish=rep(d5$jewish.1992, times=2),
                 catholic=rep(d5$catholic.1992, times=2),
                 other=rep(d5$other.1992, times=2),
                 time=c(rep(c(1:2), each=length(d5$id) )))
require(dplyr)
data<-data%>%
  arrange(id, time)%>%
  na.omit

statetable.msm(state, id, data)
twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0.))
data$state<-1+data$state
rownames(twoway2.q)<-colnames(twoway2.q)<-c("Democrat", "Republican")
twoway2.q
model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1, fnscale=2000, maxit=20000, reltol=1e-8))


hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth
t<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0, college=0, income=0,
                                                      sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
                                                      sex=1, catholic=0, age=mean(data$age, na.rm=T)))
print(t)
print(o)




plot.1992<-
  grViz("
        digraph  {
        graph [layout = neato,
        overlap = FALSE,
        forcelabels=TRUE,
        compound = true, nodesep = .5, ranksep = .25,
        color = crimson]
        node [shape = rectangle]
        
        dt1 [pos =  '0,3!', label = 'Clinton(1992, Democrat)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Bush(1992, Republican)',  fontsize=14]
        
        #Dem
        dd [pos = '-2,1!', label = 'Clinton(1996, Democrat)',  fontsize=14, xlabel=<Low=<b>0.96</b> <br/> <font color='black'> High=<i>0.74</i> </font> >]
        dr [pos = '2, 1!', label = 'Dole(1996, Republican)',  fontsize=14, xlabel=<<b>0.04</b> <br/> <font color='black'> <i>0.26</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Clinton(1996, Democrat)',  fontsize=14, xlabel=<Low=<b>0.09</b> <br/> <font color='black'> High=<i>0.13</i> </font> >]
        rr [pos = '2, -2!',  label = 'Dole(1996, Republican)',  fontsize=14, xlabel=<<b>0.91</b> <br/> <font color='black'> <i>0.87</i> </font> >]

        dt1 [pos =  '0,3!', label = 'Democratic Candidate\n(t-1)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Republican Candidate\n(t-1)',  fontsize=14]
        
        #Dem
        dd [pos = '-2,1!', label = 'Democrat',  fontsize=14, xlabel=<Low=<b>0.97</b> <br/> <font color='black'> High=<i>0.84</i> </font> >]
        dr [pos = '2, 1!', label = 'Republican',  fontsize=14, xlabel=<<b>0.03</b> <br/> <font color='black'> <i>0.16</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Democratic',  fontsize=14, xlabel=<Low=<b>0.16</b> <br/> <font color='black'> High=<i>0.23</i> </font> >]
        rr [pos = '2, -2!',  label = 'Republican',  fontsize=14, xlabel=<<b>0.84</b> <br/> <font color='black'> <i>0.77</i> </font> >]

        dt1->dd  [ headport=n]
        dt1->dr  [ headport=n]
        
        rt1->rd [ headport=n]
        rt1->rr [ headport=n]
        
        }")

plot.1992





#### Presidential Voting #####
d2$id<-c(1:dim(d2)[1])
data<-data.frame(id=rep(1:length(d2$id), times=2), state=c(d2$vote.2000, d2$vote.2004),
                 authoritarianism=rep(d2$authoritarianism, times=2),
                 sex=rep(d2$sex.2000, times=2),
                 college=rep(d2$college.2000, times=2),
                 income=rep(d2$income.2000, times=2),
                 age=(rep(d2$age.2000, times=2)-18)/79,
                 jewish=rep(d2$jewish.2000, times=2),
                 catholic=rep(d2$catholic.2000, times=2),
                 other=rep(d2$other.2000, times=2),
                 time=c(rep(c(1:2), each=length(d2$id) )))

data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)
twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0.0))
data$state<-1+data$state
rownames(twoway2.q)<-colnames(twoway2.q)<-c("Democrat", "Republican")
twoway2.q

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, obstype=1,

           method="BFGS", control=list(trace=1, REPORT=1, fnscale=2000, maxit=20000, reltol=1e-8))


hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=0, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth
t<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1, college=0, income=0,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
print(t)
print(o)

plot.2000<-
  grViz("
        digraph  {
        graph [layout = neato,
        overlap = FALSE,
        forcelabels=TRUE,
        compound = true, nodesep = .5, ranksep = .25,
        color = crimson]
        node [shape = rectangle]
        
        dt1 [pos =  '0,3!', label = 'Gore(2000, Democrat)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Bush(2000, Republican',  fontsize=14]
        
        #Dem
        dd [pos = '-2,1!', label = 'Kerry(2004, Democrat)',  fontsize=14, xlabel=<Low=<b>0.94</b> <br/> <font color='black'> High=<i>0.62</i> </font> >]
        dr [pos = '2, 1!', label = 'Bush(2004, Republican',  fontsize=14, xlabel=<<b>0.06</b> <br/> <font color='black'> <i>0.37</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Kerry(2004, Democrat)',  fontsize=14, xlabel=<Low=<b>0.01</b> <br/> <font color='black'> High=<i>0.04</i> </font> >]
        rr [pos = '2, -2!',  label = 'Bush(2004, Republican)',  fontsize=14, xlabel=<<b>0.99</b> <br/> <font color='black'> <i>0.96</i> </font> >]

        dt1 [pos =  '0,3!', label = 'Democratic Candidate\n(t-1)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Republican Candidate\n(t-1)',  fontsize=14]
        
        #Dem
        dd [pos = '-2,1!', label = 'Democrat',  fontsize=14, xlabel=<Low=<b>0.92</b> <br/> <font color='black'> High=<i>0.78</i> </font> >]
        dr [pos = '2, 1!', label = 'Republican',  fontsize=14, xlabel=<<b>0.08</b> <br/> <font color='black'> <i>0.21</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Democratic',  fontsize=14, xlabel=<Low=<b>0.01</b> <br/> <font color='black'> High=<i>0.06</i> </font> >]
        rr [pos = '2, -2!',  label = 'Republican',  fontsize=14, xlabel=<<b>0.99</b> <br/> <font color='black'> <i>0.94</i> </font> >]

        dt1->dd  [ headport=n]
        dt1->dr  [ headport=n]
        
        rt1->rd [ headport=n]
        rt1->rr [ headport=n]
        
        }")

plot.2000

#### 2012-2016 Switching

head(d4)
d4$id<-c(1:length(d4$pid3.1))
data<-data.frame(id=rep(1:length(d4$id), times=2), state=c(d4$vote.2012, d4$vote.2016),
                 authoritarianism=rep(d4$authoritarianism, times=2),
                 sex=rep(d4$sex.2012, times=2),
                 college=rep(d4$college.2012, times=2),
                 income=rep(d4$income.2012, times=2),
                 age=(rep(d4$age.2012, times=2)-22)/73,
                 jewish=rep(d4$jewish.2012, times=2),
                 catholic=rep(d4$catholic.2012, times=2),
                 other=rep(d4$other.2012, times=2),
                 time=c(rep(c(1:2), each=length(d4$id) )))

require(dplyr)
data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)



data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)
twoway2.q<-rbind(c(0,0.20),
                 c(0.2, 0.0))
data$state<-1+data$state
rownames(twoway2.q)<-colnames(twoway2.q)<-c("Democrat", "Republican")
twoway2.q

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+other+age, 
           subject=id, data=data,
           qmatrix=twoway2.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1, fnscale=2000, maxit=20000, reltol=1e-8))


hazard.msm(model)
qmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at min auth
qmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=1, catholic=0, age=mean(data$age, na.rm=T)))  ## Transitions at max auth
t<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=0, college=0, income=1,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
o<-pmatrix.msm(model, t=1, ci="none", covariates=list(authoritarianism=1, college=0, income=1,
                                                        sex=1, catholic=0, age=mean(data$age, na.rm=T)))
print(t)
print(o)



plot.2012<-
  grViz("
        digraph  {
        graph [layout = neato,
        overlap = FALSE,
        forcelabels=TRUE,
        compound = true, nodesep = .5, ranksep = .25,
        color = crimson]
        node [shape = rectangle]
        
        dt1 [pos =  '0,3!', label = 'Obama(2012, Democrat)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Romney(2012, Republican)',  fontsize=14]
        
        #Dem
        dd [pos = '-2,1!', label = 'Clinton(2016, Democrat)',  fontsize=14, xlabel=<Low=<b>0.95</b> <br/> <font color='black'> High=<i>0.71</i> </font> >]
        dr [pos = '2, 1!', label = 'Trump(2016, Republican)',  fontsize=14, xlabel=<<b>0.05</b> <br/> <font color='black'> <i>0.29</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Clinton(2016, Democrat)',  fontsize=14, xlabel=<Low=<b>0.10</b> <br/> <font color='black'> High=<i>0.02</i> </font> >]
        rr [pos = '2, -2!',  label = 'Trump(2016, Republican',  fontsize=14, xlabel=<<b>0.90</b> <br/> <font color='black'> <i>0.98</i> </font> >]

        dt1 [pos =  '0,3!', label = 'Democratic Candidate\n(t-1)',  fontsize=14]
        rt1 [pos =  '0,0!', label = 'Republican Candidate\n(t-1)',  fontsize=14]
        
        #Dem
        dd [pos = '-2,1!', label = 'Democrat',  fontsize=14, xlabel=<Low=<b>0.95</b> <br/> <font color='black'> High=<i>0.71</i> </font> >]
        dr [pos = '2, 1!', label = 'Republican',  fontsize=14, xlabel=<<b>0.05</b> <br/> <font color='black'> <i>0.29</i> </font> >]
        
        #Rep
        rd [pos = '-2,-2!', label = 'Democratic',  fontsize=14, xlabel=<Low=<b>0.10</b> <br/> <font color='black'> High=<i>0.02</i> </font> >]
        rr [pos = '2, -2!',  label = 'Republican',  fontsize=14, xlabel=<<b>0.90</b> <br/> <font color='black'> <i>0.98</i> </font> >]


        dt1->dd  [ headport=n]
        dt1->dr  [ headport=n]
        
        rt1->rd [ headport=n]
        rt1->rr [ headport=n]
        
        }")

plot.2012




#####  Stability of Democratic Coaltion ####
d4$authXsilent<-d4$authoritarianism*d4$silent.gen
d4$authXboomer<-d4$authoritarianism*d4$boomer.gen
d4$authXgenx<-d4$authoritarianism*d4$genx.gen
d4$authXmillenial<-d4$authoritarianism*d4$mil.gen


#### 2012-2017
head(d4)
d4$id<-c(1:length(d4$pid3.1))
prop.table(table(d4$pid3.1, d4$pid3.3), 1)
dim(model.matrix(lm(d4$pid3.1~d4$pid3.3)))


data<-data.frame(id=rep(1:length(d4$id), times=2), state=c(d4$pid3.1, d4$pid3.3),
                 authoritarianism=rep(d4$authoritarianism, times=2),
                 sex=rep(d4$sex.2012, times=2),
                 college=rep(d4$college.2012, times=2),
                 income=rep(d4$income.2012, times=2),
                 jewish=rep(d4$jewish.2012, times=2),
                 catholic=rep(d4$catholic.2012, times=2),
                 other=rep(d4$other.2012, times=2),
                 boomer.gen=rep(d4$boomer.gen, times=2),
                 genx.gen=rep(d4$genx.gen, times=2),
                 mil.gen=rep(d4$mil.gen, times=2),
                 silent.gen=rep(d4$silent.gen, times=2),
                 boomer.gen=rep(d4$boomer.gen, times=2),
                 genx.gen=rep(d4$genx.gen, times=2),
                 mil.gen=rep(d4$mil.gen, times=2),
                 authXsilent=rep(d4$authXsilent, times=2),
                 authXboomer=rep(d4$authXboomer, times=2),
                 authXgenx=rep(d4$authXgenx, times=2),
                 authXmillenial=rep(d4$authXmillenial, times=2),
                 time=c(rep(c(1:2), each=length(d4$id) ))
                 )

mean(with(data, authoritarianism[boomer.gen==0 & genx.gen==0  & mil.gen==0]), na.rm=T)
mean(with(data, authoritarianism[boomer.gen==1]), na.rm=T)
mean(with(data, authoritarianism[genx.gen==1]), na.rm=T)
mean(with(data, authoritarianism[mil.gen==1]), na.rm=T)

require(dplyr)
require(msm)
data<-data%>%
  arrange(id, time)%>%
  na.omit
statetable.msm(state, id, data)
twoway3.q<-rbind(c(0,0.10, 0, 0, 0),
                 c(0.1,0, 0.10, 0, 0),
                 c(0, 0.10, 0, 0.1, 0),
                 c(0 ,0, 0.10, 0, 0.1),
                 c(0 ,0, 0, 0.1, 0)
)
twoway3.q

model<-msm(state~time, covariates=~authoritarianism+sex+college+income+catholic+
             boomer.gen+genx.gen+mil.gen+authXboomer+authXgenx+authXmillenial, 
           subject=id, data=data,
           qmatrix=twoway3.q, obstype=1,
           method="BFGS", control=list(trace=1, REPORT=1,fnscale=1000, maxit=10000)) ## Only 22 greatest gen, combine with silent as base.

msm.trans<-function(ll){
  return(c(unlist(pmatrix.msm(model, covariates=list(authoritarianism=ll[1], 
                                   college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=ll[2], genx.gen=ll[3], mil.gen=ll[4],
                                   authXboomer=ll[5], authXgenx=ll[6], authXmillenial=ll[7] )))))
  
}
  




dat<-data.frame(transition=c(
  msm.trans(c(1,0,0,0,0,0,0)),
  msm.trans(c(0,0,0,0,0,0,0)),
  msm.trans(c(1,1,0,0,1*1,0,0)),
  msm.trans(c(0,1,0,0,1*0,0,0)),
  msm.trans(c(1,0,1,0,0,1*1,0)),
  msm.trans(c(0,0,1,0,0,0*1,0)),
  msm.trans(c(1,0,0,1,0,0,1*1)),
  msm.trans(c(0,0,0,1,0,0,0*1))))

dat$t1= rep(c("Dem(2012)", "Lean Dem(2012)", 
                         "Ind(2012)", "Lean Rep(2012)", 
                         "Rep(2012)"))
dat$t2= rep(c("Democrat(2016)", "Lean Democrat(2016)", 
              "Pure Independent(2016)", "Lean Republican(2016)", 
              "Republican(2016)"),each=5)
dat$generation<-rep(c("Greatest/Silent", "Boomer", "Gen X", "Millennial"), each=50)
dat$authoritarian<-rep(c("Authoritarian", "Non-Authoritarian"), each=25)


library(hrbrthemes)
library(viridis)


dat$Authoritarian_Score<-rep(c(
  mean(with(data, authoritarianism[boomer.gen==0 & genx.gen==0  & mil.gen==0]), na.rm=T),
  mean(with(data, authoritarianism[boomer.gen==1]), na.rm=T),
  mean(with(data, authoritarianism[genx.gen==1]), na.rm=T),
  mean(with(data, authoritarianism[mil.gen==1]), na.rm=T)), each=50)


dat %>%
  filter(authoritarian=="Authoritarian") %>%
  arrange(t1) %>%
   mutate(t1=factor(t1, levels=
                             c("Dem(2012)", "Lean Dem(2012)",
                             "Ind(2012)", "Lean Rep(2012)",
                             "Rep(2012)")))%>%
   mutate(t2=factor(t2, levels=
                      c("Democrat(2016)", "Lean Democrat(2016)",
                        "Pure Independent(2016)", "Lean Republican(2016)",
                        "Republican(2016)")))%>%
   mutate(generation=factor(generation, levels=
                      c("Greatest/Silent", "Boomer", "Gen X", "Millennial")
                    ))%>%
ggplot( aes(x=t1, y=transition, 
            size = Authoritarian_Score, 
            group=generation,
            fill=generation,
            colour=generation)) +
  facet_wrap(~t2, nrow=3)+
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_line(size=.1, colour="black") +
  theme(legend.position="right") +
  scale_size(range = c(0.4, 6), limits=c(0.3, 0.6))+
  theme(plot.title=element_text(face="bold", hjust=0,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=9,colour="#535353", angle=55, vjust=1, hjust=1)) +
  theme(axis.text.y=element_text(size=7, colour="#535353")) +
  theme(axis.title.y=element_text(size=9,colour="#535353",vjust=1.5)) +
  scale_y_continuous("Transition Probability", limits=c(-.2,1.1))+
  scale_fill_manual("Generation", values = c("#00AFBB", "#E7B800", "#FC4E07", "purple"))+
  scale_x_discrete("Party Identification")+
  ggtitle("PID by age cohort. Non-Authoritarian White Respondents") 
  
dev.copy(png,'ch6_F1.jpg',
         width = 750, height = 500)
dev.off()

dat %>%
  filter(authoritarian=="Non-Authoritarian") %>%
  arrange(t1) %>%
  mutate(t1=factor(t1, levels=
                     c("Dem(2012)", "Lean Dem(2012)",
                       "Ind(2012)", "Lean Rep(2012)",
                       "Rep(2012)")))%>%
  mutate(t2=factor(t2, levels=
                     c("Democrat(2016)", "Lean Democrat(2016)",
                       "Pure Independent(2016)", "Lean Republican(2016)",
                       "Republican(2016)")))%>%
  mutate(generation=factor(generation, levels=
                             c("Greatest/Silent", "Boomer", "Gen X", "Millennial")
  ))%>%
  ggplot( aes(x=t1, y=transition, 
              size = Authoritarian_Score, 
              group=generation,
              fill=generation,
              colour=generation)) +
  facet_wrap(~t2, nrow=3)+
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_line(size=.1, colour="black") +
  theme(legend.position="right") +
  scale_size(range = c(0.4, 6), limits=c(0.3, 0.6))+
  theme(plot.title=element_text(face="bold", hjust=0,vjust=2,colour="#3C3C3C",size=11)) +
  theme(axis.text.x=element_text(size=9,colour="#535353", angle=55, vjust=1, hjust=1)) +
  theme(axis.text.y=element_text(size=7, colour="#535353")) +
  theme(axis.title.y=element_text(size=9,colour="#535353",vjust=1.5)) +
  scale_y_continuous("Transition Probability", limits=c(-.2,1.1))+
  scale_fill_manual("Generation", values = c("#00AFBB", "#E7B800", "#FC4E07", "purple"))+
  scale_x_discrete("Party Identification")+
  ggtitle("PID by age cohort. Non-Authoritarian White Respondents") 

dev.copy(png,'ch6_F2.jpg',
         width = 750, height = 500)
dev.off()


pmatrix.msm(model, covariates=list(authoritarianism=1, 
                                   college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=0,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))

pmatrix.msm(model, covariates=list(authoritarianism=1, 
                                   college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=1,
                                   authXboomer=0, authXgenx=0, authXmillenial=1 ))


pmatrix.msm(model, covariates=list(authoritarianism=0, 
                                   college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=0,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))

pmatrix.msm(model, covariates=list(authoritarianism=0, 
                                   college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=1,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))

pmatrix.msm(model, covariates=list(authoritarianism=0, 
                                   college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=1,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))



  
a<-pmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=1, catholic=0, 
                                    boomer.gen=0, genx.gen=0, mil.gen=0,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))  ## Transitions at min auth
pmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=0,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))  ## Transitions at min auth


pmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=1, genx.gen=0, mil.gen=0,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))  ## Transitions at min auth
pmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=1, genx.gen=0, mil.gen=0,
                                   authXboomer=1, authXgenx=0, authXmillenial=0 ))  ## Transitions at min auth


pmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=1, mil.gen=0,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))  ## Transitions at min auth
pmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=1, mil.gen=0,
                                   authXboomer=0, authXgenx=1, authXmillenial=0 ))  ## Transitions at min auth

pmatrix.msm(model, covariates=list(authoritarianism=0, college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=1,
                                   authXboomer=0, authXgenx=0, authXmillenial=0 ))  ## Transitions at min auth
pmatrix.msm(model, covariates=list(authoritarianism=1, college=0, income=0,
                                   sex=1, catholic=0, 
                                   boomer.gen=0, genx.gen=0, mil.gen=1,
                                   authXboomer=0, authXgenx=0, authXmillenial=1 ))  ## Transitions at min auth
