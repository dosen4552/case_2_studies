# Match using violence suicide data
# updated on: 2/7/2024
################ Parameters 
controls<-2 # how many controls? Need to change 
################

# Load data
df_18violence_suicide<-read.csv("/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/violence_behavior_suicide_data.csv")


# Violence and Suicide Risk Matching
# Match suicide with accidental death
df_18violence_suicide[df_18violence_suicide$death.certificate.manner.of.death == 2,]$death.certificate.manner.of.death = 0
df_18violence_suicide[df_18violence_suicide$death.certificate.manner.of.death == 3,]$death.certificate.manner.of.death = 1


df_18violence_suicide[df_18violence_suicide$violence.behavior != 'often',]$violence.behavior = 0
df_18violence_suicide[df_18violence_suicide$violence.behavior == 'often',]$violence.behavior = 1



df_18violence_suicide<-subset(df_18violence_suicide,df_18violence_suicide$firearms.in.home != 'missing')
df_18violence_suicide<-subset(df_18violence_suicide,df_18violence_suicide$age<=94)
df_18violence_suicide<-droplevels(df_18violence_suicide)


##############################
## Matching using optmatch  ##
##############################

library('optmatch')


optmatch_rank<-function(datatemp,controls){
  rownames(datatemp)<-1:dim(datatemp)[1]
  #setMaxProblemSize(size = Inf)
  matchvec<-pairmatch(match_on(death.certificate.manner.of.death ~  age.at.death + lived.alone + family.income +
                                 marital.status + education + veteran + region + 
                                 population + violence.behavior,
                               data = datatemp,method="rank_mahalanobis"),
                      controls=controls,data=datatemp) # NOTE: no sex and race, as we stratify on it
  datatemp$matchvec=matchvec
  ## Create a matrix saying which control units each case unit is matched to
  ## Create vectors of the subject indices of the treatment units ordered by
  ## their matched set and corresponding control unit
  case.subject.index=rep(0,sum(datatemp$death.certificate.manner.of.death==1))
  matched.referent.subject.index.mat=matrix(rep(0,controls*length(case.subject.index)),ncol=controls)
  matchedset.index=substr(matchvec,start=3,stop=10)
  matchedset.index.numeric=as.numeric(matchedset.index)
  
  for(i in 1:length(case.subject.index)){
    matched.set.temp=which(matchedset.index.numeric==i)
    case.temp.index=which(datatemp$death.certificate.manner.of.death[matched.set.temp]==1)
    case.subject.index[i]=matched.set.temp[case.temp.index]
    matched.referent.subject.index.mat[i,]=matched.set.temp[-case.temp.index]
  }
  matched.referent.subject.index=matched.referent.subject.index.mat
  return(list(datatemp=datatemp,case.subject.index=case.subject.index,
              matched.referent.subject.index=matched.referent.subject.index,
              datatemp.case=datatemp[case.subject.index,],
              datatemp.referent=datatemp[matched.referent.subject.index,]))
}


##############################
## stratify before matching ##
##############################
df.M.W<-df_18violence_suicide[df_18violence_suicide$sex=="Male"&df_18violence_suicide$race=="White",]
tmp.M.W<-optmatch_rank(df.M.W,controls=controls)
tmp.M.W$datatemp.case$matchvec<-paste0("M.W.",tmp.M.W$datatemp.case$matchvec)
tmp.M.W$datatemp.referent$matchvec<-paste0("M.W.",tmp.M.W$datatemp.referent$matchvec)

df.M.B<-df_18violence_suicide[df_18violence_suicide$sex=="Male"& (df_18violence_suicide$race=="Black"| df_18violence_suicide$race=="Other"),]
(1-mean(df.M.B$death.certificate.manner.of.death))/mean(df.M.B$death.certificate.manner.of.death)
tmp.M.B<-optmatch_rank(df.M.B,controls=controls)
tmp.M.B$datatemp.case$matchvec<-paste0("M.B.",tmp.M.B$datatemp.case$matchvec)
tmp.M.B$datatemp.referent$matchvec<-paste0("M.B.",tmp.M.B$datatemp.referent$matchvec)


df.F.W<-df_18violence_suicide[df_18violence_suicide$sex=="Female"&df_18violence_suicide$race=="White",]
(1-mean(df.F.W$death.certificate.manner.of.death))/mean(df.F.W$death.certificate.manner.of.death)
tmp.F.W<-optmatch_rank(df.F.W,controls=controls)
tmp.F.W$datatemp.case$matchvec<-paste0("F.W.",tmp.F.W$datatemp.case$matchvec)
tmp.F.W$datatemp.referent$matchvec<-paste0("F.W.",tmp.F.W$datatemp.referent$matchvec)

df.F.B<-df_18violence_suicide[df_18violence_suicide$sex=="Female"& (df_18violence_suicide$race=="Black" | df_18violence_suicide$race=="Other") ,]
(1-mean(df.F.B$death.certificate.manner.of.death))/mean(df.F.B$death.certificate.manner.of.death)
tmp.F.B<-optmatch_rank(df.F.B,controls=controls)
tmp.F.B$datatemp.case$matchvec<-paste0("F.B.",tmp.F.B$datatemp.case$matchvec)
tmp.F.B$datatemp.referent$matchvec<-paste0("F.B.",tmp.F.B$datatemp.referent$matchvec)


df_matched<-rbind(tmp.M.W$datatemp.case,tmp.M.W$datatemp.referent,
                  tmp.M.B$datatemp.case,tmp.M.B$datatemp.referent,
                  tmp.F.W$datatemp.case,tmp.F.W$datatemp.referent,
                  tmp.F.B$datatemp.case,tmp.F.B$datatemp.referent)



matched.data <- df_matched


###################
## check balance ##
###################
df_unmatched<-df_18violence_suicide
library(tableone)
library(xtable)
library(caret)

dmy=dummyVars( ~ sex+race+age.at.death+lived.alone+marital.status+education+family.income+veteran+region+population + violence.behavior,data=df_unmatched)
Xmat.without.missing<-data.frame(predict(dmy,newdata=df_18violence_suicide))

casemat=Xmat.without.missing[df_unmatched$death.certificate.manner.of.death==1,];
# Standardized differences before matching
controlmat.before=Xmat.without.missing[df_unmatched$death.certificate.manner.of.death==0,];
controlmean.before=apply(controlmat.before,2,mean,na.rm=TRUE);

casemean=apply(casemat,2,mean);
casevar=apply(casemat,2,var);
controlvar=apply(controlmat.before,2,var);
stand.diff.before=(casemean-controlmean.before)/sqrt((casevar+controlvar)/2);

# Standardized differences after matching
casemat.after=Xmat.without.missing[df_matched$death.certificate.manner.of.death==1,]
controlmat.after<-data.frame(predict(dmy,newdata=df_unmatched[df_matched$death.certificate.manner.of.death==0,]))
controlmean.after=apply(controlmat.after,2,mean)


stand.diff.after=(casemean-controlmean.after)/sqrt((casevar+controlvar)/2)

res.stand.diff<-cbind(stand.diff.before,stand.diff.after)
res.mean<-cbind(casemean,controlmean.before,controlmean.after)
print(round(res.stand.diff,2))
print(res.mean)

abs.stand.diff.before=stand.diff.before
abs.stand.diff.after=stand.diff.after
covariates=names(stand.diff.before)
plot.dataframe=data.frame(abs.stand.diff=c(abs.stand.diff.before,abs.stand.diff.after),covariates=rep(covariates,2),type=c(rep("Before",length(covariates)),rep("After",length(covariates))))
plot.dataframe$covariates<-factor(plot.dataframe$covariates,levels = rev(names(abs.stand.diff.before)))
p<-ggplot(plot.dataframe,aes(x=abs.stand.diff,y=covariates))+geom_point(size=2,aes(shape=type))+scale_shape_manual(values=c(4,1))+geom_vline(xintercept=c(-.1,.1),lty=2)+xlab("standardized differences in means")+ ylab("")
p




####################################
## Create 2 x 2 contingency table ##
####################################

# Summary Table
a1 <- sum((matched.data$death.certificate.manner.of.death == 1) & (matched.data$violence.behavior == 1))
a2 <- sum((matched.data$death.certificate.manner.of.death == 1) & (matched.data$violence.behavior == 0))
a3 <- sum((matched.data$death.certificate.manner.of.death == 0) & (matched.data$violence.behavior == 1))
a4 <- sum((matched.data$death.certificate.manner.of.death == 0) & (matched.data$violence.behavior == 0))

df1 <- data.frame("have violent behavior" = c(a1 , a3  ), "not have violent behavior" = c(a2, a4), row.names = c("narrow case (suicide case)", "marginal case (accidental death)"))
odds.ratio.1 <- (a1 * a4)/(a2 * a3) 


library(gtsummary)
library(dplyr)

dem.data <- matched.data %>%
  dplyr::select(firearms.in.home,sex,race, age.at.death,means.of.suicide,
                lived.alone,marital.status,education,veteran,region,population,family.income, violence.behavior, death.certificate.manner.of.death)

dem.data %>% tbl_summary(by = death.certificate.manner.of.death, digits = everything() ~ 2, statistic = list(age.at.death ~ "{mean} ({sd})")) %>% 
  modify_caption("**Table 1. Matched Data Characteristic**")

write.csv(matched.data, file = "/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/matched_data_first_example.csv", row.names = FALSE)

