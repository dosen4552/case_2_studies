# Match using firearm suicide data
# updated on: 2/7/2024
################ Parameters 
controls<-1 # how many controls? Need to change 
################

# Load data
df_18firearm_suicide<-read.csv("/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/firearm_suicide_data.csv")


# Firearm and Suicide Risk Matching
# Match gun suicide with other type of suicide
df_18firearm_suicide[df_18firearm_suicide$means.of.suicide == 342,]$means.of.suicide = 1
df_18firearm_suicide[df_18firearm_suicide$means.of.suicide != 1,]$means.of.suicide = 0



df_18firearm_suicide<-subset(df_18firearm_suicide,df_18firearm_suicide$firearms.in.home != 'missing')
df_18firearm_suicide<-subset(df_18firearm_suicide,df_18firearm_suicide$age<=94)
df_18firearm_suicide<-droplevels(df_18firearm_suicide)


##############################
## Matching using optmatch  ##
##############################

library('optmatch')


optmatch_rank<-function(datatemp,controls){
  rownames(datatemp)<-1:dim(datatemp)[1]
  #setMaxProblemSize(size = Inf)
  matchvec<-pairmatch(match_on(means.of.suicide ~  age.at.death + lived.alone + family.income +
                                 marital.status + education + veteran + region + 
                                 population + violence.behavior,
                               data = datatemp,method="rank_mahalanobis"),
                      controls=controls,data=datatemp) # NOTE: no sex and race, as we stratify on it
  datatemp$matchvec=matchvec
  ## Create a matrix saying which control units each case unit is matched to
  ## Create vectors of the subject indices of the treatment units ordered by
  ## their matched set and corresponding control unit
  case.subject.index=rep(0,sum(datatemp$means.of.suicide==1))
  matched.referent.subject.index.mat=matrix(rep(0,controls*length(case.subject.index)),ncol=controls)
  matchedset.index=substr(matchvec,start=3,stop=10)
  matchedset.index.numeric=as.numeric(matchedset.index)
  
  for(i in 1:length(case.subject.index)){
    matched.set.temp=which(matchedset.index.numeric==i)
    case.temp.index=which(datatemp$means.of.suicide[matched.set.temp]==1)
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
df.M.W<-df_18firearm_suicide[df_18firearm_suicide$sex=="Male"&df_18firearm_suicide$race=="White",]
tmp.M.W<-optmatch_rank(df.M.W,controls=controls)
tmp.M.W$datatemp.case$matchvec<-paste0("M.W.",tmp.M.W$datatemp.case$matchvec)
tmp.M.W$datatemp.referent$matchvec<-paste0("M.W.",tmp.M.W$datatemp.referent$matchvec)

df.M.B<-df_18firearm_suicide[df_18firearm_suicide$sex=="Male"& (df_18firearm_suicide$race=="Black"| df_18firearm_suicide$race=="Other"),]
(1-mean(df.M.B$means.of.suicide))/mean(df.M.B$means.of.suicide)
tmp.M.B<-optmatch_rank(df.M.B,controls=controls)
tmp.M.B$datatemp.case$matchvec<-paste0("M.B.",tmp.M.B$datatemp.case$matchvec)
tmp.M.B$datatemp.referent$matchvec<-paste0("M.B.",tmp.M.B$datatemp.referent$matchvec)


df.F.W<-df_18firearm_suicide[df_18firearm_suicide$sex=="Female"&df_18firearm_suicide$race=="White",]
(1-mean(df.F.W$means.of.suicide))/mean(df.F.W$means.of.suicide)
tmp.F.W<-optmatch_rank(df.F.W,controls=controls)
tmp.F.W$datatemp.case$matchvec<-paste0("F.W.",tmp.F.W$datatemp.case$matchvec)
tmp.F.W$datatemp.referent$matchvec<-paste0("F.W.",tmp.F.W$datatemp.referent$matchvec)

df.F.B<-df_18firearm_suicide[df_18firearm_suicide$sex=="Female"& (df_18firearm_suicide$race=="Black" | df_18firearm_suicide$race=="Other") ,]
(1-mean(df.F.B$means.of.suicide))/mean(df.F.B$means.of.suicide)
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
df_unmatched<-df_18firearm_suicide
library(tableone)
library(xtable)
library(caret)

dmy=dummyVars( ~ sex+race+age.at.death+lived.alone+marital.status+education+family.income+veteran+region+population,data=df_unmatched)
Xmat.without.missing<-data.frame(predict(dmy,newdata=df_18firearm_suicide))

casemat=Xmat.without.missing[df_unmatched$means.of.suicide==1,];
# Standardized differences before matching
controlmat.before=Xmat.without.missing[df_unmatched$means.of.suicide==0,];
controlmean.before=apply(controlmat.before,2,mean,na.rm=TRUE);

casemean=apply(casemat,2,mean);
casevar=apply(casemat,2,var);
controlvar=apply(controlmat.before,2,var);
stand.diff.before=(casemean-controlmean.before)/sqrt((casevar+controlvar)/2);

# Standardized differences after matching
casemat.after=Xmat.without.missing[df_matched$means.of.suicide==1,]
controlmat.after<-data.frame(predict(dmy,newdata=df_unmatched[df_matched$means.of.suicide==0,]))
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
a1 <- sum((matched.data$means.of.suicide == 1) & (matched.data$firearms.in.home == 'Yes'))
a2 <- sum((matched.data$means.of.suicide == 1) & (matched.data$firearms.in.home == 'No'))
a3 <- sum((matched.data$means.of.suicide == 0) & (matched.data$firearms.in.home == 'Yes'))
a4 <- sum((matched.data$means.of.suicide == 0) & (matched.data$firearms.in.home == 'No'))

df1 <- data.frame("have gun at home" = c(a1 , a3  ), "not have gun at home" = c(a2, a4), row.names = c("narrow case (gun suicide)", "marginal case (suicide by other means)"))
odds.ratio.1 <- (a1 * a4)/(a2 * a3) 


library(gtsummary)
library(dplyr)

dem.data <- matched.data %>%
  dplyr::select(firearms.in.home,sex,race, age.at.death,means.of.suicide,
                lived.alone,marital.status,education,veteran,region,population,family.income, violence.behavior)

dem.data %>% tbl_summary(by = means.of.suicide, digits = everything() ~ 2, statistic = list(age.at.death ~ "{mean} ({sd})")) %>% 
  modify_caption("**Table 1. Matched Data Characteristic**")

write.csv(matched.data, file = "/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/matched_data_second_example.csv", row.names = FALSE)

