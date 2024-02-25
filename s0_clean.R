# Subset and define variables of interest from the 93NMFS for analysis
# 2/7/2024
# updated on: 2/7/2024
# last update: 
rm(list = ls())
#######################
##     NMFS data     ##
#######################
# National Mortality Followback Survey data
nmfs93dataraw=read.csv("/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/nmfs93datacsv.csv")
firearms.in.home=rep(NA,nrow(nmfs93dataraw)) # 1=Yes, 2=No, the rest is missing
# Wiebe studied used death certificate manner of death, age 18+
death.certificate.manner.of.death=rep(NA,nrow(nmfs93dataraw)) # 1=Natural, 2=Accidental injury, 3=Suicide, 4=Homicide/legal intervention, 6=Undetermined, 8=Not matched
place.of.fatal.injury=rep(NA,nrow(nmfs93dataraw))
means.of.suicide<-rep(NA,nrow(nmfs93dataraw))
age.at.death=rep(NA,nrow(nmfs93dataraw))
sex=rep(NA,nrow(nmfs93dataraw)) # 1 = Male, 2= Female
race=rep(NA,nrow(nmfs93dataraw)) # 1 = White, 2 = Other, 3= Black, 8 = Not Matched
lived.alone=rep(NA,nrow(nmfs93dataraw))
marital.status=rep(NA,nrow(nmfs93dataraw)) # 1=Never Married, 2=Married, 3=Widowed, 4=Divorced/Separated, 8=Not matched, 9=Not stated
education=rep(NA,nrow(nmfs93dataraw)) # 1=0-8 years, 2=9-11 years, 3=12 years, 4=13-15 years, 5=16+, 6=Not stated/not reported, 8=Not matched
veteran=rep(NA,nrow(nmfs93dataraw)) # 1 = Yes, 2= No, 8=Not reported/not matched, 9=Unknown
region=rep(NA,nrow(nmfs93dataraw)) # Region of death: 1= Northeast, 2=Midwest, 3=South, 4=West, 8=Not matched
population=rep(NA,nrow(nmfs93dataraw)) # Population size of county person lived in: 1=250,000+, 2=100,000-249,999, 3=<100,000, 4=non-Metropolitan area, 8=not matched, 9=Foreign resident; population isn't matching Wiebe, Table 4
family.income=rep(NA,nrow(nmfs93dataraw)) # Family income: 1=<1000, 2=1000-1999, 3=2000-2999, 3=3000-3999, ...,8=8000-8999, 9=9000-11499, 10=$11,500 to $13,999, 11=$14,000 to $16,499, 12=$16,500 to $18,999, 13=$19,000 to $20,999, 14=$21,000 to $23,499, 15=$23,500 to $24,999, 16=$25,000 to $49,999, 17=$50,000 to $74,999, 18=$75,000 or more
violence.behavior=rep(NA,nrow(nmfs93dataraw)) # 1=often, 2=sometimes, 3=rarely, 4=never, 5=don't know, 94=refused, 95=don't know, 96=not applicable, 98=blank/out of range, 99=non-respondent. 
for(i in 1:nrow(nmfs93dataraw)){
  firearms.in.home[i]=substr(nmfs93dataraw$Data[i],1827,1828) # 1=Yes, 2=No, the rest is missing
  death.certificate.manner.of.death[i]=substr(nmfs93dataraw$Data[i],76,76)  # 1=Natural, 2=Accidental injury, 3=Suicide, 4=Homicide/legal intervention, 6=Undetermined, 8=Not matched
  place.of.fatal.injury[i]=substr(nmfs93dataraw$Data[i],2025,2026) # 1=Home or private area around the home, 2-10=others, 94-99=missing
  means.of.suicide[i]=substr(nmfs93dataraw$Data[i],92,94) # 342=Suicide by handgun, 343=Suicide by all other and unspecified firearms.
  age.at.death[i]=substr(nmfs93dataraw$Data[i],35,37) # 998/999=missing
  sex[i]=substr(nmfs93dataraw$Data[i],32,32) # 1 = Male, 2= Female, 8= Not matched
  race[i]=substr(nmfs93dataraw$Data[i],69,69) # 1 = White, 2 = Other, 3= Black, 8 = Not Matched 
  lived.alone[i]=substr(nmfs93dataraw$Data[i],176,177) # 1=Lived alone, 2= Lived with others, 94-99= missing
  marital.status[i]=substr(nmfs93dataraw$Data[i],54,54) # 1=Never Married, 2=Married, 3=Widowed, 4=Divorced/Separated, 8-9=missing
  education[i]=substr(nmfs93dataraw$Data[i],73,73) # 1=0-8 years, 2=9-11 years, 3=12 years, 4=13-15 years, 5=16+, 6,8=missing: 1&2=high school, 3=high schoole graduate, 4=some college, 5=college graduate
  veteran[i]=substr(nmfs93dataraw$Data[i],51,51) # 1 = Yes, 2= No, 8-9=missing
  region[i]=substr(nmfs93dataraw$Data[i],53,53) # Region of death: 1= Northeast, 2=Midwest, 3=South, 4=West, 8=Not matched
  population[i]=substr(nmfs93dataraw$Data[i],63,63) # Population size of county person lived in: 1=250,000+, 2=100,000-249,999, 3=<100,000, 4=non-Metropolitan area, 8=not matched, 9=Foreign resident; population isn't matching Wiebe, Table 4
  family.income[i]=substr(nmfs93dataraw$Data[i],2349,2350) # Family income: 1=<1000, 2=1000-1999, 3=2000-2999, 3=3000-3999, ...,
  #8=8000-8999, 9=9000-11499, 10=$11,500 to $13,999, 11=$14,000 to $16,499, 12=$16,500 to $18,999, 13=$19,000 to $20,999, 
  #14=$21,000 to $23,499, 15=$23,500 to $24,999, 16=$25,000 to $49,999, 17=$50,000 to $74,999, 18=$75,000 or more
  violence.behavior[i]=substr(nmfs93dataraw$Data[i],2257,2258) # 1=often, 2=sometimes, 3=rarely, 4=never, 5=don't know, 94=refused, 95=don't know, 96=not applicable, 98=blank/out of range, 99=non-respondent. 
}
df<-data.frame(firearms.in.home,death.certificate.manner.of.death,place.of.fatal.injury,means.of.suicide,age.at.death,sex,race,
               lived.alone,marital.status,education,veteran,region,population,family.income,violence.behavior)
df$age.at.death<-as.numeric(as.character(df$age.at.death))
df$age.at.death[df$age.at.death%in%c(998,999)]<-NA

df$firearms.in.home <- factor(df$firearms.in.home)
levels(df$firearms.in.home) <- c("Yes","No","missing","missing","missing","missing","missing")

df$suicide.gunshot<-1*(df$means.of.suicide%in%c("342","343"))

df$sex <- factor(df$sex)
levels(df$sex)<-c("Male","Female","missing")

df$race <- factor(df$race)
levels(df$race)<-c("White","Other","Black","missing")

df$lived.alone <- factor(df$lived.alone)
levels(df$lived.alone)<-c("Lived alone","Lived with others","missing","missing","missing","missing","missing")

df$marital.status <- factor(df$marital.status)
levels(df$marital.status)<-c("Never married","Married","Widowed","Divorced/separated","missing","missing")

df$education <- factor(df$education)
levels(df$education)<-c("0-8yrs","9-11yrs","12yrs","13-15yrs","16+yrs","missing","missing")

df$veteran <- factor(df$veteran)
levels(df$veteran)<-c("Yes","No","missing","missing")

df$region <- factor(df$region)
levels(df$region)<-c("Northeast","Midwest","South","West","missing")

df$population <- factor(df$population)
levels(df$population)<-c("250,000+","100,000-249,999","<100,000","Non-MSA","missing","Foreign resident")

df$family.income <- factor(df$family.income)
levels(df$family.income)<-c("<1000","1000-1999","2000-2999","3000-3999","4000-4999","5000-5999","6000-6999","7000-8999",
                            "9000-13999","9000-13999","14000-18999","14000-18999","19000-24999","19000-24999","19000-24999",
                            "25000-49999","50000+","50000+","missing","missing","missing","missing")

df$place.of.fatal.injury <- factor(df$place.of.fatal.injury)
levels(df$place.of.fatal.injury)<-c("Home or private area around the home", "Others","Others","Others","Others","Others","Others","Others","Others",
                                    "Others","missing","missing","missing","missing","missing")


df$violence.behavior <- factor(df$violence.behavior)
levels(df$violence.behavior)<-c("often","sometimes","rarely","never","don't know","refused","don't know","not applicable","blank/out of range",
                                "non-response")


df_18suicide<-df[(death.certificate.manner.of.death==3 | death.certificate.manner.of.death==2) & 
                   age.at.death!="015" & age.at.death!="016" & age.at.death!="017",] # All suicides and accidental death. 7650 cases for >=18yrs


#mean(df_18suicide$suicide.gunshot)
#round(table(df_18suicide$sex)/dim(df_18suicide)[1]*100,1)
#round(table(df_18suicide$race)/dim(df_18suicide)[1]*100,1)
#mean(df_18suicide$age.at.death)
#round(table(df_18suicide$lived.alone)/dim(df_18suicide)[1]*100,1)
#round(table(df_18suicide$marital.status)/dim(df_18suicide)[1]*100,1)
#round(table(df_18suicide$education)/dim(df_18suicide)[1]*100,1)
#round(table(df_18suicide$family.income)/dim(df_18suicide)[1]*100,1)
#round(table(df_18suicide$veteran)/dim(df_18suicide)[1]*100,1)
#round(table(df_18suicide$region)/dim(df_18suicide)[1]*100,1)
#round(table(df_18suicide$population)/dim(df_18suicide)[1]*100,1)
#df_18suicide<-droplevels(df_18suicide)

write.csv(df_18suicide, file = 
            "/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/violence_behavior_suicide_data.csv", 
          row.names = FALSE)


df_18firearm_suicide<-df[(death.certificate.manner.of.death==3 ) & 
                   age.at.death!="015" & age.at.death!="016" & age.at.death!="017",] # All suicides. 1959 cases for >=18yrs


write.csv(df_18firearm_suicide, file = 
            "/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/firearm_suicide_data.csv", 
          row.names = FALSE)

