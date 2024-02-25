# Analysis for the matched data
# 2/7/2024
# updated on: 2/7/2024
# last update: 2/7/2024


# Load data
dat_violence <- read.csv('/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/matched_data_first_example.csv')
dat_firearm <- read.csv('/Users/kanchen/Dropbox/Case2_Kan, Ting, Dylan/Case Square Project/NMFS1993/output/matched_data_second_example.csv')

# Create 2x2 contigency table
a1 <- sum((dat_violence$death.certificate.manner.of.death == 1) & (dat_violence$violence.behavior == 1))
a2 <- sum((dat_violence$death.certificate.manner.of.death == 1) & (dat_violence$violence.behavior == 0))
a3 <- sum((dat_violence$death.certificate.manner.of.death == 0) & (dat_violence$violence.behavior == 1))
a4 <- sum((dat_violence$death.certificate.manner.of.death == 0) & (dat_violence$violence.behavior == 0))

df1 <- data.frame("have violent behavior" = c(a1 , a3  ), "not have violent behavior" = c(a2, a4), row.names = c("narrow case (suicide case)", "marginal case (accidental death)"))
odds.ratio.1 <- (a1 * a4)/(a2 * a3) 


a1 <- sum((dat_firearm$means.of.suicide == 1) & (dat_firearm$firearms.in.home == 'Yes'))
a2 <- sum((dat_firearm$means.of.suicide == 1) & (dat_firearm$firearms.in.home == 'No'))
a3 <- sum((dat_firearm$means.of.suicide == 0) & (dat_firearm$firearms.in.home == 'Yes'))
a4 <- sum((dat_firearm$means.of.suicide == 0) & (dat_firearm$firearms.in.home == 'No'))

df2 <- data.frame("have gun at home" = c(a1 , a3  ), "not have gun at home" = c(a2, a4), row.names = c("narrow case (gun suicide)", "marginal case (suicide by other means)"))
odds.ratio.2 <- (a1 * a4)/(a2 * a3) 


library('sensitivity2x2xk')

# Attributable effect given Theta = 1, Gamma = 1

Theta <- 1
Gamma <- 3.3

for (A in 0:df1[1,1]) {
  if(mh(matrix(c(as.matrix(df1)[1,1] - A, as.matrix(df1)[1,2], as.matrix(df1)[2,1], as.matrix(df1)[2,2]),nrow = 2, ncol = 2),Gamma = Gamma * Theta^2)$pval >= 0.05 ){
    print(A)
    break
  }
}


Theta <- 1.7
Gamma <- 1

for (A in 0:df2[1,1]) {
  if(mh(matrix(c(as.matrix(df2)[1,1] - A, as.matrix(df2)[1,2], as.matrix(df2)[2,1], as.matrix(df2)[2,2]),nrow = 2, ncol = 2),Gamma = Gamma * Theta^2)$pval >= 0.05 ){
    print(A)
    break
  }
}








