#libraries

library(tidyverse)
library(dplyr)
library(car)
library(metabaR)
library(ggplot2)
library(corrplot)
library(cowplot)
#library(MuMIn)
library(multcomp)
library(ggpubr)
library(ade4)
#library(adespatial)
#library(reldist)
#library(bipartite)
library(vegan)
library(scales)
#library(phyloseq)
#library(reshape2)
library(ggpubr)
library(BiocManager)
#library(microbiome)  




load("../resources/Metabarlist_serre_ITS_AMVG42_230505_traits.Rdata")# ITS
leaf <- AMVG42_230505_ITS_clean 

#Prepare and calculate abundance beta-null deviation metric
comm.t=leaf$reads #nontranspose reads
bbs.sp.site <- comm.t
patches=nrow(bbs.sp.site)
rand <- 99

# Create empty matrices/vectors, the dimensions of these matrices/vectors are determined based on the number of species (OTU) or sites (number of seedlings) in the data.
null.alphas <- matrix(NA, ncol(comm.t), rand)
null.alpha <- matrix(NA, ncol(comm.t), rand)
expected_beta <- matrix(NA, 1, rand)
null.gamma <- matrix(NA, 1, rand)
null.alpha.comp <- numeric()
bucket_bray_res <- matrix(NA, patches, rand)
#
# # Abundance Beta-Null Deviation Calculation
bbs.sp.site = ceiling(bbs.sp.site/max(bbs.sp.site)) #normalisation of abundance data by dividing to max abundance value
mean.alpha = sum(bbs.sp.site)/nrow(bbs.sp.site) # mean.alpha : mean abundance per site (seedlings)
gamma <- ncol(bbs.sp.site) # gamma diversity : The total number of species : number of columns in bbs.sp.site, because we transposed it at the beginning
obs_beta <- 1-mean.alpha/gamma #observed beta-diveristy : 1- mean alpha/gamma
obs_beta_all <- 1-rowSums(bbs.sp.site)/gamma #difference with obs_beta?

# #loop that iterates *rand* = 999 times to calculate the null deviation metric.
# #Create a null distribution of the community data, maintain the total abundance per species and randomly reassign individuals to sites/seedlings
for (randomize in 1:rand) {
  null.dist = comm.t
  for (species in 1:ncol(null.dist)) {
    tot.abund = sum(null.dist[,species]) #for each species (OTU), calculates the total abundance by summing the values in the corresponding column of "null.dist".
    null.dist[,species] = 0 # It then sets all values in that column to 0.
    for (individual in 1:tot.abund) {
      sampled.site = sample(c(1:nrow(bbs.sp.site)), 1) #randomly reassign individuals from the total abundance to sites/seedlings
      null.dist[sampled.site, species] = null.dist[sampled.site, species] + 1
    }
  }
  ##Calculate null deviation metrics (alpha, beta, gamma) for null patches and store
  null.alphas[,randomize] <- apply(null.dist, 2, function(x){sum(ifelse(x > 0, 1, 0))})
  null.gamma[1, randomize] <- sum(ifelse(rowSums(null.dist)>0, 1, 0))
  expected_beta[1, randomize] <- 1 - mean(null.alphas[,randomize]/null.gamma[,randomize])
  null.alpha <- mean(null.alphas[,randomize])
  null.alpha.comp <- c(null.alpha.comp, null.alpha)

  bucket_bray <- as.matrix(vegdist(decostand(null.dist, method = "hellinger"), "bray")) #dissmiliarity matrix using bray-curtis distances
  diag(bucket_bray) <- NA
  bucket_bray_res[,randomize] <- apply(bucket_bray, 2, FUN="mean", na.rm=TRUE) #Mean dissimilarity values for each species are calculated and stored.
} ## end randomize loop
# #
# ## Calculate beta-diversity for observed metacommunity
beta_comm_abund <- vegdist(decostand(comm.t, method = "hellinger"), "bray")
res_beta_comm_abund <- as.matrix(as.dist(beta_comm_abund))
diag(res_beta_comm_abund) <- NA #diagonale null comparaisson deux a deux?

# #output beta diversity (Bray)
beta_div_abund_stoch <- apply(res_beta_comm_abund, 2, FUN="mean", na.rm=TRUE)
#
# # output abundance beta-null deviation
bray_abund_null_dev_fun <- beta_div_abund_stoch - mean(bucket_bray_res) #NDV : difference between the mean observed dissimilarity values and the mean dissimilarity values from the null distributions.

# #store in R project
# save(bray_abund_null_dev_fun,file="../results/bray_abund_null_dev_ITS_leaf_nontranspose_hel.RData")

save(bray_abund_null_dev_fun,file="../results/DRYER_GH_NDV_ITS_hel_bray.RData")


#
# boxplot(bray_abund_null_dev_fun, bray_abund_null_dev_fun_root,outline=FALSE)
# t.test(bray_abund_null_dev_fun,bray_abund_null_dev_fun_root, alternative = "two.sided", var.equal = FALSE)
# #test the (null) hypothesis that two populations have equal means. It is named for its creator, Bernard Lewis Welch, is an adaptation of Student's t-test,[1] and is more reliable when the two samples have unequal variances and possibly unequal sample sizes.[2][3]
#
# #Test statistic (t-value): The t-value is -193.71. This value measures the difference between the means of the two groups relative to the variability within each group. In this case, the t-value indicates a substantial difference between the means of the two samples.
#
# # Degrees of freedom (df): The degrees of freedom for this test are 15000. It represents the number of independent pieces of information available for estimating the population parameter. A higher degree of freedom allows for a more precise estimation.
# #
# # p-value: The p-value is < 2.2e-16, which is a very small value. The p-value represents the probability of observing a test statistic as extreme as the one calculated, assuming the null hypothesis is true. In this case, the p-value is extremely small, suggesting strong evidence against the null hypothesis (the true difference in means being zero).
# #
# # Alternative hypothesis: The alternative hypothesis is that the true difference in means is not equal to 0. The results support this alternative hypothesis.
#
# df_bray_fun_endo_root<-data.frame(bray_abund_null_dev_fun_root)
# colnames(df_bray_fun_endo_root)<-c("NDV")

#plots
# plot_ITS <- ggplot()+
#   geom_violin(data=df_bray_fun_endo,aes(x=factor(0),y=NDV),colour="black", fill="#a7c957",draw_quantiles = c(0.25,0.5,0.75),trim=FALSE) +
#   geom_violin(data=df_bray_fun_endo_root,aes(x=factor(1),y=NDV),colour="black",fill="#d4a373",draw_quantiles = c(0.25,0.5,0.75),trim=FALSE)+
#   scale_x_discrete(breaks=c(0,1),labels=c("Leaf","Root"))+
#   ggtitle("ITS null beta-distribution")+
#   annotate("text",x=1,y=0.63,label="T.test, p<2.2e-16", size = 6)+
#   theme_minimal(base_size = 16)+
#   theme(axis.title.x = element_blank())
# 
# 
# ggsave("../results/plot_ITS_ndv.jpeg", plot_ITS)


