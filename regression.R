
cfw<-read.csv("~/Documents/NEONATE-FINAL_july102016/neonate_final/RAW_DATA/RTOP.csv")

one<- cfw[cfw$subject== "one",]
zero<- cfw[cfw$subject == "zero",]
store=as.data.frame(matrix(NA, ncol=22, nrow=35))

store2=as.data.frame(matrix(NA, ncol=22, nrow=35))

for (i in 3:length(cfw))
  {
  lm.sarina <- rlm(zero[[i]] ~ zero$AGE)
  f=fitted(lm.sarina)
  j=colnames(cfw)[i]
    plot(zero$AGE, zero[[i]], main=j)
    line(zero$AGE, fitted(lm.sarina))

    store2[20:35, i]=f
    residual_control=as.numeric(resid(lm.sarina))
    store[20:35, i]=residual_control
     s1=summary(lm.sarina)
    result_matrix=s1[[4]]
      intercept=result_matrix[1]
      coefficients=result_matrix[2]
#prediction by regression
  fitted_CrB=coefficients*one$AGE + intercept
  store2[1:19, i]=fitted_CrB
    residuals_CrB=one[[i]]-fitted_CrB
  store[1:19, i]=residuals_CrB
  }
write.csv(store, "~/Documents/NEONATE-FINAL_july102016/neonate_final/residual_RTOP.csv")
#write.csv(store2, "~/Desktop/projected_RTOP.csv")
 

#T-test

g=read.csv("~/Documents/NEONATE-FINAL_july102016/neonate_final/residual_MD.csv")

one<- g[g$subject== "one",]
zero<- g[g$subject == "zero",]
#ttest
X<- mat.or.vec(2,22)
for (i in 2:length(g))
  
  {
  a<- t.test(one[i:i],zero[i:i])
X[[i]]<- a[1:1]
## df 
X[[i+22]]<- a[2:2]
## P
X[[i+22*2]]<- a[3:3]
## mean
X[[i+22*3]]<- a[5:5]
}
u=data.frame(X)
write.csv(u, "~/Documents/NEONATE-FINAL_july102016/neonate_final/MD_Initial-Pvalues.csv")

p=read.csv("~/Documents/NEONATE-FINAL_july102016/neonate_final/initial_pvalue-withoutMD.csv")
attach(p)

k=p.adjust(pvalue, method="fdr", n=154)

write.csv(k, '~/Documents/NEONATE-FINAL_july102016/neonate_final/FinalPvale_GM_withoutMD.csv')
#graph

