cfw<-read.csv("RTOP.csv")

one<- cfw[cfw$subject== "one",]
zero<- cfw[cfw$subject == "zero",]

store=as.data.frame(matrix( NA, ncol=22, nrow=3))

for (i in 3:length(cfw))
{
  lm.sarina <- rlm(zero[[i]] ~ zero$AGE)
  s1=summary(lm.sarina)
  result_matrix=s1[[4]]
  se=result_matrix[2,2]
  store[1, i]=se
  coefficients=result_matrix[2]
  store[2, i]=coefficients
  tValue=result_matrix[2,3]
  store[3, i]=tValue
  #prediction by regression
}

colnames(store)=c("ODI","NA", "CC_1",	"CC_2",	"ALIC_3", 	"ALIC_4",	"PLIC_5",	"PLIC_6",	"RIC_7","RIC_8",	"ACR_9",	"ACR_10",	"SCR_11",	"SCR_12",	"PCR_13",	"PCR_14",	"SLF_25",	"SLF_26",	"UF_57","UF_58","SFOF_43",	"SFOF_44",	"IFOF_45",	"IFOF_46")
row.names(store)=c( "se", "slope", "t-value")
store2=t(store)

write.csv(store2, "LinearRegression/RTOP_LM.csv")
