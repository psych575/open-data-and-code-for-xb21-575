library(ggplot2)
library(optimx)
library(R2jags)
library(dplyr)
library(coda)
library(gridExtra)
library(loo)

setwd('C:/Users/Lenovo/Desktop/design/实验二/')
data0 <- read.csv('merged_choice_bayesian.csv', header = FALSE) # 从工作路径中导入数据
names(data0)<-c("ob","sub","trial","Ms1","Mo1","choice1","Ms2","Mo2","choice2",
                "Ms3","Mo3","choice3","Ms4","Mo4","choice4")


sub <- unique(data0$sub) # 获取所有的被试编号
ns <- length(sub) # 被试数量
nt <- 48 # 试次数量
choice_data<-as.matrix(data0[4:15]) # 提取所有的数据


## 储存choice的数据
atype1 <- matrix(1:(ns*nt),nrow=ns) # 储存是否为优势不公平试次
atype2 <- matrix(1:(ns*nt),nrow=ns)
atype3 <- matrix(1:(ns*nt),nrow=ns) 
atype4 <- matrix(1:(ns*nt),nrow=ns)
btype1 <- matrix(1:(ns*nt),nrow=ns) # 储存是否为劣势不公平试次
btype2 <- matrix(1:(ns*nt),nrow=ns)
btype3 <- matrix(1:(ns*nt),nrow=ns) 
btype4 <- matrix(1:(ns*nt),nrow=ns)

Ms1 <- matrix(1:(ns*nt),nrow=ns) # 所有的Ms和Mo
Ms2 <- matrix(1:(ns*nt),nrow=ns)
Ms3 <- matrix(1:(ns*nt),nrow=ns) 
Ms4 <- matrix(1:(ns*nt),nrow=ns)
Mo1 <- matrix(1:(ns*nt),nrow=ns)
Mo2 <- matrix(1:(ns*nt),nrow=ns)
Mo3 <- matrix(1:(ns*nt),nrow=ns)
Mo4 <- matrix(1:(ns*nt),nrow=ns)

# 提取所有的因变量choice
choice_1 <- matrix(1:(ns*nt),nrow=ns)
choice_2 <- matrix(1:(ns*nt),nrow=ns)
choice_3 <- matrix(1:(ns*nt),nrow=ns)
choice_4 <- matrix(1:(ns*nt),nrow=ns)


for (i in 1:ns){
  row1=(i-1)*48+1
  row2=i*48
  choice_1[i,] <- choice_data[row1:row2,3]
  choice_2[i,] <- choice_data[row1:row2,6]
  choice_3[i,] <- choice_data[row1:row2,9]
  choice_4[i,] <- choice_data[row1:row2,12]
  
  for (j in 1:nt){
    Ms1[i,j] <- choice_data[row1+j-1,1]
    Mo1[i,j] <- choice_data[row1+j-1,2]
    Ms2[i,j] <- choice_data[row1+j-1,4]
    Mo2[i,j] <- choice_data[row1+j-1,5]
    Ms3[i,j] <- choice_data[row1+j-1,7]
    Mo3[i,j] <- choice_data[row1+j-1,8]
    Ms4[i,j] <- choice_data[row1+j-1,10]
    Mo4[i,j] <- choice_data[row1+j-1,11]
    if (choice_data[row1+j-1,1]>=choice_data[row1+j-1,2]){
      atype1[i,j] <- 1
      btype1[i,j] <- 0
    }else{
      atype1[i,j] <- 0
      btype1[i,j] <- 1
    }
    if (choice_data[row1+j-1,4]>=choice_data[row1+j-1,5]){
      atype2[i,j] <- 1
      btype2[i,j] <- 0
    }else{
      atype2[i,j] <- 0
      btype2[i,j] <- 1
    }
    if (choice_data[row1+j-1,7]>=choice_data[row1+j-1,8]){
      atype3[i,j] <- 1
      btype3[i,j] <- 0
    }else{
      atype3[i,j] <- 0
      btype3[i,j] <- 1
    }
    if (choice_data[row1+j-1,10]>=choice_data[row1+j-1,11]){
      atype4[i,j] <- 1
      btype4[i,j] <- 0
    }else{
      atype4[i,j] <- 0
      btype4[i,j] <- 1
    }
  }
}


data <- list( "ns","nt","choice_1","choice_2","choice_3","choice_4","atype1","atype2",
              "atype3","atype4","btype1","btype2","btype3","btype4",
              "Ms1","Ms2","Ms3","Ms4","Mo1","Mo2","Mo3","Mo4") 
myinits <- list(list(miu_alpha1 = 0.5,lambda_alpha1 = 1,miu_alpha2 = 0.5,lambda_alpha2 = 1, 
                     miu_alpha3 = 0.5,lambda_alpha3 = 1,miu_alpha4 = 0.5,lambda_alpha4 = 1, 
                     miu_beta1 = 0.5,lambda_beta1 = 1,miu_beta2 = 0.5,lambda_beta2 = 1, 
                     miu_beta3 = 0.5,lambda_beta3 = 1,miu_beta4 = 0.5,lambda_beta4 = 1,
                     miu_fai1 = 0.5,lambda_fai1 = 1,miu_fai2 = 0.5,lambda_fai2 = 1,
                     miu_fai3 = 0.5,lambda_fai3 = 1,miu_fai4 = 0.5,lambda_fai4 = 1,
                     alpha1=rep(0.5,ns),alpha2=rep(0.5,ns),alpha3=rep(0.5,ns),alpha4=rep(0.5,ns),
                     beta1=rep(0.5,ns),beta2=rep(0.5,ns),beta3=rep(0.5,ns),beta4=rep(0.5,ns),
                     fai1=rep(0.5,ns),fai2=rep(0.5,ns),fai3=rep(0.5,ns),fai4=rep(0.5,ns))) #起始值
parameters <- c("alpha1", "alpha2","alpha3","alpha4","beta1","beta2","beta3","beta4",
                "fai1","fai2","fai3","fai4") # 参数

samples <- jags(data, inits=myinits, parameters,
                model.file ="choice_bayesian.txt", n.chains=1, n.iter=4000, 
                n.burnin=1000, n.thin=1, DIC=T)

dic <- samples$BUGSoutput$DIC
dic


alpha1 <- samples$BUGSoutput$sims.list$alpha1 
alpha2 <- samples$BUGSoutput$sims.list$alpha2 
alpha3 <- samples$BUGSoutput$sims.list$alpha3
alpha4 <- samples$BUGSoutput$sims.list$alpha4
beta1 <- samples$BUGSoutput$sims.list$beta1 
beta2 <- samples$BUGSoutput$sims.list$beta2 
beta3 <- samples$BUGSoutput$sims.list$beta3 
beta4 <- samples$BUGSoutput$sims.list$beta4


m_alpha1 <- array()
m_alpha2 <- array()
m_alpha3 <- array()
m_alpha4 <- array()
m_beta1 <- array()
m_beta2 <- array()
m_beta3 <- array()
m_beta4 <- array()


# 对每个被试，取分布的中位数
for (i in 1:ns){ 
  m_alpha1[i] <- median(alpha1[,i]) 
  m_alpha2[i] <- median(alpha2[,i]+alpha1[,i]) 
  m_alpha3[i] <- median(alpha3[,i]+alpha1[,i]) 
  m_alpha4[i] <- median(alpha4[,i]+alpha1[,i]) 
  m_beta1[i] <- median(beta1[,i]) 
  m_beta2[i] <- median(beta2[,i]+beta1[,i]) 
  m_beta3[i] <- median(beta3[,i]+beta1[,i]) 
  m_beta4[i] <- median(beta4[,i]+beta1[,i]) 
}


paras <- matrix(1:(ns*10),nrow=ns)
paras[,1] <- m_alpha1
paras[,2] <- m_alpha2
paras[,3] <- m_alpha3
paras[,4] <- m_alpha4
paras[,5] <- m_beta1
paras[,6] <- m_beta2
paras[,7] <- m_beta3
paras[,8] <- m_beta4


write.table (paras, file ="C:/Users/Lenovo/Desktop/design/实验二/paras.csv", sep =" ", row.names =TRUE, col.names =TRUE, quote =TRUE)

