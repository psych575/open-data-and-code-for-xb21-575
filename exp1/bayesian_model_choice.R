library(ggplot2)
library(optimx)
library(R2jags)
library(dplyr)
library(coda)
library(gridExtra)
library(loo)

setwd('C:/Users/Lenovo/Desktop/design/实验一/')
data0 <- read.csv('merged_choice_bayesian.csv', header = FALSE) # 从工作路径中导入数据
names(data0)<-c("ob","Ms1","Mo1","choice1","Ms2","Mo2","choice2","ID")


sub <- unique(data0$ID) # 获取所有的被试编号
ns <- length(sub) # 被试数量
nt <- 48 # 试次数量
choice_data<-as.matrix(data0[2:8]) # 提取所有的数据


## 储存choice的数据
atype1 <- matrix(1:(ns*nt),nrow=ns) # 储存是否为优势不公平试次
atype2 <- matrix(1:(ns*nt),nrow=ns)
btype1 <- matrix(1:(ns*nt),nrow=ns) # 储存是否为劣势不公平试次
btype2 <- matrix(1:(ns*nt),nrow=ns)

Ms1 <- matrix(1:(ns*nt),nrow=ns) # 所有的Ms和Mo
Ms2 <- matrix(1:(ns*nt),nrow=ns)
Mo1 <- matrix(1:(ns*nt),nrow=ns)
Mo2 <- matrix(1:(ns*nt),nrow=ns)

# 提取所有的因变量choice
choice_1 <- matrix(1:(ns*nt),nrow=ns)
choice_2 <- matrix(1:(ns*nt),nrow=ns)

for (i in 1:ns){
  row1=(i-1)*nt+1
  row2=i*nt
  choice_1[i,] <- choice_data[row1:row2,3]
  choice_2[i,] <- choice_data[row1:row2,6]
  
  for (j in 1:nt){
    Ms1[i,j] <- choice_data[row1+j-1,1]
    Mo1[i,j] <- choice_data[row1+j-1,2]
    Ms2[i,j] <- choice_data[row1+j-1,4]
    Mo2[i,j] <- choice_data[row1+j-1,5]
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
  }
}


data <- list( "ns","nt0","choice_1","choice_2","atype1","atype2","btype1","btype2",
              "Ms1","Ms2","Mo1","Mo2") 
myinits <- list(list(miu_alpha1 = 0.5,lambda_alpha1 = 1,miu_alpha2 = 0.5,lambda_alpha2 = 1, 
                     miu_beta1 = 0.5,lambda_beta1 = 1,miu_beta2 = 0.5,lambda_beta2 = 1, 
                     miu_fai = 0.5,lambda_fai = 1,alpha1=rep(0.5,ns),alpha2=rep(0.5,ns),
                     beta1=rep(0.5,ns),beta2=rep(0.5,ns),fai=rep(0.5,ns))) #起始值
parameters <- c("alpha1", "alpha2","beta1","beta2","fai") # 参数

samples <- jags(data, inits=myinits, parameters,
                model.file ="choice_bayesian.txt", n.chains=1, n.iter=4000, 
                n.burnin=1000, n.thin=1, DIC=T)

dic <- samples$BUGSoutput$DIC
dic

alpha1 <- samples$BUGSoutput$sims.list$alpha1 
alpha2 <- samples$BUGSoutput$sims.list$alpha2 
beta1 <- samples$BUGSoutput$sims.list$beta1 
beta2 <- samples$BUGSoutput$sims.list$beta2 
fai <- samples$BUGSoutput$sims.list$fai 


m_alpha1 <- array()
m_alpha2 <- array()
m_beta1 <- array()
m_beta2 <- array()
m_fai <- array()

# 对每个被试，取分布的中位数
for (i in 1:ns){ 
  m_alpha1[i] <- median(alpha1[,i]) 
  m_alpha2[i] <- median(alpha2[,i]+alpha1[,i]) 
  m_beta1[i] <- median(beta1[,i]) 
  m_beta2[i] <- median(beta2[,i]+beta1[,i]) 
  m_fai[i] <- median(fai[,i])
}


paras <- matrix(1:(4*ns),nrow=ns)
paras[,1] <- m_alpha1
paras[,2] <- m_alpha2
paras[,3] <- m_beta1
paras[,4] <- m_beta2


write.table (paras, file ="C:/Users/Lenovo/Desktop/design/实验一/paras_choice.csv", sep =" ", row.names =TRUE, col.names =TRUE, quote =TRUE)
# 导出参数数据


