library(ggplot2)
library(optimx)
library(R2jags)
library(dplyr)
library(coda)
library(gridExtra)
library(loo)

setwd(':/Users/Lenovo/Desktop/design/实验一/')
data0 <- read.csv('merged_rating_bayesian.csv', header = FALSE) # 从工作路径中导入数据
names(data0)<-c("ob","Ms1_v","Mo1_v","Ms2_v","Mo2_v","rating_v",
                "Ms1_iv","Mo1_iv","Ms2_iv","Mo2_iv","rating_iv",
                "ID")
sub <- unique(data0$ID) # 获取所有的被试编号
ns <- length(sub) # 被试数量
nt <- 72 # 试次数量
rating_data<-as.matrix(data0[2:12]) # 提取所有的数据

atype1_v <- matrix(1:(ns*nt),nrow=ns) # 储存是否为优势不公平试次
atype2_v <- matrix(1:(ns*nt),nrow=ns)
btype1_v <- matrix(1:(ns*nt),nrow=ns) # 储存是否为劣势不公平试次
btype2_v <- matrix(1:(ns*nt),nrow=ns)

atype1_iv <- matrix(1:(ns*nt),nrow=ns) # 储存是否为优势不公平试次
atype2_iv <- matrix(1:(ns*nt),nrow=ns)
btype1_iv <- matrix(1:(ns*nt),nrow=ns) # 储存是否为劣势不公平试次
btype2_iv <- matrix(1:(ns*nt),nrow=ns)

Ms1_v <- matrix(1:(ns*nt),nrow=ns) # 可见条件下，选中选项的Ms和Mo
Ms2_v <- matrix(1:(ns*nt),nrow=ns)
Mo1_v <- matrix(1:(ns*nt),nrow=ns) # 可见条件下，未选中选项的Ms和Mo
Mo2_v <- matrix(1:(ns*nt),nrow=ns)

Ms1_iv <- matrix(1:(ns*nt),nrow=ns) # 不可见条件下，选中选项的Ms和Mo
Ms2_iv <- matrix(1:(ns*nt),nrow=ns)
Mo1_iv <- matrix(1:(ns*nt),nrow=ns) # 不可见条件下，未选中选项的Ms和Mo
Mo2_iv <- matrix(1:(ns*nt),nrow=ns)

# 提取所有的因变量rating
rating_v <- matrix(1:(ns*nt),nrow=ns)
rating_iv <- matrix(1:(ns*nt),nrow=ns)

for (i in 1:ns){
  row1=(i-1)*nt+1
  row2=i*nt
  rating_v[i,] <- rating_data[row1:row2,5]
  rating_iv[i,] <- rating_data[row1:row2,10]
  
  for (j in 1:nt){
    Ms1_v[i,j] <- rating_data[row1+j-1,1]
    Mo1_v[i,j] <- rating_data[row1+j-1,2]
    Ms2_v[i,j] <- rating_data[row1+j-1,3]
    Mo2_v[i,j] <- rating_data[row1+j-1,4]
    
    Ms1_iv[i,j] <- rating_data[row1+j-1,6]
    Mo1_iv[i,j] <- rating_data[row1+j-1,7]
    Ms2_iv[i,j] <- rating_data[row1+j-1,8]
    Mo2_iv[i,j] <- rating_data[row1+j-1,9]
    

    if (rating_data[row1+j-1,1]>rating_data[row1+j-1,2]){
      atype1_v[i,j] <- 1
      btype1_v[i,j] <- 0
    }else if (rating_data[row1+j-1,1]<rating_data[row1+j-1,2]){
      atype1_v[i,j] <- 0
      btype1_v[i,j] <- 1
    }else{
      atype1_v[i,j] <- 0
      btype1_v[i,j] <- 0
    }
    
    if (rating_data[row1+j-1,3]>rating_data[row1+j-1,4]){
      atype2_v[i,j] <- 1
      btype2_v[i,j] <- 0
    }else if (rating_data[row1+j-1,3]<rating_data[row1+j-1,4]){
      atype2_v[i,j] <- 0
      btype2_v[i,j] <- 1
    }else{
      atype2_v[i,j] <- 0
      btype2_v[i,j] <- 0
    }
    
    
    if (rating_data[row1+j-1,6]>rating_data[row1+j-1,7]){
      atype1_iv[i,j] <- 1
      btype1_iv[i,j] <- 0
    }else if (rating_data[row1+j-1,6]<rating_data[row1+j-1,7]){
      atype1_iv[i,j] <- 0
      btype1_iv[i,j] <- 1
    }else{
      atype1_iv[i,j] <- 0
      btype1_iv[i,j] <- 0
    }
    
    if (rating_data[row1+j-1,8]>rating_data[row1+j-1,9]){
      atype2_iv[i,j] <- 1
      btype2_iv[i,j] <- 0
    }else if (rating_data[row1+j-1,8]<rating_data[row1+j-1,9]){
      atype2_iv[i,j] <- 0
      btype2_iv[i,j] <- 1
    }else{
      atype2_iv[i,j] <- 0
      btype2_iv[i,j] <- 0
    }
    
    
  }
}


data <- list( "ns","nt","rating_v","rating_iv","atype1_v","atype2_v","btype1_v","btype2_v",
              "atype1_iv","atype2_iv","btype1_iv","btype2_iv","Ms1_v","Ms2_v","Mo1_v","Mo2_v",
              "Ms1_iv","Ms2_iv","Mo1_iv","Mo2_iv") 
myinits <- list(list(miu_alpha1 = 0.5,lambda_alpha1 = 1,miu_alpha2 = 0.5,lambda_alpha2 = 1, 
                     miu_beta1 = 0.5,lambda_beta1 = 1,miu_beta2 = 0.5,lambda_beta2 = 1, 
                     miu_b0 = 0.5,lambda_b0 = 1,miu_b1 = 0.5,lambda_b1 = 1,
                     alpha1=rep(0.5,ns),alpha2=rep(0.5,ns),beta1=rep(0.5,ns),
                     beta2=rep(0.5,ns),b0=rep(0.5,ns),b1=rep(0.5,ns))) #起始值
parameters <- c("alpha1", "alpha2","beta1","beta2","b0","b1") # 参数

samples <- jags(data, inits=myinits, parameters,
                model.file ="rating_bayesian.txt", n.chains=1, n.iter=4000, 
                n.burnin=1000, n.thin=1, DIC=T)

dic <- samples$BUGSoutput$DIC
dic

alpha1 <- samples$BUGSoutput$sims.list$alpha1 
alpha2 <- samples$BUGSoutput$sims.list$alpha2 
beta1 <- samples$BUGSoutput$sims.list$beta1 
beta2 <- samples$BUGSoutput$sims.list$beta2 
b0 <- samples$BUGSoutput$sims.list$b0
b1 <- samples$BUGSoutput$sims.list$b1

m_alpha1 <- array()
m_alpha2 <- array()
m_beta1 <- array()
m_beta2 <- array()

# 对每个被试，取分布的中位数
for (i in 1:ns){ 
  m_alpha1[i] <- median(alpha1[,i]) 
  m_alpha2[i] <- median(alpha2[,i])+median(alpha1[,i]) 
  m_beta1[i] <- median(beta1[,i]) 
  m_beta2[i] <- median(beta2[,i])+median(beta1[,i]) 
}



paras <- matrix(1:(4*ns),nrow=ns)
paras[,1] <- m_alpha1
paras[,2] <- m_alpha2
paras[,3] <- m_beta1
paras[,4] <- m_beta2

write.table (paras, file ="C:/Users/Lenovo/Desktop/design/实验一/paras_rating.csv", sep =" ", row.names =TRUE, col.names =TRUE, quote =TRUE)
# 输出参数数据
