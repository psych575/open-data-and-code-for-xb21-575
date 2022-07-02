library(ggplot2)
library(optimx)
library(R2jags)
library(dplyr)
library(coda)
library(gridExtra)
library(loo)

setwd('C:/Users/Lenovo/Desktop/design/实验一/')

data0 <- read.csv('merged_choice_bayesian.csv', header = FALSE) # 导入选择条件的数据
names(data0)<-c("ob","Ms1","Mo1","choice1","Ms2","Mo2","choice2","ID")
# 1代表可见条件，2代表不可见条件

data1 <- read.csv('merged_rating_bayesian.csv', header = FALSE) # 导入评分条件的数据
names(data1)<-c("ob","Ms1_v","Mo1_v","Ms2_v","Mo2_v","rating_v",
                "Ms1_iv","Mo1_iv","Ms2_iv","Mo2_iv","rating_iv",
                "ID")
# v代表可见条件，iv代表不可见条件，1代表选中的选项，2代表未选中的选项

sub <- unique(data0$ID) # 获取所有的被试编号
ns <- length(sub) # 被试数量
nt0 <- 48 # 选择条件试次数量
choice_data<-as.matrix(data0[2:8]) # 提取所有选择条件下的数据

nt1 <- 72 # 评分条件试次数量
rating_data<-as.matrix(data1[2:12]) # 提取所有评分条件下的数据


## 储存选择条件的数据
atype1 <- matrix(1:(ns*nt0),nrow=ns) # 储存是否为优势不公平试次
atype2 <- matrix(1:(ns*nt0),nrow=ns)
btype1 <- matrix(1:(ns*nt0),nrow=ns) # 储存是否为劣势不公平试次
btype2 <- matrix(1:(ns*nt0),nrow=ns)

Ms1 <- matrix(1:(ns*nt0),nrow=ns) # 所有的Ms和Mo
Ms2 <- matrix(1:(ns*nt0),nrow=ns)
Mo1 <- matrix(1:(ns*nt0),nrow=ns)
Mo2 <- matrix(1:(ns*nt0),nrow=ns)

# 提取所有的因变量choice
choice_1 <- matrix(1:(ns*nt0),nrow=ns)
choice_2 <- matrix(1:(ns*nt0),nrow=ns)

for (i in 1:ns){
  row1=(i-1)*48+1
  row2=i*48
  choice_1[i,] <- choice_data[row1:row2,3]
  choice_2[i,] <- choice_data[row1:row2,6]
  
  for (j in 1:nt0){
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


## 储存评分条件下的数据
atype1_v <- matrix(1:(ns*nt1),nrow=ns) # 储存是否为优势不公平试次
atype2_v <- matrix(1:(ns*nt1),nrow=ns)
btype1_v <- matrix(1:(ns*nt1),nrow=ns) # 储存是否为劣势不公平试次
btype2_v <- matrix(1:(ns*nt1),nrow=ns)

atype1_iv <- matrix(1:(ns*nt1),nrow=ns) # 储存是否为优势不公平试次
atype2_iv <- matrix(1:(ns*nt1),nrow=ns)
btype1_iv <- matrix(1:(ns*nt1),nrow=ns) # 储存是否为劣势不公平试次
btype2_iv <- matrix(1:(ns*nt1),nrow=ns)

Ms1_v <- matrix(1:(ns*nt1),nrow=ns) # 可见条件下，选中选项的Ms和Mo
Ms2_v <- matrix(1:(ns*nt1),nrow=ns)
Mo1_v <- matrix(1:(ns*nt1),nrow=ns) # 可见条件下，未选中选项的Ms和Mo
Mo2_v <- matrix(1:(ns*nt1),nrow=ns)

Ms1_iv <- matrix(1:(ns*nt1),nrow=ns) # 不可见条件下，选中选项的Ms和Mo
Ms2_iv <- matrix(1:(ns*nt1),nrow=ns)
Mo1_iv <- matrix(1:(ns*nt1),nrow=ns) # 不可见条件下，未选中选项的Ms和Mo
Mo2_iv <- matrix(1:(ns*nt1),nrow=ns)

# 提取所有的因变量rating
rating_v <- matrix(1:(ns*nt1),nrow=ns)
rating_iv <- matrix(1:(ns*nt1),nrow=ns)


for (i in 1:ns){
  row1=(i-1)*72+1
  row2=i*72
  rating_v[i,] <- rating_data[row1:row2,5]
  rating_iv[i,] <- rating_data[row1:row2,10]
  
  for (j in 1:nt1){
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


data <- list( "ns","nt0","nt1","choice_1","choice_2","atype1","atype2","btype1","btype2",
              "Ms1","Ms2","Mo1","Mo2",
              "rating_v","rating_iv","atype1_v","atype2_v","btype1_v","btype2_v",
              "atype1_iv","atype2_iv","btype1_iv","btype2_iv","Ms1_v","Ms2_v","Mo1_v","Mo2_v",
              "Ms1_iv","Ms2_iv","Mo1_iv","Mo2_iv") 
myinits <- list(list(miu_alpha1 = 0.5,lambda_alpha1 = 1,miu_alpha2 = 0.5,lambda_alpha2 = 1,
                     miu_alpha3 = 0.5,lambda_alpha3 = 1,miu_alpha4 = 0.5,lambda_alpha4 = 1,
                     miu_beta1 = 0.5,lambda_beta1 = 1,miu_beta2 = 0.5,lambda_beta2 = 1,
                     miu_beta3 = 0.5,lambda_beta3 = 1,miu_beta4 = 0.5,lambda_beta4 = 1,
                     miu_b01 = 0.5,lambda_b01 = 1,miu_b11 = 0.5,lambda_b11 = 1,
                     miu_b02 = 0.5,lambda_b02 = 1,miu_b12 = 0.5,lambda_b12 = 1,
                     miu_e1 = 0.5,lambda_e1 = 1,miu_e2 = 0.5,lambda_e2 = 1,
                     miu_fai1 = 0.5,lambda_fai1 = 1, miu_fai2 = 0.5,lambda_fai2 = 1,
                     alpha1=rep(0.5,ns),alpha2=rep(0.5,ns),alpha3=rep(0.5,ns),alpha4=rep(0.5,ns),
                     beta1=rep(0.5,ns),beta2=rep(0.5,ns),beta3=rep(0.5,ns),beta4=rep(0.5,ns),
                     fai1=rep(0.5,ns),fai2=rep(0.5,ns),b01=rep(0.5,ns),b11=rep(0.5,ns),
                     b02=rep(0.5,ns),b12=rep(0.5,ns),e1=rep(1,ns),e2=rep(1,ns))) #起始值
parameters <- c("alpha1", "alpha2","alpha3", "alpha4","beta1","beta2","beta3","beta4") # 参数

samples <- jags(data, inits=myinits, parameters,
                model.file ="choice_rating_bayesian.txt", n.chains=1, n.iter=4000, 
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
  m_alpha3[i] <- median(alpha3[,i])
  m_alpha4[i] <- median(alpha3[,i]+alpha4[,i])
  m_beta1[i] <- median(beta1[,i]) 
  m_beta2[i] <- median(beta2[,i]+beta1[,i]) 
  m_beta3[i] <- median(beta3[,i])
  m_beta4[i] <- median(beta3[,i]+beta4[,i])
}


paras <- matrix(1:(ns*8),nrow=ns)
paras[,1] <- m_alpha1
paras[,2] <- m_beta1
paras[,3] <- m_alpha2
paras[,4] <- m_beta2
paras[,5] <- m_alpha3
paras[,6] <- m_beta3
paras[,7] <- m_alpha4
paras[,8] <- m_beta4

write.table (paras, file ="C:/Users/Lenovo/Desktop/design/实验一/paras.csv", sep =" ", row.names =TRUE, col.names =TRUE, quote =TRUE)

