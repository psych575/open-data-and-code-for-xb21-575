library(rms)
library(lmerTest)
library(car)
library(lme4)


# 验证对rating的建模是有效的
data2 <- read.csv('merged_rating_regression.csv', header = FALSE) #导入所有试次的数据
data3 <- data.frame(data2)
cnames <- c('ob','Ms1','Mo1','Ms2','Mo2','rating','sub','visibility')
colnames(data3)=cnames


m0.glm <- glm(rating ~ 1, family = gaussian, data = data3) # 非混合基准模型


#混合模型
m0.lmer <- lmer(rating ~ 1 + (1|sub), REML = T, data = data3)
m1.lmer <- update(m0.lmer, .~.+Ms1)
m1.lmer <- update(m1.lmer, .~.+Mo1)
m1.lmer <- update(m1.lmer, .~.+Ms2)
m1.lmer <- update(m1.lmer, .~.+Mo2)
m1.lmer <- update(m1.lmer, .~.+visibility)
summary(m1.lmer)



