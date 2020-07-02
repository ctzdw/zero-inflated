clean_data<-file.choose()
library(BhGLM)
library(nlme)
library(pROC)
library(MASS)
library(pscl)
library(purrr)
#建立数据集
data1<-data.frame(t(data0))
data1<-cbind(clean_meta,data1)
data1<-subset(data1,select = -4)
id=paste("X",1:1579,sep="")
clean_tax<-data.frame(id,clean_tax)
datax<-na.omit(data1)
datax$BMI<-as.numeric(as.character(datax$BMI))
datax<-subset(datax,datax$Age>=7)
d<-na.omit(data1)
d<-subset(d,d$Age>=7)
#建立肥胖判断指标
age_level<-c(7:17)
fe_index1<-c(13.4,13.6,13.8,14.0,14.3,14.7,15.3,16.0,16.6,17.0,17.2)
fe_index2<-c(17.2,18.1,19,20,21.1,21.9,22.6,23,23.4,23.7,23.8)
fe_index3<-c(18.9,19.9,21,22.1,23.3,24.5,25.6,26.3,26.9,27.4,27.7)
ma_index1<-c(13.9,14,14.1,14.4,14.9,15.4,15.9,16.4,16.9,17.3,17.7)
ma_index2<-c(17.4,18.1,18.9,19.6,20.3,21,21.9,22.6,23.1,23.5,23.8)
ma_index3<-c(19.2,20.3,21.4,22.5,23.6,24.7,25.7,26.4,26.9,27.4,27.8)
judge_feBMI<-data.frame(age_level,fe_index1,fe_index2,fe_index3)
judge_maBMI<-data.frame(age_level,ma_index1,ma_index2,ma_index3)
#按照其BMI指数及年龄，划分为消瘦，健康，肥胖，严重超重四个级别
dim(datax)[1]
for (i in 1:dim(datax)[1]) {
  if(datax$Sex[i]=='Female'){
    for (j in 1:dim(judge_feBMI)[1]) {
      if(datax$Age[i]==judge_feBMI$age_level[j] ){
        datax[i,]$BMI[datax[i,]$BMI < judge_feBMI$fe_index1[j]] <- 0;
        datax[i,]$BMI[judge_feBMI$fe_index1[j]<=datax[i,]$BMI & datax[i,]$BMI < judge_feBMI$fe_index2[j]] <- 1
        datax[i,]$BMI[judge_feBMI$fe_index2[j]<=datax[i,]$BMI & datax[i,]$BMI< judge_feBMI$fe_index3[j]] <- 2
        datax[i,]$BMI[judge_feBMI$fe_index3[j]<=datax[i,]$BMI] <- 2}
      }
  }
}

for (i in 1:dim(datax)[1]) {
  if(datax$Sex[i]=='Female' & datax$Age[i]>17){
       {datax[i,]$BMI[datax[i,]$BMI < judge_feBMI$fe_index1[11]] <- 0;
       datax[i,]$BMI[judge_feBMI$fe_index1[11]<=datax[i,]$BMI & datax[i,]$BMI < judge_feBMI$fe_index2[11]] <- 1
       datax[i,]$BMI[judge_feBMI$fe_index2[11]<=datax[i,]$BMI & datax[i,]$BMI< judge_feBMI$fe_index3[11]] <- 2
       datax[i,]$BMI[judge_feBMI$fe_index3[11]<=datax[i,]$BMI] <- 2}
  }
}
#男
for (i in 1:dim(datax)[1]) {
  if(datax$Sex[i]=='Male'){
    for (j in 1:dim(judge_maBMI)[1]) {
      if(datax$Age[i]==judge_maBMI$age_level[j]){
        datax[i,]$BMI[datax[i,]$BMI < judge_maBMI$ma_index1[j]] <- 0;
        datax[i,]$BMI[judge_maBMI$ma_index1[j]<=datax[i,]$BMI & datax[i,]$BMI < judge_maBMI$ma_index2[j]] <- 1
        datax[i,]$BMI[judge_maBMI$ma_index2[j]<=datax[i,]$BMI & datax[i,]$BMI< judge_maBMI$ma_index3[j]] <- 2
        datax[i,]$BMI[judge_maBMI$ma_index3[j]<=datax[i,]$BMI] <- 2}
    }
  }
}
for (i in 1:dim(datax)[1]) {
  if(datax$Sex[i]=='Male' & datax$Age[i]>17){
        datax[i,]$BMI[datax[i,]$BMI < judge_maBMI$ma_index1[11]] <- 0;
        datax[i,]$BMI[judge_maBMI$ma_index1[11]<=datax[i,]$BMI & datax[i,]$BMI < judge_maBMI$ma_index2[11]] <- 1
        datax[i,]$BMI[judge_maBMI$ma_index2[11]<=datax[i,]$BMI & datax[i,]$BMI< judge_maBMI$ma_index3[11]] <- 2
        datax[i,]$BMI[judge_maBMI$ma_index3[11]<=datax[i,]$BMI] <- 2
    }
  }

datax$BMI <- factor(datax$BMI,    #转为factor形式，替换标签
                         levels=c(0,1,2),labels=c("thin","health",'severe'))

#group也要改


datax$Group <- factor(datax$Group,    #转为factor形式，替换标签
                    levels=c('kindergarten','Pupils','mid_school','youth','mid_age','elder',"Centenarians","young soldier"),
                    labels=c('not_adult','not_adult','not_adult','adult','adult','adult','adult','adult'))
# m 模拟样本次数，f需模拟的函数
sim.fun <-function (m,f,...) { 
  sample <-1:m 
  for (i in 1:m) { 
    sample[i] <-f(...) 
  } 
  sample 
}

f <- function(n=10,p=0.5){s=rnbinom(1,n,p); (s-n*p)/sqrt(n*p*(1-p)) }
xf  <- sim.fun(1579,f)
xf1<-sim.fun(1579,f)

# NOT RUN {
## data
data("bioChemists", package = "pscl")

## without inflation
## ("art ~ ." is "art ~ fem + mar + kid5 + phd + ment")
fm_pois <- glm(art ~ ., data = bioChemists, family = poisson)
fm_qpois <- glm(art ~ ., data = bioChemists, family = quasipoisson)
fm_nb <- MASS::glm.nb(art ~ ., data = bioChemists)

## with simple inflation (no regressors for zero component)
fm_zip <- zeroinfl(art ~ . | 1, data = bioChemists)
fm_zinb <- zeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")

## inflation with regressors
## ("art ~ . | ." is "art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment")
fm_zip2 <- zeroinfl(art ~ . | ., data = bioChemists)
fm_zinb2 <- zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")



ZIPI<-zeroinfl(datax$X10 ~ datax$Group+datax$Sex+datax$BMI+datax$Diet| 1, data = datax)
ZIPI2<-zeroinfl(datax$X10 ~ datax$Group+datax$Sex+datax$BMI+datax$Diet | datax$Group+datax$Sex+datax$BMI+datax$Diet, data = datax)
zinb <- zeroinfl(datax$X10 ~datax$Group+datax$Sex+datax$BMI+datax$Diet | 1, data = datax, dist = "negbin")
zinb2 <- zeroinfl(datax$X10 ~ datax$Group+datax$Sex+datax$BMI+datax$Diet | datax$Group+datax$Sex+datax$BMI+datax$Diet, data = datax, dist = "negbin")
shrinkage(fm_nb)
library(plyr)
#建立仿真数据集,共计
nSim <- 1000
sim_group <- array(0, dim=nSim)
n <- 400#50#模拟样本量400或者50
n0<-0.5#0的概率
y0<-c()
y<-c()#sim
ZIPI<-c()#poi
ZINBI<-c()#zin
ZINBII<-c()#zinb2
for (i in 1:nSim) {
  x1 <- sample(c('Men','Women'),n,replace = TRUE,prob = c(0.5,0.5))
  x2<-sample(c('Single','Married'),n,replace = TRUE,prob = c(0.66,0.34))
  x4 <- rnorm(n,3,2)
  x3<-rnorm(n,10,2)
  x5<-runif(n,0,20)
  x6<-sample(1:2, n, replace=TRUE, prob=c(1-n0, n0))
  x<-data.frame(x1,x2,x3,x4,x5)
  colnames(x)<-colnames(bioChemists)[-1]
  y0<-predict(fm_zinb,newdata=x)
  for (j in 1:n) {
    if(x6[j]==2){
    y0[j]<-0
      }
  }
  for (j in 1:n) {
    if(y0[j]>0.3){
      y0[j]<-1
    }
    else{y0[j]<-0}
  }
  x<-data.frame(y0,x)
 ZIPI<-rbind(ZIPI,t(t(predict(fm_zip,newdata=x))))
 ZINBI<-rbind(ZINBI,t(t(predict(fm_zinb,newdata=x))))
 ZINBII<-rbind(ZINBII,t(t(predict(fm_zinb2,newdata=x))))
 y<-rbind(y,t(t(y0)))
}

library(ROCR)
da<-data.frame(y,ZIPI,ZINBI,ZINBII)
roc.list <- roc(y~ZIPI+ZINBI+ZINBII,data = da)
roc.list
g.list <- ggroc(roc.list)
g.list



#R语言的glm(){stats}可进行泊松回归分析（family=poisson）
#R语言的glm.nb(){MASS}可进行负二项回归分析；
#R语言中的zeroinfl(){pscl}可进行零膨胀泊松回归分析（dist="poisson"）或零膨胀负二项分析（dist="negbin"）；
#比较嵌套模型（嵌套模型），可用似然比检验（似然比测试）lrtest(){lmtest}进行；#
#用vuong(){pscl}可进行相同数据集非嵌套模型的比较分析；
#dat.glmnb=glm.nb(y~type+gender+offset(logn))  #负二项回归
#summary(dat.glmnb)  #输出结果
#roc(myData$label, myData$score, plot=TRUE, print.thres=TRUE, print.auc=TRUE)



##实证
#genus
data_genus<-datax[,1:10]
genus_list<-c('Incertae_Sedis','Bacteroides','Lactobacillus','Sporobacter','Lactococcus','Enterorhabdus','Butyricicoccus','Marvinbryantia','Blautia'
              ,'Intestinimonas','Peptoniphilus','Roseburia','Flavonifractor','Ruminococcus','Dialister')
a<-data.frame(genus_list)
k<-c()
for (i in 1:dim(a)[1]) {
  b<-c()
  for (j in 1:dim(datax)[1]) {
    c<-c()
    for (h in 1:1579) {
      if(clean_tax$genus[h]==a[i,1]){
        c<-cbind(c,datax[j,h+9])
      }
    }
    b<-rbind(b,sum(c))
  }
  k<-cbind(k,b) 
}

data_genus<-cbind(data_genus,k)





#family
data_family<-datax[,1:10]
family_list<-c('Ruminococcaceae','Lachnospiraceae','Prevotellaceae','Bacteroidaceae','Coriobacteriaceae','Porphyromonadaceae','Erysipelotrichaceae')
a<-data.frame(family_list)
k<-c()
for (i in 1:dim(a)[1]) {
  b<-c()
  for (j in 1:dim(datax)[1]) {
    c<-c()
    for (h in 1:1579) {
      if(clean_tax$family[h]==a[i,1]){
        c<-cbind(c,datax[j,h+9])
      }
    }
    b<-rbind(b,sum(c))
  }
 k<-cbind(k,b) 
}
data_family<-cbind(data_family,k)

#class
data_class<-datax[,1:10]
class_list<-c('Clostridiales','Bacteroidales','Lactobacillales','Coriobacteriales','Pseudomonadales','Bacillales','Burkholderiales')
a<-data.frame(class_list)
k<-c()
for (i in 1:dim(a)[1]) {
  b<-c()
  for (j in 1:dim(datax)[1]) {
    c<-c()
    for (h in 1:1579) {
      if(clean_tax$class[h]==a[i,1]){
        c<-cbind(c,datax[j,h+9])
      }
    }
    b<-rbind(b,sum(c))
  }
  k<-cbind(k,b) 
}
data_class<-cbind(data_class,k)



#order
data_order<-datax[,1:10]
order_list<-c('Clostridia','Bacteroidia','Bacilli','Gammaproteobacteria','Actinobacteria','Flavobacteriia','Fusobacteriia')
a<-data.frame(order_list)
k<-c()
for (i in 1:dim(a)[1]) {
  b<-c()
  for (j in 1:dim(datax)[1]) {
    c<-c()
    for (h in 1:1579) {
      if(clean_tax$order[h]==a[i,1]){
        c<-cbind(c,datax[j,h+9])
      }
    }
    b<-rbind(b,sum(c))
  }
  k<-cbind(k,b) 
}
data_order<-cbind(data_order,k)



#phylum
data_phylum<-datax[,1:10]
phylum_list<-c('Firmicutes','Bacteroidetes','Proteobacteria','Actinobacteria','Tenericutes','Cyanobacteria','Fusobacteria')
a<-data.frame(phylum_list)
k<-c()
for (i in 1:dim(a)[1]) {
  b<-c()
  for (j in 1:dim(datax)[1]) {
    c<-c()
    for (h in 1:1579) {
      if(clean_tax$phylum[h]==a[i,1]){
        c<-cbind(c,datax[j,h+9])
      }
    }
    b<-rbind(b,sum(c))
  }
  k<-cbind(k,b) 
}
data_phylum<-cbind(data_phylum,k)



colnames(data_genus)<-c('id','Group','Sex','Age','Height','Weight','BMI','Diet','Season','Incertae_Sedis','Bacteroides','Lactobacillus','Sporobacter','Lactococcus','Enterorhabdus','Butyricicoccus','Marvinbryantia','Blautia'
                        ,'Intestinimonas','Peptoniphilus','Roseburia','Flavonifractor','Ruminococcus','Dialister',"T")

colnames(data_family)<-c('id','Group','Sex','Age','Height','Weight','BMI','Diet','Season','Ruminococcaceae','Lachnospiraceae','Prevotellaceae','Bacteroidaceae','Coriobacteriaceae','Porphyromonadaceae','Erysipelotrichaceae',"T")


colnames(data_class)<-c('id','Group','Sex','Age','Height','Weight','BMI','Diet','Season','Clostridiales','Bacteroidales','Lactobacillales','Coriobacteriales','Pseudomonadales','Bacillales','Burkholderiales',"T")


colnames(data_order)<-c('id','Group','Sex','Age','Height','Weight','BMI','Diet','Season','Clostridia','Bacteroidia','Bacilli','Gammaproteobacteria','Actinobacteria','Flavobacteriia','Fusobacteriia',"T")

colnames(data_phylum)<-c('id','Group','Sex','Age','Height','Weight','BMI','Diet','Season','Firmicutes','Bacteroidetes','Proteobacteria','Actinobacteria','Tenericutes','Cyanobacteria','Fusobacteria',"T")

##分析
dg<-data_genus
df<-data_family
dc<-data_class
do<-data_order
dp<-data_phylum


#处理零数据，四个数据集重复
#df*0,05,dc[3,4,7]*0.01+dc[1,2]*0.05,do[3,4,5]*0.01+[2]*0.1+[1]*0.2,dp[2,3,4]*0.05+[1]*0.1,dg[567,11]+[4,12,13,10,8]*0.001+[1,2,9]*0.05
n<-0.05*datax$S
for (i in 1:dim(a)[1]) {
  for (j in 1:dim(df)[1]) {
    if(df[j,i+9]<=n){
      df[j,i+9]<-0
    }
 #   else if(df[j,i+9]>n){
 #     df[j,i+9]<-0
   # }
  }
}

ZIPI<-zeroinfl(df$Ruminococcaceae~Group+Sex+BMI+Diet,data=df,dist='poisson')
ZINBI<-zeroinfl(df$Ruminococcaceae~Group+Sex+BMI+Diet,data=df,dist='negbin')

#RESULT,group
#dg$-4
ZINBI0<-function(y0){
  cbind(coef(summary(zeroinfl(y0~Group+Sex+BMI+Season |1,data=dg,dist='negbin')))[[1]][2,c(1,4)],
        summary(zeroinfl(y0~Group+Sex+BMI+Season |1,data=dg,dist='negbin'))[[13]]#-log
        ,AIC(zeroinfl(y0~Group+Sex+BMI+Season |1,data=dg,dist='negbin'#aic
        )))
}
result0<-t(apply(dg[,genus_list],2,ZINBI0))
result0<-subset(result0,select = c(-4,-5))
#参数个数p=(aic-2*(-log))/2
p=(result0[,4]+2*result0[,3])/2
#BIC
BIC<-result0[,4]-2*p+p*log(dim(df)[1])
result0<-cbind(result0,BIC)

#df
ZINBI1<-function(y1){
  cbind(coef(summary(zeroinfl(y1~Group+Sex+BMI+Season |1,data=df,dist='negbin')))[[1]][2,c(1,4)],
        summary(zeroinfl(y1~Group+Sex+BMI+Season |1,data=df,dist='negbin'))[[13]]#-log
        ,AIC(zeroinfl(y1~Group+Sex+BMI+Season |1,data=df,dist='negbin'#aic
                      )))
}
result1<-t(apply(df[,family_list],2,ZINBI1))
result1<-subset(result1,select = c(-4,-5))
#参数个数p=(aic-2*(-log))/2
p=(result1[,4]+2*result1[,3])/2
#BIC
BIC<-result1[,4]-2*p+p*log(dim(df)[1])
result1<-cbind(result1,BIC)

#dc
ZINBI2<-function(y2){
  cbind(coef(summary(zeroinfl(y2~Group+Sex+BMI+Season |1,data=dc,dist='negbin')))[[1]][2,c(1,4)],
        summary(zeroinfl(y2~Group+Sex+BMI+Season |1,data=dc,dist='negbin'))[[13]]#-log
        ,AIC(zeroinfl(y2~Group+Sex+BMI+Season |1,data=dc,dist='negbin'#aic
        )))
}
result2<-t(apply(dc[,class_list],2,ZINBI2))
result2<-subset(result2,select = c(-4,-5))
#参数个数p=(aic-2*(-log))/2
p=(result2[,4]+2*result2[,3])/2
#BIC
BIC<-result2[,4]-2*p+p*log(dim(df)[1])
result2<-cbind(result2,BIC)

#do
ZINBI3<-function(y3){
  cbind(coef(summary(zeroinfl(y3~Group+Sex+BMI+Season |1,data=do,dist='negbin')))[[1]][2,c(1,4)],
        summary(zeroinfl(y3~Group+Sex+BMI+Season |1,data=do,dist='negbin'))[[13]]#-log
        ,AIC(zeroinfl(y3~Group+Sex+BMI+Season |1,data=do,dist='negbin'#aic
        )))
}
result3<-t(apply(do[,order_list],2,ZINBI3))
result3<-subset(result3,select = c(-4,-5))
#参数个数p=(aic-2*(-log))/2
p=(result3[,4]+2*result3[,3])/2
#BIC
BIC<-result3[,4]-2*p+p*log(dim(df)[1])
result3<-cbind(result3,BIC)

#dp
ZINBI4<-function(y4){
  cbind(coef(summary(zeroinfl(y4~Group+Sex+BMI+Season |1,data=dp,dist='negbin')))[[1]][2,c(1,4)],
        summary(zeroinfl(y4~Group+Sex+BMI+Season |1,data=dp,dist='negbin'))[[13]]#-log
        ,AIC(zeroinfl(y4~Group+Sex+BMI+Season |1,data=dp,dist='negbin'#aic
        )))
}
result4<-t(apply(dp[,phylum_list],2,ZINBI4))
result4<-subset(result4,select = c(-4,-5))
#参数个数p=(aic-2*(-log))/2
p=(result4[,4]+2*result4[,3])/2
#BIC
BIC<-result4[,4]-2*p+p*log(dim(dp)[1])
result4<-cbind(result4,BIC)


#RESULT,serve
#dg
ZINBI01<-function(y01){
  coef(summary(zeroinfl(y01~Group+Sex+BMI+Season|1,data=dg,dist='negbin')))[[1]][5,c(1,4)]
}
result01<-t(apply(dg[,genus_list],2,ZINBI01))
result01[,2]<-result01[,2]/3

#df
ZINBI11<-function(y11){
  coef(summary(zeroinfl(y11~Group+Sex+BMI+Season|1,data=df,dist='negbin')))[[1]][5,c(1,4)]
}

result11<-t(apply(df[,family_list],2,ZINBI11))
result11[,2]<-result11[,2]/10

#dc
ZINBI21<-function(y21){
  coef(summary(zeroinfl(y21~Group+Sex+BMI+Season|1,data=dc,dist='negbin')))[[1]][5,c(1,4)]
}
result21<-t(apply(dc[,class_list],2,ZINBI21))
result21[,2]<-result21[,2]/20
#do
ZINBI31<-function(y31){
  coef(summary(zeroinfl(y31~Group+Sex+BMI+Season|1,data=do,dist='negbin')))[[1]][5,c(1,4)]
}
result31<-t(apply(do[,order_list],2,ZINBI31))
result31[,2]<-result31[,2]/17
#dp
ZINBI41<-function(y41){
  coef(summary(zeroinfl(y41~Group+Sex+BMI+Season|1,data=dp,dist='negbin')))[[1]][5,c(1,4)]
}
result41<-t(apply(dp[,phylum_list],2,ZINBI41))
result41[,2]<-result41[,2]/12

library(ggplot2)
library(gcookbook)
library(dplyr)
library(cowplot)
library(showtext)
##画图
#group
#对p值进行负对数变换
log_p<-function(x){
  -log(x)
}

G_Genus<-data.frame(genus_list,log_p(result0[,2]))
Genus_figure<-ggplot(G_Genus,aes(y=reorder(G_Genus$genus_list,G_Genus$log_p.result0...2..),x=G_Genus$log_p.result0...2..))+geom_point(colour='red')+xlab('-log(p)')+ylab('Genus')+ theme_bw()+theme(axis.title.x=element_blank())
G_Fam<-data.frame(family_list,log_p(result1[,2]))
Fam_figure<-ggplot(G_Fam,aes(y=reorder(G_Fam$family_list,G_Fam$log_p.result1...2..),x=G_Fam$log_p.result1...2..))+geom_point(colour='blue')+xlab('-log(p)')+ylab('Family')+ theme_bw()+theme(axis.title.x=element_blank())

G_Class<-data.frame(class_list,log_p(result2[,2]))
Class_figure<-ggplot(G_Class,aes(y=reorder(G_Class$class_list,G_Class$log_p.result2...2..),x=G_Class$log_p.result2...2..))+geom_point(colour='orange')+xlab('-log(p)')+ylab('Class')+ theme_bw()+theme(axis.title.x=element_blank())

G_Order<-data.frame(order_list,log_p(result3[,2]))
Order_figure<-ggplot(G_Order,aes(y=reorder(G_Order$order_list,G_Order$log_p.result3...2..),x=G_Order$log_p.result3...2..))+geom_point(colour='green')+xlab('-log(p)')+ylab('Order')+ theme_bw()+theme(axis.title.x=element_blank())

G_Phylum<-data.frame(phylum_list,log_p(result4[,2]))
Phylum_figure<-ggplot(G_Phylum,aes(y=reorder(G_Phylum$phylum_list,G_Phylum$log_p.result4...2..),x=G_Phylum$log_p.result4...2..))+geom_point(colour='black')+xlab('-log(p)')+ylab('Phylum')+ theme_bw()+theme(axis.title.x=element_blank())
Group_Figure<-ggdraw()+draw_plot(Fam_figure,0,0,0.5,0.5)+draw_plot(Class_figure,0.5,0,0.5,0.5)+draw_plot(Order_figure,0.5,0.5,0.5,0.5)+draw_plot(Phylum_figure,0,0.5,0.5,0.5)+ theme_bw()+theme(axis.title.x=element_blank())

#FAT
#对p值进行负对数变换
log_p1<-function(x){
  sqrt(-log(x))
}
F_Genus<-data.frame(genus_list,log_p1(result01[,2]))
Genus_figure<-ggplot(F_Genus,aes(y=reorder(F_Genus$genus_list,F_Genus$log_p1.result01...2..),x=F_Genus$log_p1.result01...2..))+geom_point(colour='red')+xlab('-log(p)')+ylab('Genus')+ theme_bw()+theme(axis.title.x=element_blank())

F_Fam<-data.frame(family_list,log_p(result11[,2]))
Fam_figure<-ggplot(F_Fam,aes(y=reorder(F_Fam$family_list,F_Fam$log_p.result11...2..),x=F_Fam$log_p.result11...2..))+geom_point(colour='blue')+xlab('-log(p)')+ylab('Family')+ theme_bw()+theme(axis.title.x=element_blank())

F_Class<-data.frame(class_list,log_p(result21[,2]))
Class_figure<-ggplot(F_Class,aes(y=reorder(F_Class$class_list,F_Class$log_p.result21...2..),x=F_Class$log_p.result21...2..))+geom_point(colour='orange')+xlab('-log(p)')+ylab('Class')+ theme_bw()+theme(axis.title.x=element_blank())

F_Order<-data.frame(order_list,log_p(result31[,2]))
Order_figure<-ggplot(F_Order,aes(y=reorder(F_Order$order_list,F_Order$log_p.result31...2..),x=F_Order$log_p.result31...2..))+geom_point(colour='green')+xlab('-log(p)')+ylab('Order')+ theme_bw()+theme(axis.title.x=element_blank())

F_Phylum<-data.frame(phylum_list,log_p(result41[,2]))
Phylum_figure<-ggplot(F_Phylum,aes(y=reorder(F_Phylum$phylum_list,F_Phylum$log_p.result41...2..),x=F_Phylum$log_p.result41...2..))+geom_point(colour='black')+xlab('-log(p)')+ylab('Phylum')+ theme_bw()+theme(axis.title.x=element_blank())

Fat_Figure<-ggdraw()+draw_plot(Fam_figure,0,0,0.5,0.5)+draw_plot(Class_figure,0.5,0,0.5,0.5)+draw_plot(Order_figure,0.5,0.5,0.5,0.5)+draw_plot(Phylum_figure,0,0.5,0.5,0.5)+ theme_bw()+theme(axis.title.x=element_blank())

