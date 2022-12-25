library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC)
library(ggplot2)
library(vip)
library(rpart.plot)
library(DALEXtra)
library(randomForest)

path1<-"C:\\Users\\xyz\\Desktop\\vinit\\R project Data\\"
path1

housing_test<-read.csv(paste0(path1,"housing_test.csv"),sep = ',')

housing_train<-read.csv(paste0(path1,"housing_train.csv"),sep = ',')

names(housing_train)
names(housing_test)

housing_test$Price=NA

housing_train$data='train'
housing_test$data='test'
housing_all=rbind(housing_train,housing_test)


glimpse(housing_all)


CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    2
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}


char_logical=sapply(housing_all,is.character)
cat_cols=names(housing_all)[char_logical]
cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','Price'))]
cat_cols

for(col in cat_cols){
  housing_all=CreateDummies(housing_all,col,50)
}


housing_all=housing_all[!(is.na(housing_all$Price) & housing_all$data=="train"),]


for(col in names(housing_all)){
  if(sum(is.na(housing_all[,col]))>0 & !(col %in% c("data","Price"))){
    housing_all[is.na(housing_all[,col]),col]=mean(housing_all[housing_all$data=='train',col],na.rm=T)
  }
}


housing_train=housing_all %>% filter(data=='train') %>% select(-data)
housing_test=housing_all %>% filter(data=='test') %>% select(-data,-Price)

any(is.na(housing_train))
any(is.na(housing_test))

fit = randomForest(Price~., data = housing_train)

test.prediction = predict(fit, newdata = housing_test)

write.csv(test.prediction,"Vinit_Pawar_P1_Part2.csv",row.names = F)
    
