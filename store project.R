

s_train=read.csv("D:\\PROJECT\\store_train.csv",stringsAsFactors = FALSE)

s_test= read.csv("D:\\PROJECT\\store_test.csv",stringsAsFactors = FALSE)

head(s_train)

head(s_test)

dim(s_train)

dim(s_test)

library(dplyr)

glimpse(s_train)

glimpse(s_test)

#clean data----------------------------------------------

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  
  for(cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
    
  }
  data[,var]=NULL
  return(data)
}


s_test$store=NA

s_train$data="train"

s_test$data="test"

rg=rbind(s_train,s_test)

glimpse(rg)

dim(rg)

table(rg$state)

table(rg$country)

table(rg$CouSub)

unique(rg$state)

unique(rg$CouSub)

unique(rg$countyname)

unique(rg$Areaname)

unique(rg$countytownname)

unique(rg$population)

unique(rg$state_alpha)

unique(rg$store_Type)

#-----------------------------------------------
 table(rg$storecode)

unique(rg$storecode)

rg$storecode1 <-(substr(rg$storecode,1,5))

names(rg)

glimpse(rg)

#-------------------------------

lapply(rg,function(x) length (unique(x)))

names(rg)[sapply(rg,function(x) is.character(x))]

#----------------------------------------------------

table(rg$storecode)

sort(table(rg$storecode),decreasing=TRUE)

table(rg$storecode1)

unique(rg$storecode1)



#------------------------------------------------

rg=rg %>% 
   select(-Id, -storecode)


glimpse(rg)

#--------------------------------------------------

cat_cols=c("countyname","Areaname", 
           "countytownname","state_alpha","store_Type","storecode1")

for (cat in cat_cols){
 
  rg= CreateDummies(rg,cat,100)
  
}
  glimpse(rg)
#------------------------------

sum(sapply(rg,function(x) is.character(x)))
  
table(rg$store)

unique(rg$store)

nrow(rg)

sum(sapply(rg,function(x) sum(is.na(x))))


lapply(rg,function(x)sum(is.na(x)))

#------------------------------------------------------------

for(col in names (rg)){
  
if(sum(is.na(rg[,col] > 0 & !(col %in% c("data","store"))))){
  
  rg[is.na(rg[,col]),col]= mean(rg[rg$data=="train", col],na.rm=TRUE)
}
  
}

lapply(rg,function(x)sum(is.na(x)))

#---------------------------------------------

s_train=rg %>%  filter (data=="train") %>% select(-data)

s_test=rg %>%  filter (data=="test") %>% select(-data,-store)

#-------------------------------------------------------------------

set.seed(2)

s=sample(1:nrow(s_train),0.7*nrow(s_train))

s_train1=s_train[s,]

s_train2=s_train[-s,]

#-------------------------------------------------------
nrow(s_train1) #  will use for modeling

nrow(s_train2)  # will use for validation

#-----------------------------------------------------

library(car)

for_vif = lm(store ~ .,  data = s_train1)

sort(vif(for_vif),decreasing=T)


for_vif = lm(store ~ .  -sales0, data= s_train1 )

sort(vif(for_vif),decreasing=T)

for_vif = lm(store ~ .  -sales0 - sales2, data= s_train1 )

sort(vif(for_vif),decreasing=T)[1:5]

for_vif = lm(store ~ .  -sales0 - sales2 -sales3, data= s_train1 )

sort(vif(for_vif),decreasing=T)[1:5]

for_vif = lm(store ~ .  -sales0 - sales2 -sales3 -sales1, data= s_train1 )

sort(vif(for_vif),decreasing=T)[1:4]

#------------------------------------------------

names(for_vif$coefficients)

formulaName <- names(for_vif$coefficients) [2:length(for_vif$coefficients)]

form <- as.formula(paste0("store ~ " , paste(formulaName, collapse = " + ")))


#-----------------------------------------

log_fit= glm(form,data=s_train1,family="binomial")


log_fit=step(log_fit)

formula(log_fit)

log_fit=glm(store ~ state_alpha_CT + storecode1_NCNTY, data=s_train,family="binomial" )


summary(log_fit)



#------------------------------------------------------
# remove variable having lowest * marks or no star or . one by one 


log_fit=glm( store ~ state_alpha_CT + storecode1_NCNTY,
             data=s_train,family="binomial" )


summary(log_fit)


log_fit=glm( store ~  storecode1_NCNTY,
             data=s_train,family="binomial" )



#--------------------------------------------------------------
val.score_train1=predict(log_fit,newdata = s_train1,type="response")

max(val.score_train1)

min(val.score_train1)

#---------------------------------------------------

val.score =predict(log_fit,newdata = s_train2,type="response")

max(val.score)

min(val.score)


#-----------------------------------------------------------------------

install.packages('pROC')

library(pROC)

auc(roc(s_train1$store,val.score_train1))

auc(roc(s_train2$store,val.score))

#----------------------------------------------

for_vif=lm(store ~ storecode1_NCNTY, data=s_train)


log.fit.final=glm(store ~  storecode1_NCNTY,
                     data=s_train,family="binomial")

log.fit.final=step(log.fit.final)

summary(log.fit.final)

log.fit.final=glm(store ~  storecode1_NCNTY, family = "binomial", data = s_train)




summary(log.fit.final)

#----------------------------------------------------------

score_s_train <- predict(log.fit.final,newdata = s_train,type= "response")

auc(roc(s_train$store,score_s_train))

train.prob.score = predict(log.fit.final,s_train,type = "response")

test.prob.score = predict(log.fit.final,newdata=s_test, type = "response")

#------------------------------------------------

write.csv(test.prob.score,"store project.csv", row.names=F)





































