#Freshwater Movement Comparison

# Load Packages -----------------------------------------------------------
library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(oce)
library(segmented)
library(zoo)
library(mixtools)
library(randomForest)
library(here)
library(caret)
library(tidyverse)



#Theme
Theme = theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title=element_text(size=20),axis.text=element_text(size=18))



# Visualise Eye Data ------------------------------------------------------

EData = read.csv("Data Outputs/alleyedata.csv") 

#Core size
Core = subset(EData, tag=="CORE")


#Subset to most recent growth and remove non paired samples
Lakes = subset(EData, tag =="JELLY" &Lake != "HEGA" & Size == "Juv"&agency_id!="LT21_3074"&agency_id!="ST21_2055"&agency_id!="ST21_2062"&agency_id!="ST21_2260"&agency_id!="ST21_2280")
range(na.omit(Lakes$diameter_um))
Lakes = Lakes[,c(6,7,8,12,24,22)]
str(Lakes)

library(data.table)
LakesL <- melt(setDT(Lakes), id.vars = c("agency_id", "Lake", "Size"), variable.name = "Element")


#Visualise differences in stable isootpes between lakes
Lakeslt = subset(Lakes, Lake == "LT")
Lakesst = subset(Lakes, Lake == "ST")
Lakes = mutate(Lakes, Lake = recode(Lake,ST = "Storvatnet", LT = "Litjvatnet"))
#Individual Plots
B1 = ggplot(Lakes, aes(x=Lake, y=d13C))+geom_boxplot(outliers = FALSE)+geom_jitter(width=0.04,size=2,alpha=0.5)+theme_bw()+Theme+xlab("Lake of capture")+
  ylab(expression(paste("Lens ",delta^13, "C")))

wilcox.test(Lakeslt$d13C, Lakesst$d13C)

B2 = ggplot(Lakes, aes(x=Lake, y=d15N))+geom_boxplot(outliers = FALSE)+geom_jitter(width=0.04,size=2,alpha=0.5)+theme_bw()+Theme+xlab("")+
ylab(expression(paste("Lens ",delta^15, "N")))
wilcox.test(Lakeslt$d15N, Lakesst$d15N)

B3 = ggplot(Lakes, aes(x=Lake, y=d34S))+geom_boxplot(outliers = FALSE)+geom_jitter(width=0.04,size=2,alpha=0.5)+theme_bw()+Theme+xlab("")+
  ylab(expression(paste("Lens ",delta^34, "S")))
wilcox.test(Lakeslt$d34S, Lakesst$d34S)

library(cowplot)
F4 = plot_grid(B2,B1,B3,nrow=1,labels=c("(a)","(b)","(c)"))

## Fig. S3 -------------------------------------------------------------------------
ggsave("Plots/Fig. S3.tiff", plot = F4, width = 11, height = 5.5, units = "in", dpi = 300)






# Visualise Otoliths ------------------------------------------------------
OData = read.csv("Data Outputs/processedotodata2_DS.csv")
OLakes= subset(OData, Lake != "HEGA")


#Check profiles
df = OLakes %>% pivot_longer(cols = c(14:22), names_to = "Element", values_to = "Conc_ppm") %>%
  filter(!Element=="Ca43_ppm")

ggplot(filter(df), aes(Distance_um, Conc_ppm, group=agency_id)) +
  geom_line(alpha=.5, size=.8)+
  facet_wrap(~Element, scales = "free")


#Subset to juveniles
OLakes= subset(OData, Lake != "HEGA"&Size=="Juv"&agency_id!="ST21_2230")

#Subset to last 10 values for edge signal
OLakes2=OLakes%>%
  group_by(agency_id)%>%
  slice_tail(n=10)
colnames(OLakes2)
#Select one row with a mean value of these 10
OLakes2 = OLakes2%>%
  dplyr::select(c(agency_id,Size,Lake,Na23_ppm,Mg24_ppm,P31_ppm,Cr52_ppm,Mn55_ppm,Zn66_ppm,Sr88_ppm,Ba138_ppm))#Remove calcium as fixed

OLakes2 <- OLakes2 %>%
  group_by(agency_id) %>%
  summarise(Size=first(Size), Lake=first(Lake),across(c(Na23_ppm, Mg24_ppm, P31_ppm,
                                                        Cr52_ppm, Mn55_ppm, Zn66_ppm,
                                                        Sr88_ppm, Ba138_ppm),
                                                      \(x) mean(x, na.rm = TRUE)))

OLakes2%>%
  group_by(Lake)%>%
  summarise(median(Mn55_ppm))
#Log
cols_to_log <- c("Na23_ppm", "Mg24_ppm", "P31_ppm", "Cr52_ppm", 
                 "Mn55_ppm", "Zn66_ppm", "Sr88_ppm", "Ba138_ppm")

for (col in cols_to_log) {
  OLakes2[[paste0(col, "_log")]] <- log10(OLakes2[[col]])
}

OLakes2 <- OLakes2[ , !(names(OLakes2) %in% cols_to_log)]


#Turn into long format
library(data.table)
LakesL <- melt(setDT(OLakes2), id.vars = c("agency_id", "Lake", "Size"), variable.name = "Element")
LakesL=subset(LakesL, Element != ("Li7")& Element !=  ("Ca43"))
LakesL = mutate(LakesL, Lake = recode(Lake, ST = "Storvatnet", LT = "Litjvatnet"))

## S4 - Trace Element Plots -------------------------------------------------------------------------
S4 = ggplot(LakesL, aes(x=Lake, y=(value)))+geom_boxplot(outliers = FALSE)+geom_jitter(width=0.04,size=2,alpha=0.5)+theme_classic()+Theme+
  facet_wrap(~Element, scales = "free",labeller = labeller(Element = function(x) gsub("_ppm_log", "", x)))+
  xlab("Lake of capture")+ylab("log Otolith trace\nelement conentration (ppm)")+theme(strip.text = element_text(size = 16))
ggsave("Plots/Fig. S4.tiff", plot = S4, width = 16, height = 12, units = "in", dpi = 500)


#P values for differences
table=NULL
for(i in unique(LakesL$Element)){
  LT=subset(LakesL, Element ==i&Lake=="Litjvatnet")
  ST=subset(LakesL, Element ==i&Lake=="Storvatnet")
  test=wilcox.test((LT$value), (ST$value))
  table=rbind(table,data.frame(unique(LT$Element),test$statistic,test$p.value))
}
#See Mn55 Sig different but no other elements






# Random Forest Training/Testing ------------------------------------------
### Eye Lense RF ------------------------------------------------------------
#Prep Data for RF
RDFD2 = merge(Lakes, OLakes2[,-c(2,3)], by = c("agency_id"))
#RDFD2 = merge(Lakes, OLakes2[,-c(1,2)], by = c("agency_id"),all=TRUE)
RDFD = na.omit(RDFD2)
RDFD$Lake = as.factor(RDFD$Lake)
#RDFD=RDFD2


#Summary data on all fiish
LW = subset(EData, agency_id %in% RDFD$agency_id)
LW=LW%>%
  dplyr::select(c(length_mm, Lake))
LW=unique(LW)
LW%>%
  group_by(Lake)%>%
  summarise(mean(length_mm),range(length_mm))





##Eye Data
data_for_model = RDFD[,c(2,3,4,6)]
data_for_model = mutate(data_for_model, Lake = recode(Lake,Storvatnet="ST", Litjvatnet="LT"))

set.seed(1999)
# Split the data in training and test data stratified by Natal Location 
trainIndex <- createDataPartition(data_for_model$Lake, p = 1, 
                                  list = FALSE, 
                                  times = 1)

data_train <- data_for_model[ trainIndex,]
data_test  <- data_for_model[-trainIndex,]

#How many samples do we have per NAT_LOC in our training data?
count(data_train, Lake)
#How many samples do we have per NAT_LOC in our test data?
count(data_test, Lake)

#Set settings
tree_number=1000
metric_setting="Accuracy"

set.seed(1999)
RF_orig_training <- train(Lake ~ .,
                          data = data_train, method = "rf",
                          metric = metric_setting,
                          #trControl = control,
                          # tuneGrid=tunegrid,
                          ntree = tree_number,
                          strata=data_train$Lake,
                          replace=TRUE,
                          # sampsize=sample_size,
                          importance=TRUE
)


RF_orig_training

pred.RF_orig_training_orig_test <- predict(RF_orig_training, data_test)
pred.RF_orig_training_orig_test
RF_orig_training_orig_test_conf <- confusionMatrix(pred.RF_orig_training_orig_test, data_test$Lake)
RF_orig_training_orig_test_conf 

#63% accuracy, kappa 0.154 OOB
#63% accuracy, 0.25 kappa on test data

#See factors
varImp(RF_orig_training)
VarI = varImp(RF_orig_training)
VarI = VarI$importance
VarI$Factor <- as.character(rownames(VarI))

library(forcats)

#Variable importance for SI
VarI1 = VarI %>%
  mutate(
    # Remove "_ppm_log"
    Factor = gsub("_ppm_log", "", Factor),
    
    # Replace starting "d" with "δ"
    Factor = gsub("^d", "δ", Factor),
    
    # Reorder by Litjvatnet
    Factor = fct_reorder(Factor, LT)
  ) %>%
  ggplot(aes(x = Factor, y = LT)) +
  coord_flip() +
  ylab("Importance") +
  xlab("Tracer") +
  theme_classic() + Theme +
  geom_segment(aes(x = Factor, xend = Factor, y = 0, yend = LT),
               color = "gray", alpha = 0.5, linewidth = 1) +
  geom_point(size = 3)+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())





## Otolith RF --------------------------------------------------------------
OLakes4 = RDFD[,-c(1,2,3,4,5)]


#Prep
OLakes4=na.omit(OLakes4)
OLakes4 = mutate(OLakes4, Lake = recode(Lake,Storvatnet="ST", Litjvatnet="LT"))

set.seed(1999)
#RF Model
trainIndex <- createDataPartition(OLakes4$Lake, p = 1, #Vary p to .07 to see other splits
                                  list = FALSE, 
                                  times = 1)

data_train <- OLakes4[ trainIndex,]
data_test  <- OLakes4[-trainIndex,]
count(data_train, Lake)
#How many samples do we have per NAT_LOC in our test data?
count(data_test, Lake)

#Set settings
tree_number=1000
metric_setting="Accuracy"

set.seed(1999)
RF_orig_training <- train(Lake ~ .,
                          data = data_train, method = "rf",
                          metric = metric_setting,
                          #trControl = control,
                          # tuneGrid=tunegrid,
                          ntree = tree_number,
                          strata=data_train$Lake,
                          replace=TRUE,
                          # sampsize=sample_size,
                          importance=TRUE
)


RF_orig_training 

#Best model
RF_Best=RF_orig_training
#Test on test data
pred.RF_orig_training_orig_test <- predict(RF_orig_training, data_test)
pred.RF_orig_training_orig_test

RF_orig_training_orig_test_conf <- confusionMatrix(pred.RF_orig_training_orig_test, data_test$Lake)
RF_orig_training_orig_test_conf 

#Normal 88% kappa 0.71 on test data#
#73% accuracy, Kappa 0.34 OOB
varImp(RF_orig_training)
VarI = varImp(RF_orig_training)
VarI = VarI$importance
VarI$Factor <- as.character(rownames(VarI))

#Variable importance for SI
VarI2 = VarI %>%
  mutate(
    # Remove "_ppm_log"
    Factor = gsub("_ppm_log", "", Factor),
    
    # Replace starting "d" with "δ"
    Factor = gsub("^d", "δ", Factor),
    
    # Reorder by Litjvatnet
    Factor = fct_reorder(Factor, LT)
  ) %>%
  ggplot(aes(x = Factor, y = LT)) +
  coord_flip() +
  ylab("Importance") +
  xlab("Tracer") +
  theme_classic() + Theme +
  geom_segment(aes(x = Factor, xend = Factor, y = 0, yend = LT),
               color = "gray", alpha = 0.5, linewidth = 1) +
  geom_point(size = 3)+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())




## Combined RF -------------------------------------------------------------------------

data_for_model = RDFD[,-c(1,5)]
data_for_model=na.omit(data_for_model)


set.seed(1999)
# Split the data in training and test data stratified by Natal Location 
trainIndex <- createDataPartition(data_for_model$Lake, p = 1, 
                                  list = FALSE, 
                                  times = 1)

data_train <- data_for_model[ trainIndex,]
data_test  <- data_for_model[-trainIndex,]

#How many samples do we have per NAT_LOC in our training data?
count(data_train, Lake)
#How many samples do we have per NAT_LOC in our test data?
count(data_test, Lake)

#Set settings

tree_number=1000
metric_setting="Accuracy"

RF_orig_training <- train(Lake ~ .,
                          data = data_train, method = "rf",
                          metric = metric_setting,
                          #trControl = control,
                          # tuneGrid=tunegrid,
                          ntree = tree_number,
                          strata=data_train$Lake,
                          replace=TRUE,
                          # sampsize=sample_size,
                          importance=TRUE
)


(RF_orig_training)


#Test on test data
pred.RF_orig_training_orig_test <- predict(RF_orig_training,data_test)
pred.RF_orig_training_orig_test

RF_orig_training_orig_test_conf <- confusionMatrix(pred.RF_orig_training_orig_test, data_test$Lake)
RF_orig_training_orig_test_conf 

#66% accuracy and 0.23 kappa OOB
#75% accuracy and 0.385 kappa on 70%




#Visualise variable importance
varImp(RF_orig_training)
plot(varImp(RF_orig_training))
VarI = varImp(RF_orig_training)
VarI = VarI$importance
VarI$Factor <- as.character(rownames(VarI))



#Plot variable importance -------------------------------------------------------------------------

library(forcats)
# Reorder following the value of another column:
VarI3 = VarI %>%
  mutate(
    # Remove "_ppm_log"
    Factor = gsub("_ppm_log", "", Factor),
    
    # Replace starting "d" with "δ"
    Factor = gsub("^d", "δ", Factor),
    
    # Reorder by Litjvatnet
    Factor = fct_reorder(Factor, Litjvatnet)
  ) %>%
  ggplot(aes(x = Factor, y = Litjvatnet)) +
  coord_flip() +
  ylab("Importance") +
  xlab("Tracer") +
  theme_classic() + Theme +
  geom_segment(aes(x = Factor, xend = Factor, y = 0, yend = Litjvatnet),
               color = "gray", alpha = 0.5, linewidth = 1) +
  geom_point(size = 3)+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())




# #Fig. S5 Var importance ----------------------------------------------------------------
S3 = plot_grid(VarI1, VarI2,VarI3,nrow=3,labels=c("(a)","(b)","(c)"),align="v")
ggsave("Plots/Fig. S5.tiff", plot = S3, width = 5, height = 9, units = "in", dpi = 400)




# Assign Adults Using the Best Model --------------------------------------

#Where to cut for adults
#Otoliths
OLakes5=OLakes%>%
  group_by(agency_id)%>%
  slice_tail(n=10)
colnames(OLakes2)
mean(na.omit(OLakes5$Distance_um))
#Select
Adults = subset(OData, Size == "AD")
Adults=Adults[!is.na(Adults$Distance_um),]
Adults2 = Adults %>%group_by(agency_id)%>%filter(abs(Distance_um-719)==min(abs(Distance_um-719)))
Adults2 = Adults %>%
  group_by(agency_id) %>%
  arrange(abs(Distance_um - 719)) %>%
  slice_head(n = 10) %>%
  ungroup()
#Meta data
Adults2%>%
  group_by(Lake)%>%
  summarise(range(length_mm),mean(length_mm))
  

#Summarise
Adults2 <- Adults2 %>%
  group_by(agency_id) %>%
  summarise(Size=first(Size), Lake=first(Lake),Distance_um=first(Distance_um),across(c(Na23_ppm, Mg24_ppm, P31_ppm,
                                                                                       Cr52_ppm, Mn55_ppm, Zn66_ppm,
                                                                                       Sr88_ppm, Ba138_ppm),
                                                                                     \(x) mean(x, na.rm = TRUE)))

#Extract range and mean


Adults3 = Adults2%>%dplyr::select(c(agency_id,Na23_ppm,Mg24_ppm,P31_ppm,Cr52_ppm,Mn55_ppm,Zn66_ppm,Sr88_ppm,Ba138_ppm))

#Log
for (col in cols_to_log) {
  Adults3[[paste0(col, "_log")]] <- log10(Adults3[[col]])
}

Adults3 <- Adults3[ , !(names(Adults3) %in% cols_to_log)]


## 100 Loops with confidence intervals to predict Adult rearing hab --------

results = NULL
for(i in 1:100){
  #data_for_model = RDFD[,c(2,3,4,6)]
  data_for_model = RDFD[,-c(1,2,3,4,5)]
  data_for_model = mutate(data_for_model, Lake = recode(Lake,Storvatnet="ST", Litjvatnet="LT"))
  # Split the data in training and test data stratified by Natal Location 
  trainIndex <- createDataPartition(data_for_model$Lake, p = .7, 
                                    list = FALSE, 
                                    times = 1)
  
  data_train <- data_for_model[ trainIndex,]
  data_test  <- data_for_model[-trainIndex,]
  #How many samples do we have per NAT_LOC in our training data?
  count(data_train, Lake)
  #How many samples do we have per NAT_LOC in our test data?
  count(data_test, Lake)
  #Set settings
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  tunegrid <- expand.grid(.mtry=c(2:ncol(data_train)-1))
  tree_number=1000
  metric_setting="Accuracy"
  
  RF_orig_training <- train(Lake ~ .,
                            data = data_train, method = "rf",
                            metric = metric_setting,
                            #trControl = control,
                            # tuneGrid=tunegrid,
                            ntree = tree_number,
                            strata=data_train$Lake,
                            replace=TRUE,
                            # sampsize=sample_size,
                            importance=TRUE
  )
  
  
  RF_orig_training
  
  pred.RF_orig_training_orig_test = predict(RF_orig_training, Adults3,type="prob")
  pred.RF_orig_training_orig_test$agency_id=Adults3$agency_id
  results = rbind(results, pred.RF_orig_training_orig_test)
}

mean(results$LT)
mean(results$ST)
sd(results$ST)


#Prep all this for plots with mean, SD and 95% confidence intervals
results=mutate(results, Rearing=ifelse(LT>ST, "LT","ST"))

comb = NULL
i="ST21_2006"
for(i in unique(results$agency_id)){
  data = subset(results, agency_id==i)
  mLT = mean(data$LT)
  mST=mean(data$ST)
  sd=sd(data$LT)
  data2 = cbind(mLT, mST,sd,agency_id=unique(data$agency_id))
  comb=rbind(comb,data2)
}
comb =as.data.frame(comb)
comb=mutate(comb, Rearing = ifelse(mLT>mST, "LT","ST"))
comb$mLT=as.numeric(comb$mLT)
comb$mST=as.numeric(comb$mST)
comb$sd=as.numeric(comb$sd)
#Vis
comb$y_lower=comb$mLT-1.96*comb$sd
comb$y_upper=comb$mLT+1.96*comb$sd
comb$plot="plot"
comb=mutate(comb, plot = ifelse(Rearing=="LT", "LT","ST"))
#Characterize as high and low prediction certainty
comb=comb%>% mutate(Pred = ifelse(Rearing=="LT"&y_lower>0.5, "LT",NA))%>%
  mutate(Pred = ifelse(Rearing=="ST"&y_upper<0.5, "ST",Pred))%>%
  mutate(Pred = ifelse(Rearing=="ST"&y_upper>0.5, "ST",Pred))%>%
  mutate(Pred = ifelse(Rearing=="LT"&y_lower<0.5, "LT",Pred))%>%
  mutate(Conf = ifelse(Rearing=="LT"&y_lower>0.5, "High",NA))%>%
  mutate(Conf = ifelse(Rearing=="ST"&y_upper<0.5, "High",Conf))%>%
  mutate(Conf = ifelse(Rearing=="ST"&y_upper>0.5, "Low",Conf))%>%
  mutate(Conf = ifelse(Rearing=="LT"&y_lower<0.5, "Low",Conf))

results=results[,-c(5,6,7,8,9)]


results$mLT=results$LT



# Create a new column with the first two characters from 'Name'
comb$CatchLoc<- substr(comb$agency_id, 1, 2)
comb=subset(comb, CatchLoc != "HG")
comb=mutate(comb, Change =ifelse(Rearing==CatchLoc, "Same","Change"))
comb=mutate(comb, y_lower=ifelse(y_lower<0, 0,y_lower))
comb=mutate(comb, y_upper=ifelse(y_upper>1, 1,y_upper))

#Visualisation
comb = mutate(comb, Pred = recode(Pred,ST = "Storvatnet", LT = "Litjvatnet"))
probp2 = ggplot(comb, aes(y=mLT, x=Pred,group=agency_id,color=Rearing))+geom_point(size=4,position=position_dodge(width=0.5))+geom_hline(yintercept = 0.5)+
  geom_errorbar(aes(ymin = y_lower, ymax = y_upper,), position=position_dodge(width=0.5),width = 0.3,size=1)+
  theme_minimal()+labs(y="Assignment probability\nto Litjvatnet",x="Estimated adult nursery habitat",color="Predicted rearing\nlocation")+
  theme_bw()+Theme+facet_wrap(~Conf)+theme(strip.text = element_text(size = 14))


## Fig. 5 Mean assignment probability to Litjvatnet  ------------------------------------------------------------------
#F5 = plot_grid(probp1,probp2,ncol=1,labels=c("(a)","(b)"))
ggsave("Plots/Fig. 4.tiff", width = 10, height = 6, units = "in", dpi = 300)


#Data
table(comb$Pred,comb$Conf)
6/24
4/24
18/24
7/24
11/24

table(comb$Change,comb$Conf)
7/9
