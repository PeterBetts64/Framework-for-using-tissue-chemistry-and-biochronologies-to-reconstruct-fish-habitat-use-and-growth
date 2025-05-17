#Data Processing get into shape for all data viz and analysis with chemical aging, bringing in manual aging for 
#fish that could not be chemically aged

# Load Libraries ----------------------------------------------------------

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

# Age from images ---------------------------------------------------------

## Load Data ---------------------------------------------------------------

##Ears
Data = read.csv("Data Outputs/prepedotodata_DS.csv")

myFiles <- list.files("Raw Data/Chem Ages")
#Chem Age
results=NULL
for(i in levels(as.factor(unique(Data$agency_id)))){tryCatch({
  AgData = read.csv(paste0("Raw Data/Chem Ages/",i))
  AgData$agency_id = i
  AgData$Year = NA
  AgData$Age = nrow(AgData)
  for(t in c(1:nrow(AgData))){
    AgData[t,]$Year = t
  }
  results = rbind(results, AgData)}, error=function(e){})
}

CA_Data = rename(results, "Distance_um"=x, "Zn"= y)
#Get cumulative length
CA_Data2 = (CA_Data) %>%
  group_by(agency_id)%>%
  mutate(Increment = Distance_um-lag(Distance_um))
CA_Data2$Increment <- ifelse(is.na(CA_Data2$Increment), CA_Data2$Distance_um, CA_Data2$Increment)


###Now we do all of 0.5 again but using this new age instead

###Otoliths
Data = read.csv("Data Outputs/prepedotodata_DS.csv")
Data = mutate(Data, `Sr:Ca` = (Sr88_ppm/87.62)/((Ca43_ppm/40.078)/1000)) 
#OData = read.csv("C:/Users/peter/Box/TROUT_NORWAY/Data/Norway Data Analysis/Data Outputs/allotodata.csv")
###Determine First sea entry of adults
#Subset to post sea migration of seward migrants

Data1 = mutate(Data, Hab = ifelse(`Sr:Ca`>=2.42&Distance_um>=250, "Sea", "FW"))
Anad = subset(Data1, Hab =="Sea"&Size =="AD")
Mig = Anad%>%
  group_by(agency_id)%>%
  summarise(DS = Distance_um[1]) #This gives us distance on oto of first migration 

Data = mutate(Data, Habitat = ifelse(`Sr:Ca` > 2.42, "Sea","Fresh"))






###Age the fish from chemistry
Grow2 = CA_Data2
#Merge all Data
end = merge(Data, Grow2, by="agency_id",allow.cartesian=TRUE)
#Check no lost ID
length(table(end$agency_id))
length(table(Grow2$agency_id))
#Now remove all non matching pairs to end up with one data set (Fuzzy merge)
end$Dif=(end$Distance_um.x-end$Distance_um.y)
end2=subset(end, Dif >-4)
end2=subset(end2, Dif <4)
#Create data set with growth data and Habitat at that point sample 
res <- end %>%
  group_by(agency_id, Year) %>% 
  slice(which.min(abs(Distance_um.x - Distance_um.y)))


#Check if in that growth period they were at sea
Results = NULL
for(i in levels(as.factor(res$agency_id))) {
  LData<- subset(res, agency_id ==i)
  Data2 = subset(Data, agency_id ==i)
  LData$Distance2 = lag(LData$Distance_um.y)
  
  for(k in c(1:nrow(LData))){
    Data3=subset(Data2, Distance_um <= LData$Distance_um.y[k] & Distance_um >= LData$Distance2[k])
    
    Results=rbind(Results,any(na.omit(Data3$Habitat =="Sea")))
  }}
res = res%>%
  arrange(agency_id)
res$Hab = Results


res = mutate(res, HabI = ifelse(Hab ==TRUE, "Sea", "Fresh"))



# Merge with laser data ---------------------------------------------------

res1 = res%>% dplyr::select(agency_id, Distance_um.x, Year, Age,HabI,Increment) %>% rename(Distance_um = Distance_um.x)
Data = merge(Data, res1, by=c("agency_id","Distance_um"), all.x = TRUE)

###Now we interpolate age for each individual first we need to calculate the extra growth to be able to interpolate up to the capture time
age <- read_excel("Raw Data/Main Data_norway_trout_overview_samples.xlsx", 
                  sheet = "Overview R", col_types = c("text", 
                                                      "date", "numeric", "text", "text", 
                                                      "text"))

#Calculate extra growth and interpolate...the code is at the end of the 17 Age Interpolation file but I have already done alot of thta loop previous so only last few lines relevant
age$month <- format(age$date_caught, "%m")
age$Fage = (as.numeric(age$month)-3)/12
age$agency_id = sub("-", "_", age$agency_id)

results = NULL
#This will merge the age data with the main data and interpolate between ages including a projecion of age right up to edge
#However will only return fish with age data so we must extract and re merge
i ="HG21_004"
for(i in c(unique(Data$agency_id))){
  tryCatch({
    da = subset(Data, agency_id ==i)
    ag=subset(age, agency_id==i)
    ag1=as.numeric(ag[,8])
    n = length(da$Age)
    da2 = unique(na.omit(da$Age))
    da[n,27]=(ag1+da2)
    da[1,27]=0
    da$Year = zoo::na.approx(da$Year)
    results = rbind(results, da)
  }, error=function(e){})
}

AgeFish = unique(results[,1])
DataT = subset(Data, !(agency_id %in% AgeFish))

Data = rbind(results,DataT)

#Add length and otolith radius meta data
Data = merge(Data, age[,c(1,3)], by=c("agency_id"))
#Otp Radius
Orad = read_excel("Raw Data/Main Data_norway_trout_overview_samples.xlsx", 
                  sheet = "Oto Lengths")
Data = merge(Data[,-c(3)], Orad[,c(1,5)], by=c("agency_id"))


#####This Code calculates growth from the final year to the edge and sees if they were at sea (ONLY USE TO LOOK AT GROWTH PRE SEA)
AData = subset(Data, Size =="AD")
Norm = subset(AData, !(is.na(Increment)))
End = AData%>%
  group_by(agency_id)%>%
  arrange(Distance_um)%>%
  slice_tail(n=1)
#Find growth in this period
TG=Norm%>%
  group_by(agency_id)%>%
  summarise(Inc = sum(Increment))

TG2 = NULL
for(i in unique(End$agency_id)){
  End1 = subset(End, agency_id ==i)
  TG1 = subset(TG, agency_id ==i)
  TGn = as.numeric(TG1[2])
  End1 = mutate(End1, Increment = Distance_um - TGn)
  TG2 =rbind(TG2, End1)
}
Ends = rbind(Norm, TG2)
#Now we can check if the fish were at sea during this final period
Results = NULL
for(i in levels(as.factor(Ends$agency_id))) {
  LData<- subset(Ends, agency_id ==i)
  Data2 = subset(Data, agency_id ==i)
  LData$Distance2 = lag(LData$Distance_um)
  
  for(k in c(1:nrow(LData))){
    Data3=subset(Data2, Distance_um <= LData$Distance_um[k] & Distance_um >= LData$Distance2[k])
    Results=rbind(Results,any(na.omit(Data3$Habitat =="Sea")))
  }}
Ends = Ends%>%
  arrange(agency_id)
Ends$Hab = Results
res = mutate(Ends, HabI = ifelse(Hab==TRUE, "Sea", "Fresh"))
res1 = res%>% dplyr::select(agency_id, Distance_um,Year, Age,HabI,Increment)
Data =Data%>%dplyr::select(-c(Age,HabI,Increment))
Data = merge(Data, res1, by=c("agency_id","Distance_um","Year"), all.x = TRUE)



#Bring in manual aging of otos with no chemical signature
##Ears
DataEx = read.csv("Data Outputs/processedotodata_DS.csv")

#Data = Data%>% dplyr::select(-Habitat2)

DataAEx = subset(DataEx[,-c(1)],agency_id==c("HG21_012")|agency_id==c("LT21_3187")|agency_id==c("LT21_3189")|agency_id==c("LT21_3302"))#, "LT21_3187", "LT21_3189","LT21_3302","ST21_2431"))
AData = subset(Data, Size =="AD")
DataAEx = rename(DataAEx, `Sr:Ca`=`Sr.Ca`)
Corder = colnames(AData)
DataAEx = DataAEx[, Corder]

#Remove the data we are replacing first
AData = subset(AData, agency_id!=c("HG21_012")&agency_id!=c("LT21_3187")&agency_id!=c("LT21_3189")&agency_id!=c("LT21_3302"))
AData = rbind(AData, DataAEx)

JData = subset(Data, Size =="Juv")
DataFin =rbind(JData, AData)


###Exploratory plots
ggplot(AData, aes(x=Year, y=Sr88_ppm,color=Habitat))+geom_point()+facet_wrap(~agency_id)+theme_bw()+
  xlab("Estimated Age")+ylab("Strontium ppm")+ labs(color='Habitat')  + scale_color_manual(values=c("#999999","#56B4E9", "darkgreen")) + 
  scale_x_continuous(breaks=seq(0,9))

#Compare with old data
OData = read.csv("Data Outputs/processedotodata_DS.csv")
OData = subset(OData, Size =="AD")

ggplot(AData, aes(x=Year, y=Sr88_ppm,color=Habitat))+geom_point()+facet_wrap(~agency_id)+theme_bw()+
  xlab("Estimated Age")+ylab("Strontium ppm")+ labs(color='Habitat')  + scale_color_manual(values=c("#999999","#56B4E9", "darkgreen")) + 
  scale_x_continuous(breaks=seq(0,10))+geom_point(data=OData, aes(x=Year, y=Sr88_ppm,color=Habitat), alpha=0.1, color="black")

#Figure4
Fig4 = subset(AData, agency_id=="LT21_3196")
ggplot(Fig4, aes(x=Year, y=`Sr:Ca`,group=agency_id))+geom_line(size=1)+theme_bw()+Theme+
  xlab("Estimated Age")+ylab("Sr:Ca (mmol/mol)")+ labs(color='Habitat')   + 
  scale_x_continuous(breaks=seq(0,7))+geom_line(data=subset(OData, agency_id=="LT21_3196"), aes(x=Year, y=`Sr.Ca`,color=Habitat), alpha=0.2,size=1, color="black")+
  geom_vline(xintercept = c(1,2,3,4,5,6), alpha=0.1)+geom_hline(yintercept = 2.98, alpha =0.1)

#Cut where laser went off otolith
DataFin = subset(DataFin, !(agency_id =="LT21_3187" & Year>=7))


DataFin = mutate(DataFin, Habitat3 = Habitat)

#Ensure adult labeled correctly
DataFin=mutate(DataFin, Size = ifelse(length_mm<250, "Juv", "AD"))
DataFin=subset(DataFin, Lake != "HEGA")

#RSD's for Rb ppor so exclude
DataFin=DataFin%>%
  dplyr::select(-c(Rb85_ppm))



#Export Chemcially aging
write.csv(DataFin, "Data Outputs/processedotodata2_DS.csv")
