#Data Processing get into shape for all data viz and analysis with manual aging


# Library Packages --------------------------------------------------------
library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(oce)
library(segmented)
library(zoo)
library(mixtools)
library(tidyverse)

# Otoliths ----------------------------------------------------------------
Data = read.csv("Data Outputs/prepedotodata_DS.csv")

#Convert all Sr to SR:Ca
Data = mutate(Data, `Sr:Ca` = (Sr88_ppm/87.62)/((Ca43_ppm/40.078)/1000)) 


## Marine Entry - Maternal Signature ---------------------------------------

#Plot all data
ggplot(Data, aes(x=Distance_um, y=`Sr:Ca`))+geom_line()+facet_wrap(~agency_id)
#To avoid maternal signature we will cut the first 250um

JData = subset(Data, Size =="Juv"& Distance_um >=250)
max(na.omit(JData$`Sr:Ca`))
#We will define marine singal as anything above freshwtaer max, in this case 2.48
Cut1 = 2.42

###Determine First sea entry of adults
#Subset to post sea migration of seward migrants
Data1 = mutate(Data, Hab = ifelse(`Sr:Ca`>=2.42&Distance_um>=250, "Sea", "FW"))
Anad = subset(Data1, Hab =="Sea"&Size =="AD")
Mig = Anad%>%
  group_by(agency_id)%>%
  summarise(DS = Distance_um[1]) #This gives us distance on oto of first migration

Data = mutate(Data, Habitat = ifelse(`Sr:Ca` > 2.42, "Sea","Fresh"))



# Age the fish ------------------------------------------------------------


#Import ring data (will need to be updated)
#Grow <- read.csv("C:/Users/peter/Box/TROUT_NORWAY/Lab Work/Aging and Growth/Main Data/Otolith Parameters All.csv")
#Grow <- read.csv("C:/Users/peter/Box/TROUT_NORWAY/Lab Work/Aging and Growth/Full Image Ageing/Full Image Otot Age PB.csv")
Grow = read.csv("Raw Data/Full Image Otot Age PB V2.csv")

#Remove duplicates
Grow <- Grow %>% 
  mutate(Type = ifelse(grepl("T$", Sample), "T","B"))
#Subset to just 1 read per oto and exclude incomplete years (edge)
Grow = subset(Grow, Type =="T"&I != "edge")
#Set first year to zero
Grow[is.na(Grow)] <- 0
#Calculate a cumulative distance
Grow2 = (Grow) %>%
  group_by(Sample)%>%
  mutate(Distance_um = cumsum(Increment))
#Make compatable with oto data
colnames(Grow2)[1] = "Sheet"
Grow2$Sheet <- gsub(".{2}$", "", Grow2$Sheet)



## Fuzzy marge the data ----------------------------------------------------

end = merge(Data, Grow2, by="Sheet",allow.cartesian=TRUE)
#Check no lost ID
length(table(end$agency_id))
length(table(Grow2$Sheet))
#Now remove all non matching pairs to end up with one data set (Fuzzy merge)
end$Dif=(end$Distance_um.x-end$Distance_um.y)
end2=subset(end, Dif >-4)
end2=subset(end2, Dif <4)
#Create data set with growth data and Habitat at that point sample 
res <- end %>%
  group_by(Sheet, Year) %>% 
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


## Merge with the rest of the data -----------------------------------------

res = mutate(res, HabI = ifelse(Hab ==TRUE, "Sea", "Fresh"))
res1 = dplyr::select(res,agency_id, Distance_um.x, Year, Age,HabI,Increment) %>% rename(Distance_um = Distance_um.x)
res1 = res1[,-c(1)]
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
for(i in c(unique(Data$agency_id))){
  tryCatch({
  da = subset(Data, agency_id ==i)
  ag=subset(age, agency_id==i)
  ag1=as.numeric(ag[,8])
  n = length(da$Age)
  da2 = unique(na.omit(da$Age))
  da[n,27]=(ag1+da2)
  da$Year = zoo::na.approx(da$Year)
  results = rbind(results, da)
  },error=function(e){})
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


#Check all looks good
ggplot(Data, aes(y=`Sr:Ca`, x=Year,color=Habitat,group=NULL))+geom_point()+facet_wrap(~agency_id)+geom_hline(yintercept = 2.48)
ggplot(Data, aes(y=`Sr:Ca`, x=Distance_um,color=Habitat,group=NULL))+geom_point()+facet_wrap(~agency_id)+geom_hline(yintercept = 2.48)

write.csv(Data, "Data Outputs/processedotodata_DS.csv")
