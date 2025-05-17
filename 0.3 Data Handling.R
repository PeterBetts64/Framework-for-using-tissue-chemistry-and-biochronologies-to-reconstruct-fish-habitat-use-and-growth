#Brings Data together and does some data processing

#Bring in required packages
library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(oce)
library(segmented)
library(zoo)



# Otolith Data ------------------------------------------------------------

## Import ------------------------------------------------------------------
#Load Data for batch 1

#Get name for each sheet
Sheets = readxl::excel_sheets("Raw Data/20231019_jd_PB_Batch1_1.2_good_export.xlsx")

Sheets = data.frame(Sheets) 
Sheets = Sheets[c(3:31),]
ORData = lapply(setNames(Sheets, Sheets), function(x) read_excel("Raw Data/20231019_jd_PB_Batch1_1.2_good_export.xlsx", sheet=x, skip=1)) 
# attaching all dataframes together 
ORData2 = bind_rows(ORData, .id="Sheet")
#Give a run ID
ORData2$Run = 1


#Load data for batch 2
Sheets = readxl::excel_sheets("Raw Data/20231019_jd_PB_Batch2_1.2.xlsx")
Sheets = data.frame(Sheets) 
Sheets = Sheets[c(3:37),]
OR2Data = lapply(setNames(Sheets, Sheets), function(x) read_excel("Raw Data/20231019_jd_PB_Batch2_1.2.xlsx", sheet=x, skip=1)) 
# attaching all dataframes together 
OR2Data2 = bind_rows(OR2Data, .id="Sheet") 
OR2Data2$Run= 2


#load data for batch 3
Sheets = readxl::excel_sheets("Raw Data/20231019_jd_PB_Batch3_1.2_good_export.xlsx")
Sheets = data.frame(Sheets) 
Sheets = Sheets[c(3:8),]
OR3Data = lapply(setNames(Sheets, Sheets), function(x) read_excel("Raw Data/20231019_jd_PB_Batch3_1.2_good_export.xlsx", sheet=x, skip=1)) 
# attaching all dataframes together 
OR3Data2 = bind_rows(OR3Data, .id="Sheet") 
OR3Data2$Run= 3


#load data for batch 1 edges
Sheets = readxl::excel_sheets("Raw Data/20231019_jd_PB_Batch1_EDGE_1.2_good_export.xlsx")
Sheets = data.frame(Sheets) 
Sheets = Sheets[c(3:15),]
OR4Data = lapply(setNames(Sheets, Sheets), function(x) read_excel("Raw Data/20231019_jd_PB_Batch1_EDGE_1.2_good_export.xlsx", sheet=x, skip=1)) 
# attaching all dataframes together 
OR4Data2 = bind_rows(OR4Data, .id="Sheet") 
OR4Data2$Run= 1.5


#Combine the batches
ORData3 = rbind(ORData2, OR2Data2,OR3Data2)
ORData3$Run = as.character(ORData3$Run)


#Make comparable to meta data
ORData3$Sheet <- gsub(".{0,17}$", "", ORData3$Sheet)


##Attach edge data
OR4Data2$Sheet <- gsub(".{0,19}$", "", OR4Data2$Sheet)

(table(OR4Data2$Sheet))
#Import data on the overlap of the extra edged with the original run
EdgeL = read_excel("Raw Data/Main Data_norway_trout_overview_samples.xlsx", 
                   sheet = "Edge Lengths")

OR4Data3 = merge(OR4Data2, EdgeL, by=c("Sheet"))
OR4Data3$Distance_um = OR4Data3$`Elapsed Time`*3

#Subset overlapping data off
OR4Data4 = subset(OR4Data3, Distance_um > Diff)
#Now get time and distance to align with main data
OR4Data4 = OR4Data4[order(OR4Data4$`Elapsed Time`),]
OR4Data4=OR4Data4%>%
  group_by(Sheet)%>%
  mutate(`Elapsed Time` = `Elapsed Time`-min(`Elapsed Time`))
OR4Data5=NULL
for(i in levels(as.factor(OR4Data4$Sheet))){
  D = subset(OR4Data4, Sheet == i)
  M = subset(ORData3, Sheet == i)
  M2 = max(M$`Elapsed Time`)
  D2=mutate(D, `Elapsed Time` = `Elapsed Time`+M2)
  OR4Data5 = rbind(OR4Data5, D2)
}
OR4Data6 = OR4Data5[,-c(26:31)]
OR4Data6$Run=as.character(OR4Data6$Run)
#Merge edges with main
ORData3 = rbind(OR4Data6, ORData3)


## Import Meta Data --------------------------------------------------------
OMeta = read_excel("Raw Data/Main Data_norway_trout_overview_samples.xlsx", 
                   sheet = "Ear Data")
#Merge
colnames(OMeta)[13] = "Sheet"
OData = merge(OMeta,ORData3, by =c("Sheet"))
OData = subset(OData, paired =="Y")
ODataex = OData[,c(1,2,4,6,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)]
#Calculate distance along otolith
ODataex$Distance_um = ODataex$`Elapsed Time`*3


#Remove Otoliths too damaged for core to edge laser ablation
OtoLR <- read_excel("Raw Data/Main Data_norway_trout_overview_samples.xlsx", 
                      sheet = "Oto Lengths")
ODataex = merge(ODataex, OtoLR[,c(1,9,10)], by=c("agency_id"))
ODataex = subset(ODataex, Suspect =="No" & Exclude=="No")


## Element Removal/cleaning ---------------------------------------------------------
lod = read_xlsx("Raw Data/LODs.xlsx", col_names = F) %>%
  rename(Element = `...1`, LOD_ppm = `...4`)
lod=lod[,c(1,4)]

df = ODataex %>% pivot_longer(cols = c(11:29), names_to = "Element", values_to = "Conc_ppm") %>%
  filter(!Element=="Ca43_ppm") %>%
  left_join(., lod) %>%
  mutate(below_LOD = case_when(Conc_ppm < LOD_ppm ~ "y",
                               TRUE ~ "n"))

summary = df %>%
  group_by(Element, below_LOD) %>%
  summarise(n_measurements = n()) %>%
  group_by(Element) %>%
  mutate(frac_below_lod = n_measurements/sum(n_measurements))%>%
  subset(below_LOD=="n")

##Remove elements with <10% values below LOD
ODataex = ODataex%>%
  dplyr::select(-c(Fe56_ppm,Li7_ppm,K39_ppm,Cd111_ppm,Cu63_ppm,Ni60_ppm,Pb208_ppm,Co59_ppm,Ag107_ppm))



df = ODataex %>% pivot_longer(cols = c(11:20), names_to = "Element", values_to = "Conc_ppm") %>%
  filter(!Element=="Ca43_ppm") %>%
  left_join(., lod) %>%
  mutate(below_LOD = case_when(Conc_ppm < LOD_ppm ~ "y",
                               TRUE ~ "n"))


ggplot(df, aes(Distance_um, Conc_ppm, group=agency_id, color = Size)) +
  geom_line(alpha=.5, size=.8)+
  facet_wrap(~Element, scales = "free")

ggplot(filter(df, Size == "AD"), aes(Distance_um, Conc_ppm, group=agency_id)) +
  geom_line(alpha=.5, size=.8)+
  facet_wrap(~Element, scales = "free")

# Does P look bad for chemical ageing? In many fish, yes
ggplot(filter(df, Size == "AD", !agency_id=="LT21_3187", !Lake=="HEGA", Element=="P31_ppm"),
       aes(Distance_um, Conc_ppm, group=agency_id)) +
  geom_line(alpha=.5, size=.8)+
  ylim(0,300)+
  facet_wrap(~agency_id, scales = "free")

ggplot(filter(df, Size == "AD",!Lake=="HEGA", !agency_id=="LT21_3187", Element=="P31_ppm"),
       aes(Distance_um, Conc_ppm, group=agency_id)) +
  geom_line(alpha=.5, size=.8)+
  ylim(0,350)+
  facet_wrap(~agency_id)

# looks like one adult has contamination at edge "LT21_3187" - exclude for nwo
ggplot(filter(df, Size == "AD", !agency_id=="LT21_3187"), aes(Distance_um, Conc_ppm, group=agency_id)) +
  geom_line(alpha=.5, size=.8)+
  facet_wrap(~Element, scales = "free")

# Exclude data from edge of LT21_3187 as values/images suggest ablating glue
ODataex2 <- ODataex %>%
  filter(!(agency_id == "LT21_3187" & Distance_um > 1300))



## Smoothing and Spike Removal ---------------------------------------------
#sum(is.na(ODataex2[,c(11:28)]))

ODataex3 = ODataex2 %>%
  group_by(agency_id)%>%
  arrange(`Elapsed Time`)%>%
  mutate(across(Na23_ppm:Ba138_ppm, ~despike(.x, reference = "smooth", replace ="NA",n=5,k=10)))%>%
  mutate(across(Na23_ppm:Ba138_ppm, ~rollmean(.x,5,fill=NA)))

#Check
df2 = ODataex3 %>% pivot_longer(cols = c(11:20), names_to = "Element", values_to = "Conc_ppm") %>%
  filter(!Element=="Ca43_ppm") %>%
  left_join(., lod) %>%
  mutate(below_LOD = case_when(Conc_ppm < LOD_ppm ~ "y",
                               TRUE ~ "n"))

ggplot(filter(df2), aes(Distance_um, Conc_ppm, group=agency_id)) +
  geom_line(alpha=.5, size=.8)+
  facet_wrap(~Element, scales = "free")


ggplot(ODataex3, aes(x=Distance_um, y=Sr88_ppm))+facet_wrap(~agency_id)+geom_point()

ODataex4 = subset(ODataex3, Distance_um>0)
#sum(is.na(ODataex3[,c(11:29)]))
#6441/(21720*18)



#check
ggplot(ODataex3, aes(x=Distance_um, y=Sr88_ppm, group=Sheet))+geom_point(aes(color=`Size`),cex=1.1)+facet_wrap(~agency_id)

## Export Otoliths ---------------------------------------------------------

write.csv(ODataex3, "Data Outputs/prepedotodata_DS.csv")





# Eye Data ----------------------------------------------------------------

## Import ------------------------------------------------------------------

Meta = read_excel("Raw Data/Main data_norway_trout_overview_samples.xlsx", 
                  sheet = "Overview R")

Iso = read_excel("Raw Data/Main data_norway_trout_overview_samples.xlsx", 
                 sheet = "Eye LabBook2")
Iso$agency_id = sub("-", "_", Iso$agency_id)
dim(Iso)
str(Iso)
length(unique(Iso$LabID))
table(Iso$agency_id)
#All looks good

#Merge data together
tdata = merge(Iso, Meta, by = c("agency_id"))


tdata2 = subset(tdata, Paired =="Yes" & tag != "CORE" )

ID  <- read_excel("Raw Data/PROCESSED Soton Data Back.xlsx", 
                  col_types = c("text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "text", "text", "text", 
                                "text", "numeric", "numeric", "text")) #READ IN RIGHT!

Data = merge(ID, tdata2, by = c("Name"))


DataEx = Data[,c(1,2,3,4,5,6,7,8,9,14,15,16,17,18,19,20,22,23,24,28,29,30,31)]


## Export ------------------------------------------------------------------
write.csv(DataEx, "Data Outputs/alleyedata.csv")

