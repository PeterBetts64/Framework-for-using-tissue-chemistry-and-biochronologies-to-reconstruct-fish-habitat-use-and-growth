#General Data Viz

#Bring in required packages
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

# Load Packages -----------------------------------------------------------


#Theme
Theme = theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title=element_text(size=20),axis.text=element_text(size=18))



# Data Prep ----------------------------------------------------
EData = read.csv("Data Outputs/processedotodata2_DS.csv")

#Subset Juvaniles
JD=subset(EData, Size=="Juv")
range(na.omit(unique(JD$LRadius)))
mean(na.omit(unique(JD$LRadius)))

#Subset Adults
AData = subset(EData, Size =="AD")


# Example Tracer profiles -------------------------------------------------


####Select example Sr,Ba, Zn plot
Ex = subset(AData, agency_id =="LT21_3196")
Ex= mutate(Ex, `Ba.Ca` = (Ba138_ppm/137.327)/((Ca43_ppm/40.078)/1000000))
Ex= mutate(Ex, `Mg.Ca` = (Mg24_ppm/24.305)/((Ca43_ppm/40.078)/1000000))
Ex= mutate(Ex, `Zn.Ca` = (Zn66_ppm/65.38)/((Ca43_ppm/40.078)/1000000))

Sr = ggplot(Ex, aes(x=Distance_um/1000, y=`Sr.Ca`))+geom_line(size=0.8)+theme_bw()+Theme+ylab("Sr:Ca (mmol/mol)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))#+ xlab("Estimated Length (mm)")
Ba= ggplot(Ex, aes(x=Distance_um/1000, y=`Ba.Ca`))+geom_line(size=0.8)+theme_bw()+Theme+ylab(expression(paste("Ba:Ca (",mu,"mol/mol)")))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))#+ xlab("Estimated Length (mm)")
Mg = ggplot(Ex, aes(x=Distance_um/1000, y=`Mg.Ca`))+geom_line(size=0.8)+theme_bw()+Theme+ xlab(expression(paste("Otolith Radius (mm)")))+ylab(expression(paste("Mg:Ca (",mu,"mol/mol)")))+ theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
Zn = ggplot((Ex)[!is.na(Ex$`Zn.Ca`),], aes(x=(Distance_um/1000), y=(`Zn.Ca`)))+geom_line(size=0.8)+theme_bw()+Theme+ xlab(expression(paste("Otolith Radius (mm)")))+ylab(expression(paste("Zn:Ca (",mu,"mol/mol)")))+ theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

#Sulphur
EData =  read_csv("C:/Users/peter/Box/TROUT_NORWAY/Data/Norway Data Analysis/Data Outputs/alleyedata.csv")

LEeye = subset(EData, num ==1)
ggplot(LEeye, aes(x=diameter_um, y=length_mm))+geom_point()+geom_smooth(method="lm")
m1=lm(length_mm~diameter_um, LEeye)
summary(m1)
m1$coefficients[2]

Ex2 = subset(EData, agency_id =="LT21_3196")
#Adjust eye lens diameter to radius

#Bring in cores

Ex2=dplyr::select(Ex2,d13C,d34S,diameter_um,agency_id,num,tag)
#Adjust
Ex2$Rad=Ex2$diameter_um/2
Ex2 = Ex2%>%
  mutate(Rad2 = (Rad+lead(Rad))/2)

Sl = ggplot(Ex2, aes(x=Rad2/1000, y=d34S))+geom_line(size=0.8)+theme_bw()+Theme+
  xlab(expression(paste("Lens Radius (mm)")))+geom_point()+ylim(5,11)+xlim(0,1.8)+
  scale_y_continuous( limits = c(4.5, 11),sec.axis = sec_axis(trans = ~ . -35,name = expression(paste("Lens ",delta^13, "C")) ))+geom_point(aes(y=d13C+35),color="red")+
  geom_line(aes(y=d13C+35),size=0.8,color="red")+
  ylab(expression(paste("Lens ",delta^34, "S")))+ theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
blank=ggplot(Ex, aes(x=Distance_um/1000, y=))+theme_bw()+Theme+ xlab(expression(paste("Otolith Radius (mm)")))+ theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))


## Fig. 3 ------------------------------------------------------------------
F3 =plot_grid(Sr, Ba, Sl,Zn, Mg,blank, ncol=3,labels=c("a","b","c","d","e","f"), align = "v")
ggsave("Plots/Fig. 3.tiff", plot = F3, width = 15, height = 12, units = "in", dpi = 300)



# Compare Chemical vs Visua Ageing ----------------------------------------
#Import
Chem=read.csv("Data Outputs/processedotodata2_FA.csv")
Vis = read.csv("Data Outputs/processedOtoData_DS.csv")
Reread = read_excel("Data Outputs/Oto Rereads.xlsx")

#Cut to just age
Chem = Chem%>%
  dplyr::select(Sheet, Age)%>%
  na.omit()%>%
  unique()
Vis = Vis%>%
  dplyr::select(Sheet, Age)%>%
  na.omit()%>%
  unique()

#Merge Reareads
Chem=merge(unique(Chem), Reread, by=c("Sheet"),all = TRUE)
Chem=subset(Chem,Reader =="Anna")
Vis=merge(unique(Vis), Reread, by=c("Sheet"),all = TRUE)
Vis=subset(Vis,Reader =="Coralie")


#See diferences
Chem$Diff=(Chem$Age.y-Chem$Age.x)
Chem$Type="Chemical"
Vis$Diff=(Vis$Age.y-Vis$Age.x)
Vis$Type="Visual"


#Merge
Comp = rbind(dplyr::select(Vis, Diff, Type),dplyr::select(Chem, Diff, Type))
#Comp$Diff=abs(Comp$Diff)

# Fig. S2 -----------------------------------------------------------------

#Plot
S2 = ggplot(Comp, aes(y=Diff, x=Type))+geom_boxplot(outliers = FALSE)+geom_jitter(width=0.05,height=0.01,alpha=0.5,size=2)+ylim(-1,7)+theme_bw()+Theme+ylab("Difference in assigned age")+xlab("Ageing method")
ggsave("Plots/Fig. S2.tiff", plot = S2, width = 5, height = 4, units = "in", dpi = 300)

wilcox.test(Chem$Diff,Vis$Diff)
