#Comparing Different laser settings

library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(oce)
library(segmented)
library(zoo)


Theme = theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title=element_text(size=20),axis.text=element_text(size=18))


# Import Data -------------------------------------------------------------


#Fast Test
um10 <- read_excel("Raw Data/PBTEST1_10UM.xlsx", 
                           sheet = "PNAD_07_10 time series data", 
                           skip = 1)
um5 =  read_excel("Raw Data/PBTEST1_5UM.xlsx", 
                    sheet = "PNAD_07_5 time series data", 
                    skip = 1)
um10$LD = "10um"
um5$LD = "5um"
test = rbind(um10,um5)
test$Speed = "10um/s"   
test$Distance = test$`Elapsed Time`*10

#Slow test
um102 <- read_excel("Raw Data/PBTEST2_10UM.xlsx", 
                   sheet = "PNAD_07_10 time series data", 
                   skip = 1)
um52 =  read_excel("Raw Data/PBTEST2_5UM.xlsx", 
                  sheet = "PNAD_07_5 time series data", 
                  skip = 1)
um102$LD = "10um"
um52$LD = "5um"

#Bring in final data
AD7 = read_xlsx("Raw Data/20231019_jd_PB_Batch1_1.2_good_export.xlsx", 
                sheet = "PNAD_07 time series data", skip = 1)
AD7$LD = "30um"


#Merge together
test2 = rbind(um102,um52, AD7)
test2$Speed = "3um/s"
test2$Distance =3*test2$`Elapsed Time`

test=rbind(test,test2)



#Do the same data prep as before
test2 = test %>%
  group_by(LD, Speed)%>%
  arrange(`Elapsed Time`)%>%
  mutate(across(Li7_ppm:Pb208_ppm, ~despike(.x, reference = "smooth", replace ="NA",n=5,k=10)))%>%
  mutate(across(Li7_ppm:Pb208_ppm, ~rollmean(.x,5,fill=NA)))

#Convert all Sr to SR:Ca
test2 = mutate(test2, `Sr:Ca` = (Sr88_ppm/87.62)/((Ca43_ppm/40.078)/1000)) 

#Prep for plots
test2 = mutate(test2, PredHab = ifelse(`Sr:Ca`>=2.42&Distance>=250, "Sea", "FW"))



# Fig S1: Laser settings ------------------------------------------------------------------

plot1=test2[!is.na(test2$`Sr:Ca`),]

S1 = ggplot(plot1, aes(x=`Distance`, y=`Sr:Ca`,color=PredHab,group=LD))+facet_wrap(~LD*Speed)+geom_line(size =0.5)+
  theme_bw()+Theme+xlab(expression(paste("Distance", " (", mu,"m)")))+ylab("Sr:Ca (mmol/mol)")+ labs(color='Habitat')  + 
  scale_color_manual(values=c("#999999","#56B4E9"))+geom_point(size=0.9)+
  theme(strip.text = element_text(size = 15))
ggsave("Plots/Fig. S1.tiff", plot = S1, width = 7, height = 5, units = "in", dpi = 400)



