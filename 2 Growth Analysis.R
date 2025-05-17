##Data analysis of growth between habitats


# Load libraries ----------------------------------------------------------

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
library(mgcv)

#Theme
Theme = theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title=element_text(size=20),axis.text=element_text(size=18))


# Import Data-------------------------------------------------------------------------

Data = read.csv("Data Outputs/processedotodata2_DS.csv")
AData = subset(Data, Size  =="AD")
JData = subset(Data, Size  =="Juv")



# Data Vis and Exploration ------------------------------------------------

# Threshold for marine migration based on max Sr value observed in immature reference samples outside of the core (250um otolith radius)
max(JData$Sr88_ppm[JData$Distance_um > 250], na.rm = T)
max(JData$Sr.Ca[JData$Distance_um > 250], na.rm = T)

ggplot(Data, aes(x=Distance_um, y = Sr88_ppm, color = Habitat3)) + geom_point() + 
  facet_wrap(~Size*agency_id)

# Some fish are obviously anadromous and resident. Some are confusing - perhaps spending time in fjord?
ggplot(AData, aes(x=Distance_um, y = Sr88_ppm, color = Habitat3)) + geom_point() + 
  facet_wrap(~agency_id)

# fish not sure what phenotype they are as Sr increase but not to same concs as more 'obvious' sea trout with repeat migrations
Uncertain = c("LT21_3187", "LT_3302", "LT21_3241", "LT21_3302", "ST21_2006")

#Mark if adult fish ever went to sea in otolith material outside the core
Sea = subset(AData, Habitat3 =="Sea" & Distance_um > 250)
range(na.omit(unique(Sea$Age)))
Sea = unique(Sea$agency_id)

AData = AData %>% mutate(Phenotype = case_when(agency_id %in% Uncertain ~ "Uncertain", 
                                               agency_id %in% c(Sea) ~ "Anadromous",
                                               TRUE ~ "Resident"),
                         # BASED ON LENS DATA  PUT ALL UNUSURE FISH AS ANADROMOUS
                         Phenotype2 = case_when(agency_id %in% c(Sea) ~ "Anadromous",
                                                agency_id %in% Uncertain ~ "Anadromous", 
                                                TRUE ~ "Resident"),
                         # BASED PURELY ON SR CUTOFF LT21_3187 IS PUT AS RESIDENT
                         Phenotype_cutoff_based = case_when(agency_id %in% c(Sea) ~ "Anadromous",
                                                            TRUE ~ "Resident"))

phenotypes = distinct(AData, agency_id, Phenotype, Phenotype2, Phenotype_cutoff_based)
write.csv(phenotypes, 'Data Outputs/phenotype_assignments.csv')

#Does Ba help to interpret those 4? Nope - even some of the residents have really low and consistent Ba
ggplot(AData, aes(x=Distance_um, y = Ba138_ppm, color = Habitat3)) + geom_point() + 
  facet_wrap(~Phenotype*agency_id)+ylim(0,8)

# age at first outmigration
first_marine = filter(AData, Phenotype2 == "Anadromous" & Habitat == "Sea" & Distance_um > 250) %>%
  group_by(agency_id) %>% slice(which.min(Distance_um)) %>% mutate(first_sea_entry = "y")

AData = left_join(AData, first_marine)


#First marine migration and ages at sea

# Fig. S6 -----------------------------------------------------------------

ggplot(filter(AData, !is.na(Habitat3), !is.na(Year), Year<=8), aes(x=Year, y = `Sr.Ca`, color = Habitat3)) + geom_point() + 
  geom_hline(yintercept = 2.42, linetype = "dotted", color="turquoise4")+
  geom_point(data = first_marine, aes(x=Year, y = Sr.Ca), color = "black", size = 3)+
  geom_line(data = filter(AData, !is.na(Habitat3), !is.na(Year)), aes(x=Year, y = Sr.Ca), color = "black", size = 0.5, alpha=0.5)+
  facet_wrap(~Phenotype*agency_id)+labs(x = "Estimated age", y = "Otolith Sr:Ca (mmol/mol)", color = "Assigned\nhabitat")+
  scale_x_continuous(breaks = c(0:9))+
  #xlim(0,8)+
  theme_bw(base_size=15)+
  theme(panel.grid.major.x = element_line(color = "grey80"),   # Style for gridlines
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())+Theme + 
  scale_color_manual(values=c("#999999","#56B4E9"))                # Remove minor gridlines

ggsave('Plots/Fig. S6.tiff', width = 12, height = 10, units = "in", dpi = 300)

#distribution of outmigration sizes?

mean(first_marine$Year[first_marine$Phenotype=="Anadromous"])
sd(first_marine$Year[first_marine$Phenotype=="Anadromous"])


# Growth differences -----------------------

# just look at annulus widths
inc_widths = filter(AData, !is.na(Increment) & Increment > 0)

ggplot(inc_widths, aes(x=Year, y = Increment, color = Phenotype)) + 
  geom_smooth(se=F) + geom_point() + geom_line(data = inc_widths, aes(group = agency_id), alpha = .3)




library(mgcv)  # ensure mgcv is loaded for gam support

## Fig. 5a -----------------------------------------------------------------

growth <- ggplot(filter(inc_widths, !is.na(Habitat), Year <= 7), 
                 aes(x = Year, y = Increment, color = Phenotype2)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs",k=7), se = TRUE) +  # GAM smoother
  geom_point(size = 4, alpha = 0.5, aes(shape = Habitat)) + 
  geom_line(data = filter(inc_widths, !is.na(Habitat), Year <= 7), 
            aes(group = agency_id), alpha = 0.3) +
  labs(x = "Age", y = "Annulus width (µm)", color = "Phenotype") +
  scale_x_continuous(breaks = c(1:7)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

growth





## GAM to compare growth-------------------------------------------------------------------------


# first try includes Uncertain fish but assumes ALL are anadromous (i.e. goes against threshold for LT21_3187)
# exclude rows where Year is not a whole number or for ages > 7 as only 1 fish
inc_widths2 = filter(inc_widths, !is.na(Habitat), Year<=7) %>%
  mutate(Phenotype2 = as.factor(Phenotype2),
         Phenotype_cutoff_based = as.factor(Phenotype_cutoff_based),
         agency_id = as.factor(agency_id))

mod = gam(Increment ~ Phenotype2 + Habitat + # is there an overall difference in growth among phenotypes or habitats - Nope
            #s(Year, k=6)+ # Exclude as if this is included all the variation is explained here (ie ontogeny)
            s(Year, by=Phenotype2, k=6)+# is there a different slope between phenotypes? Some weak suggestion of yes - for age 1 only
            s(agency_id, Year, bs = 're'), # random effect of animal 
            data=inc_widths2, na.action = "na.omit", method="REML")

summary(mod)

library(gratia)
diff1 <- difference_smooths(mod, smooth = "s(Year)") 

## Fig 5b ------------------------------------------------------------------
gam_diff = draw(diff1) & theme_bw() & xlab("Age") & scale_x_continuous(breaks = c(1:7)); gam_diff
# Difference between smooths is sig for age 1 only (i.e. Anadromous fish have faster first year growth but caveat that we have 3 resident fish!!)

cowplot::plot_grid(growth, gam_diff, ncol=1, align = "v", labels = "AUTO")

ggsave("Plots/Fig. 5.tiff", height = 7, width=5)




##Remove strange fish
# Try and excludes Uncertain fish - sensitivity analysis
inc_widths3 = filter(inc_widths2, !Phenotype=="Uncertain")
mod2 = gam(Increment ~ Phenotype2 + Habitat + # is there an overall difference in growth among phenotypes or habitats - Nope
            #s(Year, k=6)+ # Exclude as if this is included all the variation is explained here (ie ontogeny)
            s(Year, by=Phenotype2, k=6)+# is there a different slope between phenotypes? Some weak suggestion of yes - for age 1 only
            s(agency_id, Year, bs = 're'), # random effect of animal 
          data=inc_widths3, na.action = "na.omit", method="REML")

summary(mod2)

diff2 <- difference_smooths(mod2, smooth = "s(Year)") 


#Plot for growth without 4 strange fish

# #Fig. S8 -----------------------------------------------------------------
gam_diff2 = draw(diff2) & theme_bw()&theme(panel.grid.minor = element_blank(),axis.title=element_text(size=20),axis.text=element_text(size=18)) & xlab("Age") & scale_x_continuous(breaks = c(1:7)); gam_diff2
# Difference between smooths is sig for age 1 only (i.e. Anadromous fish have faster first year growth but caveat that we have 3 resident fish!!)
ggsave("Plots/Fig. S8.jpg", height = 5, width=5)





# Other marine tracers ----------------------------------------------------

EData<- read_csv("Data Outputs/alleyedata.csv")
EData=merge(EData, phenotypes, by=c("agency_id"))

EData <- EData %>%
  mutate(
    LineTypeGroup = ifelse(agency_id == "LT21_3187", "LT21_3187", "other")
  )

ggplot(EData, aes(x = diameter_um, y = d34S, 
                  color = Phenotype, 
                  group = agency_id, 
                  linetype = LineTypeGroup)) +
  facet_wrap(~Phenotype) +
  geom_line(size = 1) +
  theme_bw() + Theme +
  xlab("Eye lens diameter") +
  ylab(expression(paste("Lens ", delta^34, "S"))) +
  theme(strip.text = element_text(size = 16)) +
  scale_linetype_manual(values = c("LT21_3187" = "dashed", "other" = "solid"))


## Fig. S7 d34S ------------------------------------------------------------
ggsave("Plots/Fig. S7.tiff", height = 6, width=9,units = "in")
