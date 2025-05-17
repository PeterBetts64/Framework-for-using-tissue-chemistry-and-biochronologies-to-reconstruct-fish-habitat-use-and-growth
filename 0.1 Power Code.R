
##Code to look at the sample number required for each example

library(pwr)
library(readxl)


# Differences between lakes EX1 -----------------------------------------------

d=0.83#Assume effect size as 0.83
pwr.t.test(d = d , sig.level = 0.05, power = .80, type = c("two.sample", "one.sample", "paired"))


# Differences in Growth EX2  ---------------------------------------------------

#From https://www.researchgate.net/publication/227229347_Growth_and_survival_rates_of_the_anadromous_trout_Salmo_trutta_from_the_Vardnes_River_Northern_Norway?enrichId=rgreq-1072976dd1ca8a76b7773393602c181e-XXX&enrichSource=Y292ZXJQYWdlOzIyNzIyOTM0NztBUzoxNDIwODcyNTczMzM3NjBAMTQxMDg4NzY1MDY3OQ%3D%3D&el=1_x_3&_esc=publicationCoverPdf

SeaG = c(5.8, 6.8, 5.0, 6.7, 5.9, 7.1, 8.2, 4.6, 4.7, 5.3, 5.8)
SeaG=SeaG
mean(SeaG)
sd(SeaG)


FreshG = c(0.6,1.5,1.7,1.3,1.1,-0.4,0.6,1.3,1.1)
FreshG=FreshG
mean(FreshG)
sd(FreshG)

#Power
sd_pooled <- sqrt((sd(SeaG)^2 +sd(FreshG)^2) / 2)
sd_pooled
Diff = abs(mean(SeaG)-mean(FreshG))
Diff
d=Diff/sd_pooled
d
pwr.t.test(d = d , sig.level = 0.05, power =.80, type = c("two.sample", "one.sample", "paired"))
