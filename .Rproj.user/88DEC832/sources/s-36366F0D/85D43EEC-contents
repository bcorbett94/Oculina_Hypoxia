#Attempt to convert excel thesis data to r data
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(knitr)

df <- read_csv("environment_dat.csv")
#Experimental Data 
tmp_graph<-ggplot(data = df,aes(x = day,y = temp, color = treatment ))+
  geom_line()+
  scale_y_continuous(name = "Temperature (C)")+
  scale_x_continuous(name = "Experimental Day", breaks = seq(1,14, by = 1))+
  geom_point()+
  theme_classic2()

#K-wallis test to prove there was no affect of temperature on endpoints as it was equal through both treatments
compare_means (DO~treatment, data = df, method =  "kruskal.test")

DO_graph<-ggplot(data = df, aes(x = day, y = DO, color = treatment))+
  geom_line()+
  scale_y_continuous(name = "Dissolved Oxygen (mg/L)")+
  scale_x_continuous(name = "Experimental Day", breaks = seq(1,14, by = 1))+
  geom_point()+
  theme_classic2()+
  annotate(geom = "text", x = 7, y = 5, label = "p-value = <.001")


#K-wallis test to prove that DO levels /were/ significantly different 
kruskal.test(DO~treatment, data = df)



# Beginning of graphs for Dot blot endpoints
DD_dot<-read_csv("dot_blot_DD.csv")
PC_dot<-read_csv("dot_blot_PC.csv")
LP_dot<-read_csv("dot_blot_LP.csv")

#******************************DD stats*********************

#Does DD intensity level depend on symbiont and treatment 
DD_anov<-aov(DD~Treatment+Symbiont, data = DD_dot)
summary(DD_anov)

#Wilcoxon Test, matches with JMP data, there is no sig diff between DD intensity between Symbioses
DD_wilcox<-wilcox.test(DD~Symbiont, data = DD_dot, exact = FALSE)

# DD graphical representation

DD_treatment<-ggbarplot(
  DD_dot, x = "Treatment", y = "DD", color = "Treatment",
  add = c("mean", "jitter"),
  position = position_dodge(.2)
) + ggtitle("DNA Damage")
DD_treatment
DD_treatment + labs( y = "Intensity", x = "Treatments")+theme(legend.position = "none")

sym_dd<-DD_dot%>%group_by(Treatment)%>%
  filter(Symbiont == "Sym")%>%
  group_by(Genotype)

apo_dd<-DD_dot%>%group_by(Treatment)%>%
  filter(Symbiont == "Apo")%>%
  group_by(Genotype)
#Each paramater doesn't have a data point for every ID, does that mean I won't be able to put them on the same graph

DD_geno_sym<-ggbarplot(
  sym_dd, x ="Genotype", y = "DD",color = "Treatment",
  add = c("mean_se_", "jitter"),
  order = c("A","B","C","D","E","F","G","H","I"),
  position = position_dodge(.7)
)
DD_geno_sym
DD_geno_apo<-ggbarplot(
  apo_dd, x ="Genotype", y = "DD",color = "Treatment" ,
  add = c("mean_se_", "jitter"),
  order = c("J","K","L","M","N","O","P","Q","R"),
  position = position_dodge(.7)
)

#******************************PC stats*********************




# PC graphical representation

PC_treatment<-ggbarplot(
  PC_dot, x = "Treatment", y = "PC", color = "Treatment",
  add = c("mean_se_", "jitter"),
  position = position_dodge(.7)
) +ggtitle("Protein Carbonylation")

PC_treatment
PC_treatment + labs( y = "Intensity", x = "Treatments")+theme(legend.position = "none")


sym_pc<-PC_dot%>%group_by(Treatment)%>%
  filter(Symbiont == "Sym")%>%
  group_by(Genotype)
apo_pc<-PC_dot%>%group_by(Treatment)%>%
  filter(Symbiont == "Apo")%>%
  group_by(Genotype) 

PC_geno_sym<-ggbarplot(
  sym_pc, x ="Genotype", y = "PC",color = "Treatment" ,
  add = c("jitter","mean_se_"),
  order = c("A","B","C","D","E","F","G","H","I"),
  position = position_dodge(.7)
)

PC_geno_apo<-ggbarplot(
  apo_pc, x ="Genotype", y = "PC",color = "Treatment",
  add = c("jitter","mean_se_"),
  order = c("J","K","L","M","N","O","P","Q","R"),
  position = position_dodge(.7)
)

#******************************LP stats*********************

LP_anov<-aov(LP~Treatment+Symbiont, data = LP_dot)
summary(LP_anov)

#*Combination of Genotype & Treatment
LP_anov2<-aov(LP~Genotype*Treatment, data = apo_lp)

summary(LP_anov2)

tukey<-TukeyHSD(LP_anov2)
tukey


#Lipid peroxidation graphical representation
LP_treatment<-ggbarplot(
  LP_dot, x = "Treatment", y = "LP", color = "Treatment",
  add = c("mean_se_", "jitter"),
  position = position_dodge(.7)
) +ggtitle("Lipid Peroxidation")

LP_treatment
LP_treatment + labs( y = "Intensity", x = "Treatments")+theme(legend.position = "none")


sym_lp<-LP_dot%>%group_by(Treatment)%>%
  filter(Symbiont == "Sym")%>%
  group_by(Genotype)
apo_lp<-LP_dot%>%group_by(Treatment)%>%
  filter(Symbiont == "Apo")%>%
  group_by(Genotype) 

LP_geno_sym<-ggbarplot(
  sym_lp, x ="Genotype", y = "LP",color = "Treatment" ,
  add = c("jitter","mean_se_"),
  order = c("A","B","C","D","E","F","G","H","I"),
  position = position_dodge(.7)
)

LP_geno_apo<-ggbarplot(
  apo_lp, x ="Genotype", y = "LP",color = "Treatment",
  add = c("jitter","mean_se_"),
  order = c("J","K","L","M","N","O","P","Q","R"),
  position = position_dodge(.7)
)

