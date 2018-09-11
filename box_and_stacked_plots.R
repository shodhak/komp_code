rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)

#load complete unscaled dataset
final_unscaled <- read.csv("final_unscaled.csv")

#Prepare box plots
#Daily Sleep Percent
dunnetts_sleep_percent <- read.csv("dunnetts_sleep_percent.csv")
#Extract genes significant in Dunnetts test into a vector
geno_sleep_percent <- dunnetts_sleep_percent[which(dunnetts_sleep_percent[,5]<0.05),1]
#Use gsub to remove "-CONTROL" tags in Dunnetts test results
geno_sleep_percent <- gsub("-CONTROL","",geno_sleep_percent)
#include CONTROLS (since CONTROLS will be a part of final plot)
geno_sleep_percent <- append("CONTROL",geno_sleep_percent)
#Extract sleep percent data for genotypes that were found to be significant in dunnetts test
sig_sleep_percent <- dplyr::select(final_unscaled, Genotype, Results.Sleep.Daily.Percent)
sig_sleep_percent <- dplyr::filter(sig_sleep_percent, Genotype %in% geno_sleep_percent)

#plot results
p<-ggplot(sig_sleep_percent,aes(x = reorder(Genotype, Results.Sleep.Daily.Percent, mean), y = Results.Sleep.Daily.Percent))
q<-p+geom_boxplot(width=0.5)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))#+stat_boxplot(geom='errorbar')
r<-q+theme(axis.text.x=element_text(angle=50,hjust=1))+ coord_cartesian(ylim = c(20, 70))+geom_hline(aes(yintercept=mean(sig_sleep_percent$Results.Sleep.Daily.Percent)),linetype="dashed")
s<-r+labs(title="24 Hours",x="Genotype",y="Percent Sleep (%)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))
s+geom_hline(aes(yintercept=mean(sig_sleep_percent$Results.Sleep.Daily.Percent)-1.96*(sd(sig_sleep_percent$Results.Sleep.Daily.Percent))),linetype="dashed") +
  geom_hline(aes(yintercept=mean(sig_sleep_percent$Results.Sleep.Daily.Percent)+1.96*(sd(sig_sleep_percent$Results.Sleep.Daily.Percent))),linetype="dashed")

#Sleep Light Phase Percent
#Load csv file containing dunnett's test results
dunnetts_light_sleep_percent <- read.csv("dunnetts_light_sleep_percent.csv")
#Extract genes significant in Dunnetts test into a vector and include CONTROLS (since CONTROLS will be a part of final plot). Use gsub to remove "-CONTROL" tags in Dunnetts test results
geno_light_sleep_percent <- dunnetts_light_sleep_percent[which(dunnetts_light_sleep_percent[,5]<0.05),1]
geno_light_sleep_percent <- gsub("-CONTROL","",geno_light_sleep_percent)
geno_light_sleep_percent <- append("CONTROL",geno_light_sleep_percent)

#Extract genotype and sleep percent information from the final_unscaled dataset for the significant genotypes
sig_light_sleep_percent <- dplyr::select(final_unscaled, Genotype, Results.Sleep.Light.Phase.Percent)
sig_light_sleep_percent <- dplyr::filter(sig_light_sleep_percent, Genotype %in% geno_light_sleep_percent)

#Prepare plot
p<-ggplot(sig_light_sleep_percent,aes(x = reorder(Genotype, Results.Sleep.Light.Phase.Percent, mean), y = Results.Sleep.Light.Phase.Percent))
q<-p+geom_boxplot(width=0.5)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
r<-q+theme(axis.text.x=element_text(angle=50,hjust=1))+ coord_cartesian(ylim = c(20,100))+geom_hline(aes(yintercept=mean(sig_light_sleep_percent$Results.Sleep.Light.Phase.Percent)),linetype="dashed")
r+labs(title="Light Phase",x="Genotype",y="Percent Sleep (%)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))

#Sleep Dark Phase Percent
#Load csv file containing dunnett's test results
dunnetts_dark_sleep_percent <- read.csv("dunnetts_dark_sleep_percent.csv")
#Extract genes significant in Dunnetts test into a vector and include CONTROLS (since CONTROLS will be a part of final plot). Use gsub to remove "-CONTROL" tags in Dunnetts test results
geno_dark_sleep_percent <- dunnetts_dark_sleep_percent[which(dunnetts_dark_sleep_percent[,5]<0.05),1]
geno_dark_sleep_percent <- gsub("-CONTROL","",geno_dark_sleep_percent)
geno_dark_sleep_percent <- append("CONTROL",geno_dark_sleep_percent)

#Extract genotype and sleep percent information from the final_unscaled dataset for the significant genotypes
sig_dark_sleep_percent <- dplyr::select(final_unscaled, Genotype, Results.Sleep.Dark.Phase.Percent)
sig_dark_sleep_percent <- dplyr::filter(sig_dark_sleep_percent, Genotype %in% geno_dark_sleep_percent)

#Prepare plot
p<-ggplot(sig_dark_sleep_percent,aes(x = reorder(Genotype, Results.Sleep.Dark.Phase.Percent, mean), y = Results.Sleep.Dark.Phase.Percent))
q<-p+geom_boxplot(width=0.5)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
r<-q+theme(axis.text.x=element_text(angle=50,hjust=1))+ coord_cartesian(ylim = c(0,70))+geom_hline(aes(yintercept=mean(sig_dark_sleep_percent$Results.Sleep.Dark.Phase.Percent)),linetype="dashed")
r+labs(title="dark Phase",x="Genotype",y="Percent Sleep (%)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))

#Sleep Bout lengths Mean
#Load csv file containing dunnett's test results
dunnetts_bout_length <- read.csv("dunnetts_sleep_bout_length.csv")
#Extract genes significant in Dunnetts test into a vector and include CONTROLS (since CONTROLS will be a part of final plot). Use gsub to remove "-CONTROL" tags in Dunnetts test results
geno_bout_length <- dunnetts_bout_length[which(dunnetts_bout_length[,5]<0.05),1]
geno_bout_length <- gsub("-CONTROL","",geno_bout_length)
geno_bout_length <- append("CONTROL",geno_bout_length)

#Extract genotype and sleep percent information from the final_unscaled dataset for the significant genotypes
sig_bout_length <- dplyr::select(final_unscaled, Genotype, Results.Sleep.Bout.Lengths.Mean)
sig_bout_length <- dplyr::filter(sig_bout_length, Genotype %in% geno_bout_length)

#Prepare plot
p<-ggplot(sig_bout_length,aes(x = reorder(Genotype, Results.Sleep.Bout.Lengths.Mean, mean), y = Results.Sleep.Bout.Lengths.Mean))
q<-p+geom_boxplot(width=0.5)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
r<-q+theme(axis.text.x=element_text(angle=50,hjust=1))+ coord_cartesian(ylim = c(100,800))+geom_hline(aes(yintercept=mean(sig_bout_length$Results.Sleep.Bout.Lengths.Mean)),linetype="dashed")
r+labs(title="24 Hours",x="Genotype",y="Bout Length (s)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))

#Sleep Light Phase Bout Length
#Load csv file containing dunnett's test results
dunnetts_light_bout_length <- read.csv("dunnetts_light_sleep_bout_length.csv")
#Extract genes significant in Dunnetts test into a vector and include CONTROLS (since CONTROLS will be a part of final plot). Use gsub to remove "-CONTROL" tags in Dunnetts test results
geno_light_bout_length <- dunnetts_light_bout_length[which(dunnetts_light_bout_length[,5]<0.05),1]
geno_light_bout_length <- gsub("-CONTROL","",geno_light_bout_length)
geno_light_bout_length <- append("CONTROL",geno_light_bout_length)

#Extract genotype and sleep percent information from the final_unscaled dataset for the significant genotypes
sig_light_bout_length <- dplyr::select(final_unscaled, Genotype, Results.Light.Sleep.Bout.Lengths.Mean)
sig_light_bout_length <- dplyr::filter(sig_light_bout_length, Genotype %in% geno_light_bout_length)

#Prepare plot
p<-ggplot(sig_light_bout_length,aes(x = reorder(Genotype, Results.Light.Sleep.Bout.Lengths.Mean, mean), y = Results.Light.Sleep.Bout.Lengths.Mean))
q<-p+geom_boxplot(width=0.5)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
r<-q+theme(axis.text.x=element_text(angle=50,hjust=1))+ coord_cartesian(ylim = c(100,1200))+geom_hline(aes(yintercept=mean(sig_light_bout_length$Results.Light.Sleep.Bout.Lengths.Mean)),linetype="dashed")
r+labs(title="Light Phase",x="Genotype",y="Bout Length (s)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))

#Sleep Dark Phase Bout Length
#Load csv file containing dunnett's test results
dunnetts_dark_bout_length <- read.csv("dunnetts_dark_sleep_bout_length.csv")
#Extract genes significant in Dunnetts test into a vector and include CONTROLS (since CONTROLS will be a part of final plot). Use gsub to remove "-CONTROL" tags in Dunnetts test results
geno_dark_bout_length <- dunnetts_dark_bout_length[which(dunnetts_dark_bout_length[,5]<0.05),1]
geno_dark_bout_length <- gsub("-CONTROL","",geno_dark_bout_length)
geno_dark_bout_length <- append("CONTROL",geno_dark_bout_length)

#Extract genotype and sleep percent information from the final_unscaled dataset for the significant genotypes
sig_dark_bout_length <- dplyr::select(final_unscaled, Genotype, Results.Dark.Sleep.Bout.Lengths.Mean)
sig_dark_bout_length <- dplyr::filter(sig_dark_bout_length, Genotype %in% geno_dark_bout_length)

#Prepare plot
p<-ggplot(sig_dark_bout_length,aes(x = reorder(Genotype, Results.Dark.Sleep.Bout.Lengths.Mean, mean), y = Results.Dark.Sleep.Bout.Lengths.Mean))
q<-p+geom_boxplot(width=0.5)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
r<-q+theme(axis.text.x=element_text(angle=50,hjust=1))+ coord_cartesian(ylim = c(0,500))+geom_hline(aes(yintercept=mean(sig_dark_bout_length$Results.Dark.Sleep.Bout.Lengths.Mean)),linetype="dashed")
r+labs(title="Dark Phase",x="Genotype",y="Bout Length (s)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))

#Breath Rate
#Load csv file containing dunnett's test results
dunnetts_breath_rate <- read.csv("dunnetts_breath_rate.csv")
#Extract genes significant in Dunnetts test into a vector and include CONTROLS (since CONTROLS will be a part of final plot). Use gsub to remove "-CONTROL" tags in Dunnetts test results
geno_breath_rate <- dunnetts_breath_rate[which(dunnetts_breath_rate[,5]<0.05),1]
geno_breath_rate <- gsub("-CONTROL","",geno_breath_rate)
geno_breath_rate <- append("CONTROL",geno_breath_rate)

#Extract genotype and sleep percent information from the final_unscaled dataset for the significant genotypes
sig_breath_rate <- dplyr::select(final_unscaled, Genotype, Results.Breath.Rate.During.Sleep.Mean)
sig_breath_rate <- dplyr::filter(sig_breath_rate, Genotype %in% geno_breath_rate)

#Prepare plot
p<-ggplot(sig_breath_rate,aes(x = reorder(Genotype, Results.Breath.Rate.During.Sleep.Mean, mean), y = Results.Breath.Rate.During.Sleep.Mean))
q<-p+geom_boxplot(width=0.7)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
r<-q+theme(axis.text.x=element_text(angle=50,hjust=1))+ coord_cartesian(ylim = c(1,4))+geom_hline(aes(yintercept=mean(sig_breath_rate$Results.Breath.Rate.During.Sleep.Mean)),linetype="dashed")
r+labs(title="Breath Rate",x="Genotype",y="Breath Rate")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))

#For the six piezo variables for sleep percent and bout length, create a list of unique genes that are significant for one or more of these variables
piezo_genes <- c(as.character(unique(sig_sleep_percent$Genotype)),as.character(unique(sig_light_sleep_percent$Genotype)),as.character(unique(sig_dark_sleep_percent$Genotype)),as.character(unique(sig_bout_length$Genotype)),as.character(unique(sig_light_bout_length$Genotype)),as.character(unique(sig_dark_bout_length$Genotype)))
piezo_genes <- unique(piezo_genes)

#Save the list of piezo genes in a text file
genelist_file <- file("piezo_genelist.txt")
writeLines(piezo_genes, genelist_file)
close(genelist_file)

#Function to make stacked plots
stacked_plot <- function(sleep_var){
  komp2_ana <- data.frame(final_unscaled$Genotype, sleep_var)
  komp2_ana <- dplyr::filter(komp2_ana, final_unscaled.Genotype %in% piezo_genes)
  
  total_mean<-mean(komp2_ana[,2])
  total_sd<-sd(komp2_ana[,2])
  
  sleep_var_genes <- vector()
  for (i in 1:length(komp2_ana[,1])){
    if (sleep_var == final_unscaled$Results.Sleep.Daily.Percent) {sleep_var_genes <- geno_sleep_percent}
    else if (sleep_var == final_unscaled$Results.Sleep.Light.Phase.Percent) {sleep_var_genes <- geno_light_sleep_percent}
    else if (sleep_var == final_unscaled$Results.Sleep.Dark.Phase.Percent) {sleep_var_genes <- geno_dark_sleep_percent}
    else if (sleep_var == final_unscaled$Results.Sleep.Bout.Lengths.Mean) {sleep_var_genes <- geno_bout_length}
    else if (sleep_var == final_unscaled$Results.Light.Sleep.Bout.Lengths.Mean) {sleep_var_genes <- geno_light_bout_length}
    else if (sleep_var == final_unscaled$Results.Dark.Sleep.Bout.Lengths.Mean) {sleep_var_genes <- geno_dark_bout_length}
    else {sleep_var_genes <- geno_breath_rate}
    
    komp2_ana[,3] <- ifelse(komp2_ana$final_unscaled.Genotype %in% sleep_var_genes, "sig", "ns")
    colnames(komp2_ana)[3] <- "Significance"
  }
  
  #Prepare plot
  color_palette <- c("cornflowerblue","red")
  p<-ggplot(komp2_ana,aes(x = reorder(komp2_ana[,1]), y = komp2_ana[,2], fill = factor(komp2_ana$Significance))) +geom_hline(yintercept=total_mean-total_sd,size=1,color="red")+geom_hline(yintercept=total_mean+total_sd,size=1,color="red")
  q<-p+geom_boxplot(outlier.color = NA, coef=0)+theme_bw()+theme(panel.grid = element_blank())+theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5), axis.ticks.y = element_blank(), legend.position = "none")
  r<-q+geom_hline(aes(yintercept=mean(komp2_ana[,2])),linetype="dashed") + coord_fixed(ratio = 0.3)
  s<-r + scale_fill_manual(values = color_palette) #+ opts(plot.margin = unit(c(5,2,5,2),"cm"))
  if (sleep_var == final_unscaled$Results.Sleep.Daily.Percent) {s+labs(title="A. 24 Hours",x="Genotype",y="Daily Sleep Percent (%)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"), axis.title.y = element_blank())+scale_y_continuous(expand = c(0,0), limits = c(30, 60))+coord_flip()}
  else if (sleep_var == final_unscaled$Results.Sleep.Light.Phase.Percent) {s+labs(title="B. Light Phase",x="Genotype",y="Light Phase Sleep Percent (%)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"),axis.title.y = element_blank())+scale_y_continuous(expand = c(0,0), limits = c(40, 80))+coord_flip()}
  else if (sleep_var == final_unscaled$Results.Sleep.Dark.Phase.Percent) {s+labs(title="C. Dark Phase",x="Genotype",y="Dark Phase Sleep Percent (%)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"),axis.title.y = element_blank())+scale_y_continuous(expand = c(0,0), limits = c(10, 40))+coord_flip()}
  else if (sleep_var == final_unscaled$Results.Sleep.Bout.Lengths.Mean) {s+labs(title="D. 24 Hours",x="Genotype",y="Sleep Bout Lengths Mean (s)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"),axis.text.y = element_blank())+scale_y_continuous(expand = c(0,0), limits = c(200, 600))+coord_flip()}
  else if (sleep_var == final_unscaled$Results.Light.Sleep.Bout.Lengths.Mean) {s+labs(title="E. Light Phase",x="Genotype",y="Light Sleep Bout Lengths Mean (s)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"),axis.title.y = element_blank())+scale_y_continuous(expand = c(0,0), limits = c(200, 800))+coord_flip()}
  else if (sleep_var == final_unscaled$Results.Dark.Sleep.Bout.Lengths.Mean) {s+labs(title="F. Dark Phase",x="Genotype",y="Dark Sleep Bout Lengths Mean (s)")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"),axis.title.y = element_blank())+scale_y_continuous(expand = c(0,0), limits = c(100, 400))+coord_flip()}
  else s+labs(title="24 Hours",x="Genotype",y="Breath Rate")+theme(title=element_text(face="bold"),axis.title=element_text(face="bold"))+scale_y_continuous(expand = c(0,0), limits = c(0, 10))+coord_flip()
}


stacked_plot(final_unscaled$Results.Sleep.Daily.Percent)
stacked_plot(final_unscaled$Results.Sleep.Light.Phase.Percent)
stacked_plot(final_unscaled$Results.Sleep.Dark.Phase.Percent)
stacked_plot(final_unscaled$Results.Sleep.Bout.Lengths.Mean)
stacked_plot(final_unscaled$Results.Light.Sleep.Bout.Lengths.Mean)
stacked_plot(final_unscaled$Results.Dark.Sleep.Bout.Lengths.Mean)
