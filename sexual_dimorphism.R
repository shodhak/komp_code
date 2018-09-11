rm(list=ls())

library(reshape2)
library(grid)
library(ggplot2)

controls_unscaled <- read.csv("controls_unscaled.csv")
controls_scaled <- read.csv("controls_scaled.csv")

variable <- vector()
mean_Female <- vector()
mean_Male <- vector()
p_value <- vector()

#Make a dataframe with mean and sd of sleep variables for male and female mice
#Also include p-value from t-test to check if the difference in values is significant between males and females
for(i in 1:7){
  t <- t.test(controls_unscaled[,i+20]~controls_unscaled$Sex)
  variable[i]<-colnames(controls_unscaled)[i+20]
  mean_Female[i] <- paste(round(t$estimate[[1]],2),"+-",round(sd(controls_unscaled[,i+20][controls_unscaled$Sex == "Female"])/sqrt(summary(controls_scaled$Sex)[[1]]),2))
  mean_Male[i] <- paste(round(t$estimate[[2]],2),"+-",round(sd(controls_unscaled[,i+20][controls_unscaled$Sex == "Male"])/sqrt(summary(controls_scaled$Sex)[[2]]),2))
  p_value[i] <- round(t$p.value,6)
}
#Store dataframe within a variable
mvsf_pvals <- data.frame(variable,mean_Female,mean_Male,p_value)
#Store dataframe as csv file
write.csv(mvsf_pvals,"male_vs_fem_pvals.csv")

#Sleep Percent Plot comparing male and female controls
control_percent_plot<-dplyr::select(controls_unscaled,Sex,Results.Sleep.Daily.Percent,Results.Sleep.Light.Phase.Percent,Results.Sleep.Dark.Phase.Percent)
#Rename columns
colnames(control_percent_plot)[2]<-"24 Hours"
colnames(control_percent_plot)[3]<-"Light Phase"
colnames(control_percent_plot)[4]<-"Dark Phase"

sleep_melt<-melt(control_percent_plot)
sleep_summary<-group_by(sleep_melt,Sex,variable)
sleep_sum<-summarise(sleep_summary,mean=mean(value),sd=sd(value),se=0)

#Number of male and female mice for calculating standard error
summary(controls_unscaled$Sex)

#Calculate Std Error
sleep_sum[1,5] <- sleep_sum[1,4]/sqrt(918)
sleep_sum[2,5] <- sleep_sum[2,4]/sqrt(918)
sleep_sum[3,5] <- sleep_sum[3,4]/sqrt(918)
sleep_sum[4,5] <- sleep_sum[4,4]/sqrt(894)
sleep_sum[5,5] <- sleep_sum[5,4]/sqrt(894)
sleep_sum[6,5] <- sleep_sum[6,4]/sqrt(894)

#Male vs female sleep percent plot
my_grob = grobTree(textGrob(c("***","***","***"), x=c(0.13,0.445,0.76) , y=c(0.53,0.73,0.37), hjust=0,gp=gpar(fontsize=20,face="bold")))
#Bar Graph
p<-ggplot(sleep_sum,aes(x=variable,y=mean,fill=factor(Sex)))+labs(y="Percent Sleep",x=element_blank())
q<-p+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),stat="identity",position=position_dodge(0.5),width=0.25)#,face="bold"
r<-q+theme_bw()+theme(panel.grid = element_blank(),legend.title=element_blank(),legend.position="bottom",legend.key.height=unit(0.5,"cm"),legend.key.width=unit(0.50,"cm"),axis.text=element_text(face="bold")) + scale_y_continuous(expand = c(0,0), limits = c(0, 100)) + scale_fill_manual(values = c("grey80", "grey20"))
r+geom_bar(stat="identity",position="dodge",width=0.5)+annotation_custom(my_grob)

#Bout Length plot comparing male and female controls
control_bout_plot<-dplyr::select(controls_unscaled,Sex,Results.Sleep.Bout.Lengths.Mean,Results.Light.Sleep.Bout.Lengths.Mean,Results.Dark.Sleep.Bout.Lengths.Mean)
#Rename columns
colnames(control_bout_plot)[2]<-"24 Hours"
colnames(control_bout_plot)[3]<-"Light Phase"
colnames(control_bout_plot)[4]<-"Dark Phase"

sleep_melt<-melt(control_bout_plot)
sleep_summary<-group_by(sleep_melt,Sex,variable)
sleep_sum<-summarise(sleep_summary,mean=mean(value),sd=sd(value),se=0)

#Calculate Std Error
sleep_sum[1,5] <- sleep_sum[1,4]/sqrt(918)
sleep_sum[2,5] <- sleep_sum[2,4]/sqrt(918)
sleep_sum[3,5] <- sleep_sum[3,4]/sqrt(918)
sleep_sum[4,5] <- sleep_sum[4,4]/sqrt(894)
sleep_sum[5,5] <- sleep_sum[5,4]/sqrt(894)
sleep_sum[6,5] <- sleep_sum[6,4]/sqrt(894)

#Male vs female bout length plot
my_grob = grobTree(textGrob(c("***","***","***"), x=c(0.12,0.44,0.76) , y=c(0.55,0.76,0.37), hjust=0,gp=gpar(fontsize=20,face="bold")))
#Bar Graph
p<-ggplot(sleep_sum,aes(x=variable,y=mean,fill=factor(Sex)))+labs(y="Bout Length (s)",x=element_blank())
q<-p+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),stat="identity",position=position_dodge(0.5),width=0.25)#,face="bold"
r<-q+theme_bw()+theme(panel.grid = element_blank(),legend.title=element_blank(),legend.position="bottom",legend.key.height=unit(0.5,"cm"),legend.key.width=unit(0.50,"cm"),axis.text=element_text(face="bold")) + scale_y_continuous(expand = c(0,0), limits = c(0, 1000)) + scale_fill_manual(values = c("grey80", "grey20"))
r+geom_bar(stat="identity",position="dodge",width=0.5)+annotation_custom(my_grob)
