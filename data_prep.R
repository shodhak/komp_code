rm(list=ls())

library(tidyr)
library(dplyr)
library(chemometrics)

#set the working directory
#load the master dataset
all_data <- read.csv("sleep_all.csv")

#Calculate total number of obersvations and print them on the screen
cat(nrow(all_data), " observations of ",ncol(all_data)," variables")
#Number of male and female mice
summary(all_data$Sex)

#Summary of data with class of variables
str(all_data)

#Drop all sleep variables except for sleep percent, bout length, and breath rate
drop <- c("Results.Daily.Sleep", "Results.Light.Dark.Sleep", "Results.Wake.State.Data..text.", "Results.Wake.State.Image", "Results.Daily.Sleep.1", "Results.Comments.1", "Results.Light.Dark.Sleep.1", "Results.Wake.State.Data..text..1", "Results.Wake.State.Image.1")
all_data <- all_data %>%
  select(-one_of(drop))

#Identify variables for which more than 50% of the data is missing
#Calculate percent missing data
m1 <- colMeans(is.na(all_data))*100
#Split variables with more than and less than 50% missing data
m2 <- split(m1, m1>50)
m2 <- as.data.frame(m2$`TRUE`)
names(m2) <- "Percent Missing"
#Print variables with more than 50% missing data
print(m2)
#Print variables with less than 50% missing data
print(m1)
#Drop variables that have more than 50% missing data
all_data <- all_data %>%
  select(-one_of(names(which(colMeans(is.na(all_data))*100 > 50))))

#Variables not directly related to the analysis were dropped
not_analyzed <- c("Results.Diurnal.Wake.Ratio.Median", "Results.Peak.Wake.wrt.Dark.Onset.Median", "Results.Breath.Rate.During.Sleep.Standard.Deviation", "Results.Dark.Sleep.Bout.Lengths.Standard.Deviation", "Results.Light.Sleep.Bout.Lengths.Standard.Deviation", "Results.Sleep.Bout.Lengths.Standard.Deviation", "Results.Light.Onset.Wake.Median")
all_data <- all_data %>%
  select(-one_of(not_analyzed))

#Data from only those mice was included for which it was verified that they have completed the phenotyping pipeline,
#were euthanized and the data was approved to be included in the impc database
all_data <- all_data %>%
  filter(all_data$Exit.Reason == "Finished Pipeline")

all_data <- all_data %>%
  filter(all_data$Status == "Complete")

all_data <- all_data %>%
  filter(all_data$Life.Status == "Euthanized")

all_data <- all_data %>%
  filter(all_data$Study == "KOMP Phenotype")

ra <- c("Submitted to DCC","Approved")
all_data <- all_data %>%
  filter(all_data$Review.Action %in% ra)

#Only sleep recordings with quality metric higher than 0.6 were included in the analysis
all_data <- all_data %>%
  filter(all_data$Results.Data.Confidence.Level >= 0.6)

#Number of observations in the final dataset that was used for analysis
cat(nrow(all_data), " observations of ",ncol(all_data)," variables")
#Number fo male and female mice
summary(all_data$Sex)

#Processing gene names
all_data[,29] <- NA
names(all_data)[29] <- "Zygosity"

#In the raw dataset the genotype and zygosity of mice are included as single variable
#Separate genotype and zygosity into two different variables
all_data$Genotype <- as.character(all_data$Genotype)

for(i in 1:nrow(all_data)){
  breakdown <- strsplit(all_data$Genotype[i],"\\s")
  all_data$Genotype[i] <- breakdown[[1]][1]
  all_data$Zygosity[i] <- breakdown[[1]][2]  
  all_data$Genotype[i] <- gsub("&lt.*","",all_data$Genotype[i],perl = TRUE)
}
#Values for zygosity in the data
unique(all_data$Zygosity)
#Exclude homozygous and heterozygous positive/dominant mutants, and the ones with error and unknown information, include everything else
zygokeep <- c("-/-","A7","AX-9","",NA)
all_data <- all_data %>%
  filter (all_data$Zygosity %in% zygokeep)

#Unknown genotype and littermate were not included in the analysis data
all_data <- all_data[-which(all_data$Genotype=="B6N(Cg)-Tmem181atm1b(EUCOMM)Wtsi/J&gt;"),]
all_data <- all_data[-which(all_data$Genotype=="LITTERMATE"),]

#Remove column for zygosity since it won't be required anymore
all_data <- all_data[,-29]

#Rearrange columns
all_data <- all_data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,22,25,26,27,24,21,23,19,28)]

#Scale all the sleep varibles in KOMP data
komp_scaled <- all_data
komp_scaled[,21:27] <- scale(komp_scaled[,21:27])

#Plot distributions of sleeo variables
par(mfrow=c(3, 3))
colnames <- dimnames(komp_scaled)[[2]]
for (i in 21:27) {
  hist(komp_scaled[,i], xlim=c(-8, 8), ylim = c(0,0.45), main=colnames[i], probability=TRUE, col="gray", border="white", cex.main = 0.8)
  d <- density(komp_scaled[,i])
  lines(d, col="red")
}

#Make separate datasets for comtrol mice
controls_scaled <- komp_scaled[grep("CONTROL*",komp_scaled$Genotype),]
controls_unscaled <- all_data[grep("CONTROL*",all_data$Genotype),]

#Identify outliers among the control mice
mahadist <- Moutlier(controls_scaled[,21:27])
mahadist$cutoff
length(which(mahadist$md > mahadist$cutoff))
#Remove outliers with mahalanobis distance above threshold
controls_scaled <- controls_scaled[-which(mahadist$md > mahadist$cutoff),]
controls_unscaled <- controls_unscaled[-which(mahadist$md > mahadist$cutoff),]

#Make separate scaled and unscaled datasets for knockout mice
ko_scaled <- komp_scaled %>%
  filter(komp_scaled$Genotype != "CONTROL")

ko_unscaled <- all_data %>%
  filter(all_data$Genotype != "CONTROL")

#Make complete scaled and unscaled datasets for control and knockout (ko) mice, both combined and individually
final_scaled <- rbind(controls_scaled, ko_scaled)
final_unscaled <- rbind(controls_unscaled, ko_unscaled)
final_unscaled$Genotype <- as.factor(final_unscaled$Genotype)

#Total number of mice in the final dataset
nrow(final_scaled)
#Number of male and female mice
summary(final_scaled$Sex)
#Calculate mean for sleep variables
aggregate(controls_unscaled[, 21:27], list(controls_unscaled$Genotype), mean)
#Calculate standard deviation (sd) for sleep variables
aggregate(controls_unscaled[, 21:27], list(controls_unscaled$Genotype), sd)

#Total number of genotypes in the dataset
length(unique(final_scaled$Genotype))-1 #because one of them is control

#save this final data into a new file 
write.csv(final_scaled, "final_scaled.csv")
write.csv(final_unscaled, "final_unscaled.csv")
#Save data for control mice
write.csv(controls_scaled, "controls_scaled.csv")
write.csv(controls_unscaled, "controls_unscaled.csv")
#Save data for ko mice
write.csv(ko_scaled, "ko_scaled.csv")
write.csv(ko_unscaled, "ko_unscaled.csv")
