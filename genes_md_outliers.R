rm(list=ls())

library(DescTools)

final_unscaled <- read.csv("final_unscaled.csv")

#Perform multivariate outlier analysis for genes
#Calculate mean values for each piezo variable for each genotype
piezo_means <- aggregate(final_unscaled[, 21:27], list(final_unscaled$Genotype), mean)
mahadist <- Moutlier(piezo_means[,2:7])
mahadist$cutoff
length(which(mahadist$md > mahadist$cutoff))
genes_md_outliers <- piezo_means[which(mahadist$md > mahadist$cutoff),]
genes_md_outliers <- genes_md_outliers$Group.1
genes_md_outliers <- as.character(genes_md_outliers)