#this code just represents some of the attempts at performing PCA in R
#The PCA analysis for the paper was done in JMP

#Principal Component Analysis
#File to perform PCA in JMP
write.csv(final_scaled,"KOMP2_pca.csv")

komp_aggregate <- aggregate(final_unscaled[, 21:27], list(final_unscaled$Genotype), mean)
pca_df <- final_scaled[,21:26]
#Perform PCA
komp_pca <- princomp(pca_df)

melted <- melt(komp_pca$loadings[,1:3])
barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value, fill=Var1), stat="identity") +
  facet_wrap(~Var2) 

#Calculate percent variablility in the data explained by each principal component
per_var <- format(round(komp_pca$sdev^2/sum(komp_pca$sdev^2)*100,2), scientific = FALSE)
pc_names <- c("PC1","PC2","PC3","PC4","PC5","PC6")
var_exp <- cbind.data.frame(pc_names,per_var)

plot_varexp <- ggplot(data = var_exp) + geom_bar(aes(x = pc_names, y = per_var), stat = "identity") +
  xlab("Principal Components") + ylab("Variance Explained (%)")

#Extract principal components and make a data frame
komp_pc_values <- data.frame(final_scaled[,5], komp_pca$scores)
colnames(komp_pc_values)[1] <- "Genotype"

#Perform anova and dunnetts test for first principal component
pc1_anova <- aov(Comp.1 ~ Genotype, komp_pc_values)
summary(pc1_anova)

pc1_dunnett <- DunnettTest(Comp.1 ~ Genotype, control = "CONTROL", data = komp_pc_values)
write.csv(pc1_dunnett$CONTROL, "pc1_dunnett.csv")
pc1_dunnett <- read.csv("pc1_dunnett.csv")
geno_pc1_dunnett <- pc1_dunnett[which(pc1_dunnett[,5]<0.05),1]
geno_pc1_dunnett <- gsub("-CONTROL","",geno_pc1_dunnett) #List of genes for pc1

pc2_dunnett <- DunnettTest(Comp.2 ~ Genotype, control = "CONTROL", data = komp_pc_values)
write.csv(pc2_dunnett$CONTROL, "pc2_dunnett.csv")
pc2_dunnett <- read.csv("pc2_dunnett.csv")
geno_pc2_dunnett <- pc2_dunnett[which(pc2_dunnett[,5]<0.05),1]
geno_pc2_dunnett <- gsub("-CONTROL","",geno_pc2_dunnett) #List of genes for pc1

pc3_dunnett <- DunnettTest(Comp.3 ~ Genotype, control = "CONTROL", data = komp_pc_values)
write.csv(pc3_dunnett$CONTROL, "pc3_dunnett.csv")
pc3_dunnett <- read.csv("pc3_dunnett.csv")
geno_pc3_dunnett <- pc3_dunnett[which(pc3_dunnett[,5]<0.05),1]
geno_pc3_dunnett <- gsub("-CONTROL","",geno_pc3_dunnett) #List of genes for pc1

pca_genes <- unique(c(geno_pc1_dunnett, geno_pc2_dunnett, geno_pc3_dunnett))
komp_pca$call
komp_pca_scores <- komp_pca$scores

#Create data frame for individual contributions by each genotype to PC1
komp_pca1 <- prcomp(komp_aggregate[,2:7], scale=TRUE)

fviz_pca_var(komp_pca1)

fviz_contrib(komp_pca, choice = "ind")

#scatter(komp_pca)
