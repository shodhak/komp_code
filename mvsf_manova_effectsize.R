rm(list=ls())
library(DescTools)

#Load male and female unscaled datasets
males_unscaled <- read.csv("males_unscaled.csv")
females_unscaled <- read.csv("females_unscaled.csv")

#Sex Analysis through two way ANOVA (MANOVA and Factorial)
#Factorial anova for identifying genes significant for sex
#Create dataset for genes that are being included in the analysis by sex
sex_genes_unscaled <- bind_rows(males_unscaled, females_unscaled)
#Perform crosstabulation
with(sex_genes_unscaled, table(Genotype, Sex))
sleep_numbers <- scale(sex_genes_unscaled[,23:29])
test_manova_sex = manova(sleep_numbers ~ Genotype * Sex, data = sex_genes_unscaled)
summary.manova(test_manova_sex) #MANOVA
summary.aov(test_manova_sex) #Factorial ANOVA

#Calculating Factorial ANOVA significance and effect sizes
sex_effect_size <- function(sex_piezo_var){
  model = lm(sex_piezo_var ~ Genotype*Sex, data=sex_genes_unscaled)
  EtaSq(model, anova = TRUE)
}

#Refer to Cohen's guidelines when analyzing effect size (Acc to him, small = 0.01, medium = 0.059, large = 0.138)
sex_effect_size(sex_genes_unscaled$Results.Sleep.Daily.Percent) #Medium effect size for intercation (0.06)
sex_effect_size(sex_genes_unscaled$Results.Sleep.Light.Phase.Percent) #Medium effect size for interaction (.05)
sex_effect_size(sex_genes_unscaled$Results.Sleep.Dark.Phase.Percent) #Medium effect size for intercation (0.05)
sex_effect_size(sex_genes_unscaled$Results.Sleep.Bout.Lengths.Mean) #Medium effect size for intercation (0.05)
sex_effect_size(sex_genes_unscaled$Results.Light.Sleep.Bout.Lengths.Mean) #Medium effect size for intercation (0.04)
sex_effect_size(sex_genes_unscaled$Results.Dark.Sleep.Bout.Lengths.Mean) #Medium effect size for intercation (0.05)
sex_effect_size(sex_genes_unscaled$Results.Breath.Rate.During.Sleep.Mean) ##Medium effect size for intercation (0.04)