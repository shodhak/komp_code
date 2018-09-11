rm(list = ls())

#load the complete unscaled dataset
final_unscaled <- read.csv("final_unscaled.csv")

#Prepare datasets for sex based analysis
#Make table containing frequencies of male and female mice
sex_freq <- table(final_unscaled$Genotype, final_unscaled$Sex)
#Extract genotypes containing at least 3 male and 3 female mice
sex_analysis_genes <- vector()
for(i in 1:length(unique(final_unscaled$Genotype))){
  if (sex_freq[i,1] > 2 && sex_freq[i,2] > 2) {
    sex_analysis_genes <- c(sex_analysis_genes, rownames(sex_freq)[i])
  }  
}
#Genotypes satisfying the criterion
length(sex_analysis_genes)

#Extract data for males from final_unscaled dataset
males_unscaled <- final_unscaled %>%
  filter(final_unscaled$Sex == "Male")
#Extract data for females from final_unscaled dataset
females_unscaled <- final_unscaled %>%
  filter(final_unscaled$Sex == "Female")

#Extract relevant data for male and female mice
males_unscaled <- filter(males_unscaled, Genotype %in% sex_analysis_genes)
females_unscaled <- filter(females_unscaled, Genotype %in% sex_analysis_genes)

#Store male and female sample data in separate csv files
write.csv(males_unscaled,"males_unscaled.csv")
write.csv(females_unscaled,"females_unscaled.csv")
