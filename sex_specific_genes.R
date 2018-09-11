rm(list=ls())
library(xlsx)

#Load dunnetts Test output for male and female datasets

#Read male csv files
male_sleep_percent <- read.csv("male_dunnetts_sleep_percent.csv")
male_light_sleep_percent <- read.csv("male_dunnetts_light_sleep_percent.csv")
male_dark_sleep_percent <- read.csv("male_dunnetts_dark_sleep_percent.csv")
male_sleep_bout_length <- read.csv("male_dunnetts_sleep_bout_length.csv")
male_light_sleep_bout_length <- read.csv("male_dunnetts_light_sleep_bout_length.csv")
male_dark_sleep_bout_length <- read.csv("male_dunnetts_dark_sleep_bout_length.csv")
male_breath_rate <- read.csv("male_dunnetts_breath_rate.csv")

#Read female csv files
female_sleep_percent <- read.csv("female_dunnetts_sleep_percent.csv")
female_light_sleep_percent <- read.csv("female_dunnetts_light_sleep_percent.csv")
female_dark_sleep_percent <- read.csv("female_dunnetts_dark_sleep_percent.csv")
female_sleep_bout_length <- read.csv("female_dunnetts_sleep_bout_length.csv")
female_light_sleep_bout_length <- read.csv("female_dunnetts_light_sleep_bout_length.csv")
female_dark_sleep_bout_length <- read.csv("female_dunnetts_dark_sleep_bout_length.csv")
female_breath_rate <- read.csv("female_dunnetts_breath_rate.csv")

#The dunnett test output datasets contain genotypes from which at least 3 males and 3 females were recorded for sleep
#Remove "-CONTROL" from the names of genotypes and store the ourpur in an array
male_genotypes <- gsub("-CONTROL","",male_sleep_percent$X)

#Function to specify p-values less than 0.05 and the direction of change.
#Input argument is the piezo variable of interest
sex_sig <- function(piezo_var) {
  #Limit the number of decimal places to 2
  is.num <- sapply(piezo_var, is.numeric)
  piezo_var[is.num] <- lapply(piezo_var[is.num], round, 2)
  temp_vec <- vector()
  for ( i in 1:length(piezo_var$X))  
    if (piezo_var$pval[i] < 0.05 && piezo_var$diff[i] > 0){
      temp_vec[i] <- "p<0.05 \u2191" #Unicode for upper direction arrow
    }
  else if (piezo_var$pval[i] <= 0.05 && piezo_var$diff[i] < 0){
    temp_vec[i] <- "p<0.05 \u2193" #Unicode for lower direction arrow
  }
  else {
    temp_vec[i] <- piezo_var$pval[i]
  }
  return(data.frame(piezo_var$X,temp_vec))
}  

#Prepare dataframe containing p-values for each genotype for bothe males and females
sex_significant_genes <- data.frame(male_genotypes,
                                    sex_sig(male_sleep_percent)$temp_vec,
                                    sex_sig(male_light_sleep_percent)$temp_vec,
                                    sex_sig(male_dark_sleep_percent)$temp_vec,
                                    sex_sig(male_sleep_bout_length)$temp_vec,
                                    sex_sig(male_light_sleep_bout_length)$temp_vec,
                                    sex_sig(male_dark_sleep_bout_length)$temp_vec,
                                    sex_sig(male_breath_rate)$temp_vec,
                                    sex_sig(female_sleep_percent)$temp_vec,
                                    sex_sig(female_light_sleep_percent)$temp_vec,
                                    sex_sig(female_dark_sleep_percent)$temp_vec,
                                    sex_sig(female_sleep_bout_length)$temp_vec,
                                    sex_sig(female_light_sleep_bout_length)$temp_vec,
                                    sex_sig(female_light_sleep_bout_length)$temp_vec,
                                    sex_sig(female_breath_rate)$temp_vec)

#Create a list of sex specific genes "without breath rate"
#sex_significant_genes <- data.frame(male_genotypes,
#                                   sex_sig(male_sleep_percent)$temp_vec,
#                                  sex_sig(male_light_sleep_percent)$temp_vec,
#                                 sex_sig(male_dark_sleep_percent)$temp_vec,
#                                sex_sig(male_sleep_bout_length)$temp_vec,
#                               sex_sig(male_light_sleep_bout_length)$temp_vec,
#                              sex_sig(male_dark_sleep_bout_length)$temp_vec,
#                             sex_sig(female_sleep_percent)$temp_vec,
#                            sex_sig(female_light_sleep_percent)$temp_vec,
#                           sex_sig(female_dark_sleep_percent)$temp_vec,
#                          sex_sig(female_sleep_bout_length)$temp_vec,
#                         sex_sig(female_light_sleep_bout_length)$temp_vec,
#                        sex_sig(female_light_sleep_bout_length)$temp_vec)

#Function to extract indexes of genes with p<0.05
#Input arguments are the dataframe with p-values and column number (cn)
sig_genes <- function(df,cn){
  temp <- df[which(grepl("0.05",df[,cn])),1]
  return(temp)
}

#Extract indexes of significant genotypes (p<0.05) for all piezo variables
sig_gene_combined <- vector()
for(i in 2:ncol(sex_significant_genes)){
  sig_gene_combined <- c(sig_gene_combined, sig_genes(sex_significant_genes,i))
}

#Final dataframe with only significant genes. Along with p<0.05, it also tells about the direction of change.
sig_gene_combined <- unique(sig_gene_combined)
sex_specific_genes <- sex_significant_genes[sig_gene_combined,]
colnames(sex_specific_genes)[1] <- "Genotypes"
write.csv(sex_specific_genes,"sex_specific_genes_original.xls")

#Make a list of sex specific genes in the form of vector
sex_genes <- as.character(sex_specific_genes$Genotypes)
