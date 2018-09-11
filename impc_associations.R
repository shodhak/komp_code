impc_dataset <- read.csv("impc_dataset.csv")
impc_dataset$mp_term <- as.character(impc_dataset$mp_term)

mp_term <- vector()
for (i in 1:length(impc_dataset$marker_symbol)){
  mp_term <- c(mp_term, strsplit(as.character(impc_dataset$mp_term[i]),'\\|')[[1]])
}
mp_term <- unique(mp_term)

mp_data <- data.frame(matrix(NA,length(impc_dataset$mp_term),length(mp_term)))  
rownames(mp_data) <- impc_dataset$marker_symbol  
colnames(mp_data) <- mp_term  
for (i in 1:length(mp_data$hyperactivity)){
  term <- strsplit(impc_dataset$mp_term[i],'\\|')[[1]]
  for (k in 1:length(mp_term)){
    if (colnames(mp_data)[k] %in% term){
      mp_data[i,k] = 1
    }
    else {
      mp_data[i,k] = 0
    }
  }
}  

mp_sums <- data.frame(matrix(NA, length(mp_term), 2))
mp_sums[,1] <- colnames(mp_data)  
for (i in 1:length(mp_term)){
  mp_sums[i,2] <- sum(mp_data[,i])
}  
colnames(mp_sums) <- c("mp_term","Sum")
mp_sums <- mp_sums[order(-mp_sums$Sum),]
write.csv(mp_sums,"mp_sums.csv")

#Arrange columns in mp_data by the values in mp_sums (highest to lowest)
mp_data <- mp_data[,mp_sums$mp_term]
write.csv(mp_data,"mp_data.csv")

gene_mp_sums <- data.frame(matrix(NA,length(impc_dataset$marker_symbol),2))
gene_mp_sums[,1] <- rownames(mp_data)
colnames(gene_mp_sums) <- c("Genotype","Sum")
for (i in 1:100){
  gene_mp_sums[i,2] <- sum(mp_data[i,])
}
gene_mp_sums <- gene_mp_sums[order(-gene_mp_sums$Sum),]
write.csv(gene_mp_sums, "gene_mp_sums.csv")
