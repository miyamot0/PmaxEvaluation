# Shawn Gilroy
# GPL-V3
# Pmax Evaluation

# Correlations, to the sixth decimal
correlationTable<-round(cor(compareFrame), 6)

# Print out matrix
correlationTableClean<-correlationTable
correlationTableClean[lower.tri(correlationTable, diag=TRUE)]<-""
correlationTableClean<-as.data.frame(correlationTableClean)
correlationTableClean