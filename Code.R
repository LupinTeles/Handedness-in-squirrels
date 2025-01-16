#Import data
data<- read.csv("CombinedCSV.csv", header= TRUE, stringsAsFactors=FALSE)

#View data
str(data)

#Upload CSV with selected columns
Variables<- read.csv("SelectedColumns.csv", header=TRUE, stringsAsFactors = FALSE)

#View data
str(Variables)
