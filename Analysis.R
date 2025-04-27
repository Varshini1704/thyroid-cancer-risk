#Loading data
data=read.csv(file.choose(),header=TRUE)
head(data)

#SUMMARY STATISTICS 

summary(data$Age) #Age summary

freq=table(data$Gender, data$Diagnosis) 
addmargins(freq) #Gender and diagnosis distribution

table(data$Thyroid_Cancer_Risk) #Risk level count
prop.table(table(data$Thyroid_Cancer_Risk))*100 #Percentage

#VISUALISATION

library(ggplot2)
library(dplyr)

ggplot(data, aes(x=Diagnosis, fill=Thyroid_Cancer_Risk))+ 
geom_bar(position="dodge")+
labs(title="DIAGNOSIS DISTRIBUTION") #Bar chart

freq=table(data$Smoking)
percent=round(100*freq/sum(freq),1)
labels=paste(names(freq),percent,"%") 
pie(freq,labels=labels,main="Smoker's Distribution",col=rainbow(length(freq))) #Pie chart

#MULTIPLE LINEAR REGRESSION

#Loading data
df=read.csv(file.choose(),header=TRUE)
head(data)

library(readr)
library(dplyr)

# Convert character columns to factors, then to numeric
df_encoded <- df %>%
  mutate(across(where(is.character), ~ as.numeric(as.factor(.))))
# Compute correlation matrix (removing Patient_ID)
cor_matrix <- cor(df_encoded %>% select(-Patient_ID), use = "complete.obs")

# Get correlations with Thyroid_Cancer_Risk
risk_correlations <- cor_matrix[, "Thyroid_Cancer_Risk"]

# Sort by absolute correlation
risk_correlations <- sort(abs(risk_correlations), decreasing = TRUE)
risk_correlations <- risk_correlations[names(risk_correlations) != "Thyroid_Cancer_Risk"]

print(risk_correlations)

library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

library(nnet)

# Convert to factor
df$Thyroid_Cancer_Risk <- as.factor(df$Thyroid_Cancer_Risk)

# Fit multinomial logistic regression using top features
model_multinom <- multinom(Thyroid_Cancer_Risk ~ Diagnosis + Radiation_Exposure + Family_History + Ethnicity, data = df)

# View summary
summary(model_multinom)
