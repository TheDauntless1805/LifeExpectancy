#===============================================================================
  
  
#=========================Exploratory Data Analysis=============================
getwd()
setwd("/Users/anchalgarg/Documents/Praxis/AML")

data1 = read.csv("/Users/anchalgarg/Downloads/Life Expectancy Data.csv")


#========================================================================================================
# SOME INITIAL STEPS WITH DATA
#========================================================================================================
# Checking the dimension of the data

dim(data1)

# NO. of rows and columns in data

nrow(data1)
ncol(data1)

#Display of the loaded data
head(data1)
summary(data1)

# Complete view of the data
View(data1)


##Checking null values in the data
colSums(is.na(data1))# this will give null values of all the coulmns in the data.

## Checking the percentage of missing value data.
colSums(is.na(data1)/lengths(data1)*100)


## Data contains lots of missing values lets impute those values

str(data1)
for (i in 1:ncol(data1))
{
  print(names(data1)[i])
}

# create a copy of the dataset

#data1 = data

#A.	Impute the missing value by mean.

#* median imputation of Diphtheria as data is highly skewed for this variable 
#* median is higher than mean
data1$Life.expectancy[is.na(data1$Life.expectancy)] = mean(data1$Life.expectancy[!is.na(data1$Life.expectancy)])
data1$Adult.Mortality[is.na(data1$Adult.Mortality)] = mean(data1$Adult.Mortality[!is.na(data1$Adult.Mortality)])
data1$Alcohol[is.na(data1$Alcohol)] = mean(data1$Alcohol[!is.na(data1$Alcohol)])
data1$BMI[is.na(data1$BMI)] = mean(data1$BMI[!is.na(data1$BMI)])
data1$Polio[is.na(data1$Polio)] = median(data1$Polio[!is.na(data1$Polio)])
data1$Total.expenditure[is.na(data1$Total.expenditure)] = mean(data1$Total.expenditure[!is.na(data1$Total.expenditure)])
data1$Diphtheria[is.na(data1$Diphtheria)] = median(data1$Diphtheria[!is.na(data1$Diphtheria)])
data1$Schooling[is.na(data1$Schooling)] = mean(data1$Schooling[!is.na(data1$Schooling)])
data1$thinness..1.19.years[is.na(data1$thinness..1.19.years)] = mean(data1$thinness..1.19.years[!is.na(data1$thinness..1.19.years)])
data1$thinness.5.9.years[is.na(data1$thinness.5.9.years)] = mean(data1$thinness.5.9.years[!is.na(data1$thinness.5.9.years)])
data1$Hepatitis.B[is.na(data1$Hepatitis.B)] = median(data1$Hepatitis.B[!is.na(data1$Hepatitis.B)])
data1$Income.composition.of.resources[is.na(data1$Income.composition.of.resources)] = mean(data1$Income.composition.of.resources[!is.na(data1$Income.composition.of.resources)])
#data1$GDP[is.na(data1$GDP)] = mean(data1$GDP[!is.na(data1$GDP)])
#data1$Population[is.na(data1$Population)] = mean(data1$Population[!is.na(data1$Population)])


colSums(is.na(data1))
### Mean Imputation Of Column "GDP","Population","Income.composition.of.resources" on the basis of Column "Status" 

unique(data1$Status) # checking the unique values of the data

# after looking at mean and median imputation mean imputation works well.
data1$GDP[is.na(data1$GDP[data1$Status <= 'Developing'])]  = 9780.859485# mean imputaion
data1$GDP[is.na(data1$GDP[data1$Status <= 'Developed'])]  = 7381.496237# mean imputation


data1$Population[is.na(data1$Population[data1$Status <= 'Developed'])] = 10616524.52 # mean imputaion
data1$Population[is.na(data1$Population[data1$Status <= 'Developing'])] = 9380299.804 # mean imputaion

colSums(is.na(data1))# again checking the non null values

#View(data1) # read the data


# Exploratory data analysis


# Univariate Analysis


data1_con = data1[,c("Adult.Mortality","infant.deaths","Measles","under.five.deaths",
                     "Polio","Diphtheria", "Population","Life.expectancy","Alcohol",
                     "percentage.expenditure","Hepatitis.B","BMI", "Total.expenditure",
                     "HIV.AIDS", "GDP", "thinness..1.19.years","thinness.5.9.years", 
                     "Income.composition.of.resources","Schooling")]




for (i in 1:ncol(data_con))

{
  par(mfrow=c(2,1))# it says two rows and one columns and par stands for partition
  
  hist(data_con[,i], xlab=names(data_con)[i], main = paste("histogran of",names(data_con)[i], col ="lightblue"))
  
 boxplot(data_con[,i], xlab=names(data_con)[i],horizontal = T,
          main = paste("boxplot of",names(data_con)[i], col = "lightblue"))# horizontal == T makes boxplot horizontal
  
}

summary(data1)
## scaterplot
plot(data1$Life.expectancy, data1$Adult.Mortality) 
plot(data1$Life.expectancy, data1$BMI)
plot(data1$Life.expectancy, data1$HIV.AIDS)
plot(data1$Life.expectancy, data1$Income.composition.of.resources)
plot(data1$Life.expectancy, data1$Schooling)

## BOXPLOT
boxplot(data1$Population ~ data1$Status)
boxplot(data1$GDP ~ data1$Status)
boxplot(data1$Life.expectancy ~ data1$Status)
boxplot(data1$Life.expectancy ~ data1$Status)
boxplot(data1$infant.deaths ~ data1$Status)
boxplot(data1$Hepatitis.B ~ data1$Status)


# correlation matrix
round(cor(data1[4:22]),1)


#Saving Imputed and after eda  dataset in csv format.
write.csv(data1,"///Users/anchalgarg/Documents/Praxis/AML/after_edadata.csv", row.names = FALSE)








