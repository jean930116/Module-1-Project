install.packages("plotrix") #Download all these packages
install.packages("ggplot2") #Data Visualization purpose
install.packages("magrittr")
install.packages("gplots")
install.packages("dplyr")
install.packages("corrplot")
library(plotrix) #Import libraries
library(ggplot2)
library(magrittr)
library(gplots)
library(dplyr)
library(corrplot)
library(readr) #Read_csv function
getwd()
AmesHousing <- read.csv("C:/Users/JinYoung/Downloads/AmesHousing.csv") #Download the dataset
AmesHousing
nrow(AmesHousing) #Check the number of rows
ncol(AmesHousing) #Check the number of columns
colnames(AmesHousing) #Check the name of columns
rownames(AmesHousing) #Check the name of rows
head(AmesHousing, n=10) #Using head/tail function to print the records.
tail(AmesHousing, n=10)
str(AmesHousing)
summary(AmesHousing)
summary(AmesHousing$SalePrice)
class(AmesHousing)
dim(AmesHousing)
sum(is.na(AmesHousing))
missing_row <- AmesHousing[!complete.cases(AmesHousing),] #List rows of data that have missing values 
head(missing_row)
nrow(missing_row)
var_name <- names(AmesHousing) #List all variables names
var_name
a <- c("Order","PID","MS SubClass","Year Built","Year Remod/Add","Gr Liv Area", "Garage Yr Blt",
                "Mo Sold","Yr Sold","SalePrice")
summary(a)
head(a)

hist(AmesHousing$SalePrice, main = "Sale Price")
boxplot(AmesHousing$MS.SubClass, main="MS.SubClass")


AmesHousing$Lot.Frontage[is.na(AmesHousing$Lot.Frontage)]<-mean(AmesHousing$Lot.Frontage,na.rm = TRUE)
AmesHousing$BsmtFin.SF.1[is.na(AmesHousing$BsmtFin.SF.1)]<-mean(AmesHousing$BsmtFin.SF.1,na.rm = TRUE)
AmesHousing$BsmtFin.SF.2[is.na(AmesHousing$BsmtFin.SF.2)]<-mean(AmesHousing$BsmtFin.SF.2,na.rm = TRUE)
AmesHousing$Total.Bsmt.SF[is.na(AmesHousing$Total.Bsmt.SF)]<-mean(AmesHousing$Total.Bsmt.SF,na.rm = TRUE)
new_housing <-cor(AmesHousing[, unlist(lapply(AmesHousing, is.numeric))])
new_housing


install.packages("corrplot")
library(corrplot)
corrplot<-corrplot(new_housing)                                                                
                                                                
x <-plot(AmesHousing$SalePrice, AmesHousing$Garage.Area, xlab="SalePrice", ylab="Garage.Area")
abline(x)
model2 <- lm(SalePrice ~ Bedroom.AbvGr + Overall.Qual + Overall.Cond, data = AmesHousing)
model2
summary(model2)
plot(model2)
par(mfrow=c(2,2))

housing <- AmesHousing[, c(3,6,19,20)] # print the 4 rows
housing
res<-cor(housing)
round(res, 2)
cor(housing, use = "complete.obs")



install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
housing <- AmesHousing[, c(3,6,19,20)]
chart.Correlation(housing, histogram=TRUE, pch=19)

install.packages("corrplot")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot<-corrplot(new_housing)

install.packages("CARS")
library(car)
vif(model2) #check the multicollinearity 

install.packages("ggplot2") 
library(ggplot2)
a1 <- ggplot(AmesHousing, aes(x=SalePrice, y=Lot.Area)) + 
geom_point(shape=1) +  geom_smooth(method=lm , color="Blue", se=FALSE)+
ggtitle(" Scatter plot of SalePrice and Lot.Area") + theme(plot.title = element_text(hjust = 0.5)) # Show the scatter plot of Lot.Area
a1 

b1 <- ggplot(AmesHousing, aes(x=SalePrice, y=Gr.Liv.Area)) + 
geom_point(shape=2) +  geom_smooth(method=lm , color="Dark Green", se=FALSE)+
ggtitle(" Scatter plot of SalePrice and Gr.Liv.Area") + theme(plot.title = element_text(hjust = 0.5))
b1

c1 <- ggplot(AmesHousing, aes(x=SalePrice, y=Bedroom.AbvGr)) + 
geom_point(shape=3) +  geom_smooth(method=lm , color="orange", se=FALSE)+
ggtitle(" Scatter plot of SalePrice and Bedroom.AbvGr") + theme(plot.title = element_text(hjust = 0.5))
c1

d1 <- ggplot(AmesHousing, aes(x=SalePrice, y=TotRms.AbvGrd)) + 
geom_point(shape=1) +  geom_smooth(method=lm , color="purple", se=FALSE)+
ggtitle(" Scatter plot of SalePrice and TotRms.AbvGrd") + theme(plot.title = element_text(hjust = 0.5))
d1

install.packages("gridExtra")
library(gridExtra)
grid.arrange(a1,b1,c1,d1)

install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(AmesHousing, 2, p)
md.pattern(AmesHousing)
md.pairs(AmesHousing)

install.packages("mlbench")
library(mlbench)                 
set.seed(100)
AmesHousing[sample(1:nrow(AmesHousing), 82), "rad"] <- NA
AmesHousing[sample(1:nrow(AmesHousing), 82), "ptratio"] <- NA
head(AmesHousing)
md.pattern(AmesHousing) # pattern or missing values in data.

install.packages("Hmisc")
library(Hmisc)
impute(AmesHousing$ptratio, mean)  # replace with mean
md.pattern(AmesHousing)

AmesHousing <- read.csv("C:/Users/JinYoung/Downloads/AmesHousing.csv", header = TRUE, sep = ",", na.strings = c("")) #Download the dataset
print(AmesHousing)

any(is.na(AmesHousing))# Using is.na() function
nrow(AmesHousing[!complete.cases(AmesHousing), ])/nrow(AmesHousing)*100# Using complete.cases() function to get percentage of missing value
install.packages("mice")# Looking at the missing data pattern
library(mice)
md.pattern(AmesHousing)

install.packages("Hmisc")# Using impute function from Hmisc package
library(Hmisc)
impute(AmesHousing$Garage.Cars, mean)  # replace with mean
AmesHousing$Garage.Cars[is.na(AmesHousing$Garage.Cars)] = mean(AmesHousing$Garage.Cars, na.rm=TRUE)# Filling missing values with Mean
impute(AmesHousing$Garage.Area, mean)  # replace with mean
AmesHousing$Garage.Area[is.na(AmesHousing$Garage.Area)] = mean(AmesHousing$Garage.Area, na.rm=TRUE)# Filling missing values with Mean
impute(AmesHousing$Garage.Qual, mean)  # replace with mean
AmesHousing$Garage.Qual[is.na(AmesHousing$Garage.Qual)] = mean(AmesHousing$Garage.Qual, na.rm=TRUE)# Filling missing values with Mean
impute(AmesHousing$Garage.Cond, mean)  # replace with mean
AmesHousing$Garage.Cond[is.na(AmesHousing$Garage.Cond)] = mean(AmesHousing$Garage.Cond, na.rm=TRUE)# Filling missing values with Mean
impute(AmesHousing$BsmtFin.Type.2, mean)  # replace with mean
AmesHousing$BsmtFin.Type.2[is.na(AmesHousing$BsmtFin.Type.2)] = mean(AmesHousing$BsmtFin.Type.2, na.rm=TRUE)# Filling missing values with Mean
impute(AmesHousing$Bsmt.Full.Bath, mean)  # replace with mean
AmesHousing$Bsmt.Full.Bath[is.na(AmesHousing$Bsmt.Full.Bath)] = mean(AmesHousing$Bsmt.Full.Bath, na.rm=TRUE)# Filling missing values with Mean



