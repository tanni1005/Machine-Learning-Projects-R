
Author: Tanvi Bhagat
AIM: Placement of student using KNN classification.

#Installing required packages
install.packages("ggplot2")
install.packages("GGally")
install.packages('corrplot')
install.packages('plotly')
install.packages('caTools')

library(ggplot2)
library(corrplot)
library(plotly)
library(caTools)

#loading the data
placement <- read.csv("~/OneDrive - Northeastern University/Q3/Predictive analytics/Placement_dataset_kNN.csv")
head(placement)
tail(placement)

#checking the dimensions
dim(placement)

str(placement)

#EDA

placement %>% ggplot(aes(x = workex)) +  geom_bar(aes(fill = status)) + ggtitle("Placement status of students with respect to work experience")
placement %>% ggplot(aes(x = gender, fill = status)) +  geom_bar(aes(fill = )) + ggtitle("Placement status of students with respect to gender") + labs(colour = "Iris species")


ggplot(placement)+geom_density(mapping = aes(x=degree_p, fill = status), size=3, position = "Stack") + ggtitle("Distribution of Placement status of students with respect to degree percentage")

b <- ggplot(placement)+geom_boxplot(mapping = aes(y=degree_p, fill = status))
b
b+facet_grid(workex~gender)

# Whether  Males and Females have work experience?
ggplot(placement,aes(gender)) + geom_bar(aes(fill=workex), position = 'dodge') +
  ggtitle("Gender vs Work Experience") #+ theme(axis.text.x  = element_text(angle=90, hjust = 1))

ggplot(placement, aes(degree_p))+geom_histogram(binwidth = 5, fill = "darkred", color = "white")+
  scale_x_continuous(breaks = seq(50,90,5))+xlab("Degree Percentage (%)")+
  labs(caption = "Fig1 - Histogram of the degree percentage")+
  theme(plot.caption = element_text(hjust = 0.5))+
  geom_vline(aes(xintercept = mean(degree_p)), color = "blue1", linetype = "dashed", lwd = 1)

ggplot(placement, aes(x = specialisation, y = salary, fill = specialisation)) +
  geom_boxplot(alpha = 0.8) +
  ggtitle("Salary by Specialization", subtitle = "Marketing and Finance specializations exhibited a higher and tighter salary distribution") +
  xlab("Specialization") + 
  ylab("Salary") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill="gray94", colour=NA))



#checking if there are any null values
sum(is.na(placement))
placement[is.na(placement)] <- 0
sum(is.na(placement))


#Dropping sl_no column and salary columns
placement = placement[,!names(placement) %in% 'sl_no']
placement = placement[,!names(placement) %in% 'salary']
placement = placement[,!names(placement) %in% 'hsc_s']
placement = placement[,!names(placement) %in% 'degree_t']
placement[placement==''] <- NA #To make sure missing data is NA
head(placement)


# converting factor columns  to integer
a <- sub("M", "0", placement$gender)
b <- sub("F","1",a)
placement$gender <- b
placement$gender <-as.numeric(placement$gender)


c <- sub("Others", "0", placement$ssc_b)
d <- sub("Central","1",c)
placement$ssc_b <- d
placement$ssc_b <-as.numeric(placement$ssc_b)


e <- sub("Not Placed", "0", placement$status)
f <- sub("Placed","1",e)
placement$status <- f
placement$status <-as.numeric(placement$status)

g <- sub("Mkt&HR", "0", placement$specialisation)
h <- sub("Mkt&Fin","1",g)
placement$specialisation <- h
placement$specialisation <-as.numeric(placement$specialisation)

k <- sub("No", "0", placement$workex)
l <- sub("Yes","1",k)
placement$workex <- l
placement$workex <-as.numeric(placement$workex)

m <- sub("Others", "0", placement$hsc_b)
n <- sub("Central","1",m)
placement$hsc_b <- n
placement$hsc_b <-as.numeric(placement$hsc_b)
head(placement)

#typeof(placement$specialisation)
#class(placement$specialisation)
#placement[,'specialisation'] <- factor(placement[,'specialisation'])
#typeof(placement$specialisation)


#checking for correlation and plotting the correlation matrix 
M<-cor(placement)
head(round(M,2))
corrplot(M, type="full", method="square", main="CORRELATION")

set.seed(101)
#splitting the data into train and test with 70:30 ratio.
sample <- sample.split(placement$status,SplitRatio = 0.70)

train <- subset(placement,sample==TRUE)

test <- subset(placement,sample==FALSE)

#we need the class package to implement kNN
install.packages('class')   
library(class)# for KNN 

#kNN implementation
predicted.status <- knn(train[1:9],test[1:9],train$status,k=1)


#Error in prediction
error <- mean(predicted.status!=test$status)

#Let us predict by using different K values from 1 to 10
predicted.status <- NULL
error.rate <- NULL

#checking the model fork values from 1 to 10.
for (i in 1:10) {
  predicted.status <- knn(train[1:9],test[1:9],train$status,k=i)
  error.rate[i] <- mean(predicted.status!=test$status)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

#Visualizing the best K value 
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

### We can see that the prediction error for k=1 is approximately 25%, which means that the kNN model accuracy is 75%.
### It can be also noted that the accuracy is the highest (around 95%) when the value of k changes to 5,6,9 and 10.


