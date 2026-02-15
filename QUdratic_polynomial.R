library(readxl)
library(dplyr)
library(tidyverse)
new_educational_dataset<-read_xlsx(choose.files())
new_educational_dataset
head(new_educational_dataset)
plot(new_educational_dataset$StudyHours_LMS_perWeek,new_educational_dataset$PostTestScore_0to100, xlab = "study_hours", ylab="Post_score", main ="curvlinear")
simple_regression<-lm(PostTestScore_0to100~StudyHours_LMS_perWeek, data = new_educational_dataset)
simple_regression
summarise(simple_regression)
abline(simple_regression,col="red",lwd=2)
curvelinear<-lm(PostTestScore_0to100~poly(PostTestScore_0to100 ,2, raw=TRUE ,  data=new_educational_dataset))  
curvelinear<-lm(PostTestScore_0to100~poly(PostTestScore_0to100,2,raw = TRUE),data = new_educational_dataset)
curvelinear
library(ggplot2)
ggplot(new_educational_dataset,aes(StudyHours_LMS_perWeek,PostTestScore_0to100))+
  geom_point()+
geom_smooth(method = "lm",formula = y~poly(x,2),se=TRUE)
plot(new_educational_dataset$StudyHours_LMS_perWeek,new_educational_dataset$PostTestScore_0to100)
new_simple_regression<-lm(PostTestScore_0to100~StudyHours_LMS_perWeek,data = new_educational_dataset)
new_simple_regression
abline(new_simple_regression,col="red",lwd=2)
summary(new_simple_regression)
polynomial_regression<-lm(PostTestScore_0to100~poly(StudyHours_LMS_perWeek,2,raw = TRUE),data = new_educational_dataset)

polynomial_regression
ggplot(new_educational_dataset,aes(StudyHours_LMS_perWeek,PostTestScore_0to100))+geom_point()+
geom_smooth(method = "lm",formula = y~poly(x,2))