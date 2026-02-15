library(dplyr)
library(tidyverse)
library(readxl)
 checking_model<-read_xlsx(choose.files())
 print(checking_model)
 unique(checking_model)
 linear_model<-lm(Exam_Score~Study_Hours,data = checking_model)
linear_model
plot(linear_model)
Return
new_hours<-data.frame(Study_Hours=c(2, 4, 6, 8, 10))
new_hours
Prediction_our_model<-predict(linear_model,newdata = new_hours,interval = "confidence")
Prediction_our_model
library(ggplot2)
ggplot(checking_model,aes(Study_Hours,Exam_Score))+
geom_point()+
  stat_smooth(method = "lm",col="lightblue")
 plot(checking_model$Exam_Score~checking_model$Study_Hours)
 lines(lwd=2,col="red")
 model<-lm(Exam_Score~Study_Hours,data = checking_model)
 plot(Exam_Score~Study_Hours,data = checking_model)
 abline(model,col="red",lwd=2)
 model<-lm(Exam_Score~Study_Hours,data = checking_model)
 plot(Exam_Score~Study_Hours,data = checking_model)
   