#Abdul Ahad
#TP058513

# Installing Libraries
install.packages("ggplot2")
install.packages("ggridges")
install.packages("dplyr")
# libararies 
library(ggplot2)
library(ggridges)
library(dplyr)
mydata <- read.csv("D:\\Others\\University\\Degree\\3rd Semester\\Programming For Data Analysis\\R Submission\\Abdul_Ahad_TP058513_R\\Placement_Data_Full_Class.csv",header = TRUE)
#
options(scipen=999)
# Data Exploration
show(mydata)
View(mydata)
summary(mydata)
ncol(mydata)
nrow(mydata)

# Question 1: Which gender is more dominant in different fields ?
# Analysis 1.1: Relationship between Gender and Status
  ggplot(mydata, aes(x = gender, fill = status )) + geom_bar()+
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count")
# Analysis 1.2: Relationship between Gender and Family Support
  ggplot(mydata, aes(x = famsup, fill = gender )) + geom_bar(position = position_dodge()) + 
  coord_flip() + xlab("Family Support")  
# Analysis 1.3: Relationship between Gender and Degree Course
  ggplot(mydata, aes(x = gender, fill = degree_t )) + geom_bar() +
  facet_wrap(~degree_t) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count") + 
  labs(fill= "Degree Program")
# Question 2: How much marks plays an important role in getting placement? 
# Analysis 2.1: Relationship between  SSC Marks and Status
  ggplot(mydata, aes(x = ssc_p, fill = status)) + 
  geom_histogram(color = "black") +
  xlab("Secondary Education Percentage") + facet_wrap(~status)
# Analysis 2.2: Relationship between HSC Marks and Status
  ggplot(mydata,aes(x= hsc_p,fill=status ))+
  geom_histogram() + 
  xlab("Higher Secondary Education Percentage") + facet_wrap(~status)
# Analysis 2.3: Relationship between Degree Marks and Status
  ggplot(mydata, aes(x = degree_p, y = status, fill= status)) + 
  geom_boxplot() + coord_flip() +
  xlab("Degree Percentage")  
# Analysis 2.4: Relationship between Master Marks and Status
  ggplot(mydata, aes(x = mba_p, y = status, fill= status)) + 
  geom_boxplot() + xlab("Master Course Percentage")
# Analysis 2.5: Relationship between Employability Test and Status
  ggplot(mydata, aes(x = etest_p, y = status, fill= status)) + 
  geom_boxplot() + xlab("Employability Test Percentage") 
# Question 3: Does services help the student to getting placement ? 
# Analysis 3.1: Relationship between Family Support and Status
  ggplot(mydata, aes(x = famsup, fill = status )) + geom_bar(color = "black") +
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count") +
  xlab("Family Support")
# Analysis3.2: Relationship between Paid Classes and Status
  ggplot(mydata, aes(x = paid, fill = status )) + geom_bar()  +
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count")
# Analysis 3.3: Relationship between Activities and Status
  ggplot(mydata, aes(x = activities, fill = status )) + geom_bar() +
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count")
# Analysis 3.4: Relationship between Internet and Status
# Internet
  GotInternet <-  filter(mydata, internet == "yes")
  
  Piechartinternet = subset(GotInternet, select = c(internet, status))
  Area = table(GotInternet$status)
  percentage=round(Area/sum(Area)*100, digits=2)
  lbl=paste(names(Area),"=",percentage,"%",sep = "  ")
  
  pie(Area,main ="Student with Internet",labels=lbl) 
# No Internet
  GotInternet <-  filter(mydata, internet == "no")
  
  Piechartinternet = subset(GotInternet, select = c(internet, status))
  Area = table(GotInternet$status)
  percentage=round(Area/sum(Area)*100, digits=2)
  lbl=paste(names(Area),"=",percentage,"%",sep = "  ")
  
  pie(Area,main ="Student without Internet",labels=lbl,
  col=c("steelblue4", "steelblue2", "skyblue1"))
  
# Question 4: Does the parents profession and education help the student to get placement ?
# Analysis 4.1: Relationship between Father Job and Status
  ggplot(mydata, aes(x = Fjob, fill = status )) + geom_bar(color = "black") + 
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count") +
  xlab("Father Job")
# Analysis 4.2: Relationship between  Mother Job and Status
  ggplot(mydata, aes(x = Mjob, fill = status )) + geom_bar(color = "black") + 
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count") +
  xlab("Mother Job")
# Analysis 4.3: Relationship between Father Education and Status
  ggplot(mydata, aes(x = Fedu, fill = status )) + geom_bar() + 
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count") + 
  xlab("Father Education")
# Analysis 4.4: Relationship between Mother Education and Status
  ggplot(mydata, aes(x = Medu, fill = status )) + geom_bar() + 
  facet_wrap(~status) + geom_text(aes(label=..count..), 
  vjust = -0.5, postion= position_dodge(0.9), size = 3.5, stat = "count") + 
  xlab("Mother Education")
# Question 5: Does education field and working experience helps to get high salary?
# Analysis 5.1: Relationship between HSC Course and Salary
  ggplot(mydata, aes(x = salary, fill = hsc_s)) + 
  geom_histogram(color = "black",bins= 15)  +
  labs(fill= "HSC Course") + facet_wrap(~hsc_s)
# Analysis 5.2: Relationship between Degree Course and Salary
  ggplot(mydata, aes(x = salary, y = degree_t, fill = degree_t)) +
  geom_density_ridges() + ylab("Degree Course")
# Analysis 5.3: Relationship between Master Course and Salary
  ggplot(mydata,aes(x=salary, y=specialisation, fill=specialisation)) + 
  geom_violin() +  coord_flip() + ylab("Master Course") +  labs(fill= "Master Course")
# Analysis 5.4: Relationship between Experience and Salary
  ggplot(mydata, aes(x = salary, fill = workex)) + 
  geom_histogram(color = "black",bins= 15) + 
  labs(fill= "Work Experience")+  facet_wrap(~workex) 
# Extra Feature 1 
# Analysis 5.2: Relationship between Degree Course and Salary
  ggplot(mydata, aes(x = salary, y = degree_t, fill = degree_t)) +
  geom_density_ridges() + ylab("Degree Course")
# Extra Feature 2
# Analysis 5.2: Relationship between Degree Course and Salary
  ggplot(mydata,aes(x=salary, y=specialisation, fill=specialisation)) + 
  geom_violin() +  coord_flip() + ylab("Master Course") +  labs(fill= "Master Course")
  
  