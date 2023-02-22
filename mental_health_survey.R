rm(list=ls())
library(dplyr) #use for data manipulation, cotain function as select, filter amd more
mdata <- read.csv("mental_health.csv")
library("VIM") # for visulization and imputation of missing values
summary(mdata) #total 1259 rows and 27 columns
#Also age has value in negative and greater than 1000 which is not possible, so lets clean these data
mdata[mdata$Age < 0,]# 3 rows in which age is -29, -1729, -1
mdata[mdata$Age > 100,] # 2 rows with age greater than 300 and 9000
mdata<-subset(mdata, Age >0 & Age < 100)
sum(is.na(mdata)) #1892 missing values total
aggr(mdata, numbers = TRUE, prop = TRUE)
sapply(mdata, function(x) sum(is.na(x))/1259) #state - 40%,self_employed 1.4%,
#work_interference has 21% amd comment has 86% of missing values
#so we will drop column comments and state and timestamp has no use
mdata<-subset(mdata, select = -c(Timestamp,state,comments))
# imputing the missing values in work_interference and self-employed column
unique(mdata[["work_interfere"]]) #often, rarely, never, sometimes
mdata$work_interfere<-as.factor(mdata$work_interfere)
summary(mdata$work_interfere) #264 missing value, sometines is mode, occured 465 times
mdata$self_employed<-as.factor(mdata$self_employed)
summary(mdata$self_employed) #18 missing values, no occured 1095
# before imputing these 2 columns, lets analyse the type of data present in pther columns
length(unique(mdata))
unique(mdata$Gender)# in this we can see that gender has 49 unique values but ideally it should be either 2 or 3(male female and transgender)
male_data<- c("M","Cis Male","cis male","Male (CIS)","male","Male-ish","m","maile","Mal","Make","Male ","Man","msle","Mail","Malr","Cis Man")
female_data<- c("female","Cis Female","F","Woman","f","Femake","woman","Female ","femail","cis-female/femme","Female (cis)")
trans_data<-c("Trans-female","something kinda male?","queer/she/they","non-binary", "Nah","Enby","fluid","All","Genderqueer","Androgyne","Agender","Guy (-ish) ^_^","queer","Female (trans)","Neuter","male leaning androgynous","Trans woman","A little about you","ostensibly male, unsure what that really means","p")
mdata$Gender[mdata$Gender %in% male_data] <- "Male"
mdata$Gender[mdata$Gender %in% female_data] <- "Female"
mdata$Gender[mdata$Gender %in% trans_data] <- "Trans"
unique(mdata$Gender)
#Gender column now has 3 values , male, female and trans
#now imputing the 2 columns(self_employed and work_interfere) mice imputation
library("mice")
temp<-mdata
imp_temp<-mice(temp, m =5, maxit = 10)
imp_temp$imp
mdata<-complete(imp_temp)
aggr(mdata, numbers = TRUE, prop = TRUE)# no missing values now
sum(is.na(mdata))
summary(mdata$work_interfere)# sometimes was 465 before imputation and now its 601, which means 136 values out of 264 is fill with sometimes 
# converting other columns as factor
col<-c("Gender","self_employed","family_history","treatment","no_employees","remote_work","tech_company","benefits","care_options","wellness_program","seek_help","anonymity","leave","mental_health_consequence","phys_health_consequence","coworkers","supervisor","mental_health_interview","phys_health_interview","mental_vs_physical","obs_consequence")
mdata[col]<- lapply(mdata[col],factor)
mdata$Age<- as.numeric(mdata$Age)
summary(mdata)
## ----------------------------------Lets see some analysis on the basis of given information--------------------------------------------------------------------------------------------------------------------------------
 library(ggplot2)
mdata%>%ggplot(aes(x = Age, fill = Gender))+
  geom_histogram(binwidth = 5, col = c('white'))+ 
  facet_wrap(~obs_consequence)
#need to make group for age 1-15,16-25,26-35,36-45,46-55,56-

# the age between 25 to 35 years shows maximum number of people suffer from mental health and 
#14% of them were observed to have negative consequences on theor work because of mental health
table(mdata$Gender) # 247 female, 988 male and 19 trans.
library(dplyr)
mdata %>% group_by(Gender,obs_consequence)%>%dplyr::summarise(n())
# shows that 1 out of 5 female and 1 out of 8 male and 1 out of 18 trans faced 
# negative consequences in their work because of mental health
# the ration is high for female, which may show that more number of female face the consequences of their bad mental health on work than male


Interfere_G <-mdata%>% group_by(Gender,work_interfere) %>% dplyr::summarise(n())
#storing the count of people who found that mental health interfere in their work
Interfere_G <-Interfere_G%>%dplyr::rename("count" = "n()")
Interfere_G<-Interfere_G%>%group_by(Gender)%>%dplyr:: mutate(percentage = count/sum(count)*100)
Interfere_G<-Interfere_G%>%arrange(desc(work_interfere))
#ploting
ggplot(data = Interfere_G)+geom_bar(aes(x = "", y = percentage,fill = work_interfere), stat = "identity")+
  geom_text(aes(x = 1.5,y= percentage, label = paste0(round(percentage), "%")),size = 2, position = position_stack(vjust = .5))+
labs(x = NULL, y = NULL, fill = NULL)+
   coord_polar(theta = "y")+ ggtitle("count of people with their opinion on mental health affecting work")+
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_brewer(palette = "Greens", name = "work_interfere")+theme_void()+
  facet_wrap(~Gender)
# The above plot shows that half of the female and almost half of the male acknowledge that 
#their mental health sometimes affect their work, this seems skeptical because
#as per the above obs_consequence, it was argued that only 20% of female and 12% of male
# were seen to have negative consequence on their work which mean majority of the people did know that 
#their mental health affect their work but very less people acknowledged this fact
treatment_G<-mdata%>%group_by(Gender,treatment)%>%dplyr::summarise(n())%>%dplyr::rename("count" = "n()")
ggplot(treatment_G, aes(x = Gender, y = count, fill = treatment))+
  geom_bar(stat = "identity")
# this graph shows that the proportion of female who sought for mental_treatment is more than that of male,
# infact, there are more number of male who didnt sought for treatment than those who did, which is opposite of females

size_cons<- mdata%>%group_by(no_employees,obs_consequence)%>%dplyr::summarise(n())%>%dplyr::rename("count1"="n()")
size_cons<-size_cons%>%arrange(no_employees,count1)
library(plyr) #loading this library to use ddply() function, this function help to split the dataframe , compute the calculation and then combine ot again to give computed result
size_cons<-ddply(size_cons,"no_employees",transform,y_label = cumsum(count1))
ggplot(data = size_cons,aes(x = no_employees,y = count1, fill = obs_consequence))+
geom_bar(stat = "identity",col = c('white'))+labs(title = "Company size verses affect of mental health on work")+
geom_text(aes(y =y_label , label = count1), color ="black",vjust = 1, size = 2)
# from the above graph, the proportion of the employees who faced challenge in their work because of mental health problem
# is same for company with size 1-5 and with more than 1000(proportion was 18%),
# if the number of data collected for each size of company is same then the company with size 6-25 has least proportion of the employes who faced the impact of mental health on work
# and as the company size increases from 6-25, the proportion also increases
p<-ggplot(data = mdata, aes(x = remote_work, fill = obs_consequence))+
  geom_bar(width = 0.25, position = "dodge2")+ scale_fill_manual(values = c('orange 2','yellow 2'))
p+geom_text(
  aes(label=..count..),
  stat='count',
  va='bottom',
  position = position_dodge(0.2),
  size = 2.5)+
  labs(title = "Effect of mental health on work verses remote work")
# the above graph show that the proportion of employees who faced challenge with their work because of mental health is more for companies 
#who do not allow remote work(15%) than those companies who allow remote work(12%)
k<-ggplot(data = mdata, aes(x = tech_company, fill = wellness_program))+
  geom_bar(width = 0.25, position = "dodge2")+ scale_fill_manual(values = c('salmon 2','orange 2','yellow 2'))
k+geom_text(
  aes(label=..count..),
  stat='count',
  va='bottom',
  position = position_dodge(0.2),
  size = 2.5)+labs(title = "Employers discussing mental health as apart of welness program verses type of company")
# from the above graph, a large majority of the tech company do not discuss mental health as ap rt of wellness program
b<-ggplot(data = mdata, aes(x = tech_company, fill = benefits))+
  geom_bar(width = 0.25, position = "dodge2")+ scale_fill_manual(values = c('paleturquoise 3','turquoise','seagreen 3'))
b+geom_text(
  aes(label=..count..),
  stat='count',
  va='bottom',
  position = position_dodge(0.2),
  size = 2.5)+labs(title = "Companies providing mental health benefits verses type of companies")
#############
prop.table(table(mdata$family_history)) #60% of employees has no family history
###########Company providing benefits
Benefit_P<-mdata%>%dplyr::filter(benefits == 'Yes')%>%dplyr::select(tech_company,wellness_program,care_options,seek_help)
Benefit_P%>%ggplot(aes(x= tech_company, fill = care_options))+
  geom_bar(color = c('white'))+labs(title = "Awareness of the emploees on the mental health benefits provided by the company verses Type of company providing mental health benefits")+
  theme(plot.title = element_text(size = 8))
# From, the above graph, it can be observed that there are almost more than 1/3rd of the total employess who are not aware that their company provide mental health benefits
Benefit_P%>%ggplot(aes(x= tech_company, fill = seek_help))+
  geom_bar(color = c('white'))+labs(title = "Type of company providing mental health benefits verses number of companies who make affort to make employess aware of this benefit and procedures involved")+
  theme(plot.title = element_text(size = 8))
# From the graph, it can be observed that even though the companies provide mental health benefits, more than half of the employers do not make afford to make their employees aware of this schema and seek help
openess<-mdata%>%group_by(mental_health_consequence)%>%dplyr::summarise(count = n())
openess<-openess%>%mutate(percent = round(count/sum(count), digits = 2))
library(scales)
openess$percent<-percent(openess$percent, accuracy = 1)
openess%>%ggplot(aes(x = "", y = count, fill = mental_health_consequence))+
  geom_bar(stat = 'identity', width= 1)+ coord_polar("y",start = 0)+theme_void()+
  geom_text(aes(label = percent), position = position_stack(vjust=0.5))+
  labs(title = "Count of the response of the employee on Question:", subtitle = "Do you think that discussing a mental health issue with your employer would have negative consequences?")+
  theme(plot.title = element_text(size = 10), plot.subtitle = element_text(size = 8))
# from above graph, 23 percent of the employess think that discussing their mental health issue with employers can have negative consequences on their work and 38% are not sure
willingness<-mdata%>%group_by(supervisor)%>%dplyr::summarise(count = n())
willingness<-willingness%>%mutate(percent1 = round(count/sum(count), digits = 2))
library(scales)
willingness$percent1<-percent(willingness$percent1, accuracy = 1)
willingness%>%ggplot(aes(x = "", y = count, fill = supervisor))+
  geom_bar(stat = 'identity', width= 1)+ coord_polar("y",start = 0)+theme_void()+
  geom_text(aes(label = percent1), position = position_stack(vjust=0.5))+
  labs(title = "Count of the response of the employee on Question:", subtitle = "Would you be willing to discuss a mental health issue with your direct supervisor(s)")+
  theme(plot.title = element_text(size = 10), plot.subtitle = element_text(size = 8))
# 31% of employees are not willing to discuss theor mental issue wioth theor supervisors
trust<-mdata%>%group_by(coworkers)%>%dplyr::summarise(count = n())
trust<-trust%>%mutate(percent1 = round(count/sum(count), digits = 2))
# the above trust table show that 21% of the employess are not willing to discuss their mental health with theor coworkers
leave<- mdata%>%group_by(leave,obs_consequence)%>%dplyr::summarise(countn = n())
leave<- leave%>%arrange(leave,desc(obs_consequence))
leave<-ddply(leave,"leave",transform,y_label = cumsum(countn))
ggplot(data = leave,aes(x = leave,y = countn, fill = obs_consequence))+
  geom_bar(stat = "identity",col = c('white'))+labs(title = "Response on taking leave verses neg impact of mental health on work")+
  geom_text(aes(y =y_label , label = countn), color ="black",vjust = 1, size = 2)
# from the above graph, it should be notice that the higher the dificulty in taking leave the more is the ration of the number of employee who said "yes":"No"(for facing impact of mental issue on work)
# which means, the organisation in which it is difficult to take leave, the employes of that orgainisation do face and higher mental issue and that impact their work performance
leave_com<-mdata%>%dplyr::select(tech_company,leave)%>%group_by(tech_company,leave)%>%dplyr::summarise(countN=n())
leave_com%>%ggplot(aes(x = leave, y = countN, fill = leave))+
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90))+
           scale_fill_manual(values = c('paleturquoise 3','#915F6D','#8D4E85','#B3446C','#E25098'))+facet_wrap(~tech_company)+
  labs(title = "Response of the employees in taking leave in tech and non-tech company")
#20% of those employees who work in non-tech company find it difficult or somewhat difficult to take leave
#17% of the employees working in tech company find it very difficult or somewhat difficult to take leave
# Encoding for Age column
fifteen_less<-mdata%>%filter(Age %in% (1:15))
#example for encoding age#dat$Credit_score <- ifelse(dat$Credit_score == "Satisfactory",1,0)
tempr<-mdata
library(dplyr)
tempr<-tempr%>%dplyr::mutate(agegroup = case_when(Age>=1 & Age<=15 ~ '1-15',
                                           Age>=16 & Age<=25 ~ '16-25',
                                           Age>=26 & Age<=35 ~ '26-35',
                                           Age>=36 & Age<=45 ~ '36-45',
                                           Age>=46 & Age<=55 ~ '46-55',
                                           Age>=56 & Age<=65 ~ '56-65',
                                           Age>=66 & Age<=100 ~ '66-100'))
tempr<-subset(tempr, select = -c(Age,Country))
tempr$agegroup<-as.factor(tempr$agegroup)
tempr<-tempr%>%dplyr::select(obs_consequence, everything())
install.packages('corrplot')
library(corrplot)
#Coverting columns in numerical value for prediction and correlation
tempr[sapply(tempr, is.factor)] <- data.matrix(tempr[sapply(tempr, is.factor)])
#converting from integer to factor data type
tempr<-data.frame(lapply(tempr, factor))
str(tempr)
install.packages("ggcorrplot")
library(ggcorrplot)
library(dplyr)
#model.matrix(~0+., data=tempr) %>% 
#  cor(use="pairwise.complete.obs") %>% 
#  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
#commenting above 3 lines, because the corrgram is not properly drawn
str(mdata)
reg_data<-mdata
reg_data$obs_consequence<-ifelse(reg_data$obs_consequence=="Yes",1,0)
reg_data<-reg_data%>%dplyr::mutate(agegroup = case_when(Age>=1 & Age<=15 ~ '1-15',
                                                  Age>=16 & Age<=25 ~ '16-25',
                                                  Age>=26 & Age<=35 ~ '26-35',
                                                  Age>=36 & Age<=45 ~ '36-45',
                                                  Age>=46 & Age<=55 ~ '46-55',
                                                  Age>=56 & Age<=65 ~ '56-65',
                                                  Age>=66 & Age<=100 ~ '66-100'))
reg_data$agegroup<-as.factor(reg_data$agegroup)
reg_data$obs_consequence<-as.factor(reg_data$obs_consequence)
reg_data<-subset(reg_data, select = -c(Age,Country))
set.seed(1234)
rows<-sample(nrow(reg_data)) #shuffle the dataset
reg_data<-reg_data[rows,] #get the shuffled dataset
split<-nrow(reg_data)*0.8 #taking 80% of datset for training
split<- 1003 # 1003.2 in decimal, so converting to integer
train<- reg_data[1:split, ]
test<- reg_data[(split+1):nrow(reg_data), ]
###################prediction with Logistic regression#############
mylogit<-glm(obs_consequence~Gender+self_employed+family_history+
               treatment+work_interfere+no_employees+remote_work+tech_company+
               benefits+care_options+wellness_program+seek_help+anonymity+
               leave+mental_health_consequence+phys_health_consequence+
               coworkers+supervisor+mental_health_interview+
               phys_health_interview+mental_vs_physical+agegroup, data = train, family="binomial")
summary(mylogit) #mentalVSphysical, leave, coworker:Some of them has significant impact on the result
library(MASS)
modelN<-stepAIC(mylogit)
sum(is.na(train))
# Of all the variables, below variables are significant :
#agegroup
#remote_work
#leave                        
# self_employed              
# coworkers                  
# treatment                  
# mental_health_consequence  
# seek_help  
# mental_vs_physical
pred_test<-predict(modelN, test, type = 'response')
pred_test
table(Actualvalue = test$obs_consequence, Predictedvalue = pred_test>0.5)
accuracyLR = (206+7)/(206+3+35+7)
accuracyLR # shows that it predicted 85% of data correct
##############knn prediction############################
knntrain<-tempr[1:split,]
knntest<-tempr[(split+1):1254,-1]
library(class)
library(gmodels)
pred_knn<-knn(knntrain[,-1], knntest[,], knntrain$obs_consequence, k =5)
table(pred_knn)
table(actual = tempr[(split+1):1254,1],predicted = pred_knn)
accuracyknn = (201+3)/(201+3+5+42) # 81% accuracy
######################## Naive bayes #####################
library(e1071)
trainNB<-subset(train, select = -c(obs_consequence))
testNB<-subset(test, select = -c(obs_consequence))
trainL<-train$obs_consequence
testL<-test$obs_consequence
health_classifier <- naiveBayes(trainNB, trainL)
health_test_pred <- predict(health_classifier, testNB)
table(actual = testL, predicted = health_test_pred)
accuracyNB<-(191+24)/(191+18+18+24)# 85% accuracy from naive bayes model
##############With Laplace#######################################
train_classifier_laplace <- naiveBayes(trainNB, trainL, laplace = 1)
health_test_pred_laplace <- predict(train_classifier_laplace, testNB)
table(actual = testL, predicted = health_test_pred_laplace)
accuracyNBLap<-(192+24)/(192+18+17+24)# 86% accuracy
##########################Decision Tree search#######################
library(C50)
library(tidyverse)
trainDT<-subset(train, select = -c(obs_consequence))
testDT<-subset(test, select = -c(obs_consequence))
dec_T_model <- C5.0(trainDT, trainL)
DT_test_pred <- predict(dec_T_model, testDT)
table(actual = testL, predicted = DT_test_pred)
accuracy_DT<-(204+6)/(204+5+36+6)
accuracy_DT  # 84% accuracy with Decision Tree model
###########################Decision tree##########################
install.packages("randomForest")
library(randomForest)
testRF<-subset(test, select = -c(obs_consequence))
ran_F_model<-randomForest(obs_consequence~ .,data = train)
pred_ran_F<- predict(ran_F_model,testRF)
table(actual = testL, predicted = pred_ran_F)
accuracyRF<-(208+4)/(208+1+38+4) # 84.4% as per Random Forest Model