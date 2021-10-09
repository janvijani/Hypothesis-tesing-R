# hypothesis testing

#mu= given in question
#xbar= estimation (mean is the estimator in this case)
#n= population 

#type1 error= when H0 is true but we reject the null hypothesis 
#helps us to decide to accept of reject H0
#alpha: probability of making type1 error
#P value is PROBABILITY of making type1 error when we REJECT null hypothesis

#type2 error= when H0 is false but we accept it 
#helps us to determine the correctness of the hypothesis
#beta: probability of making type2 error
# therefore, beta= pr(Type2 error)
#power of test: 1-beta
#therefore, lower the Beta value, higher is the power of test and vise versa. 
#we obviously need high power of test and low value of beta

#hypothesis testing is of two types: 
# 1. parametric test : tests implemented on normally distributed data (T distribution)
# 2. non-parametric test : tests implemented on not normal data

#TYPE 1 ERROR ARE USED TO CHECK WEATHER TO ACCEPT OR TO REJECT THE HYPOTHESIS
#TYPE 2 ERROR ARE USED TO CHECK WEATHER THE TEST ITSELF OS GOOD OR NOT 

#Question 1
#here, H0= emplowees take 90 minutes to finish a report 
#Ha= employees take more than 90 minutes to finish a report 
ostt 
mean(ostt$Time)
sd(ostt$Time)
t.test(ostt$Time, alternative = "greater",mu=90) 
  

#here, p-value=0.04074
#probability of making type1 error when H0 is rejected is 4% 
#if prob of making type1 error when we reject H0 is < 5%, then it is safe to reject H0. 
#hence, in this case, we reject H0. 

#if p-value=0.80
#probability of making type1 error when H0 is rejected is 80% 
#hence, in this case, it is not safe to reject H0.; accept H0

#if p-value=0.01
#probability of making type1 error when H0 is rejected is 1% 
#hence; accept H0

#if p-value=0.96
#probability of making type1 error when H0 is rejected is 96% 
#hence; in this case, it is not safe to reject H0.; accept H0

#threshold for p value is 0.05
#if p<0.05, it is safe to reject H0
#if p>0.05, it is not safe to reject H0
#0.05 is called the level of significance 

#here, 95 percent confidence interval: 90.22748      Inf
# which means 95% employees take time between 90.22748 to +ve infinity
# Hence, we reject H0. 


#Question 2
#H0= Avg cholesterol value = 200
#Ha=  Avg cholesterol value !=200

t.test(heart$chol, mu=200, alternative ="two.sided" )
#alternative = c("two.sided", "less", "greater")
#alternative ="two.sided" basically means != 
#here, p value is really really less, therefore, it is safe to reject H0
#here, since we passed a two sided test, we get clear values for confidence interval unlike infinity in last concept 

#####################################

#THIS WAS PERFORMED ON DIABETIES DATA; 
#H0 ; average age is 45
#Ha ; average age < 45

t.test(diabetes$Age, alternative = "less", mu=45)
#since p<0.05, it is safe to reject H0

#####################################
#H0 = avg BMI is 26.5 
#HA = avg BMI is not equal to 26.5 

t.test(diabetes$BMI, mu=26.5, alternative='two.sided')
#since p<0.05, it is safe to reject H0

#####################################
#check if avg BP is SIGNIFICANTLY different than 90
# H0 = BP=90
# HA = BP!=90

t.test(diabetes$BloodPressure, mu= 90, alternative= 'two.sided')
#since p<0.05, it is safe to reject H0
#therefore, BP is SIGNIFICANTLY different than 90

#independent sample T test for equality of means. 
# Avg salary of male and female in HR dept is similar 
# Avg score of players in Series1 ans Series2 is similar 

####################################

#here , time_g1 is the group of people who have experience 0-1 years
#here , time_g2 is the group of people who have experience 1-2 years

#H0 = time taken to complete an activity by group 1 and group 2 is similar 
#in other words, experience doesent matter

#H0 = time taken to complete an activity by group 1 and group 2 is not similar 
#in other words, experience matters

t.test(INDEPENDENT.SAMPLES.t.TEST$time_g1, INDEPENDENT.SAMPLES.t.TEST$time_g2)

#Therefore, p-value is >0.05 (p-value = 0.8282 which means there is 82% chance of making TYPE1 error)
#Hence, we accept H0 as it is not safe to reject H0. 
#in other words, experience is not a significant factor in deciding the time taken to complete the task 

t.test(time~group, data= INDEPENDENT.SAMPLES.t.TEST_2)
#Here, INDEPENDENT.SAMPLES.t.TEST_2 we need to change long to wide format 
#hence, we put numerical first (here, it is time) and then categorical data later (i.e. group here)


#####################################

# H0= Avg BMI for diabetic and non-diabetic is similar
# HA= Avg BMI for diabetic and non-diabetic is NOT similar

t.test(BMI~Outcome, data= diabetes)

#here, P value<0.05
#Hence, we reject H0
#Avg BMI for diabetic and non-diabetic is similar
#BMI is a significant variable when you wish to decide if a person is diabetic or not 

########################################

#STMT; avg glucose is similar for diabetes and non diabetes
#H0 ; Avg glucose is similar for diabetes and non diabetes
#DIfference in mean = 0
#HA ; avg glucose is NOT similar for diabetes and non diabetes
#DIfference in mean != 0

t.test(Glucose~Outcome, data = diabetes)
#Here, P<0.05
#Hence, we reject H0
#H0 ; Avg glucose is similar for diabetes and non diabetes
#DIfference in mean != 0
#GLucose is a significant variable when you wish to decide if a person is diabetic or not 

#################################################

# Paired T test a.k.a Dependent sample t test ; before / after kinda questions 

#pain level of person before and after medications 
#score of student in exam before and after tuitions
#salary of person before and after training hours 

#to check if time taken by employees to create a report before and after training programme
#H0; time taken by employees to create a report before and after training programme is equal
#diff=0
#HA; time taken by employees to create a report before and after training programme is not equa
#diff!=0

t.test(PAIRED.t.TEST$time_before, PAIRED.t.TEST$time_after, data= PAIRED.t.TEST, paired=TRUE)
#Dont forget Paired = TRUE
#Here, P-value is < 0.05 
#Hence, we reject H0
#Hence, there is a diff in timer req to create report is not similar before and after training
  
###############################################
#correlation- statistical measure of relationship between two variable
#if 1 or -1, it means that variables are directly or directly co-related (perfectly)
#if 0, that means no correlation 

#company wants to assess weather aptitude of aperson is related to efficiency of the person 
#H0 ; aptitude of aperson is NOT related to efficiency of the person 
#Cor=0 
#HA ; aptitude of aperson is related to efficiency of the person
#Cor=1 


#NOtice how we always compare H0 statements to 0 (difference is 0 ) and all that
#Hence here, H0 and HA are opposites ; 

cor.test(JOBPROF$aptitude,JOBPROF$job_prof )
#Here P < 0.05 Hence, we reject H0
#Hence, there is significant co-relation betweeen job proficiency and aptitude 

#######################

#cor between BMI and blood pressure of diabetes dataset
#cor and cor.test are both related to co-relation 
#but cor.test is to know if the co-relation is similar or not 
cor.test(diabetes$BloodPressure, diabetes$BMI)
#Hence, there IS +ve CORRELATION BETWEEN THE TWO
#Therefore, if one increases, the other increases too 


##############################################

#variance testing ; F test 
# test of variance 
#H0 ; variance in two samples under study is similar
  # ratio of variance of two columns is 1 

#HA ; variance in two samples under study is NOT similar
  # ratio of vsriance of two columns is dignificantly different than 1 

#var and var.test are both related to var 
#but var.test is to know if the varicance is similar or not 

#to check if variance in age is high or not for poeple having diabetes
var.test(Age~Outcome, data = diabetes)
#Here, p > 0.05
#hence, we accept Null Hupothesis 

###############################################

#test of normality ; to see if data is normailly distributed or not 
#H0; data is normally distributed 
#HA ; data is NOT normally distributed 
# to see if age of person in diabetes dataset follows normal distrivutuin 

shapiro.test(diabetes$Age)
# p value < 0.05 
# Hence, H0 is rejected 

# you can also use;
 summary(diabetes$Age)

  