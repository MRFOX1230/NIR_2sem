#install.packages("devtools")
#devtools::install_github("bdemeshev/rlms")
#library("rlms")

library("lmtest")
library("data.table")
library("car")

#Считываемые данные с таблицы
#зарплата, пол, семейное положение, наличие высшего образования, 
#возраст, тип населенного пункта, длительность рабочей недели

data <- read.csv("C:\\Users\\trudo\\YandexDisk\\NIR\\r14i_os26b.csv")


sex = ifelse(data$jh5 == 2, 0, 1) #пол (44)

wed1 <- ifelse(data$j_marst == 2, 1, 0)
wed2 <- ifelse(data$j_marst == 4 | data$j_marst == 5, 1, 0)
wed3 <- ifelse(data$j_marst == 1, 1, 0)
familystatus = data.frame(wed1, wed2, wed3) #семейное положение (34)

education = data$j_diplom #наличие высшего образования (38)
highter_educ <- ifelse(education == 6, 1, 0)

locality = data$status #тип населенного пункта (28)
city_status <- ifelse(locality == 1 | locality == 2, 1, 0)

salary = data$jj13.2 #зарплата (83 столбец)
age = data$j_age #возраст (47)
worktime = data$jj6.1a #длительность рабочей недели (72)


ourdata1 <- data.frame(salary,
                      sex,
                      familystatus,
                      highter_educ,
                      age,
                      city_status,
                      worktime)

#удаляем строки, где есть NA или данные числа
ourdata1 <- na.omit(ourdata)
ourdata1 <- subset(ourdata, salary != 99999997 & salary != 99999998 & salary != 99999999 & 
                           worktime != 99999996 & worktime != 99999997 & worktime != 99999998 & worktime != 99999999) 

ourdata = ourdata1
ourdata$salary <- (ourdata$salary - mean(ourdata$salary))/sqrt(var(ourdata$salary))
ourdata$age = (ourdata$age - mean(ourdata$age))/sqrt(var(ourdata$age))
ourdata$worktime = (ourdata$worktime - mean(ourdata$worktime))/sqrt(var(ourdata$worktime))
print(str(ourdata))


model1 = lm(salary~sex+wed1+wed2+wed3+highter_educ+age+city_status+worktime, data=ourdata)#R^2 = 0.1465 
summary(model1)
model1R2 = summary(model1)$adj.r.squared

#summary(model1)$adj.r.squared
vif(model1) #vif хороший(<5), переменные можно изпользовать вместе 

#логарифмы
logmodel1 = lm(salary~sex+wed1+wed2+wed3+highter_educ+I(log(abs(age)))+city_status+worktime, data=ourdata) #R^2 = 0.1465
summary(logmodel1)
logmodel2 = lm(salary~log(abs(sex+wed1+wed2+wed3+highter_educ+age+city_status+worktime)), data=ourdata) #R^2 = 0.03141 
summary(logmodel2)
#логарифмы не особо помогают улучшить зависимость между переменными

degbettermodel = model1
degree = 0
R2 = model1R2
for(i in 1:20){
  n <- 0.1*i
  degree = n
  modeltime <- lm(salary~I(sex^n)+wed1+wed2+wed3+highter_educ+age+city_status+worktime, data=ourdata)
  R2time <- summary(modeltime)$adj.r.squared
  if(R2time > R2){
    degbettermodel <- modeltime
    R2 <- R2time
  }
  
  modeltime <- lm(salary~sex+wed1+wed2+I(wed3^n)+highter_educ+age+city_status+worktime, data=ourdata)
  R2time <- summary(modeltime)$adj.r.squared
  if(R2time > R2){
    degbettermodel <- modeltime
    R2 <- R2time
  }
  
  modeltime <- lm(salary~sex+wed1+wed2+wed3+I(highter_educ^n)+age+city_status+worktime, data=ourdata)
  R2time <- summary(modeltime)$adj.r.squared
  if(R2time > R2){
    degbettermodel <- modeltime
    R2 <- R2time
  }
  
  modeltime <- lm(salary~sex+wed1+wed2+wed3+highter_educ+I(age^n)+city_status+worktime, data=ourdata)
  R2time <- summary(modeltime)$adj.r.squared
  if(R2time > R2){
    degbettermodel <- modeltime
    R2 <- R2time
  }
  
  modeltime <- lm(salary~sex+wed1+wed2+wed3+highter_educ+age+I(city_status^n)+worktime, data=ourdata)
  R2time <- summary(modeltime)$adj.r.squared
  if(R2time > R2){
    degbettermodel <- modeltime
    R2 <- R2time
  }

}
print(degree)
summary(degbettermodel)
vif(degbettermodel)
#наилучшая модель lm(formula = salary ~ sex + wed1 + wed2 + wed3 + highter_educ + I(age^2) + city_status + worktime, data = ourdata)
#она имеет скоректированный коэффициент детерминации R^2 = 0.1815, что незначительно, но лучше исходной модели model1

#произведения
mulmodel1 = lm(salary~wed3+highter_educ+age+I(city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.1375
summary(mulmodel1)
mulmodel2 = lm(salary~wed3+highter_educ+I(age*city_status)+sex+wed1+wed2+worktime, data=ourdata)#R^2 = 0.1127
summary(mulmodel2)
mulmodel3 = lm(salary~wed3+highter_educ+I(age*city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.05111  
summary(mulmodel3)
mulmodel4 = lm(salary~wed3+I(highter_educ*age)+city_status+sex+wed1+wed2+worktime, data=ourdata)#R^2 = 0.1066
summary(mulmodel4)
mulmodel5 = lm(salary~wed3+I(highter_educ*age)+I(city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.09523
summary(mulmodel5)
mulmodel6 = lm(salary~wed3+I(highter_educ*age*city_status)+sex+wed1+wed2+worktime, data=ourdata)#R^2 = 0.06295
summary(mulmodel6)
mulmodel7 = lm(salary~wed3+I(highter_educ*age*city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.008498
summary(mulmodel7)
mulmodel8 = lm(salary~I(wed3*highter_educ)+age+city_status+sex+wed1+wed2+worktime, data=ourdata)#R^2 = 0.1103
summary(mulmodel8)
mulmodel9 = lm(salary~I(wed3*highter_educ)+age+I(city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.09931
summary(mulmodel9)
mulmodel10 = lm(salary~I(wed3*highter_educ)+I(age*city_status)+sex+wed1+wed2+worktime, data=ourdata)#R^2 = 0.06791
summary(mulmodel10)
mulmodel11 = lm(salary~I(wed3*highter_educ)+I(age*city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.01325
summary(mulmodel11)
mulmodel12 = lm(salary~I(wed3*highter_educ*age)+city_status+sex+wed1+wed2+worktime, data=ourdata)#R^2 = 0.1046 
summary(mulmodel12)
mulmodel13 = lm(salary~I(wed3*highter_educ*age)+I(city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.09315 
summary(mulmodel13)
mulmodel14 = lm(salary~I(wed3*highter_educ*age*city_status)+sex+wed1+wed2+worktime, data=ourdata)#R^2 = 0.06166 
summary(mulmodel14)
mulmodel15 = lm(salary~I(wed3*highter_educ*age*city_status*sex)+wed1+wed2+worktime, data=ourdata)#R^2 = 0.008929 
summary(mulmodel15)
#по полученным R^2 ясно, что ни одна из моделей не является лучше исходной

#исходя из вышеперечисленных моделей, наилучшей можно считать модель вида lm(formula = salary ~ sex + wed1 + wed2 + wed3 + highter_educ + I(age^2) + city_status + worktime, data = ourdata)
#она обладает скоректированным коэффициентом детерминации большим, чем в исходной модели (0.1815 > 0.1465)
#если же смотреть на кол-во зведочек возле регрессоров, то у полученной модели подле wed1 они отсутствуют,
#в остальном исходная и полученная модели схожи

#Судя по моделям также можно сказать, что на зарплату сильнее всего влияют образование, пол, город и менее значительно возраст
#есть также хоть и не такая значительная кореляция с тем, что человек никогда не состоял в браке
#остальные переменные, как оказалось, не особо коррелируют с уровнем зарплаты

#5)
newdata <- ourdata
newdata <- subset(newdata, sex == 0) #оставляем из выборки только женщин

newdata$salary <- (newdata$salary - mean(newdata$salary))/sqrt(var(newdata$salary))
newdata$age = (newdata$age - mean(newdata$age))/sqrt(var(newdata$age))
newdata$worktime = (newdata$worktime - mean(newdata$worktime))/sqrt(var(newdata$worktime))

singlewoman <- newdata
singlewoman <- subset(newdata, wed1 == 0) #женщины, ни разу не бывшие в браке

singlewomanmodel = lm(salary ~ highter_educ + I(age^2) + city_status + worktime, data = singlewoman)#R^2 = 0.1211
summary(singlewomanmodel) #R^2 = 0.1111 

divwoman <- newdata #женщины, разведенные, живущие в городе
divwoman <- subset(newdata, wed2 == 0 & city_status == 1)

divwomanmodel = lm(salary ~ highter_educ + I(age^2) + worktime, data = divwoman)#R^2 = 0.1211
summary(divwomanmodel) #R^2 = 0.09088

#у singlewoman корреляция у R^2 выше, чем у  divwoman (0.1111 > 0.09088)
#соответственно, зарплата выше у женщин, не бывавших в браке