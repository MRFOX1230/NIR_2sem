library("lmtest") #подключаем библиотеку для выявления линейной зависимости
#Набор данных: swiss
#Объясняемая переменная: Infant.Mortality
#Регрессоры: Catholic, Agriculture, Education

data = swiss#подключаем набор данных swiss


#/////////////////////////////////////////////////////////
#2.1
#1) Проверяем регрессоры на линейную независимость
linmodel1 = lm(Catholic~Agriculture, data)
summary(linmodel1) #R^2 = 0.1422 - нет зависимиости между регрессорами (R^2<30%)

linmodel2 = lm(Catholic~Education, data)
summary(linmodel2) #R^2 = 0.001976 - нет зависимости между регрессорами (R^2<30%)

linmodel3 = lm(Agriculture~Education, data)
summary(linmodel3) #R^2 = 0.3959 - есть зависимость между регрессорами (R^2>30%)
#наибольшие зависимости наблюдаются там, где имеется параметр Agriculture, поэтому попробуем его исключить

modeltest1 = lm(Infant.Mortality~Catholic + Education, data)
summary(modeltest1) #R^2 = -0.007655 (R^2<30%)
#скоректированный коэффициент детерминации получился отрицательным, что указывает на плохую взаимосвязь между
#объясняемой переменной и регрессорами

#попробуем из модели исключить зависимую переменную Education
modeltest2 = lm(Infant.Mortality~Catholic + Agriculture, data)
summary(modeltest2) #R^2 = 0.008206 (R^2<30%)
#взимосвязь между Infant.Mortality и регрессорами улучшилась, но модель все еще считается плохой

#проверим модель со всеми переменными
modeltest3 = lm(Infant.Mortality~Catholic + Agriculture + Education, data)
summary(modeltest3) #R^2 = 0.03409 (R^2<30%)
#в этом случае коэффициент детерминации больше, поэтому оставим в дальнейшем исходную модель без изменений

#//////////////////////////////////////////////////////////////////////
#2) Проверим на зависимость объясняемую переменную от регрессоров
#из модели modeltest3 R^2 = 0.03409 (R^2<30%)
#и есть "." возле Catholic (pr = 0.0954) и Agriculrure (pr = 0.0957), но " " у Education (pr = 0.1472),
#из этого есть зависимость между Infant.Mortality и регрессорами (кроме Education), но она минимальна


#//////////////////////////////////////////////////////////////////
#3) Попробуем ввести в нашу модель логарифмы регрессоров
logmodel1 = lm(Infant.Mortality~I(log(Catholic)) + Agriculture + Education, data)
summary(logmodel1) #R^2 = 0.05165 (R^2<30%)
#значение R^2 лучше предыдущей модели

logmodel2 = lm(Infant.Mortality~I(log(Agriculture)) + Catholic + Education, data)
summary(logmodel2) #R^2 = -0.02406 (R^2<30%)
#R^2<0, плохая модель

logmodel3 = lm(Infant.Mortality~I(log(Education)) + Catholic + Agriculture, data)
summary(logmodel3) #R^2 = -0.0001944  (R^2<30%)
#R^2<0, плохая модель

logmodel4 = lm(Infant.Mortality~I(log(Catholic + Agriculture + Education)), data)
summary(logmodel4) #R^2 = -0.004418 (R^2<30%)
#R^2<0, плохая модель


#из представленных логарифмических моделей наилучшей оказалась logmodel1,
#поэтому мы можем заменить modeltest3 на нее


#/////////////////////////////////////////////////////////////////
#4) Введем в нашу модель всевозможные пары прооизведений регрессоров
mulmodel1 = lm(Infant.Mortality~Agriculture^2+Catholic+Education, data)
summary(mulmodel1) #R^2 = 0.03409
#нет изменений по сравнению с исходной моделью

mulmodel2 = lm(Infant.Mortality~Agriculture+Catholic^2+Education, data)
summary(mulmodel2) #R^2 = 0.03409
#нет изменений

mulmodel3 = lm(Infant.Mortality~Agriculture+Catholic+Education^2, data)
summary(mulmodel3) #R^2 = 0.03409
#нет изменений

mulmodel4 = lm(Infant.Mortality~Agriculture*Catholic*Education, data)
summary(mulmodel4) #R^2 = 0.1114
#R^2 лучше, чем в исходной модели

mulmodel5 = lm(Infant.Mortality~Agriculture*Catholic+Education, data)
summary(mulmodel5) #R^2 = 0.1587
#R^2 лучше, чем в предыдущей модели

mulmodel6 = lm(Infant.Mortality~Agriculture+Catholic*Education, data)
summary(mulmodel6) #R^2 = 0.04858
#R^2 лучше, чем в исходной модели, но хуже, чем в предыдущих 2-ух

mulmodel7 = lm(Infant.Mortality~Agriculture*Education+Catholic, data)
summary(mulmodel7) #R^2 = 0.02573
#R^2 хуже, чем в исходной модели

#следовательно, mulmodel4, mulmodel5, mulmodel6 лучше отображают связь между
#объясняемой переменной и регрессорами, чем modeltest3


#//////////////////////////////////////////////////////////////////
#2.2
#1-2) Оценим доверительные интервалы для всех коэффициентов в модели modeltest3, p = 95%
modeltest3 = lm(Infant.Mortality~Catholic + Agriculture + Education, data)
summary(modeltest3) #R^2 = 0.03409 (R^2<30%)

#Intercept: est = 22.36903, error = 1.75389
#Catholic: est = 0.01904, error = 0.01117
#Agriculture: est = -0.04490, error = 0.02636
#Education: est = -0.08520, error = 0.05771

#p = 0.975 (95%)
#k = 43 - 4 = 39
t = qt(0.975, 39) #критерий Стьюдента
print(t)
#t = 2.022691

#Пределы ошибок при оценке каждой переменной
#Intercept: est = 22.36903+-1.75389*2.022691 = [18.82145, 25.91661] (0 не попадает в данный интервал, статистическая гипотеза отвергнута)
#Catholic:est = 0.01904+-0.01117*2.022691 = [-0.00355, 0.04163] (0 попадает в данный интервал, статистическая гипотеза верна)
#Agriculture: est = -0.04490+-0.02636*2.022691 = [-0.09822, 0.00842] (0 попадает в данный интервал, статистическая гипотеза верна)
#Education: est = -0.08520+-0.05771*2.022691 = [-0.96873, -0.73527] (0 не попадает в данный интервал, статистическая гипотеза отвергнута)

#////////////////////////////////////////////////////////////////
#3) Найдем доверительный интервал для прогноза Infant.Mortality

new.data = data.frame(Catholic = 30, Agriculture = 20, Education = 20) #набор значений регрессоров выбрали сами
predict(modeltest3, new.data, interval = "confidence")
#fit = 20.33831 (прогноз)
#lwr = 18.89036 (нижнее значение)
#upr = 21.78626 (верхнее значение)