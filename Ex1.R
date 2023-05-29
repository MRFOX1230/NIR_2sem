library("lmtest") #подключаем библиотеку lmtest для работы с линейной регрессией
data = swiss #подключаем набор данных swiss

#Объясняемая переменная: Education
#Объясняющие переменные (регрессоры): Agriculture, Fertility

#//////////////////////////////////////////////
#1) Вычислить средние значения, десперсии и СКО
#Вычисляем средние значения Education, Agriculture, Fertility
mean(data$Education) # 10.97872
mean(data$Agriculture) # 50.65957
mean(data$Fertility) # 70.14255

#для иллюстрации наших зависимостей
plot(data$Agriculture, data$Education) + abline(a = 10.97872, b = 0, col = "red")
plot(data$Fertility, data$Education) + abline(a = 10.97872, b = 0, col = "red")

#Вычисляем дисперсии Education, Agriculture, Fertility
var(data$Education) # 92.45606 (дисперсия малая, т.к. меньше 100)
var(data$Agriculture) # 515.7994 (большая дисперсия)
var(data$Fertility) # 156.0425 (большая дисперсия)

#Вычисляем средние квадраты отклонений Education, Agriculture, Fertility
sd(data$Education) # 9.615407 (приемлемое СКО)
sd(data$Agriculture) # 22.71122 (большое)
sd(data$Fertility) # 12.4917 (большое)


#/////////////////////////////////////////////
#2) Построить зависимости вида y = a + bx (y - объясняемая переменная, x - регрессор)
model1 = lm(Education~Agriculture, data)
summary(model1)
#Education=24.69527-0.27076*Agriculture (график накланен вниз)
#R^2=0.3959 (хорошая зависимость)
#"***" у регрессора (между переменными есть взаимосвязь)
model2 = lm(Education~Fertility, data)
summary(model2)
#Education=46.81788-0.51095*Fertility  (график наклонен вниз)
#R^2=0.4282 (хорошая зависимость)
#"***" у регрессора (между переменными есть взаимосвязь)

#изображаем получившиеся графики наших зависимостей
#для model1
plot(data$Agriculture, data$Education) + abline(a = 24.69527, b = -0.27076, col = "red")
#для model2
plot(data$Fertility, data$Education) + abline(a = 46.81788, b = -0.51095, col = "red")
