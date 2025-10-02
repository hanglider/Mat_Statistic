house <- read.delim2("/Users/ivan/IT/Mat_Statistic/House.txt")
advertising <- read.csv("/Users/ivan/IT/Mat_Statistic/Advertising.csv")
ad <- advertising

# Зависимость продаж от ТВ

plot(ad)
l1 <- lm(ad$Sales ~ ad$TV)
summary(l1)
plot(l1, 1:2)
l1$coefficients
l1$fitted.values
plot(ad$Sales, l1$fitted.values)

# Зависимость продаж от радио

l2 <- lm(ad$Sales ~ ad$Radio)
summary(l2)
plot(l2)

# Зависимость продаж от рекламмы в газете

l3 <- lm(ad$Sales ~ ad$Newspaper)
summary(l3)
plot(l2)

# Множественная регрессия со всеми факторами

l4 <- lm(Sales ~ TV + Radio + Newspaper, data = ad)
summary(l4)
predict(l1, level = 0.95)
predict(l2, newdata = data.frame(x = 300)) #что это ?