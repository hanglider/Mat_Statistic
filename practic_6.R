doctor <- read_excel("IT/Mat_Statistic/doctor.xlsx")

doctor$y <- as.factor(doctor$y)
doctor$x4 <- as.factor(doctor$x4)
str(doctor)
logit <- glm(y ~ x1 + x2, data = doctor, family = binomial("logit"))
summary(logit)

# Проверка значимости модели (Критерий Вальтера)
install.packages("aod")
library(aod)
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 2)

# Доверительные интервалы коэфицентов модели 
confint(logit)

# Предсказынне значения в модели
prlogit <- predict(logit, newdata = doctor, type = "response")
prlogit

# Преобразуем предсказынные значения в 0, 1
doctor$pr01 <- ifelse(prlogit >= 0.5, 1, 0)

# Таблица истиных и ложных значений
table(doctor$y, doctor$pr01)

# Чувствительность модели
s <- 33/37
s
# Специфинчость
sp <- 17/21
sp
# ROC Кривая
install.packages("sjPlot")
library(sjPlot)
plot_model(logit, type = "pred", term = "x1", labels=(y = "1"))

# Построение ROC Кривой
install.packages("pROC")
library(pROC)
res <- roc(as.factor(y) ~ fitted(logit), data = doctor)
plot(res)
probit <- glm(y ~ x1 + x2, data = doctor, family = binomial("probit"))
summary(probit)
prprobit <- predict(probit, newdata = doctor, type = "response")
prprobit
doctor$p01 <- ifelse(prprobit >= 0.5, 1, 0)
doctor$p01
table(doctor$y, doctor$p01)

l <- glm(y ~ x1 + x2 + x3 + x4 + x5, data = doctor, family = binomial("logit"))
summary(l)
str(doctor)
install.packages("MASS")
library(MASS)
stepAIC(l)
