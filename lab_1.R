#===========================================
# Лабораторная работа №1
# Анализ датасетов Airport и Babyboom
#===========================================

#==============================
# 1. Датасет Airport
#==============================

# Чтение построчно, парсинг регуляркой
txt <- readLines("IT/Mat_Statistic/Data/airportdat.txt", warn = FALSE)

# Шаблон строки: два текстовых поля, потом 5 числовых
pat <- "^\\s*(.+?)\\s{2,}(.+?)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([0-9]+\\.?[0-9]*)\\s+([0-9]+\\.?[0-9]*)\\s*$"

proto <- data.frame(
  Airport_Name = character(),
  City = character(),
  Scheduled_Dep = integer(),
  Performed_Dep = integer(),
  Enplaned_Passengers = integer(),
  Enplaned_Revenue_Tons_Freight = double(),
  Enplaned_Revenue_Tons_Mail = double(),
  stringsAsFactors = FALSE
)

airport <- strcapture(pattern = pat, x = txt, proto = proto)

# Проверка структуры
str(airport)
summary(airport)

# Берём только числовые переменные
data_airport <- airport[, c("Scheduled_Dep", "Performed_Dep", 
                            "Enplaned_Passengers", 
                            "Enplaned_Revenue_Tons_Freight", 
                            "Enplaned_Revenue_Tons_Mail")]

# --- Гистограммы ---
par(mfrow = c(3, 2))
hist(data_airport$Scheduled_Dep, main = "Scheduled Departures", xlab = "", col = "lightblue", border = "white")
hist(data_airport$Performed_Dep, main = "Performed Departures", xlab = "", col = "lightblue", border = "white")
hist(data_airport$Enplaned_Passengers, main = "Enplaned Passengers", xlab = "", col = "lightblue", border = "white")
hist(data_airport$Enplaned_Revenue_Tons_Freight, main = "Revenue Tons of Freight", xlab = "", col = "lightblue", border = "white")
hist(data_airport$Enplaned_Revenue_Tons_Mail, main = "Revenue Tons of Mail", xlab = "", col = "lightblue", border = "white")

# --- Boxplot ---
par(mfrow = c(3, 2))
boxplot(data_airport$Scheduled_Dep, horizontal = TRUE, main = "Scheduled Departures", col = "lightgreen")
boxplot(data_airport$Performed_Dep, horizontal = TRUE, main = "Performed Departures", col = "lightgreen")
boxplot(data_airport$Enplaned_Passengers, horizontal = TRUE, main = "Enplaned Passengers", col = "lightgreen")
boxplot(data_airport$Enplaned_Revenue_Tons_Freight, horizontal = TRUE, main = "Revenue Tons of Freight", col = "lightgreen")
boxplot(data_airport$Enplaned_Revenue_Tons_Mail, horizontal = TRUE, main = "Revenue Tons of Mail", col = "lightgreen")

# --- Основные статистики ---
summary(data_airport)

mean(data_airport$Scheduled_Dep, na.rm = TRUE)
mean(data_airport$Performed_Dep, na.rm = TRUE)
mean(data_airport$Enplaned_Passengers, na.rm = TRUE)
mean(data_airport$Enplaned_Revenue_Tons_Freight, na.rm = TRUE)
mean(data_airport$Enplaned_Revenue_Tons_Mail, na.rm = TRUE)

var(data_airport$Scheduled_Dep, na.rm = TRUE)
sd(data_airport$Scheduled_Dep, na.rm = TRUE)

var(data_airport$Performed_Dep, na.rm = TRUE)
sd(data_airport$Performed_Dep, na.rm = TRUE)

var(data_airport$Enplaned_Passengers, na.rm = TRUE)
sd(data_airport$Enplaned_Passengers, na.rm = TRUE)

var(data_airport$Enplaned_Revenue_Tons_Freight, na.rm = TRUE)
sd(data_airport$Enplaned_Revenue_Tons_Freight, na.rm = TRUE)

var(data_airport$Enplaned_Revenue_Tons_Mail, na.rm = TRUE)
sd(data_airport$Enplaned_Revenue_Tons_Mail, na.rm = TRUE)

# --- Квантили и выбросы ---
Q1 <- quantile(data_airport$Enplaned_Passengers, 0.25, na.rm = TRUE)
Q3 <- quantile(data_airport$Enplaned_Passengers, 0.75, na.rm = TRUE)
U <- Q3 + 1.5 * (Q3 - Q1)
L <- Q1 - 1.5 * (Q3 - Q1)
subset(data_airport, data_airport$Enplaned_Passengers < L | data_airport$Enplaned_Passengers > U)

# --- Эмпирические функции распределения ---
par(mfrow = c(3, 2))
plot(ecdf(data_airport$Scheduled_Dep), main = "ECDF Scheduled", xlab = "", ylab = "F(x)")
plot(ecdf(data_airport$Performed_Dep), main = "ECDF Performed", xlab = "", ylab = "F(x)")
plot(ecdf(data_airport$Enplaned_Passengers), main = "ECDF Passengers", xlab = "", ylab = "F(x)")
plot(ecdf(data_airport$Enplaned_Revenue_Tons_Freight), main = "ECDF Freight", xlab = "", ylab = "F(x)")
plot(ecdf(data_airport$Enplaned_Revenue_Tons_Mail), main = "ECDF Mail", xlab = "", ylab = "F(x)")

# --- Попарные коэффициенты корреляции ---
cor(data_airport, use = "pairwise.complete.obs")


#==============================
# 2. Датасет Babyboom
#==============================

baby <- read.table("IT/Mat_Statistic/Data/babyboom.dat.txt", header = FALSE)
colnames(baby) <- c("Time", "Sex", "Weight", "MinutesAfterMidnight")

# Проверка структуры
str(baby)
summary(baby)

# --- Гистограммы ---
par(mfrow = c(1, 2))
hist(baby$Weight, main = "Вес новорожденных (г)", xlab = "", col = "lightblue", border = "white")
hist(baby$MinutesAfterMidnight, main = "Минуты после полуночи", xlab = "", col = "lightblue", border = "white")

# --- Boxplot ---
par(mfrow = c(1, 2))
boxplot(baby$Weight, horizontal = TRUE, main = "Вес новорожденных", col = "lightgreen")
boxplot(baby$MinutesAfterMidnight, horizontal = TRUE, main = "Время рождения", col = "lightgreen")

# --- Основные статистики ---
summary(baby$Weight)
summary(baby$MinutesAfterMidnight)

mean(baby$Weight)
var(baby$Weight)
sd(baby$Weight)
quantile(baby$Weight, c(0.25, 0.5, 0.75))

mean(baby$MinutesAfterMidnight)
var(baby$MinutesAfterMidnight)
sd(baby$MinutesAfterMidnight)
quantile(baby$MinutesAfterMidnight, c(0.25, 0.5, 0.75))

# --- Проверка выбросов методом Тьюки ---
Q1_w <- quantile(baby$Weight, 0.25)
Q3_w <- quantile(baby$Weight, 0.75)
U_w <- Q3_w + 1.5 * (Q3_w - Q1_w)
L_w <- Q1_w - 1.5 * (Q3_w - Q1_w)
subset(baby, baby$Weight < L_w | baby$Weight > U_w)

Q1_t <- quantile(baby$MinutesAfterMidnight, 0.25)
Q3_t <- quantile(baby$MinutesAfterMidnight, 0.75)
U_t <- Q3_t + 1.5 * (Q3_t - Q1_t)
L_t <- Q1_t - 1.5 * (Q3_t - Q1_t)
subset(baby, baby$MinutesAfterMidnight < L_t | baby$MinutesAfterMidnight > U_t)

# --- Эмпирическая функция распределения ---
par(mfrow = c(1, 2))
plot(ecdf(baby$Weight), main = "ECDF веса", xlab = "Вес (г)", ylab = "F(x)")
plot(ecdf(baby$MinutesAfterMidnight), main = "ECDF времени", xlab = "Минуты", ylab = "F(x)")

# --- Корреляция ---
cor(baby$Weight, baby$MinutesAfterMidnight)