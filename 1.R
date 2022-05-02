install.packages("ggplot2")
library(ggplot2)
Laufen <- readRDS("C:/Users/carlo/Downloads/201 RCode (2).Rds")


Laufen

reg_laufen <- lm(HR ~ pace, data = Laufen)
summary(reg_laufen)
streu_laufen <- ggplot(data = Laufen, aes(x = pace, y = HR)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE)
streu_laufen

# AUfgabe 1b Geschwindigkeit HR
Laufen_geschwindigkeit <- Laufen
Laufen_geschwindigkeit$pace <- 60 / Laufen_geschwindigkeit$pace
View(Laufen_geschwindigkeit)
reg_laufen_geschwindigkeit <- lm(HR ~ pace, data = Laufen_geschwindigkeit)
summary(reg_laufen_geschwindigkeit)
streu_laufen_geschwindigkeit <- ggplot(data = Laufen_geschwindigkeit, aes(x = pace, y = HR)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE)
streu_laufen_geschwindigkeit                               

# Aufgabe 1d Meilen pro Stunde, HF pro Sekunde
Laufen_meilen <- Laufen_geschwindigkeit
Laufen_meilen$pace <- Laufen_geschwindigkeit$pace/1.61
Laufen_meilen$HR <- Laufen_geschwindigkeit$HR/60
View(Laufen_meilen)
reg_laufen_meilen <- lm(HR ~ pace, data = Laufen_meilen)
summary(reg_laufen_meilen)
streu_laufen__meilen <- ggplot(data = Laufen_meilen, aes(x = pace, y = HR)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE)
streu_laufen__meilen

# Aufgabe 1 e Zentrieren speed
Laufen_zentrieren <- Laufen_geschwindigkeit
Laufen_zentrieren$pace <- Laufen_zentrieren$pace- mean(Laufen_zentrieren$pace)
Laufen_zentrieren 
reg_laufen_zentrieren <- lm(HR ~ pace, data = Laufen_zentrieren)
summary(reg_laufen_zentrieren)
streu_laufen__zentrieren <- ggplot(data = Laufen_zentrieren, aes(x = pace, y = HR)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE)
streu_laufen__zentrieren
cor(Laufen_zentrieren$pace, Laufen_zentrieren$HR)
# 0.561747
gsg