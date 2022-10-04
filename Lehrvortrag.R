## Benötigte Pakete laden
# install.packages("lme4")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("scales")
library(lme4)
library(ggplot2)
library(dplyr)
library(scales)

## Daten importieren
data <- read.csv("https://github.com/timoadam/Lehrvortrag/blob/main/Bike_Sharing.csv")
data$Station <- as.factor(data$Station) # Station faktorisieren
head(data)
dim(data)

## Farbpalette mit 5 Farben erstellen
stations <- c("31229", "31603", "31124", "31245", "31125")
pal <- viridis_pal()(5)
col <- rep(pal[1], nrow(data))
col[which(data$Station == stations[2])] <- pal[2]
col[which(data$Station == stations[3])] <- pal[3]
col[which(data$Station == stations[4])] <- pal[4]
col[which(data$Station == stations[5])] <- pal[5]

## Poisson-Modell (Folie 5)

# Modell schätzen
mod_1 <- glm(Anzahl ~ Temperatur, family = "poisson", data = data)

## Geschätztes Modell
ggplot(data = data) +
  geom_point(aes(x = Temperatur, y = Anzahl), alpha = 0.25, size = 1.5) +
  geom_line(aes(x = Temperatur, y = mod_1$fitted), size = 1) +
  xlab("Temperatur (°C)") + ylab("Anzahl") + ggtitle("Poisson-Modell") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Poisson-Modell mit Residuen (Folie 6)

## Modell mit Residuen
ggplot(data = data) +
  geom_point(aes(x = Temperatur, y = Anzahl), col = col, alpha = 0.25, size = 1.5) +
  geom_segment(aes(x = Temperatur, xend = Temperatur, y = mod_1$fitted, yend = Anzahl), color = col, size = 0.1) +
  geom_line(aes(x = Temperatur, y = mod_1$fitted), size = 1) +
  xlab("Temperatur (°C)") + ylab("Anzahl") + ggtitle("Poisson-Modell") + 
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Residuenverteilung (Folie 6):

## Residuen berechnen und als neue Spalte zu den Daten hinzufügen
residuals_mod_1 <- cbind(data, residuals = data$Anzahl - fitted(mod_1))

## Residuenverteilung
ggplot(residuals_mod_1) +
  geom_histogram(aes(x = residuals, y = ..density.., fill = Station), bins = 40, position = "identity") +
  geom_density(aes(x = residuals, y = after_stat(density), group = Station, color = Station), size = 1) +
  scale_fill_manual(values = alpha(pal[c(3, 5, 1, 4, 2)], 0.25)) +
  scale_color_manual(values = pal[c(3, 5, 1, 4, 2)]) +
  geom_vline(xintercept = 0, size = 1, linetype = "dashed") +
  xlab("Residuen") + ylab("Dichte") + ggtitle("Residuenverteilung") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Random Intercept-Modell (Folie 8)

## Modell schätzen
mod_2 <- glmer(Anzahl ~ scale(Temperatur) + (1 | Station), data = data, family = "poisson", control = glmerControl(optimizer = "Nelder_Mead"))

## Gefittete Werte berechnen
y_hat_1 = predict(mod_2, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[1]))
y_hat_2 = predict(mod_2, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[2]))
y_hat_3 = predict(mod_2, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[3]))
y_hat_4 = predict(mod_2, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[4]))
y_hat_5 = predict(mod_2, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[5]))

## Geschätztes Modell
ggplot(data = data) +
  geom_point(aes(x = Temperatur, y = Anzahl), col = col, alpha = 0.25, size = 1.5) +
  geom_line(aes(x = seq(min(Temperatur), max(Temperatur), length = 1821), y = exp(fixef(mod_2)[1] + fixef(mod_2)[2] * scale(seq(min(Temperatur), max(Temperatur), length = 1821)))), size = 1, linetype = "dashed") + 
  geom_line(aes(x = Temperatur, y = y_hat_1), col = pal[1], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_2), col = pal[2], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_3), col = pal[3], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_4), col = pal[4], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_5), col = pal[5], size = 1) +
  xlab("Temperatur (°C)") + ylab("Anzahl") + ggtitle("Random Intercept-Modell") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Modell mit Residuen (Folie 9)
ggplot(data = data) +
  geom_point(aes(x = Temperatur, y = Anzahl), col = col, alpha = 0.25, size = 1.5) + 
  geom_line(aes(x = Temperatur, y = predict_mod21), col = pal[1], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod22), col = pal[2], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod23), col = pal[3], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod24), col = pal[4], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod25), col = pal[5], size = 1) + 
  geom_segment(aes(x = Temperatur, xend = Temperatur, y = fitted(mod_2), yend = Anzahl), color = col, size = 0.1) +
  geom_line(aes(x = seq(min(Temperatur), max(Temperatur), length = 1821), y = exp(fixef(mod_2)[1] + fixef(mod_2)[2] * scale(seq(min(Temperatur), max(Temperatur), length = 1821)))), size = 1, linetype = "dashed") +
  xlab("Temperatur (°C)") + ylab("Anzahl") + ggtitle("Random Intercept-Modell") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Residuenverteilung (Folie 9)

## Residuen berechnen und als neue Spalte zu den Daten hinzufügen
residuals_mod_2 <- cbind(data, residuals = data$Anzahl - fitted(mod_2))

## Residuenverteilung
ggplot(residuals_mod_2) +
  geom_histogram(aes(x = residuals, y = ..density.., fill = Station), bins = 40, position = "identity") +
  geom_density(aes(x = residuals, y = after_stat(density), group = Station, color = Station), size = 1) +
  scale_fill_manual(values = alpha(pal[c(3, 5, 1, 4, 2)], 0.25)) +
  scale_color_manual(values = pal[c(3, 5, 1, 4, 2)]) +
  geom_vline(xintercept = 0, size = 1, linetype = "dashed") +
  xlab("Residuen") + ylab("Dichte") + ggtitle("Residuenverteilung") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Random Slope-Modell (Folie 11)

## Modell schätzen
mod_3 <- glmer(Anzahl ~ scale(Temperatur) + (scale(Temperatur) | Station), family = "poisson", control = glmerControl(optimizer = "Nelder_Mead"), data = data)

## Gefittete Werte berechnen
y_hat_1 = predict(mod_3, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[1]))
y_hat_2 = predict(mod_3, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[2]))
y_hat_3 = predict(mod_3, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[3]))
y_hat_4 = predict(mod_3, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[4]))
y_hat_5 = predict(mod_3, type = "response", newdata = data.frame(Temperatur = data$Temperatur, Station = stations[5]))

## Geschätztes Modell
ggplot(data = data) +
  geom_point(aes(x = Temperatur, y = Anzahl), col = col, alpha = 0.25, size = 1.5) + 
  geom_line(aes(x = Temperatur, y = y_hat_1), col = pal[1], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_2), col = pal[2], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_3), col = pal[3], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_4), col = pal[4], size = 1) +
  geom_line(aes(x = Temperatur, y = y_hat_5), col = pal[5], size = 1) +
  geom_line(aes(x = seq(min(Temperatur), max(Temperatur), length = 1821), y = exp(fixef(mod_3)[1] + fixef(mod_3)[2] * scale(seq(min(Temperatur), max(Temperatur), length = 1821)))), size = 1, linetype = "dashed") +
  xlab("Temperatur (°C)") + ylab("Anzahl") + ggtitle("Random Slope-Modell") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Modell mit Residuen (Folie 12)
plot_4b <- ggplot(data = data) +
  geom_point(aes(x = Temperatur, y = Anzahl), col = col, alpha = 0.25, size = 1.5) + 
  geom_line(aes(x = Temperatur, y = predict_mod31), col = pal[1], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod32), col = pal[2], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod33), col = pal[3], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod34), col = pal[4], size = 1) +
  geom_line(aes(x = Temperatur, y = predict_mod35), col = pal[5], size = 1) + 
  geom_segment(aes(x = Temperatur, xend = Temperatur, y = fitted(mod_3), yend = Anzahl), color = col, size = 0.1) +
  geom_line(aes(x = seq(min(Temperatur), max(Temperatur), length = 1821), y = exp(fixef(mod_3)[1] + fixef(mod_3)[2] * scale(seq(min(Temperatur), max(Temperatur), length = 1821)))), size = 1, linetype = "dashed") +
  xlab("Temperatur (°C)") + ylab("Anzahl") + ggtitle("Random Slope-Modell") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")

## Residuenverteilung (Folie 12)

## Residuen berechnen und als neue Spalte zu den Daten hinzufügen
residuals_mod_3 <- cbind(data, residuals = data$Anzahl - fitted(mod_3))

## Residuenverteilung
ggplot(residuals_mod_3) +
  geom_histogram(aes(x = residuals, y = ..density.., fill = Station), bins = 40, position = "identity") +
  geom_density(aes(x = residuals, y = after_stat(density), group = Station, color = Station), size = 1) +
  scale_fill_manual(values = alpha(pal[c(3, 5, 1, 4, 2)], 0.25)) +
  scale_color_manual(values = pal[c(3, 5, 1, 4, 2)]) +
  geom_vline(xintercept = 0, size = 1, linetype = "dashed") +
  xlab("Residuen") + ylab("Dichte") + ggtitle("Residuenverteilung") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "gray97"), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none")
