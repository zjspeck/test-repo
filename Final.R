library(readr)
Pitching <- read_csv("~/Desktop/DATA 824/Pitching.csv")
Salaries <- read_csv("~/Desktop/DATA 824/Salaries.csv")

Pitching <- subset(Pitching, select = c(playerID, yearID, teamID, W, L, G, GS, CG, SHO, SV, IPouts, H, ER, HR, BB, SO, ERA))

library(dplyr)  

PitchSals<-full_join(Salaries, Pitching, by = c("playerID"="playerID", "yearID"="yearID", "teamID"="teamID")) 
PitchSals <- na.omit(PitchSals)
View(PitchSals)

#install.packages("priceR")
library(priceR)
PitchSals$Salarie_2015 <- adjust_for_inflation(PitchSals$salary, PitchSals$yearID, "US", to_date = 2015)

PitchSals$Win_Perct <- (PitchSals$W/PitchSals$G)
PitchSals$Loss_Perct <- (PitchSals$L/PitchSals$G)

library(hflights)
library(tidyr)
Summary_Salarie<-summarise(PitchSals, 
                     `min Salarie`  = min(Salarie_2015, na.rm = T),
                     `max Salarie`  = max(Salarie_2015, na.rm = T),
                     `mean Salarie` = mean(Salarie_2015, na.rm = T),
                     `median Salarie`  = median(Salarie_2015, na.rm = T))
View(Summary_Salarie)

library(ggplot2)

PitchSals %>% 
  ggplot(aes(x = Salarie_2015)) +
  geom_histogram(binwidth = 100, color = "black", fill = "deepskyblue3") +
  xlab("Adjusted Salaries") +
  ylab("Frequency") +
  ggtitle("Pitcher Salarie Frequency over 30 years") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

PitchSals %>% 
  ggplot(aes(x = Salarie_2015)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("Adjusted Salarie") +
  ylab("Density") +
  ggtitle("Pitcher Salarie - Density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

library(corrplot)

PitchCor <- subset(PitchSals, select = c(Salarie_2015, Win_Perct, Loss_Perct, GS, CG, SHO, SV, IPouts, H, ER, HR, BB, SO, ERA))
M<-cor(PitchCor,method="pearson")
corrplot.mixed(M,
               lower = "number", 
               upper = "circle",
               tl.col = "black")
corrplot(M,method="pie")

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(PitchCor, histogram = TRUE, method = "pearson")

ggplot(data = PitchSals, mapping = aes(x = Win_Perct, y = Salarie_2015)) +
  geom_point(size = 5, alpha = 1/3) +
  geom_smooth(method = "lm")

PitchSals %>% 
  ggplot(aes(x = Win_Perct, y = Salarie_2015, color = Win_Perct)) +
  geom_boxplot(size = 1.3, 
               outlier.alpha = 1/15, 
               outlier.size = 5) +
  #scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_color_viridis_d(option = "plasma") +
  xlab("Win Percentage") +
  ylab("Adjusted Salaries") +
  ggtitle("Pitchers Salaries VS Win Percentage - Boxplot") +
  coord_flip() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

install.packages("googleVis")
library(googleVis)
b <- gvisBubbleChart(PitchSals, idvar="playerID",
                     xvar="Win_Perct", yvar="Salarie_2015",
                     colorvar="teamID", sizevar="SO")

plot(b)
