rm(list=ls(all=TRUE))
Sys.setenv(TZ='GMT')
setwd("~/GitHub/H2S Sargassum workers")

# Load Libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

#Load data
H2SWorkers= read.csv("H2S Sargassum workers.csv", sep = ",")
str(H2SWorkers)

#Transform data
H2SWorkers$Date= as.Date(H2SWorkers$Date, format = "%d/%m/%Y", tz = "GMT")
H2SWorkers$Hour <- as.POSIXct(H2SWorkers$Hour, format = "%H:%M:%S")
H2SWorkers$Municipality = as.factor (H2SWorkers$Municipality)
H2SWorkers$Locality = as.factor (H2SWorkers$Locality)
H2SWorkers$Site = as.factor (H2SWorkers$Site)
H2SWorkers$Code = factor (H2SWorkers$Code, levels = c('PM1', 'PM2', 'PC1', 'PC2', 'PC3', 'OPB1', 'OPB2', 'OPB3'))
H2SWorkers$Sensor = as.factor (H2SWorkers$Sensor)
H2SWorkers$Condition_Sarg = as.factor (H2SWorkers$Condition_Sarg)
H2SWorkers$Brown_tide = as.factor (H2SWorkers$Brown_tide)

#Summary data
Table1<- ddply(H2SWorkers, .(Code), summarise, 
                 N    = length(H2S),
                 Min = round(min(H2S), 1),
                 Max = round(max(H2S), 1),
                 Median=round(median(H2S), 1),
                 Q1 = quantile(H2S, 0.25, na.rm = T),
                 Q3 = quantile(H2S, 0.75, na.rm = T),
                 Mean = round (mean(H2S), 1),
                 sd   =round (sd(H2S), 1))

Table1

#Figure 2 - boxplot H2S concentration by site
ggboxplot(H2SWorkers, x = "Code", y = "H2S", size=0.5, color = "steelblue")+
  labs(x="Site", y = expression(H[2]*S~"(ppm)"))+
  geom_hline(yintercept = mean(H2SWorkers$H2S), linetype = 2, linewidth=1, colour="red")+
  stat_compare_means(label.y = 50, label.x=2, size =5)+        # Add global Kruskal-Wallis p-value
  theme_grey(base_size=14) + theme(axis.text.x = element_text(angle=0), legend.position = "none")

# Kruskal-Wallis rank sum tests among sites
library (pgirmess) 
kruskal.test(H2SWorkers$H2S~H2SWorkers$Code)
kruskalmc(H2SWorkers$H2S~H2SWorkers$Code) 

#Figure 3 - Histograms H2S concentration
#Total
Tot <- H2SWorkers %>%
  mutate(category = cut(H2S,
                        breaks = c(0, 1, 5, 10, 20, 55),
                        include.lowest = TRUE,
                        right = FALSE,
                        labels = c("<1", "1-5", "5-10", "10-20", ">20")))

# Calculate percentage per category
frecuencias0 <- Tot %>%
  group_by(category) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Porcentaje = 100 * Frecuencia / sum(Frecuencia))

# Show percent table
print(frecuencias0)

# Plot histogram with percentages (total)
ggplot(frecuencias0, aes(x = category, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total",
       x = expression(H[2]*S~"(ppm)"),
       y = "Percentage (%)") +
  theme_minimal(base_size =14)



#Per Municipality
PM = H2SWorkers[H2SWorkers$Municipality=='PM',]
PC = H2SWorkers[H2SWorkers$Municipality=='PC',]
OPB = H2SWorkers[H2SWorkers$Municipality=='OPB',]

#Puerto Morelos
PM <- PM %>%
  mutate(category = cut(H2S,
                        breaks = c(0, 1, 5, 10, 20, 25),
                        include.lowest = TRUE,
                        right = FALSE,
                        labels = c("<1", "1-5", "5-10", "10-20", ">20")))

# Calculate percentage per category
frecuencias <- PM %>%
  group_by(category) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Porcentaje = 100 * Frecuencia / sum(Frecuencia))

# Show percent table
print(frecuencias)

# Plot histogram with percentages (Puerto Morelos)
ggplot(frecuencias, aes(x = category, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Puerto Morelos",
       x = expression(H[2]*S~"(ppm)"),
       y = "Percentage (%)") +
    theme_minimal(base_size =14)

#Playa del Carmen
PC <- PC %>%
  mutate(category = cut(H2S,
                        breaks = c(0, 1, 5, 10, 20, 35),
                        include.lowest = TRUE,
                        right = FALSE,
                        labels = c("<1", "1-5", "5-10", "10-20", ">20")))

# Calculate percentage per category
frecuencias2 <- PC %>%
  group_by(category) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Porcentaje = 100 * Frecuencia / sum(Frecuencia))

# Show percent table
print(frecuencias2)

# Plot histogram with percentages (Playa del Carmen)
ggplot(frecuencias2, aes(x = category, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Playa del Carmen",
       x = expression(H[2]*S~"(ppm)"),
       y = "Percentage (%)") +
  theme_minimal(base_size =14)


#Othón P. Blanco
OPB <- OPB %>%
  mutate(category = cut(H2S,
                        breaks = c(0, 1, 5, 10, 20, 55),
                        include.lowest = TRUE,
                        right = FALSE,
                        labels = c("<1", "1-5", "5-10", "10-20", ">20")))

# Calculate percentage per category
frecuencias3 <- OPB %>%
  group_by(category) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Porcentaje = 100 * Frecuencia / sum(Frecuencia))

# Show percent table
print(frecuencias3)

# Plot histogram with percentages (Othón P. Blanco)
ggplot(frecuencias3, aes(x = category, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Othón P. Blanco",
       x = expression(H[2]*S~"(ppm)"),
       y = "Percentage (%)") +
  theme_minimal(base_size =14)
