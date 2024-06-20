#import libraries
library(tidyr) # for `gather`
library(dplyr) # for `rename` & `select`
library(readr)
library(ggplot2)
library(broom)
library(caret)
library(stargazer)
library(reshape2)

#import dataset
df <- read_csv("Kaya_OWID.csv")

df_pop <- subset (df, select = c(Year, Population))
pop <- na.omit(df_pop)
stargazer(summary(pop))
df_gdpc <- subset (df, select = c(Year, GDPpercapita))
gdpc <- na.omit(df_gdpc)
summary(gdpc)
df_energyintensity <- subset (df, select = c(Year, EnergyperGDP)) #https://ourworldindata.org/grapher/energy-intensity?tab=chart&country=~IND
energyintensity <- na.omit(df_energyintensity)
summary(energyintensity)
df_co2intensity <- subset (df, select = c(Year, AnnualCO2emissionsunitenergy)) #https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2021
emissionsintensity <- na.omit(df_co2intensity)
summary(emissionsintensity)


#Analysis of EKC data
par(mfrow=c(2,2))
plot(Population ~ Year, data = pop,
     ylab = "Total population")
plot(GDPpercapita ~ Year, data = gdpc,
     ylab = "GDP per capita (constant 2015 US$)")
plot(EnergyperGDP ~ Year, data = energyintensity,
     ylab = "Energy intensity of GDP")
plot(AnnualCO2emissionsunitenergy ~ Year, data = emissionsintensity,
     ylab = "Emissions intensity of energy")
mtext("Trends in Population, GDP per capita, Energy intensity & Emissions intensity", side=3,line=-17, outer=TRUE, cex = 1.0)

#Correlation
df3 <- subset (df, select = c(Year, Population, GDPpercapita, EnergyperGDP, AnnualCO2emissionsunitenergy)) #https://ourworldindata.org/grapher/energy-intensity?tab=chart&country=~IND
df3 <- na.omit(df3) #removing all missing values

names(df3)[names(df3)=="AnnualCO2emissionsunitenergy"] <- "Carbonintensity"
names(df3)[names(df3)=="EnergyperGDP"] <- "Energyintensity"

cor(df3)


library(readr)
dfpop <- read_csv("predicteddata/PredictedPopulation.csv")
dfGDPpc <- read_csv("predicteddata/PredictedGDPpc.csv")
dfEnergyperGDP <- read_csv("predicteddata/PredictedEnergyperGDP.csv")
dfCO2intensity <- read_csv("predicteddata/PredictedAnnualCO2emissionsunitenergy.csv")

df_kaya <- list(dfpop, dfGDPpc, dfEnergyperGDP, dfCO2intensity)

library(tidyverse)
kaya <- df_kaya %>% reduce(full_join, by='Year')

kaya$totalcarbon <- kaya$Population*kaya$GDPpercapita*kaya$EnergyperGDP*kaya$AnnualCO2emissionsunitenergy

kaya <- kaya[kaya$totalcarbon >= 0, ]

kaya
stargazer(kaya, summary = FALSE)

kaya.graph <- ggplot(kaya, aes(x= Year, y = totalcarbon)) + 
  geom_point() + theme_bw() +
  geom_vline(xintercept = 2030, color = "black", size = 1) +
  geom_vline(xintercept = 2005, color = "black", size = 1) +
  geom_text(x = 2008, y = 2.8e+12, label="2005") +
  geom_text(x = 2033, y = 2.8e+12, label="2030") +
  labs(title = expression("Annual" ~"CO"["2"] ~ "emissions - predicted till 2050"),
       x = "Year", y = expression("Annual" ~"CO"["2"] ~ "emissions")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
kaya.graph
