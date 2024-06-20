#import libraries
library(tidyr) # for `gather`
library(dplyr) # for `rename` & `select`
library(ggplot2)
library(readr)
library(reshape2)
library(viridis)
library(hrbrthemes)


#import dataset
df <- read_csv("Energyaccess.csv")

df %>% select(Year, electricityaccess, cookingfueltechaccess) %>%
  pivot_longer(., cols = c(electricityaccess, cookingfueltechaccess),
               names_to = "Access", values_to = "Growth (%)")%>%
  ggplot(aes(x = Year, y = `Growth (%)`, fill = Access)) +
  geom_point() + geom_line() + theme_bw() + theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(#title = "Access to energy in India",
       y = "% of population") +
  facet_grid(Access ~ ., scales ="free_y",
             labeller = as_labeller( # redefine the text that shows up for the facets
               c(electricityaccess = "Electricity",
                 cookingfueltechaccess = "Clean fuels & technologies for cooking")))

#import dataset
df1 <- read_csv("Energyuse.csv")

df1 %>% select(Year, `Primaryenergyconsumption(TWh)`, `Energypercapita(kWh)`) %>%
  pivot_longer(., cols = c(`Primaryenergyconsumption(TWh)`, `Energypercapita(kWh)`),
               names_to = "Usage", values_to = "Consumption")%>%
  ggplot(aes(x = Year, y = Consumption, fill = Usage)) +
  geom_point() + geom_line() + theme_bw() + theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(#title = "Energy consumption in India",
       y = "Energy") +
  facet_grid(Usage ~ ., scales ="free_y",
             labeller = as_labeller( # redefine the text that shows up for the facets
               c(`Primaryenergyconsumption(TWh)` = "Primary energy consumption (TWh)",
                 `Energypercapita(kWh)` = "Energy per person per year (kWh)")))

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

df1 %>% select(Year, `Primaryenergyconsumption(TWh)`, `Energypercapita(kWh)`) %>%
  pivot_longer(., cols = c(`Primaryenergyconsumption(TWh)`, `Energypercapita(kWh)`),
               names_to = "Allocation", values_to = "Emissions")%>%
  ggplot(aes(x = Year, y = Emissions, colour = Allocation)) +
  geom_line() + geom_point() + theme_bw() + theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(#title = "Energy consumption in India", 
       x = "Year", 
       y = "Energy") +
  scale_color_manual(name="Consumption",
                     labels=c("Primary energy consumption (TWh)", 
                              "Energy per person per year (kWh)"), 
                     #values=c(df$Consumption, df$Production))
                     values=cbbPalette)

#import dataset #Biofuels, Solar, Wind, Hydro, Nuclear, Gas, Coal, Oil, Geo
df2 <- read_csv("Energymix.csv")

data <- df2 %>% select(Year, Biofuels, Solar, Wind, Hydro, Nuclear, Gas, Coal, Oil, Geo) %>%
  pivot_longer(., cols = c(Biofuels, Solar, Wind, Hydro, Nuclear, Gas, Coal, Oil, Geo),
               names_to = "Source", values_to = "Energyconsumption")
  


# Compute percentages with dplyr
datap <- data  %>%
  group_by(Year, Source) %>%
  summarise(n = sum(Energyconsumption)) %>%
  mutate(Percentage = n / sum(n))

# Plot
ggplot(data, aes(x=Year, y=Energyconsumption, fill=Source)) + 
  geom_area(alpha=0.6 , size=1, colour="white") +
  labs(#title = "Energy consumption in India based on sources",
       y = "Energy consumption (TWh)") +
  theme_bw() + theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#---------------------------------------------------------------------UNUSED----
ggplot(datap, aes(x=Year, y=Percentage, fill=Source)) + 
  geom_area(alpha=0.6 , size=1, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum()


