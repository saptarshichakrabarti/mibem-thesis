#import libraries
library(tidyr) # for `gather`
library(dplyr) # for `rename` & `select`
library(ggplot2)
library(readr)
library(reshape2)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

#import dataset
df <- read_csv("CO2inttrade.csv")

df %>% select(Year, Consumption, Production) %>%
  pivot_longer(., cols = c(Consumption, Production),
               names_to = "Allocation", values_to = "Emissions")%>%
  ggplot(aes(x = Year, y = Emissions, colour = Allocation)) +
  geom_line() + geom_point() + theme_bw() + theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(#title = expression("CO"["2"] ~ "emissions embedded in international trade"), 
       x = "Year", 
       y = expression("CO"["2"] ~ "emissions in metric tonnes")) +
  scale_color_manual(name="Allocation",
                     labels=c("Consumption-based emissions", 
                              "Production-based emissions"), 
                     #values=c(df$Consumption, df$Production))
                     values=cbbPalette)
