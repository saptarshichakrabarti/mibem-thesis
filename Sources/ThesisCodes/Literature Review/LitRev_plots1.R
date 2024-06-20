#import libraries
library(tidyr) # for `gather`
library(dplyr) # for `rename` & `select`
library(ggplot2)
library(readr)
library(RColorBrewer)
library(ggthemes)

#import dataset
df <- read_csv("EcoAnN.csv")


#Plot all economic components - NOT USED
# df %>% select(Year, GFCE, PFCE, GFCF, CIS,
#               `Exports of goods and services`,
#               `Imports of goods and services`) %>%
#   pivot_longer(., cols = c(GFCE, PFCE, GFCF, CIS,
#                            `Exports of goods and services`,
#                            `Imports of goods and services`),
#                names_to = "Factors", values_to = "Growth (%)") %>%
#   ggplot(aes(x = Year, y = `Growth (%)`, fill = Factors, group = 1)) +
#   geom_point() + geom_line() + theme(legend.position="none") +
#   labs(title = "Growth  of India") +
#   facet_grid(Factors ~ ., scales = "free_y",
#              labeller = as_labeller( # redefine the text that shows up for the facets
#                c(GFCE = "GFCE",
#                  PFCE = "PFCE", GFCF = "GFCF", CIS = "CIS",
#                  `Exports of goods and services` = "Exports",
#                  `Imports of goods and services` = "Imports")))

#Plot economic components
df %>% select(Year,
              `Exports of goods and services`,
              `Imports of goods and services`) %>%
  pivot_longer(., cols = c(`Exports of goods and services`,
                           `Imports of goods and services`),
               names_to = "Factors", values_to = "Growth (%)") %>%
  ggplot(aes(x = Year, y = `Growth (%)`, fill = Factors, group = 1)) +
  geom_point() + geom_line() + theme_bw() + theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #  labs(title = "Growth in imports and exports of India") +
  facet_grid(Factors ~ ., scales = "free_y",
             labeller = as_labeller( # redefine the text that shows up for the facets
               c(`Exports of goods and services` = "Exports",
                 `Imports of goods and services` = "Imports")))

#Plot economic activities
df %>% select(Year, Agriculture, Industry, Services) %>%
  pivot_longer(., cols = c(Agriculture, Industry, Services),
               names_to = "Factors", values_to = "Growth (%)") %>%
  ggplot(aes(x = Year, y = `Growth (%)`, fill = Factors, group = 1)) +
  geom_point() + geom_line() + theme_bw() + theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
#  labs(title = "Sectorwise growth of India") +
  facet_grid(Factors ~ ., scales = "free_y")

#Plot economic activities
df %>% select(Year, GDP) %>%
  pivot_longer(., cols = c(GDP),
               names_to = "Factors", values_to = "Growth (%)") %>%
  ggplot(aes(x = Year, y = `Growth (%)`, fill = Factors, group = 1)) +
  geom_point() + geom_line() + theme_bw() + theme(legend.position="none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_text(label = round(df$GDP, 3), nudge_x=-0.10, nudge_y=-0.60,
            check_overlap=T, cex = 2.5) 
#  labs(title = "GDP growth rate of India")

#Sectorwise GDP growth

slices <- c(16.38,	2.37,	16.92,	2.46,	7.6,	17.73,
            23.07,	13.47)

lbls <- c("Agriculture, forestry & fishing",
          "Mining & quarrying",	"Manufacturing",
          "Energy water supply & other utility services",
          "Construction",
          "Trade, transport & communication",
          "Financial, real estate & professional services",
          "Public admin., defence & other services")

pilabel <- paste0(slices, "%") # ad % to labels

color <- brewer.pal(length(slices), "Set2")
pie(slices, labels = pilabel, #main = "Sectorwise share of the GDP of India (%)", 
    col = color, border = color, lty = 5, cex = 0.60)
legend("bottomleft", lbls, cex = 0.65,
       fill = color)
