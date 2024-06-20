library(tidyr) # for `gather`
library(ggplot2)
library(readr)



f <- function(x) 0 + (1*x) - (1*x*x)
tmp <- data.frame(x=0:1, y=f(0:1))
# Make plot object
p <- qplot(x, y, data = tmp, xlab = "Stage of economic development",
           ylab = "Environmental degradation") +
#p <- p + 
  stat_function(fun=f) + 
  geom_vline(xintercept = as.numeric(0.30), color = "black", size = 0.5) + 
  geom_vline(xintercept = as.numeric(0.70), color = "black", size = 0.5) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Environmental Kuznets Curve")

p

