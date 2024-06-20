library(quantmod)
library(dbnlearn)
library(bnviewer)
library(ggplot2)
library(MLmetrics)
library(readr)
library(tidyr) # for `gather`
library(dplyr) # for `rename` & `select`
library(ggplot2)
library(broom)
library(stargazer)
library(texreg)

#import dataset
df <- read_csv("EKC.csv")
#remove Year column
df0 <- subset (df, select = -Year)
df1 <- subset (df0, select = -`Particulate Pollution (µg/m3)`)
df2 <- subset (df0, select = -`CO2emissionsmetrictonspercapita`)

summary(df0)

#Analysis of EKC data

par(mfrow=c(1,2))
plot(`CO2emissionsmetrictonspercapita` ~ Year, data = df,
     ylab = expression("CO"["2"] ~ "emissions (metric tonnes per capita)"))

plot(`GDPpercapitaconstant2015US$` ~ Year, data = df,
     ylab = "GDP per capita (constant 2015 US$)")

## Building the Environmental Kuznets Curve

#Partial EKC
par(mfrow=c(1,1))
plot(`CO2emissionsmetrictonspercapita` ~ 
       `GDPpercapitaconstant2015US$`, data = df1)

#Check for correlation between the different variables
cor(df)

## Polynomial regression

qm <- lm(`CO2emissionsmetrictonspercapita` ~ poly(`GDPpercapitaconstant2015US$`, 
                                                  2, raw = TRUE), data = df1)
summary(qm)
tidy(qm) -> tqm
stargazer(tqm, type='latex', summary=FALSE)
texreg(qm)

par(mfrow = c(2,2))
plot(qm)
mtext("Summary of model for Environmental Kuznets Curve", side=3,line=-25, 
      outer=TRUE, cex = 1.2)

#developing the quadratic equation

matrix_coef <- qm$coefficients  # Extract coefficients in matrix
matrix_coef                                         # Return matrix of coefficients

a <- matrix_coef[3]
b <- matrix_coef[2]
c <- matrix_coef[1]

# Create Quadratic function from regression model coefficients

qf <- function(x) (c + (b*x) + (a*x*x))
# the function qf has the following solutions: x = 79.7035, 5353.7002 
# --- needed for plots
# Solving the equation to find data bounds

quad <- function(a, b, c)
{
  a <- as.complex(a)
  answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
              (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  if(all(Im(answer) == 0)) answer <- Re(answer)
  if(answer[1] == answer[2]) return(answer[1])
  answer
}

sol = quad(a, b, c)
sol

# Create dataset based on end point of available observations and quadratic equation formed above
generated <- data.frame(x=sol[1]:sol[2], y=qf(sol[1]:sol[2]))

#tweak this dataframe to include predicted GDP per capita
predicted <- data.frame(x=df1$`GDPpercapitaconstant2015US$`, 
                        y=qf(df1$`GDPpercapitaconstant2015US$`))
real <- df1

# Tweak "predicted" to match column labels with original dataset
names(predicted)[1] <- "GDPpercapitaconstant2015US$"
names(predicted)[2] <- "CO2emissionsmetrictonspercapita"

names(generated)[1] <- "GDPpercapitaconstant2015US$"
names(generated)[2] <- "CO2emissionsmetrictonspercapita"

#Model evaluation
MAPE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)
MSE(predicted$CO2emissionsmetrictonspercapita, 
    real$CO2emissionsmetrictonspercapita)
RMSE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)

# Final EKC plot combining the regression curve (line) with original/modified data (points)
EKCplot <- ggplot(NULL, aes(x = `GDPpercapitaconstant2015US$`,
                            y = CO2emissionsmetrictonspercapita)) + 
  geom_line(data = generated) +
  geom_point(data = real) + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = as.numeric(tail(df1$`GDPpercapitaconstant2015US$`, n=1) + 0), 
             color = "black", size = 0.5) +
  geom_text(x = 10, y = 1, label="Observed") +
  geom_text(x = 19, y = 1, label="Predicted") +
  labs(title = "Environmental Kuznets Curve for India", 
       x = "GDP per capita (constant 2015 US$)", 
       y = expression("CO"["2"] ~ "emissions (metric tonnes per capita)"))
EKCplot

max(generated$CO2emissionsmetrictonspercapita)

#-------------------------------------------------------------------------------
## Polynomial regression
dfn1 <- subset (df, select = -`CO2emissionsmetrictonspercapita`)
dfn2 <- na.omit(dfn1)
summary(dfn2)

cor(dfn2)

#Analysis of EKC data

par(mfrow=c(1,2))

plot(`Particulate Pollution (µg/m3)` ~ Year, data = dfn2,
     ylab = expression("Particulate Pollution (µg/" ~ "m"^"3" ~") in NCT of Delhi"))

plot(`GDPpercapitaconstant2015US$` ~ Year, data = dfn2,
     ylab = "GDP per capita (constant 2015 US$)")

#Partial EKC
par(mfrow=c(1,1))
plot(`Particulate Pollution (µg/m3)` ~ 
       `GDPpercapitaconstant2015US$`, data = dfn2)

dfn <- na.omit(df2)

qm1 <- lm(`Particulate Pollution (µg/m3)` ~ poly(`GDPpercapitaconstant2015US$`, 
                                                  2, raw = TRUE), data = dfn)
summary(qm1)
tidy(qm1) -> tqm1
stargazer(tqm1, type='latex', summary=FALSE)
texreg(qm1)

par(mfrow = c(2,2))
plot(qm1)
mtext("Summary of model for Environmental Kuznets Curve for NCT of Delhi", side=3,line=-25, 
      outer=TRUE, cex = 1.2)

#developing the quadratic equation

matrix_coef <- qm1$coefficients  # Extract coefficients in matrix
matrix_coef                                         # Return matrix of coefficients

a <- matrix_coef[3]
b <- matrix_coef[2]
c <- matrix_coef[1]

# Create Quadratic function from regression model coefficients

qf <- function(x) (c + (b*x) + (a*x*x))
# the function qf has the following solutions: x = 79.7035, 5353.7002 
# --- needed for plots
# Solving the equation to find data bounds

quad <- function(a, b, c)
{
  a <- as.complex(a)
  answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
              (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  if(all(Im(answer) == 0)) answer <- Re(answer)
  if(answer[1] == answer[2]) return(answer[1])
  answer
}

sol = quad(a, b, c)
sol

# Create dataset based on end point of available observations and quadratic equation formed above
generated <- data.frame(x=sol[1]:sol[2], y=qf(sol[1]:sol[2]))

#tweak this dataframe to include predicted GDP per capita
predicted <- data.frame(x=dfn$`GDPpercapitaconstant2015US$`, 
                        y=qf(dfn$`GDPpercapitaconstant2015US$`))
real <- dfn

# Tweak "predicted" to match column labels with original dataset
names(predicted)[1] <- "GDPpercapitaconstant2015US$"
names(predicted)[2] <- "Particulate Pollution (µg/m3)"

names(generated)[1] <- "GDPpercapitaconstant2015US$"
names(generated)[2] <- "Particulate Pollution (µg/m3)"

#Model evaluation
MAPE(predicted$`Particulate Pollution (µg/m3)`, 
     real$`Particulate Pollution (µg/m3)`)
MSE(predicted$`Particulate Pollution (µg/m3)`, 
    real$`Particulate Pollution (µg/m3)`)
RMSE(predicted$`Particulate Pollution (µg/m3)`, 
     real$`Particulate Pollution (µg/m3)`)

# Final EKC plot combining the regression curve (line) with original/modified data (points)
EKCplot <- ggplot(NULL, aes(x = `GDPpercapitaconstant2015US$`,
                            y = `Particulate Pollution (µg/m3)`)) + 
  geom_line(data = generated) +
  geom_point(data = real) + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = as.numeric(tail(dfn$`GDPpercapitaconstant2015US$`, n=1) + 0), 
             color = "black", size = 0.5) +
  geom_text(x = 10, y = 1, label="Observed") +
  geom_text(x = 19, y = 1, label="Predicted") +
  labs(title = "Environmental Kuznets Curve for the National Capital Territory of Delhi, India", 
       x = "GDP per capita (constant 2015 US$)", 
       y = expression("Particulate Pollution (µg/" ~ "m"^"3" ~") in NCT of Delhi"))
EKCplot

max(generated$`Particulate Pollution (µg/m3)`)

