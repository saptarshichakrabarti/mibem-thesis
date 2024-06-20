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
dfni <- read_csv("useuchina.csv")

#remove Year column
dfus <- subset (dfni, select = c(Year, UnitedStatesGDP, UnitedStatesCO2))
us <- na.omit(dfus)
ekcus <- subset (us, select = -Year)

dfeu <- subset (dfni, select = c(Year, EuropeanUnionGDP, EuropeanUnionCO2))
EU <- na.omit(dfeu)
ekcEU <- subset (EU, select = -Year)

dfcn <- subset (dfni, select = c(Year, ChinaGDP, ChinaCO2))
cn <- na.omit(dfcn)
ekccn <- subset (cn, select = -Year)

summary(us)
summary(eu)
summary(cn)

#Analysis of EKC data
par(mfrow=c(1,2))
plot(UnitedStatesCO2 ~ Year, data = us,
     ylab = "CO2 emissions metric tons per capita")

plot(UnitedStatesGDP ~ Year, data = us,
     ylab = "GDP per capita (constant 2015 US$)")

## Building the Environmental Kuznets Curve

#-------United States-----------------------------------------------------------

#Partial EKC
par(mfrow=c(1,1))
plot(UnitedStatesCO2 ~ 
       UnitedStatesGDP, data = ekcus)

## Building the Environmental Kuznets Curve

## Polynomial regression

ekcusm <- lm(UnitedStatesCO2 ~ poly(UnitedStatesGDP, 
                                                  2, raw = TRUE), data = ekcus)
summary(ekcusm)
# tidy(ekcusm) -> tekcusm
# stargazer(tekcusm, type='latex', summary=FALSE)

par(mfrow = c(2,2))
plot(ekcusm)
mtext("Summary of model for Environmental Kuznets Curve", side=3,line=-25, 
      outer=TRUE, cex = 1.2)

#developing the quadratic equation

matrix_coef <- ekcusm$coefficients  # Extract coefficients in matrix
matrix_coef                                         # Return matrix of coefficients

a <- matrix_coef[3]
b <- matrix_coef[2]
c <- matrix_coef[1]

# Create Quadratic function from regression model coefficients

ekcusf <- function(x) (c + (b*x) + (a*x*x))
# the function ekcusf has the following solutions: x = 79.7035, 5353.7002 
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
generated <- data.frame(x=sol[1]:sol[2], y=ekcusf(sol[1]:sol[2]))

#tweak this dataframe to include predicted GDP per capita
predicted <- data.frame(x=ekcus$UnitedStatesGDP, 
                        y=ekcusf(ekcus$UnitedStatesGDP))
real <- ekcus

# Tweak "predicted" to match column labels with original dataset
names(predicted)[1] <- "GDPpercapitaconstant2015US$"
names(predicted)[2] <- "CO2emissionsmetrictonspercapita"

names(generated)[1] <- "GDPpercapitaconstant2015US$"
names(generated)[2] <- "CO2emissionsmetrictonspercapita"

names(real)[1] <- "GDPpercapitaconstant2015US$"
names(real)[2] <- "CO2emissionsmetrictonspercapita"

#Model evaluation
MAPE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)
MSE(predicted$CO2emissionsmetrictonspercapita, 
    real$CO2emissionsmetrictonspercapita)
RMSE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)

# Final EKC plot combining the regression curve (line) with original/modified data (points)
EKCplotUS <- ggplot(NULL, aes(x = `GDPpercapitaconstant2015US$`,
                            y = CO2emissionsmetrictonspercapita)) + 
  geom_line(data = generated) +
  geom_point(data = real) +
  geom_vline(xintercept = as.numeric(tail(ekcus$UnitedStatesGDP, n=1) + 0), 
             color = "black", size = 0.5) +
  geom_text(x = 10, y = 1, label="Observed") +
  geom_text(x = 19, y = 1, label="Predicted") +
  labs(title = "Environmental Kuznets Curve for the US", 
       x = "GDP per capita (constant 2015 US$)", 
       y = expression("CO"["2"] ~ "emissions (metric tonnes per capita)"))
EKCplotUS

max(generated$CO2emissionsmetrictonspercapita)

#-------EuropeanUnion-----------------------------------------------------------

#Analysis of EKC data
par(mfrow=c(1,2))
plot(EuropeanUnionCO2 ~ Year, data = EU,
     ylab = "CO2 emissions metric tons per capita")

plot(EuropeanUnionGDP ~ Year, data = EU,
     ylab = "GDP per capita (constant 2015 US$)")

## Building the Environmental Kuznets Curve

#Partial EKC
par(mfrow=c(1,1))
plot(EuropeanUnionCO2 ~ 
       EuropeanUnionGDP, data = ekcEU)

## Building the Environmental Kuznets Curve

## Polynomial regression

ekcEUm <- lm(EuropeanUnionCO2 ~ poly(EuropeanUnionGDP, 
                                    2, raw = TRUE), data = ekcEU)
summary(ekcEUm)
# tidy(ekcEUm) -> tekcEUm
# stargazer(tekcEUm, type='latex', summary=FALSE)

par(mfrow = c(2,2))
plot(ekcEUm)
mtext("Summary of model for Environmental Kuznets Curve", side=3,line=-25, 
      outer=TRUE, cex = 1.2)

#developing the quadratic equation

matrix_coef <- ekcEUm$coefficients  # Extract coefficients in matrix
matrix_coef                                         # Return matrix of coefficients

a <- matrix_coef[3]
b <- matrix_coef[2]
c <- matrix_coef[1]

# Create Quadratic function from regression model coefficients

ekcEUf <- function(x) (c + (b*x) + (a*x*x))
# the function ekcEUf has the following solutions: x = 79.7035, 5353.7002 
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
generated <- data.frame(x=sol[1]:sol[2], y=ekcEUf(sol[1]:sol[2]))

#tweak this dataframe to include predicted GDP per capita
predicted <- data.frame(x=ekcEU$EuropeanUnionGDP, 
                        y=ekcEUf(ekcEU$EuropeanUnionGDP))
real <- ekcEU

# Tweak "predicted" to match column labels with original dataset
names(predicted)[1] <- "GDPpercapitaconstant2015US$"
names(predicted)[2] <- "CO2emissionsmetrictonspercapita"

names(generated)[1] <- "GDPpercapitaconstant2015US$"
names(generated)[2] <- "CO2emissionsmetrictonspercapita"

names(real)[1] <- "GDPpercapitaconstant2015US$"
names(real)[2] <- "CO2emissionsmetrictonspercapita"

#Model evaluation
MAPE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)
MSE(predicted$CO2emissionsmetrictonspercapita, 
    real$CO2emissionsmetrictonspercapita)
RMSE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)

# Final EKC plot combining the regression curve (line) with original/modified data (points)
EKCplotEU <- ggplot(NULL, aes(x = `GDPpercapitaconstant2015US$`,
                            y = CO2emissionsmetrictonspercapita)) + 
  geom_line(data = generated) +
  geom_point(data = real) +
  geom_vline(xintercept = as.numeric(tail(ekcEU$EuropeanUnionGDP, n=1) + 0), 
             color = "black", size = 0.5) +
  geom_text(x = 10, y = 1, label="Observed") +
  geom_text(x = 19, y = 1, label="Predicted") +
  labs(title = "Environmental Kuznets Curve for the EU", 
       x = "GDP per capita (constant 2015 EU$)", 
       y = expression("CO"["2"] ~ "emissions (metric tonnes per capita)"))
EKCplotEU

max(generated$CO2emissionsmetrictonspercapita)

#-------China-------------------------------------------------------------------

#Analysis of EKC data
par(mfrow=c(1,2))
plot(ChinaCO2 ~ Year, data = cn,
     ylab = "CO2 emissions metric tons per capita")

plot(ChinaGDP ~ Year, data = cn,
     ylab = "GDP per capita (constant 2015 US$)")

## Building the Environmental Kuznets Curve

#Partial EKC
par(mfrow=c(1,1))
plot(ChinaCO2 ~ 
       ChinaGDP, data = ekccn)

## Building the Environmental Kuznets Curve

## Polynomial regression

ekccnm <- lm(ChinaCO2 ~ poly(ChinaGDP, 
                                     2, raw = TRUE), data = ekccn)
summary(ekccnm)
# tidy(ekccnm) -> tekccnm
# stargazer(tekccnm, type='latex', summary=FALSE)

par(mfrow = c(2,2))
plot(ekccnm)
mtext("Summary of model for Environmental Kuznets Curve", side=3,line=-25, 
      outer=TRUE, cex = 1.2)

#developing the quadratic equation

matrix_coef <- ekccnm$coefficients  # Extract coefficients in matrix
matrix_coef                                         # Return matrix of coefficients

a <- matrix_coef[3]
b <- matrix_coef[2]
c <- matrix_coef[1]

# Create Quadratic function from regression model coefficients

ekccnf <- function(x) (c + (b*x) + (a*x*x))
# the function ekccnf has the following solutions: x = 79.7035, 5353.7002 
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
generated <- data.frame(x=sol[1]:sol[2], y=ekccnf(sol[1]:sol[2]))

#tweak this dataframe to include predicted GDP per capita
predicted <- data.frame(x=ekccn$ChinaGDP, 
                        y=ekccnf(ekccn$ChinaGDP))
real <- ekccn

# Tweak "predicted" to match column labels with original dataset
names(predicted)[1] <- "GDPpercapitaconstant2015US$"
names(predicted)[2] <- "CO2emissionsmetrictonspercapita"

names(generated)[1] <- "GDPpercapitaconstant2015US$"
names(generated)[2] <- "CO2emissionsmetrictonspercapita"

names(real)[1] <- "GDPpercapitaconstant2015US$"
names(real)[2] <- "CO2emissionsmetrictonspercapita"

#Model evaluation
MAPE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)
MSE(predicted$CO2emissionsmetrictonspercapita, 
    real$CO2emissionsmetrictonspercapita)
RMSE(predicted$CO2emissionsmetrictonspercapita, 
     real$CO2emissionsmetrictonspercapita)

# Final EKC plot combining the regression curve (line) with original/modified data (points)
EKCplotCN <- ggplot(NULL, aes(x = `GDPpercapitaconstant2015US$`,
                            y = CO2emissionsmetrictonspercapita)) + 
  geom_line(data = generated) +
  geom_point(data = real) +
  geom_vline(xintercept = as.numeric(tail(ekccn$ChinaGDP, n=1) + 0), 
             color = "black", size = 0.5) +
  geom_text(x = 10, y = 1, label="Observed") +
  geom_text(x = 19, y = 1, label="Predicted") +
  labs(title = "Environmental Kuznets Curve for China", 
       x = "GDP per capita (constant 2015 US$)", 
       y = expression("CO"["2"] ~ "emissions (metric tonnes per capita)"))
EKCplotCN

max(generated$CO2emissionsmetrictonspercapita)

#-------India-------------------------------------------------------------------
#import dataset
dfi <- read_csv("Thesiswork/EKC.csv")
#remove Year column
dfin <- subset (dfi, select = -Year)

summary(dfin)

#Analysis of EKC data
par(mfrow=c(1,2))
plot(`CO2emissionsmetrictonspercapita` ~ Year, data = dfi,
     ylab = "CO2 emissions metric tons per capita")

plot(`GDPpercapitaconstant2015US$` ~ Year, data = dfi,
     ylab = "GDP per capita (constant 2015 US$)")

## Building the Environmental Kuznets Curve

#Partial EKC
par(mfrow=c(1,1))
plot(`CO2emissionsmetrictonspercapita` ~ 
       `GDPpercapitaconstant2015US$`, data = dfin)

#Check for correlation between the different variables
cor(dfi)

## Polynomial regression

qm <- lm(`CO2emissionsmetrictonspercapita` ~ poly(`GDPpercapitaconstant2015US$`, 
                                                  2, raw = TRUE), data = dfin)
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
predicted <- data.frame(x=dfin$`GDPpercapitaconstant2015US$`, 
                        y=qf(dfin$`GDPpercapitaconstant2015US$`))
real <- dfin

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
EKCplotIN <- ggplot(NULL, aes(x = `GDPpercapitaconstant2015US$`,
                            y = CO2emissionsmetrictonspercapita)) + 
  geom_line(data = generated) +
  geom_point(data = real) +
  geom_vline(xintercept = as.numeric(tail(dfin$`GDPpercapitaconstant2015US$`, n=1) + 0), 
             color = "black", size = 0.5) +
  geom_text(x = 10, y = 1, label="Observed") +
  geom_text(x = 19, y = 1, label="Predicted") +
  labs(title = "Environmental Kuznets Curve for India", 
       x = "GDP per capita (constant 2015 US$)", 
       y = expression("CO"["2"] ~ "emissions (metric tonnes per capita)"))
EKCplotIN

max(generated$CO2emissionsmetrictonspercapita)


library(ggplot2)
library("ggpubr")
# theme_set(
#   theme_bw() +
#     theme(legend.position = "top")
#)
figure <- ggarrange(EKCplotUS, EKCplotEU, EKCplotCN, EKCplotIN,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
figure



