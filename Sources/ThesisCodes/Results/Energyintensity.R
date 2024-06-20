library(quantmod)
library(dbnlearn)
library(bnviewer)
library(ggplot2)
library(MLmetrics)
library(readr)
library(tidyr) # for `gather`
library(dplyr) # for `rename` & `select`
library(ggplot2)
library(stargazer)

#import dataset
df1 <- read_csv("Kaya_OWID.csv")

df <- na.omit(df1)
y <- df$EnergyperGDP
x <- df$Year

#---Regression Analysis---------------------------------------------------------

df_kayac <- subset (df1, select = c(Year, EnergyperGDP))
kayac <- na.omit(df_kayac)

lm_real <- kayac

#Regression models--------------------------------------------------------------
kayac.lm1 <- lm(kayac$EnergyperGDP ~ poly(kayac$Year, 1, raw=T), data=kayac)

lm_mape <- MAPE(predict(kayac.lm1), lm_real$EnergyperGDP)
lm_mse <- MSE(predict(kayac.lm1), lm_real$EnergyperGDP)
lm_rmse <- RMSE(predict(kayac.lm1), lm_real$EnergyperGDP)

kayac.lm2 <- lm(kayac$EnergyperGDP ~ poly(kayac$Year, 2, raw=T), data=kayac)

lm2_mape <- MAPE(predict(kayac.lm2), lm_real$EnergyperGDP)
lm2_mse <- MSE(predict(kayac.lm2), lm_real$EnergyperGDP)
lm2_rmse <- RMSE(predict(kayac.lm2), lm_real$EnergyperGDP)

kayac.lm3 = lm(kayac$EnergyperGDP ~ poly(kayac$Year, 3, raw=T), data=kayac)

lm3_mape <- MAPE(predict(kayac.lm3), kayac$EnergyperGDP)
lm3_mse <- MSE(predict(kayac.lm3), kayac$EnergyperGDP)
lm3_rmse <- RMSE(predict(kayac.lm3), kayac$EnergyperGDP)

kayac.lm4 = lm(kayac$EnergyperGDP ~ poly(kayac$Year, 4, raw=T), data=kayac)

lm4_mape <- MAPE(predict(kayac.lm4), kayac$EnergyperGDP)
lm4_mse <- MSE(predict(kayac.lm4), kayac$EnergyperGDP)
lm4_rmse <- RMSE(predict(kayac.lm4), kayac$EnergyperGDP)

kayac.lm5 = lm(kayac$EnergyperGDP ~ poly(kayac$Year, 5, raw=T), data=kayac)

lm5_mape <- MAPE(predict(kayac.lm5), kayac$EnergyperGDP)
lm5_mse <- MSE(predict(kayac.lm5), kayac$EnergyperGDP)
lm5_rmse <- RMSE(predict(kayac.lm5), kayac$EnergyperGDP)

#Cubic regression-----------------------------------------------------------

#view summary of cubic model
summary(kayac.lm3)
par(mfrow=c(2,2))
plot(kayac.lm3)
mtext("Summary of cubic model for Energy per GDP", side=3,line=-18.5, 
      outer=TRUE, cex = 1.2)

matrix_coef <- summary(kayac.lm3)$coefficients  # Extract coefficients in matrix
matrix_coef                                # Return matrix of coefficients

a <- matrix_coef[4]
b <- matrix_coef[3]
c <- matrix_coef[2]
d <- matrix_coef[1]

cf <- function(x) (d + (c*x) + (b*x*x) + (a*x*x*x))

# evaluation
lm3_real <- kayac
lm3_prediction <- data.frame(Year=kayac$Year, EnergyperGDP=cf(kayac$Year))

evaluationplot <- ggplot(NULL, aes(x = Year,
                                   y = `EnergyperGDP`)) + 
  geom_line(data = lm3_prediction, colour = "Orange") +
  geom_point(data = lm3_real, colour = "Black") +  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Evaluation of cubic model for energy intensity", 
       x = "Year", 
       y = "Energy per GDP")
evaluationplot

lm3_mape <- MAPE(lm3_prediction$EnergyperGDP, lm3_real$EnergyperGDP)
lm3_mse <- MSE(lm3_prediction$EnergyperGDP, lm3_real$EnergyperGDP)
lm3_rmse <- RMSE(lm3_prediction$EnergyperGDP, lm3_real$EnergyperGDP)

# Create dataset based on end point of available observations and quadratic equation formed above
predictedkayac_lm3 <- data.frame(Year=(tail(kayac$Year, n=1) + 1):2050, 
                                EnergyperGDP=cf((tail(kayac$Year, n=1) + 1):2050))
predictedkayac_lm3 <- rbind(kayac, predictedkayac_lm3)
predictedkayac_lm3

#Plot above data to make sure it looks like the incomplete portion of the data

predictedkayac_lm3.graph <- ggplot(predictedkayac_lm3,
                                  aes(x= Year,
                                      y = EnergyperGDP)) + geom_point() +
  geom_smooth(method=lm, formula = y ~ poly(x, 3, raw = T), level=0.95, se=T) +
  geom_vline(xintercept = as.numeric(tail(kayac$Year, n=1) + 0.5), 
             color = "black", size = 1) + theme_bw() +
  geom_text(x = 1970, y = 1.375, label="Observed") +
  geom_text(x = 2025, y = 1.375, label="Predicted") +
  labs(title = "Energy per GDP predicted till 2050",
       x = "Year", y = "Energy per GDP") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
predictedkayac_lm3.graph

# #-----------Export to CSV-------------------------------------------------------
# write.csv(predictedkayac_lm3,
#           "C:\\Users\\sapta\\Documents\\Thesiswork\\Codes_final\\predicteddata\\PredictedEnergyperGDP.csv",
#           row.names = FALSE)

# #------------------Save all plots-----------------------------------------------
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# 
# file.copy(from=plots.png.paths, to="~/Thesiswork/Exports/")
# 
# plots.png.detials <- file.info(plots.png.paths)
# plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
# sorted.png.names <- gsub(plots.dir.path, "~/Thesiswork/Exports/", row.names(plots.png.detials), fixed=TRUE)
# numbered.png.names <- paste0("~/Thesiswork/Exports/", 1:length(sorted.png.names), ".png")
# 
# # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
# file.rename(from=sorted.png.names, to=numbered.png.names)