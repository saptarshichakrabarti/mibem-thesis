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
y <- df$AnnualCO2emissionsunitenergy
x <- df$Year

#---Regression Analysis---------------------------------------------------------

df_kayac <- subset (df1, select = c(Year, AnnualCO2emissionsunitenergy))
kayac <- na.omit(df_kayac)

lm_real <- kayac

#Regression models--------------------------------------------------------------
kayac.lm1 <- lm(kayac$AnnualCO2emissionsunitenergy ~ poly(kayac$Year, 1, raw=T), data=kayac)

lm_mape <- MAPE(predict(kayac.lm1), lm_real$AnnualCO2emissionsunitenergy)
lm_mse <- MSE(predict(kayac.lm1), lm_real$AnnualCO2emissionsunitenergy)
lm_rmse <- RMSE(predict(kayac.lm1), lm_real$AnnualCO2emissionsunitenergy)

kayac.lm2 <- lm(kayac$AnnualCO2emissionsunitenergy ~ poly(kayac$Year, 2, raw=T), data=kayac)

lm2_mape <- MAPE(predict(kayac.lm2), lm_real$AnnualCO2emissionsunitenergy)
lm2_mse <- MSE(predict(kayac.lm2), lm_real$AnnualCO2emissionsunitenergy)
lm2_rmse <- RMSE(predict(kayac.lm2), lm_real$AnnualCO2emissionsunitenergy)

kayac.lm3 = lm(kayac$AnnualCO2emissionsunitenergy ~ poly(kayac$Year, 3, raw=T), data=kayac)

lm3_mape <- MAPE(predict(kayac.lm3), kayac$AnnualCO2emissionsunitenergy)
lm3_mse <- MSE(predict(kayac.lm3), kayac$AnnualCO2emissionsunitenergy)
lm3_rmse <- RMSE(predict(kayac.lm3), kayac$AnnualCO2emissionsunitenergy)

kayac.lm4 = lm(kayac$AnnualCO2emissionsunitenergy ~ poly(kayac$Year, 4, raw=T), data=kayac)

lm4_mape <- MAPE(predict(kayac.lm4), kayac$AnnualCO2emissionsunitenergy)
lm4_mse <- MSE(predict(kayac.lm4), kayac$AnnualCO2emissionsunitenergy)
lm4_rmse <- RMSE(predict(kayac.lm4), kayac$AnnualCO2emissionsunitenergy)

kayac.lm5 = lm(kayac$AnnualCO2emissionsunitenergy ~ poly(kayac$Year, 5, raw=T), data=kayac)

lm5_mape <- MAPE(predict(kayac.lm5), kayac$AnnualCO2emissionsunitenergy)
lm5_mse <- MSE(predict(kayac.lm5), kayac$AnnualCO2emissionsunitenergy)
lm5_rmse <- RMSE(predict(kayac.lm5), kayac$AnnualCO2emissionsunitenergy)

#Cubic regression-----------------------------------------------------------

#view summary of cubic model
summary(kayac.lm3)
par(mfrow=c(2,2))
plot(kayac.lm3)
mtext(expression("Summary of cubic model for annual" ~"CO"["2"] ~ "emissions per unit energy"), side=3,line=-18.5, 
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
lm3_prediction <- data.frame(Year=kayac$Year, AnnualCO2emissionsunitenergy=cf(kayac$Year))

evaluationplot <- ggplot(NULL, aes(x = Year,
                                   y = AnnualCO2emissionsunitenergy)) + 
  geom_line(data = lm3_prediction, colour = "Orange") +
  geom_point(data = lm3_real, colour = "Black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Evaluation of cubic model for Carbon intensity", 
       x = "Year", 
       y = expression("Annual" ~"CO"["2"] ~ "per unit energy"))
evaluationplot

# Create dataset based on end point of available observations and quadratic equation formed above
predictedkayac_lm3 <- data.frame(Year=(tail(kayac$Year, n=1) + 1):2050, 
                                AnnualCO2emissionsunitenergy=cf((tail(kayac$Year, n=1) + 1):2050))
predictedkayac_lm3 <- rbind(kayac, predictedkayac_lm3)
predictedkayac_lm3

#Plot above data to make sure it looks like the incomplete portion of the data

predictedkayac_lm3.graph <- ggplot(predictedkayac_lm3,
                                  aes(x= Year,
                                      y = AnnualCO2emissionsunitenergy)) + geom_point() +
  geom_smooth(method=lm, formula = y ~ poly(x, 3, raw = T), level=0.95, se=T) +
  geom_vline(xintercept = as.numeric(tail(kayac$Year, n=1) + 0.5), 
             color = "black", size = 1) + theme_bw() +
  geom_text(x = 1970, y = 0.287, label="Observed") +
  geom_text(x = 2030, y = 0.287, label="Predicted") +
  labs(title = expression("Annual" ~"CO"["2"] ~ "emissions per unit energy predicted till 2050"),
       x = "Year", y = expression("Annual" ~"CO"["2"] ~ "per unit energy")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
predictedkayac_lm3.graph

# #-----------Export to CSV-------------------------------------------------------
# write.csv(predictedkayac_lm3,
#           "C:\\Users\\sapta\\Documents\\Thesiswork\\Codes_final\\predicteddata\\PredictedAnnualCO2emissionsunitenergy.csv",
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