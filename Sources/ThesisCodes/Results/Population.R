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
y <- df$Population
x <- df$Year

#---Regression Analysis---------------------------------------------------------

df_kayac <- subset (df1, select = c(Year, Population))
kayac <- na.omit(df_kayac)

lm_real <- kayac

#Regression models--------------------------------------------------------------
kayac.lm1 <- lm(kayac$Population ~ poly(kayac$Year, 1, raw=T), data=kayac)

lm_mape <- MAPE(predict(kayac.lm1), lm_real$Population)
lm_mse <- MSE(predict(kayac.lm1), lm_real$Population)
lm_rmse <- RMSE(predict(kayac.lm1), lm_real$Population)

kayac.lm2 <- lm(kayac$Population ~ poly(kayac$Year, 2, raw=T), data=kayac)

lm2_mape <- MAPE(predict(kayac.lm2), lm_real$Population)
lm2_mse <- MSE(predict(kayac.lm2), lm_real$Population)
lm2_rmse <- RMSE(predict(kayac.lm2), lm_real$Population)

kayac.lm3 = lm(kayac$Population ~ poly(kayac$Year, 3, raw=T), data=kayac)

lm3_mape <- MAPE(predict(kayac.lm3), kayac$Population)
lm3_mse <- MSE(predict(kayac.lm3), kayac$Population)
lm3_rmse <- RMSE(predict(kayac.lm3), kayac$Population)

kayac.lm4 = lm(kayac$Population ~ poly(kayac$Year, 4, raw=T), data=kayac)

lm4_mape <- MAPE(predict(kayac.lm4), kayac$Population)
lm4_mse <- MSE(predict(kayac.lm4), kayac$Population)
lm4_rmse <- RMSE(predict(kayac.lm4), kayac$Population)

kayac.lm5 = lm(kayac$Population ~ poly(kayac$Year, 5, raw=T), data=kayac)

lm5_mape <- MAPE(predict(kayac.lm5), kayac$Population)
lm5_mse <- MSE(predict(kayac.lm5), kayac$Population)
lm5_rmse <- RMSE(predict(kayac.lm5), kayac$Population)


#cubic regression-----------------------------------------------------------

tidy(kayac.lm3) -> lm3
#stargazer(tlm3, type='latex', summary=FALSE)

#view summary of cubic model
summary(kayac.lm3)
par(mfrow=c(2,2))
plot(kayac.lm3)
mtext("Summary of cubic model for Population", side=3,line=-18.5, 
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
lm3_prediction <- data.frame(Year=kayac$Year, Population=cf(kayac$Year))

evaluationplot <- ggplot(NULL, aes(x = Year,
                                   y = `Population`)) + 
  geom_line(data = lm3_prediction, colour = "Orange") +
  geom_point(data = lm3_real, colour = "Black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_colour_manual("", 
                      breaks = c("Model", "Observations"),
                      values = c("Orange", "Black")) +
  theme(legend.position = "bottom") +
  labs(title = "Evaluation of cubic model for population", 
       x = "Year", 
       y = "Population")
evaluationplot

lm3_mape <- MAPE(lm3_prediction$Population, lm3_real$Population)
lm3_mse <- MSE(lm3_prediction$Population, lm3_real$Population)
lm3_rmse <- RMSE(lm3_prediction$Population, lm3_real$Population)

# Create dataset based on end point of available observations and cubic equation formed above
predictedkayac_lm3 <- data.frame(Year=(tail(kayac$Year, n=1) + 1):2050, 
                                Population=cf((tail(kayac$Year, n=1) + 1):2050))
predictedkayac_lm3 <- rbind(kayac, predictedkayac_lm3)
predictedkayac_lm3

#Plot above data to make sure it looks like the incomplete portion of the data

predictedkayac_lm3.graph <- ggplot(predictedkayac_lm3,
                                  aes(x= Year,
                                      y = Population)) + geom_point() +
  geom_smooth(method=lm, formula = y ~ poly(x, 3, raw = T), level=0.95, se=T) +
  geom_vline(xintercept = as.numeric(tail(kayac$Year, n=1) + 0.5), 
             color = "black", size = 1) + theme_bw() +
  geom_text(x = 1970, y = 1.55e+09, label="Observed") +
  geom_text(x = 2030, y = 1.55e+09, label="Predicted") +
  labs(title = "Population predicted till 2050",
       x = "Year", y = "Population") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
predictedkayac_lm3.graph

# #-----------Export to CSV-------------------------------------------------------
# write.csv(predictedkayac_lm3,
#           "C:\\Users\\sapta\\Documents\\Thesiswork\\Codes_final\\predicteddata\\PredictedPopulation.csv",
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