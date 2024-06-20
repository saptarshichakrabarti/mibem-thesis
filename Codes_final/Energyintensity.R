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
df1 <- read_csv("Thesiswork/Datasets/Kaya_OWID.csv")

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

# #-----------Dynamic Bayesian Network--------------------------------------------
# 
# ts <- df$EnergyperGDP
# 
# #Time Series Preprocessing with time window = 30
# X.ts = dbn.preprocessing(ts, window = 7)
# 
# #Define 70% Train and 30% Test Data Set
# percent = 0.7
# n = nrow(X.ts)
# 
# trainIndex <- seq_len(length.out = floor(x = percent * n))
# X.ts.train <- X.ts[trainIndex,]
# X.ts.test <- X.ts[-trainIndex,]
# 
# #Dynamic Bayesian Network Structure Learning
# ts.learning = dbn.learn(X.ts.train)
# 
# # #Visualization
# # viewer(ts.learning,
# #        bayesianNetwork.width = "100%",
# #        bayesianNetwork.height = "100vh",
# #        bayesianNetwork.enabled.interactive.mode = TRUE,
# #        edges.smooth = FALSE,
# #        bayesianNetwork.layout = "layout_with_gem",
# #        node.colors = list(background = "#f4bafd",
# #                           border = "#2b7ce9",
# #                           highlight = list(background = "#97c2fc",
# #                                            border = "#2b7ce9")),
# #        
# #        clusters.legend.title = list(text = "Legend"),
# #        
# #        clusters.legend.options = list(
# #          
# #          list(label = "Target (t)",
# #               shape = "icon",
# #               icon = list(code = "f111", size = 50, color = "#e91e63")),
# #          
# #          list(label = "Time Window (t-n)",
# #               shape = "icon",
# #               icon = list(code = "f111", size = 50, color = "#f4bafd"))
# #        ),
# #        
# #        clusters = list(
# #          list(label = "Target",
# #               shape = "icon",
# #               icon = list(code = "f111", color = "#e91e63"),
# #               nodes = list("X_t")),
# #          list(label = "Time Window (t-n)",
# #               shape = "icon",
# #               icon = list(code = "f111", color = "#f4bafd"),
# #               nodes = list("X_t1"))
# #        )
# #        
# # )
# 
# 
# #Dynamic Bayesian Network Fit
# ts.fit = dbn.fit(ts.learning, X.ts.train)
# 
# #Predict values
# prediction = dbn.predict(ts.fit, X.ts.test)
# 
# #Plot Real vs Predict
# real = X.ts.test[, "X_t"]
# 
# df.validation = data.frame(list(real = real, prediction = prediction))
# 
# dbnplot <- ggplot(df.validation, aes(seq(1:nrow(df.validation)))) +
#   geom_line(aes(y = real, colour="real")) +
#   geom_line(aes(y = prediction, colour="prediction")) +
#   scale_color_manual(values = c(
#     'real' = 'deepskyblue',
#     'prediction' = 'maroon1')) +
#   labs(title = "Dynamic Bayesian Network",
#        subtitle = "EnergyperGDP",
#        colour = "Legend",
#        x = "Time index",
#        y = "EnergyperGDP") #+ theme_minimal()
# 
# dbn_mape <- MAPE(prediction, real)
# dbn_mse <- MSE(prediction, real)
# dbn_rmse <- RMSE(prediction, real)
# 
# #-----------Summary of evaluation-----------------------------------------------
# Models <- c("Regression - 1", "Regression - 2","Regression - 3", "Regression - 4", "Regression - 5", "DBN")
# MAPE <- c(lm_mape, lm2_mape, lm3_mape, lm4_mape, lm5_mape, dbn_mape)
# MSE <- c(lm_mse, lm2_mse, lm3_mse, lm4_mse, lm5_mse, dbn_mse)
# RMSE <- c(lm_rmse, lm2_rmse, lm3_rmse, lm4_rmse, lm5_rmse, dbn_rmse)
# modelevaluation <- data.frame(Models, MSE, RMSE, MAPE)
# 
# modelevaluation
# stargazer(modelevaluation, summary = FALSE)
# 
# modelevaluation
# # stargazer(modelevaluation, summary = FALSE)


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
  geom_point(data = lm3_real, colour = "Black") +
  #guides(colour=guide_legend(override.aes=list(shape=c(16,NA), linetype=c(0,1)))) +
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
             color = "black", size = 1) +
  geom_text(x = 1970, y = 1.375, label="Observed") +
  geom_text(x = 2025, y = 1.375, label="Predicted") +
  labs(title = "Energy per GDP predicted till 2050",
       x = "Year", y = "Energy per GDP")
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