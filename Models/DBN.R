library(quantmod)
library(dbnlearn)
library(bnviewer)
library(ggplot2)
library(MLmetrics)
library(readr)

#amz <- getSymbols("AMZN", src = "yahoo", from = "2015-01-01", to = "2020-07-01", auto.assign = FALSE)

#import dataset
df1 <- read_csv("Thesiswork/Datasets/Kaya_OWID.csv")

df <- na.omit(df1)
#Amazon Stock Time Series
#ts <- amz$AMZN.Open

#ts <- gdpc
ts <- df$GDPpercapita
#ts <- ts(df$GDPpercapita, start = c(1965), end = c(2020), frequency = 1)

#Time Series Preprocessing with time window = 30
X.ts = dbn.preprocessing(ts, window = 4)
X.ts

#Define 70% Train and 30% Test Data Set
percent = 0.7
n = nrow(X.ts)

trainIndex <- seq_len(length.out = floor(x = percent * n))
X.ts.train <- X.ts[trainIndex,]
X.ts.test <- X.ts[-trainIndex,]

#Dynamic Bayesian Network Structure Learning
ts.learning = dbn.learn(X.ts.train)

#Visualization
viewer(ts.learning,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "100vh",
       bayesianNetwork.enabled.interactive.mode = TRUE,
       edges.smooth = FALSE,
       bayesianNetwork.layout = "layout_with_gem",
       node.colors = list(background = "#f4bafd",
                          border = "#2b7ce9",
                          highlight = list(background = "#97c2fc",
                                           border = "#2b7ce9")),
       
       clusters.legend.title = list(text = "Legend"),
       
       clusters.legend.options = list(
         
         list(label = "Target (t)",
              shape = "icon",
              icon = list(code = "f111", size = 50, color = "#e91e63")),
         
         list(label = "Time Window (t-n)",
              shape = "icon",
              icon = list(code = "f111", size = 50, color = "#f4bafd"))
       ),
       
       clusters = list(
         list(label = "Target",
              shape = "icon",
              icon = list(code = "f111", color = "#e91e63"),
              nodes = list("X_t")),
         list(label = "Time Window (t-n)",
              shape = "icon",
              icon = list(code = "f111", color = "#f4bafd"),
              nodes = list("X_t1"))
       )
       
)


#Dynamic Bayesian Network Fit
ts.fit = dbn.fit(ts.learning, X.ts.train)

#Predict values
prediction = dbn.predict(ts.fit, X.ts.test)

#Plot Real vs Predict
real = X.ts.test[, "X_t"]

df.validation = data.frame(list(real = real, prediction = prediction))

ggplot(df.validation, aes(seq(1:nrow(df.validation)))) +
  geom_line(aes(y = real, colour="real")) +
  geom_line(aes(y = prediction, colour="prediction")) +
  scale_color_manual(values = c(
    'real' = 'deepskyblue',
    'prediction' = 'maroon1')) +
  labs(title = "Dynamic Bayesian Network",
       subtitle = "GDP per capita",
       colour = "Legend",
       x = "Year",
       y = "GDP per capita") + theme_minimal()

MAPE(prediction, real)
MSE(prediction, real)

dbn.predict(ts.fit)

library(dbnR)
data(motor)

size <- 3
dt_train <- motor[200:2500]
dt_val <- motor[2501:3000]
net <- learn_dbn_struc(dt_train, size)
plot_dynamic_network(net)
f_dt_train <- fold_dt(dt_train, size)
fit <- fit_dbn_params(net, f_dt_train, method = "mle")
fit
res_fore <- suppressWarnings(dbnR::forecast_ts(f_dt_val, fit, obj_vars = c("pm_t_0", "stator_winding_t_0"), ini = 100, len = 70))
set.seed(42)
res_fore <- suppressWarnings(dbnR::forecast_ts(f_dt_val, fit, obj_vars = c("pm_t_0"), ini = 100, len = 70, mode = "approx", num_p = 150, rep = 3))
