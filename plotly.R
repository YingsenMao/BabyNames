install.packages("devtools")  # so we can install from github
library("devtools")
install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)
library(ggplot2)
install.packages("viridis") # dependency
install.packages('httr')
install.packages('stringi')
install.packages('Rcpp')
library(Rcpp)
library(httr)
library(viridis)
install.packages("devtools")
devtools::install_github("ropensci/plotly")
library(plotly)
plotly:::verify("yingsenmao")
plotly:::verify("w96zpo76k0")
Sys.setenv("plotly_username"="yingsenmao")
Sys.setenv("plotly_api_key"="w96zpo76k0")
data(diamonds)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)

py <- plotly(username="r_user_guide", key="mw5isa4yqp")  # open plotly connection
ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)

py$ggplotly(ggiris)


py <- plot_ly(username="yingsenmao", key="w96zpo76k0")
