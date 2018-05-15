library(rgl)
library(htmlwidgets)
library(purrr)

setwd("C:/Users/Wenyao/Desktop/R/Brownian_Motion_Visualization")
source("./functions.R")


# Trend Stationary without slope
tn1 <- plot_trend_stationary(beta = 0, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = 50)

# Trend Stationary with slope
tn2 <- plot_trend_stationary(beta = 1, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = 50)

# Brownian Motion without drift
bm1 <- plot_brownian_motion(miu = 0, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = 50)

# Brownian Motion with drift
bm2 <- plot_brownian_motion(miu = 1, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = 50)

# Geometric Brownian Motion with drift
gb <- plot_geo_brownian_motion(miu = 0.1, sigma = 0.5, x0 = 2.5, xlim = c(0, 5), ylim = c(0, 5), step = 50)

# Vasicek model
vasicek <- plot_vasicek(a = 1, b = 3, sigma = 1, x0 = 0.01, xlim = c(0, 5), ylim = c(-1, 4), step = 50)

# Cox–Ingersoll–Ross model
CIR <- plot_CIR(a = 1, b = 3, sigma = 2, x0 = 0.01, xlim = c(0, 5), ylim = c(-1, 4), step = 50)

setwd("./webGL")

# write to html
list(
  rglscene = list(tn1, tn2, bm1, bm2, gb, vasicek, CIR),
  description=c("tn1", "tn2", "bm1", "bm2", "gb", "vasicek", "CIR")
) %>% 
  pmap(
    function(rglscene, description){
      rglwidget(
        x = rglscene, 
        width = 450, 
        height = 450
      ) %>% 
        saveWidget(
          file = paste0(description, ".html"), 
          selfcontained = FALSE, 
          libdir = "files"
        )
    }
  ) 

