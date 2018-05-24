library(rgl)
library(htmlwidgets)
library(purrr)

setwd("C:/Users/Wenyao/Desktop/R/Visualization-of-Stochastic-Processes")
source("./functions.R")

plot_size <- 450
plot_step <- 50

# Trend Stationary without slope
tn1 <- plot_trend_stationary(beta = 0, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Trend Stationary with slope
tn2 <- plot_trend_stationary(beta = 1, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Brownian Motion without drift
bm1 <- plot_brownian_motion(miu = 0, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Brownian Motion with drift
bm2 <- plot_brownian_motion(miu = 1, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Geometric Brownian Motion with drift
gb <- plot_geo_brownian_motion(miu = 0.1, sigma = 0.5, x0 = 2.5, xlim = c(0, 5), ylim = c(-1, 5), step = plot_step)

# Vasicek model
vasicek <- plot_vasicek(a = 1, b = 3, sigma = 1, x0 = 0.01, xlim = c(0, 5), ylim = c(-1, 4), step = plot_step)

# Cox–Ingersoll–Ross model
CIR <- plot_CIR(a = 1, b = 3, sigma = 2, x0 = 0.01, xlim = c(0, 5), ylim = c(-1, 4), step = plot_step)

# Poisson point process
poisson <- plot_poisson(lambda = 10, x0 = 0, xlim = c(0,5), ylim = c(0, 50), step = plot_step)

setwd("./webGL")

# write to html
list(
  rglscene = list(tn1, tn2, bm1, bm2, gb, vasicek, CIR, poisson),
  description=c("tn1", "tn2", "bm1", "bm2", "gb", "vasicek", "CIR", "Poisson")
) %>% 
  pmap(
    function(rglscene, description){
      rglwidget(
        x = rglscene, 
        width = plot_size, 
        height = plot_size
      ) %>% 
        saveWidget(
          file = paste0(description, ".html"), 
          selfcontained = FALSE, 
          libdir = "files"
        )
    }
  ) 

