# write each plot to inidividual html file

library(rgl)
library(htmlwidgets)
library(purrr)
library(stringr)

setwd("C:/Users/Wenyao/Desktop/R/Visualization-of-Stochastic-Processes")
source("./functions.R")

setwd("./html_output")

plot_size <- 400
plot_step <- 50

# Trend Stationary Model (without Drift)
tn1 <- plot_trend_stationary(beta = 0, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Trend Stationary Model
tn2 <- plot_trend_stationary(beta = 1, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Brownian Motion (without Drift)
bm1 <- plot_brownian_motion(miu = 0, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Brownian Motion
bm2 <- plot_brownian_motion(miu = 1, sigma = 1, x0 = 0, xlim = c(0, 5), ylim = c(-5, 5), step = plot_step)

# Brownian Bridge
bb <- plot_brownian_bridge(sigma = 1, initial_value = 5, terminal_value = 0, term = 5, xlim = c(0, 5), ylim = c(0, 6), step = plot_step)

# Geometric Brownian Motion
gb <- plot_geo_brownian_motion(miu = 0.1, sigma = 0.5, x0 = 2.5, xlim = c(0, 5), ylim = c(-1, 5), step = plot_step)

# Vasicek Model
vasicek <- plot_vasicek(a = 1, b = 3, sigma = 1, x0 = 0.01, xlim = c(0, 5), ylim = c(-1, 4), step = plot_step)

# Cox–Ingersoll–Ross Model
CIR <- plot_CIR(a = 1, b = 3, sigma = 2, x0 = 0.01, xlim = c(0, 5), ylim = c(-1, 4), step = plot_step)

# Poisson Process
poisson <- plot_poisson(lambda = 10, x0 = 0, xlim = c(0, 5), ylim = c(-5, 50), step = plot_step)

# Compensated Poisson Process
comp_poisson <- plot_comp_poisson(lambda = 10, x0 = 0, xlim = c(0, 5), ylim = c(-25, 25), step = plot_step)


list(
  index = 1:10,
  plot = list(tn1, tn2, bm1, bm2, bb, gb, vasicek, CIR, poisson, comp_poisson),
  short_title = c("tn1", "tn2", "bm1", "bm2", "bb", "gb", "vasicek", "CIR", "poisson", "comp_poisson"),
  title = c(
    "Trend Stationary Model (without Drift)",
    "Trend Stationary Model",
    "Brownian Motion (without Drift)",
    "Brownian Motion",
    "Brownian Bridge",
    "Geometric Brownian Motion",
    "Vasicek Model",
    "Cox–Ingersoll–Ross Model",
    "Poisson Process",
    "Compensated Poisson Process"
  )
) %>% 
  pmap(
    function(index, plot, short_title, title){
      rglwidget(
        x = plot,
        width = plot_size,
        height = plot_size
      ) %>%
        saveWidget(
          file = paste0(str_pad(index, width = 2, side = "left", pad = "0"), "_", short_title, ".html"),
          selfcontained = FALSE, 
          libdir = "files",
          background = "white",
          title = title
        )
      
      return(invisible(NULL))
    }
  )
