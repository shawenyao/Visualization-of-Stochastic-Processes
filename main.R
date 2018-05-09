library(rgl)
library(htmlwidgets)
library(purrr)

# Brownian Motion without drift
bm1 <- plot_brownian_motion(drift = 0, volatility = 1, xlim = c(0, 10), ylim = c(-5, 5), step = 50)

# Brownian Motion with drift
bm2 <- plot_brownian_motion(drift = 1, volatility = 1, xlim = c(0, 10), ylim = c(-5, 5), step = 50)

# Trend Normal without slope
tn1 <- plot_trend_normal(slope = 0, intercept = 0, volatility = 1, xlim = c(-5, 5), ylim = c(-5, 5), step = 50)

# Trend Normal with slope
tn2 <- plot_trend_normal(slope = 1, intercept = 1, volatility = 1, xlim = c(-5, 5), ylim = c(-5, 5), step = 50)


setwd("C:/Users/Wenyao/Desktop/R/Brownian_Motion_Visualization/webGL")

list(
  rglscene = list(bm1, bm2, tn1, tn2),
  description=c("bm1", "bm2", "tn1", "tn2")
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

