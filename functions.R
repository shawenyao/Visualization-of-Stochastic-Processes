#' 
#' @description 
#' plot the evolution of a specific probability desnity function over time
#' 
#' @param xpoints
#' @param ypoints
#' @param zpoints
#' 
#' @return an rgl scene object
#' 
plot_pdf3d <- function(xpoints, ypoints, zpoints){
  open3d()
  
  # plot the origin
  plot3d(
    x = 0, y = 0, z = 0,
    xlim = range(xpoints), ylim = range(ypoints), zlim = c(0, 1),
    size=5, col="red",
    xlab="t", ylab="x", zlab="f(x)"
  )
  
  # plot the probablity density functions
  lapply(
    1:length(xpoints),
    function(i){
      lines3d(
        x = xpoints[i], 
        y = ypoints, 
        z = zpoints[i,],
        lwd = 2, 
        col = rainbow(length(xpoints))[i]
      )
    }
  )
  
  # plot the highest points
  lapply(
    1:length(xpoints),
    function(i){
      plot3d(
        x = xpoints[i], 
        y = ypoints[which.max(zpoints[i, ])], 
        z = zpoints[i, which.max(zpoints[i, ])], 
        size = 5, 
        col = rainbow(length(xpoints))[i], 
        add = TRUE
      )
    }
  )
  
  # view from a specific point
  rgl.viewpoint(userMatrix = rotationMatrix(1.5, -0.5, 0.2, 0.4))
  
  return(scene3d())
}


#' @description 
#' visulize the probabiltiy density function of Brownian motion
#' 
#' @param drift
#' @param xlim
#' @param step
#' @param ylim
#' @param ystep
#' 
#' @return 
#' 
plot_brownian_motion <- function(drift, volatility, xlim, ylim, step){
  
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], length = step)
  zpoints <- matrix(
    dnorm(
      x = rep(ypoints, times = length(xpoints)),
      mean = xpoints[1] + rep(xpoints, each = length(ypoints)) * drift,
      sd = sqrt(rep(xpoints, each = length(ypoints)) - xpoints[1]) * volatility
    ),
    nrow = length(xpoints),
    ncol = length(ypoints),
    byrow = TRUE
  )
  
  # disregard the deterministic case of Brownian motion at time 0
  zpoints[1,] <- NA
  
  plot_pdf3d(xpoints = xpoints, ypoints = ypoints, zpoints = zpoints)
}

#' @description 
#' visulize the probabiltiy density function of trend normal model
#' 
#' @param slope
#' @param intercept
#' @param volatility
#' @param xlim
#' @param step
#' @param ylim
#' @param ystep
#' 
#' @return 
#' 
plot_trend_normal <- function(slope, intercept, volatility, xlim, ylim, step){
  
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], length = step)
  zpoints <- matrix(
    dnorm(
      x = rep(ypoints, times = length(xpoints)),
      mean = slope * rep(xpoints, each = length(ypoints)) + intercept,
      sd = volatility
    ),
    nrow = length(xpoints),
    ncol = length(ypoints),
    byrow = TRUE
  )
  
  plot_pdf3d(xpoints = xpoints, ypoints = ypoints, zpoints = zpoints)
}