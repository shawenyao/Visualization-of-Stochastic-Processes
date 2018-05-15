#' plot a 3-D surface
#' 
#' @param xpoints
#' @param ypoints
#' @param zpoints
#' 
#' @return an rgl scene object
#' 
plot_3d_surface <- function(xpoints, ypoints, zpoints){
  open3d()
  
  # plot the origin point
  plot3d(
    x = 0, y = 0, z = 0,
    xlim = range(xpoints), ylim = range(ypoints), zlim = c(0, max(zpoints, na.rm = TRUE)),
    size = 5, col = "grey",
    xlab = "t", ylab = "x", zlab = "f(x)"
  )
  
  # plot the probablity density functions
  lapply(
    seq_along(xpoints),
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
    seq_along(xpoints),
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
  
  # set camera angle
  rgl.viewpoint(userMatrix = rotationMatrix(1.5, -0.5, 0.2, 0.4))
  
  return(scene3d())
}


#' visulize the probabiltiy density function of trend stationary model
#' 
#' @description 
#' X(t) = X(0) + beta * dt + epsilon(t)
#' 
#' @param beta slope
#' @param sigma standard deviation of error term
#' @param x0 initial value at time 0
#' @param xlim
#' @param ylim
#' @param step
#' 
#' @return an rgl scene object
#' 
plot_trend_stationary <- function(beta, sigma, x0, xlim, ylim, step){
  
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], length = step)
  zpoints <- matrix(
    dnorm(
      x = rep(ypoints, times = step),
      mean = beta * rep(xpoints, each = step) + x0,
      sd = sigma
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # disregard the deterministic case time 0
  zpoints[1,] <- NA
  
  plot_3d_surface(xpoints = xpoints, ypoints = ypoints, zpoints = zpoints)
}


#' visulize the probabiltiy density function of Brownian motion
#' 
#' @description 
#' dX(t) = miu * dt + sigma * dW(t)
#' 
#' @param miu drift
#' @param sigma instantaneous volatility
#' @param x0 initial value at time 0
#' @param xlim
#' @param ylim
#' @param step
#' 
#' @return an rgl scene object
#' 
plot_brownian_motion <- function(miu, sigma, x0, xlim, ylim, step){
  
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], length = step)
  zpoints <- matrix(
    dnorm(
      x = rep(ypoints, times = step),
      mean = rep(xpoints, each = step) * miu + x0,
      sd = sqrt(rep(xpoints, each = step) - xpoints[1]) * sigma
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # disregard the deterministic case time 0
  zpoints[1,] <- NA
  
  plot_3d_surface(xpoints = xpoints, ypoints = ypoints, zpoints = zpoints)
}


#' visulize the probabiltiy density function of geometric Brownian motion
#' 
#' @description 
#' dX(t) = miu * X(t) * dt + sigma * X(t) * dW(t)
#' 
#' @param miu drift
#' @param sigma instantaneous volatility
#' @param x0 initial value at time 0
#' @param xlim
#' @param ylim
#' @param step
#' 
#' @return an rgl scene object
#' 
plot_geo_brownian_motion <- function(miu, sigma, x0, xlim, ylim, step){
  
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], length = step)
  zpoints <- matrix(
    dlnorm(
      x = rep(ypoints, times = step),
      meanlog = rep(xpoints, each = step) * (miu - 0.5 * sigma^2) + log(x0),
      sdlog = sqrt(rep(xpoints, each = step) - xpoints[1]) * sigma
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # disregard the deterministic case time 0
  zpoints[1,] <- NA
  
  plot_3d_surface(xpoints = xpoints, ypoints = ypoints, zpoints = zpoints)
}


#' visulize the probabiltiy density function of Vasicek model
#' 
#' @description 
#' dX(t) = a * (b - X(t)) * dt + sigma * dW(t)
#' 
#' @param a the speed of reversion
#' @param b long term mean level
#' @param sigma instantaneous volatility
#' @param x0 initial value at time 0
#' @param xlim
#' @param ylim
#' @param step
#' 
#' @return an rgl scene object
#' 
plot_vasicek <- function(a, b, sigma, x0, xlim, ylim, step){
  
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], length = step)
  zpoints <- matrix(
    dnorm(
      x = rep(ypoints, times = step),
      mean = x0 * exp(-a * rep(xpoints, each = step)) + 
        b * (1 - exp(-a * rep(xpoints, each = step))),
      sd = sqrt(sigma^2 / (2 * a) * (1 - exp(-2 * a * rep(xpoints, each = step))))
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # disregard the deterministic case time 0
  zpoints[1,] <- NA
  
  plot_3d_surface(xpoints = xpoints, ypoints = ypoints, zpoints = zpoints)
}


#' visulize the probabiltiy density function of Cox–Ingersoll–Ross model
#' 
#' @description 
#' dX(t) = a * (b - X(t)) * dt + sigma * sqrt(X(t)) * dW(t)
#' 
#' @param a the speed of reversion
#' @param b long term mean level
#' @param sigma instantaneous volatility
#' @param x0 initial value at time 0
#' @param xlim
#' @param ylim
#' @param step
#' 
#' @return an rgl scene object
#' 
plot_CIR <- function(a, b, sigma, x0, xlim, ylim, step){
  
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], length = step)
  zpoints <- matrix(
    dchisq(
      x = rep(ypoints, times = step) *
        2 * (2 * a / ((1 - exp(-a * rep(xpoints, each = step))) * sigma^2)),
      df = 4 * a * b / sigma^2,
      ncp = 2 * (2 * a / ((1 - exp(-a * rep(xpoints, each = step))) * sigma^2)) * 
        x0 * exp(-a * rep(xpoints, each = step))
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # disregard the deterministic case time 0
  zpoints[1,] <- NA
  
  plot_3d_surface(xpoints = xpoints, ypoints = ypoints, zpoints = zpoints)
}
