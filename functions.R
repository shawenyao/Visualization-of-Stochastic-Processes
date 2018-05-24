#' plot the origin point in 3-dimensional space
#' 
#' @param xlim
#' @param ylim
#' @param zlim
#' 
plot_origin <- function(xlim, ylim, zlim){
  # plot the origin point
  plot3d(
    x = 0, y = 0, z = 0,
    xlim = xlim, ylim = ylim, zlim = zlim,
    size = 5, col = "grey",
    xlab = "t", ylab = "x", zlab = "f(x)"
  )
}


#' plot the evolution of probability density function over time
#' 
#' @param xpoints
#' @param ypoints
#' @param zpoints
#' @param mean_ypoints
#' @param mean_zpoints
#' @param xlim
#' @param ylim
#' 
#' @return an rgl scene object
#' 
plot_pdf <- function(xpoints, ypoints, zpoints, mean_ypoints, mean_zpoints, xlim, ylim){
  open3d()
  
  plot_origin(xlim = xlim, ylim = ylim, zlim = c(0, max(zpoints, na.rm = TRUE)))
  
  # plot the probablity density function over time
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
  
  # plot the expected value points
  points3d(
    x = xpoints,
    y = mean_ypoints,
    z = mean_zpoints,
    size = 5,
    col = rainbow(length(xpoints))
  )
  
  # set camera angle
  rgl.viewpoint(userMatrix = rotationMatrix(1.5, -0.5, 0.2, 0.4), zoom = 0.9)
  
  return(scene3d())
}


#' plot the evolution of probability mass function over time
#' 
#' @param xpoints
#' @param ypoints
#' @param zpoints
#' @param mean_ypoints
#' @param mean_zpoints
#' @param xlim
#' @param ylim
#' 
#' @return an rgl scene object
#' 
plot_pmf <- function(xpoints, ypoints, zpoints, mean_ypoints, mean_zpoints, xlim, ylim){
  open3d()
  
  plot_origin(xlim = xlim, ylim = ylim, zlim = c(0, max(zpoints, na.rm = TRUE)))
  
  # plot the probablity mass function over time
  lapply(
    seq_along(xpoints),
    function(i){
      points3d(
        x = xpoints[i], 
        y = ypoints, 
        z = zpoints[i,],
        size = 2, 
        col = rainbow(length(xpoints))[i]
      )
    }
  )
  
  # plot the expected value points
  points3d(
    x = xpoints,
    y = mean_ypoints,
    z = mean_zpoints,
    size = 5,
    col = rainbow(length(xpoints))
  )
  
  # set camera angle
  rgl.viewpoint(userMatrix = rotationMatrix(1.5, -0.5, 0.2, 0.4), zoom = 0.9)
  
  return(scene3d())
}


#' visulize the probabiltiy density function of trend stationary model
#' 
#' @description 
#' X(t) = X(0) + beta * t + epsilon(t)
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
      mean = x0 + beta * rep(xpoints, each = step),
      sd = sigma
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # the expected values and the density of expected values
  mean_ypoints <- x0 + beta * xpoints
  mean_zpoints <- dnorm(
    x = mean_ypoints,
    mean = x0 + beta * xpoints,
    sd = sigma
  )
  
  # disregard the deterministic case at time 0 in plotting
  plot_pdf(
    xpoints = xpoints[-1], ypoints = ypoints, zpoints = zpoints[-1,], 
    mean_ypoints = mean_ypoints[-1], mean_zpoints = mean_zpoints[-1],
    xlim = xlim, ylim = ylim
  )
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
      mean = x0 + miu * rep(xpoints, each = step),
      sd = sigma * sqrt(rep(xpoints, each = step) - xpoints[1])
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # the expected values and the density of expected values
  mean_ypoints <- x0 + miu * xpoints
  mean_zpoints <- dnorm(
    x = mean_ypoints,
    mean = x0 + miu * xpoints,
    sd = sigma * sqrt(xpoints - xpoints[1])
  )
  
  # disregard the deterministic case at time 0 in plotting
  plot_pdf(
    xpoints = xpoints[-1], ypoints = ypoints, zpoints = zpoints[-1,], 
    mean_ypoints = mean_ypoints[-1], mean_zpoints = mean_zpoints[-1],
    xlim = xlim, ylim = ylim
  )
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
      meanlog = log(x0) + rep(xpoints, each = step) * (miu - 0.5 * sigma^2),
      sdlog = sigma * sqrt(rep(xpoints, each = step) - xpoints[1])
    ),
    nrow = step,
    ncol = step,
    byrow = TRUE
  )
  
  # the expected values and the density of expected values
  mean_ypoints <- exp(miu * xpoints) * x0
  mean_zpoints <- dlnorm(
    x = mean_ypoints,
    meanlog = log(x0) + xpoints * (miu - 0.5 * sigma^2),
    sdlog = sigma * sqrt(xpoints - xpoints[1])
  )
  
  # disregard the deterministic case at time 0 in plotting
  plot_pdf(
    xpoints = xpoints[-1], ypoints = ypoints, zpoints = zpoints[-1,], 
    mean_ypoints = mean_ypoints[-1], mean_zpoints = mean_zpoints[-1],
    xlim = xlim, ylim = ylim
  )
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
  
  # the expected values and the density of expected values
  mean_ypoints <- x0 * exp(-a * xpoints) + b * (1 - exp(-a * xpoints))
  mean_zpoints <- dnorm(
    x = mean_ypoints,
    mean = x0 * exp(-a * xpoints) + b * (1 - exp(-a * xpoints)),
    sd = sqrt(sigma^2 / (2 * a) * (1 - exp(-2 * a * xpoints)))
  )
  
  # disregard the deterministic case at time 0 in plotting
  plot_pdf(
    xpoints = xpoints[-1], ypoints = ypoints, zpoints = zpoints[-1,], 
    mean_ypoints = mean_ypoints[-1], mean_zpoints = mean_zpoints[-1],
    xlim = xlim, ylim = ylim
  )
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
  
  # the expected values and the density of expected values
  mean_ypoints <- x0 * exp(-a * xpoints) + b * (1 - exp(-a * xpoints))
  mean_zpoints <- dchisq(
    x = mean_ypoints *
      2 * (2 * a / ((1 - exp(-a * xpoints)) * sigma^2)),
    df = 4 * a * b / sigma^2,
    ncp = 2 * (2 * a / ((1 - exp(-a * xpoints)) * sigma^2)) * 
      x0 * exp(-a * xpoints)
  )
  
  # disregard the deterministic case at time 0 in plotting
  plot_pdf(
    xpoints = xpoints[-1], ypoints = ypoints, zpoints = zpoints[-1,], 
    mean_ypoints = mean_ypoints[-1], mean_zpoints = mean_zpoints[-1],
    xlim = xlim, ylim = ylim
  )
}


#' visulize the probabiltiy mass function of possion process
#' 
#' @description 
#' P(X_t - X_s = k) = exp(-lambda(t-s) * lambda^k * (t - s)^k / k!)
#' 
#' @param lambda the intensity of the possion process
#' @param x0 initial value at time 0
#' @param xlim
#' @param ylim
#' @param step
#' 
#' @return an rgl scene object
plot_poisson <- function(lambda, x0, xlim, ylim, step){
  xpoints <- seq(from = xlim[1], to = xlim[2], length = step)
  ypoints <- seq(from = ylim[1], to = ylim[2], by = 1)
  zpoints <- matrix(
    dpois(
      x = rep(ypoints, times = step),
      lambda = rep(xpoints, each = length(ypoints)) * lambda
    ),
    nrow = step,
    ncol = length(ypoints),
    byrow = TRUE
  )
  
  # the expected values (or the closet permissible values) and the probability mass
  mean_ypoints <- (xpoints * lambda) %>% round(0)
  mean_zpoints <- dpois(
    x = mean_ypoints,
    lambda = xpoints * lambda
  )
  
  # disregard the deterministic case at time 0 in plotting
  plot_pmf(
    xpoints = xpoints[-1], ypoints = ypoints, zpoints = zpoints[-1,], 
    mean_ypoints = mean_ypoints[-1], mean_zpoints = mean_zpoints[-1],
    xlim = xlim, ylim = ylim
  )
}
