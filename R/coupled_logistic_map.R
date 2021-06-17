coupled_logistic_map <- function(x0 = 0.2,
                                 y0 = 0.6,
                                 rx = 3.65,
                                 ry = 3.8,
                                 bxy = 0,
                                 byx = 0.4,
                                 N = 100,
                                 N_skip = 0) {
  X <- rep(0, N)
  Y <- rep(0, N)
  if (N_skip > 0) {
    #
    # Iterate for N_trans generations without collecting data (only
    # X[1] and Y[1] are updated, so the last data point will be
    # used as the new X[1] and Y[1]).
    #
    X0 <- x0
    Y0 <- y0
    for (t in 1:N_skip) {
      X[1] <-  X0 * (rx - rx * X0 - bxy * Y0)
      Y[1] <-  Y0 * (ry - ry * Y0 - byx * X0)
      X0 <-  X[1]
      Y0 <-  Y[1]
    }
  }
  
  # Iterate the coupled maps for N generations
  for (t in 2:N) {
    X[t] <-  X[t - 1] * (rx - rx * X[t - 1] - bxy * Y[t - 1])
    Y[t] <-  Y[t - 1] * (ry - ry * Y[t - 1] - byx * X[t - 1])
  }
  
  return(
    data.frame(
      time = 1:N,
      X = X,
      Y = Y
    )
  )
}