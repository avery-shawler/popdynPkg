
#' Simulation of predator and prey dynamics
#'
#' @param x this is the hare population size
#' @param y this is the lynx population size
#' @param z there is no z
#'
#' @return data.frame of hare and lynx sizes over time
#' @export
#'
#' @examples
#' sim()
#'
sim <- function(a = 0.01, b = 0.01, c = 0.01, d = 0.01, H0, P0, t) {
  #intialize
  H <- numeric(t)
  P <- numeric(t)

  H[1] <- H0
  P[1] <- P0

  # loop
  for(i in 1:(t-1)){
    H[i+1] = H[i] + H[i]*PredGrowth(a=a,b=b,P=P[i])
    P[i+1] = P[i] + H[i]*PreyGrowth(c=c, H=H[i], d=d)
  }

  #output
  tibble(year=1:t, Predator = P, Prey = H)
}

