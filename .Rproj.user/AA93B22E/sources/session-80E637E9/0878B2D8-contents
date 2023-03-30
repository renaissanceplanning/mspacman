
# Plot the complete trips curve ===============================================

#' Plot a complete trips curve
#'
#' @param complete_trips_curve A complete trips curve
#'
#' @return a `ggplot2` plot of the complete trips curve
#'
#' @export
plot_complete_trips_curve = function(complete_trips_curve){
  ctpd = complete_trips_curve(1440) - complete_trips_curve(0)
  dt = data.table(x=720:2159) %>%
    .[, y := complete_trips_curve(x, deriv=0)]
  dt[x > 1440, y := y-ctpd]
  dt[x > 1440, x := x-1440]
  dt = rbind(dt, data.table(x=0, y=dt$y[dt$x==1440]-ctpd))
  ymax = max(dt$y)
  xmax = 1440
  ggplot(data=dt) +
    geom_line(aes(x=x, y=y), color="red") +
    geom_hline(aes(yintercept=0)) +
    geom_vline(aes(xintercept=0)) +
    scale_x_continuous(name="Time (hours)",
                       breaks=seq(0,1440,120),
                       limits=c(0,xmax),
                       labels=seq(0,24,2),
                       expand=c(0.025,0.025)) +
    scale_y_continuous(name="Completed trips",
                       # breaks=seq(0,ymax,2000),
                       limits=c(0,ymax),
                       expand=c(0.025,0.025),
                       labels=scales::comma) +
    labs(title="Raw completed trips")
}

# Plot productivity ============================================================

#' Plot productivity for a complete trips curve
#'
#' @param complete_trips_curve A complete trips curve
#' @param smooth Should the curve be smoothed using `ggplot::geom_smooth`?
#'
#' @return a `ggplot2` plot of productivity
#'
#' @export
plot_productivity = function(complete_trips_curve, smooth=TRUE){
  dt = data.table(x=720:2159) %>%
    .[, y := productivity(x, complete_trips_curve)]
  dt[x > 1440, x := x-1440]
  dt = rbind(dt, data.table(x=0, y=dt$y[dt$x==1440]))
  ymax = max(dt$y)
  xmax = 1440
  gg = ggplot(data=dt) +
    geom_line(aes(x=x, y=y), color="red") +
    geom_hline(aes(yintercept=0)) +
    geom_vline(aes(xintercept=0)) +
    scale_x_continuous(name="Time (hours)",
                       breaks=seq(0,1440,120),
                       limits=c(0,xmax),
                       labels=seq(0,24,2),
                       expand=c(0.025,0.025)) +
    scale_y_continuous(name="Productivity",
                       # breaks=0:ymax,
                       limits=c(0,ymax)) +
    labs(title="Productivity")
  if(smooth==TRUE){
    gg + geom_smooth(aes(x=x, y=y))
  } else{
    gg
  }
}

# Plot efficiency ==============================================================

#' Plot efficiency for a complete trips curve
#'
#' @param complete_trips_curve A complete trips curve
#' @param window The size of the window (in minutes) over which efficiency
#' should be calculated
#' @param smooth Should the curve be smoothed using `ggplot::geom_smooth`?
#'
#' @return a `ggplot2` plot of efficiency
#'
#' @export
plot_efficiency = function(complete_trips_curve, window=1, smooth=TRUE){
  dt = data.table(x=720:2159) %>%
    .[, y := efficiency(x-window/2, x+window/2, complete_trips_curve)]
  dt[x > 1440, x := x-1440]
  dt = rbind(dt, data.table(x=0, y=dt$y[dt$x==1440]))
  ymax = max(dt$y)
  xmax = 1440
  gg = ggplot(data=dt) +
    geom_line(aes(x=x, y=y), color="red") +
    geom_hline(aes(yintercept=0)) +
    geom_vline(aes(xintercept=0)) +
    scale_x_continuous(name="Time (hours)",
                       breaks=seq(0,1440,120),
                       limits=c(0,xmax),
                       labels=seq(0,24,2),
                       expand=c(0.025,0.025)) +
    scale_y_continuous(name="Efficiency",
                       # breaks=0:ymax,
                       limits=c(0,ymax),
                       expand=c(0.025,0.025)) +
    labs(title=paste0("Efficiency (within ", window, " minute windows)"))
  if(smooth==TRUE){
    gg + geom_smooth(aes(x=x, y=y))
  } else{
    gg
  }
}
