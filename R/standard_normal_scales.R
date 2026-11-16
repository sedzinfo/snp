##########################################################################################
# STANDARD NORMAL SCALES
##########################################################################################
#' @title Plot the standard normal curve with score scales
#' @description Draws the standard normal curve, divided at every standard deviation and marked
#' with the percentage of cases in each interval, with the z, T, cumulative percentage,
#' percentile, sten, stanine, grade and IQ scales lined up under it, so that a score on one
#' scale can be read off on the others. In blues, reds or greens, colour shows how common a
#' score is; the diverging palette colours the bands blue below the mean and red above it. Each
#' palette has a light and a dark mode.
#' @details All scales are placed on the z axis:
#' \itemize{
#'   \item \strong{z}: standard score, mean 0 and standard deviation 1.
#'   \item \strong{T}: \eqn{50 + 10z}.
#'   \item \strong{Cumulative \%}: the percentage of cases below each standard deviation, from
#'     \eqn{z = -3} to \eqn{z = 3}: 0.1, 2.3, 15.9, 50, 84.1, 97.7 and 99.9.
#'   \item \strong{Percentile}: the z values of the 1st, 5th, 10th, 25th, 50th, 75th, 90th,
#'     95th and 99th percentiles.
#'   \item \strong{Sten}: ten bands half a standard deviation wide, sten 1 below \eqn{z = -2}
#'     and sten 10 above \eqn{z = 2} (mean 5.5, standard deviation 2).
#'   \item \strong{Stanine}: nine bands half a standard deviation wide, stanine 5 from
#'     \eqn{z = -0.25} to \eqn{z = 0.25} (mean 5, standard deviation 2).
#'   \item \strong{Grade}: groups of stanines, E (1-2), D (3), C (4-6), B (7) and A (8-9).
#'   \item \strong{IQ}: \eqn{100 + 15z}.
#' }
#' The chart is \code{\link{standard_normal_custom}} drawn with
#' \code{\link{standard_normal_scales_data}()}: start from those to change the scales.
#' @inheritParams standard_normal_custom
#' @param size Text size of all labels, in millimetres (ggplot2 text size). The default of 3
#'   suits figures around 7 inches wide; about 5 suits figures around 12 inches wide.
#' @return A ggplot object. Print it to draw the chart, or save it with
#'   \code{ggplot2::ggsave()}. The chart keeps a fixed aspect ratio and is about 2.2 times as
#'   wide as it is tall, so a figure with those proportions, such as 7 by 3.2 inches, has no
#'   empty margins.
#' @seealso \code{\link{standard_normal_custom}}, \code{\link{standard_normal_scales_data}}
#' @import ggplot2
#' @export
#' @examples
#' standard_normal_scales()
#' standard_normal_scales(palette="reds")
#' standard_normal_scales(palette="diverging",mode="dark")
#' standard_normal_scales(size=5)
#' # save it with the chart's proportions
#' ggsave(file.path(tempdir(),"standard_normal_scales.png"),standard_normal_scales(),
#'        width=7,height=3.2)
standard_normal_scales<-function(size=3,palette=c("blues","reds","greens","diverging"),mode=c("light","dark")) {
  standard_normal_custom(standard_normal_scales_data(),size=size,palette=match.arg(palette),mode=match.arg(mode))
}
