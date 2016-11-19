#' Plot several groups of repeated observations as a <%= if (exists("plottype"))plottype %>.
#'
#' This is intended to be called from \code{\link{plotgroups}}. <%= if(exists("additionaldesc"))additionaldesc %>
#'
#' @family plotgroups plotting functions
#' @seealso \code{\link{plotgroups}}
#' @format list of functions \code{plot}, \code{ylim}, \code{features}
#' \code{plot} accepts the following parameters:
#' \describe{
#' \item{data}{list, each element is a vector of replicates for one combination of parameters}
#' \item{at}{numeric vector giving the X locations where the plots should be drawn}
#' \item{stats}{list containing the summary statistics (see \code{\link{plotgroups}})}
#' \item{colors}{colors for plotting}
#' \item{features}{the features to plot. <%= if (exists("featuresdesc"))featuresdesc %>}
#' \item{barwidth}{width of the individual bars/boxes etc. as fraction of 1}
#' <%= if (exists("additionalparams")) gsub('\n', "\n#' ", additionalparams, fixed=TRUE) %>
#' }
