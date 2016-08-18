#' Plot several groups of repeated observations as a <%= if (exists("plottype"))plottype %>.
#'
#' This is intended to be called from \code{\link{plotgroups}}. <%= if(exists("additionaldesc"))additionaldesc %>
#'
#' @family plotgroups plotting functions
#' @seealso \code{\link{plotgroups}}
#' @param data list, each element is a vector of replicates for one combination of parameters
#' @param at numeric vector giving the X locations where the plots should be drawn
#' @param stats list containing the summary statistics (see \code{\link{plotgroups}})
#' @param colors colors for plotting
#' @param ylim whether to compute Y axis limits
#' @param features the features to plot. <%= if (exists("featuresdesc"))featuresdesc %>
#' @param barwidth width of the individual bars/boxes etc. as fraction of 1
