#' @templateVar plottype violin plot
#' @templateVar additionaldesc Requires the \code{vioplot} package.
#' @templateVar additionalparams
#' \item{boxpars}{parameters passed to \code{\link{plotgroups.boxplot}}}
#' \item{boxcol}{color of the boxes}
#' \item{boxwidth}{width of the boxes}
#' \item{...}{addtional parameters passed to \code{\link[vioplot]{vioplot}}}
#' @template plotgroups.-
#' @return List with the following components:
#'        \item{vioplot}{List containing the aggregated return values of \code{\link[vioplot]{vioplot}}}
#'        \item{boxplot}{Return value of \code{\link{plotgroups.boxplot}}}
#' @seealso \code{\link[vioplot]{vioplot}}
#' @export
#' @importFrom rlist list.merge
plotgroups.vioplot <- list(
plot=function(data, at, stats, colors, features, barwidth, boxpars, boxcol="white", boxwidth=barwidth/4, ...)
{
    colors <- rep_len(colors, length(data))
    dots <- list(...)
    pars <- list(horizontal=FALSE)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    vioplot.results <- do.call(vioplot, list.merge(pars, list(x=data, at=at, col=colors, add=TRUE, wex=barwidth, drawRect=FALSE)))
    if (missing(boxpars) || is.null(boxpars))
        boxpars <- list()
    if (is.null(boxpars$notch))
        boxpars$notch <- FALSE
    bxp.toreturn <- do.call(plotgroups.boxplot$plot, list.merge(boxpars, list(data=data, at=at, stats=stats, colors=boxcol, features=features, barwidth=boxwidth)))
    invisible(list(vioplot=vioplot.results, boxplot=bxp.toreturn))
},
ylim=function(data, ...)range(unlist(data)),
features=function(features)
{
    if (!is.na(features) && length(features)) {
        allparamcheck(features)
    } else {
        features
    }
}
)
