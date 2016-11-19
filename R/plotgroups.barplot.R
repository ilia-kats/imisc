#' @templateVar plottype barplot
#' @templateVar featuresdesc At the moment, either \code{mean} or \code{median} can be plotted. Also, only one of \code{box}, \code{iqr}, \code{sd}, \code{sem}, \code{ci} can be plotted at the moment.
#' @template plotgroups.-
#' @param whiskerswidth width of the whiskers as fraction of 1
#' @param whiskerslwd line width of the whiskers
#' @param whiskerscol color of the whiskers
#' @param bordercol color of the border
#' @param ... additional parameters passed to \code{\link[graphics]{rect}}
#' @export
#' @importFrom rlist list.merge
plotgroups.barplot <- list(
plot=function(data, at, stats, colors, features, barwidth, whiskerswidth=barwidth, whiskerslwd=par("lwd"), whiskerscol="black", whiskerslty=1, bordercol="black", ...)
{
    bars <- threeparamsstats(stats, features)
    dots <- list(...)
    pars <- list(names.arg=NULL)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(rect, list.merge(pars, list(xleft=at - barwidth/2, ybottom=par("usr")[3], xright=at + barwidth/2, ytop=bars$m, col=colors, border=bordercol)))

    if (!is.null(bars$u) && !is.null(bars$l))
        segments(at, bars$l, at, bars$u, col=whiskerscol, lend='butt', lwd=whiskerslwd, lty=whiskerslty)

    for (b in bars[c("u", "l")]) {
        if (!is.null(b))
            segments(at - whiskerswidth / 2, b, at + whiskerswidth / 2, b, col=whiskerscol, lend='butt', lwd=whiskerslwd, lty=whiskerslty)
    }
    invisible(NULL)
},
ylim=function(...)NULL,
features=threeparamcheck)
