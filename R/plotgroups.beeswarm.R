#' @templateVar plottype beeswarm plot
#' @templateVar additionaldesc Requires the \code{beeswarm} package.
#' @templateVar featuresdesc At the moment, either \code{mean} or \code{median} can be plotted. Also, only one of \code{box}, \code{iqr}, \code{sd}, \code{sem}, \code{ci} can be plotted at the moment.
#' @templateVar additionalparams
#' \item{palpha}{opacity of the individual points}
#' \item{bxplwd}{line width for the simplified boxplot}
#' \item{bxpcols}{colors for the simplified boxplot}
#' \item{...}{additional parameters passed to \code{\link[beeswarm]{beeswarm}}}
#' @template plotgroups.-
#' @return Same as \code{\link[beeswarm]{beeswarm}}
#' @seealso \code{\link[beeswarm]{beeswarm}}
#' @export
#' @importFrom rlist list.merge
#' @importFrom graphics segments
plotgroups.beeswarm <- list(
plot=function(data, at, stats, colors, features, barwidth, palpha=1, bxplwd=par("lwd"), bxpcols=colors, ...)
{
    if (!requireNamespace("beeswarm", quietly = TRUE))
        stop("Please install the beeswarm package for this plot.")
    dots <- list(...)
    pars <- list(method="swarm", corral="random", priority="random", pch=16)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)

    toreturn <- do.call(beeswarm::beeswarm, list.merge(pars, list(x=data, at=at, corralWidth=barwidth, add=TRUE, col=adjustcolor(colors, alpha.f=palpha), yaxs='i', xaxt='n')))

    if (!is.na(features) && length(features)) {
        bars <- threeparamsstats(stats, features)
        if (!is.null(bars$u) && !is.null(bars$l))
            segments(at, bars$l, at, bars$u, col=bxpcols, lend='butt', lwd=bxplwd)

        for (b in bars) {
            if (!is.null(b))
                segments(at - barwidth / 2, b, at + barwidth / 2, b, col=bxpcols, lend='butt', lwd=bxplwd)
        }
    }
    invisible(toreturn)
},
ylim=function(data, stats, features, ...) {
    r <- range(unlist(data), na.rm=TRUE)
    if (!is.na(features) && length(features)) {
        bars <- threeparamsstats(stats, features)
        r <- range(c(r, bars$u, bars$l, bars$m), na.rm=TRUE)
    }
    r
},
features=function(features)
{
    if (!is.na(features) && length(features)) {
        threeparamcheck(features)
    } else {
        features
    }
})
