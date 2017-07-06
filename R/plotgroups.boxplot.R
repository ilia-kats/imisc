#' @templateVar plottype boxplot (extended with mean, standard deviation, and standard error of the mean)
#' @templateVar additionalparams
#' \item{bxppars}{additional parameters passed to \code{\link[graphics]{boxplot}} as
#'        sig\code{pars} parameter}
#' \item{swarm}{whether to overplot the boxplot with a beeswarm plot}
#' \item{swarmcols}{color of the beeswarm points}
#' \item{beeswarmpars}{additional parameters passed to \code{\link{plotgroups.beeswarm}}}
#' \item{...}{additional parameters passed to \code{\link[graphics]{boxplot}} as \code{...}. Can also
#'        contain the following:
#'        \describe{
#'                  \item{meanlty, meanlwd, meancol, meanpch, meancex}{Mean line type, line width,
#'                        color, point character, and point size expansion. The default
#'                        \code{meanpch=NA} suppresses the point, and \code{meanlty="blank"}
#'                        does so for the line. Note that \code{meanlwd} defaults to 3x the
#'                        default lwd and \code{meancol} defaults to \code{"red"}}
#'                  \item{sdwhisklty, sdwhisklwd, sdwhiskcol}{Whisker line type(default:
#'                        \code{"solid"}), width, and color for standard deviation whiskers.}
#'                  \item{sdstaplelty, sdstaplelwd, sdstaplecol}{Staple (end of whisker) line type,
#'                        width, and color for standard deviation whiskers}
#'                  \item{semwhisklty, semwhisklwd, semwhiskcol}{Whisker line type(default:
#'                        \code{"solid"}), width, and color (default: \code{"#EDA217"})
#'                        for standard error of the mean whiskers.}
#'                  \item{semstaplelty, semstaplelwd, semstaplecol}{Staple (end of whisker) line type,
#'                        width, and color (default: \code{"#090E97"}) for standard error
#'                        of the mean whiskers}
#'                  \item{cistaplelty, cistaplelwd, cistaplecol}{Staple (end of whisker) line type,
#'                        width, and color (default: \code{"#EDA217"}) for confidence interval
#'                        whiskers}}}
#' @template plotgroups.-
#' @return Same as \code{\link[graphics]{boxplot}}
#' @seealso \code{\link[graphics]{boxplot}}
#' @export
#' @importFrom rlist list.merge
#' @importFrom graphics boxplot segments points
plotgroups.boxplot <- list(
plot=function(data, at, stats, colors, features, barwidth, bxppars, swarm=FALSE, swarmcols='black', beeswarmpars=NULL, ...)
{
    if (missing(bxppars) || is.null(bxppars))
        bxppars <- list()
    lwd.base <- par("lwd")
    if (is.null(bxppars$boxwex))
        bxppars$boxwex <- barwidth
    dots <- list(...)
    pars <- list(notch=TRUE, notch.frac=0.9, outpch=NA,
                 meanlty=1, meanlwd=3*lwd.base, meancol="red", meanpch=NA, meancex=1,
                 sdwhisklty=1, sdwhisklwd=lwd.base, sdwhiskcol="black",
                 sdstaplelty=1, sdstaplelwd=lwd.base, sdstaplecol="black",
                 semwhisklty=0, semwhisklwd=lwd.base, semwhiskcol="#090E97",
                 semstaplelty=1, semstaplelwd=lwd.base, semstaplecol="#090E97",
                 ciwhisklty=0, ciwhisklwd=lwd.base, ciwhiskcol="#EDA217",
                 cistaplelty=1, cistaplelwd=lwd.base, cistaplecol="#EDA217")

    if (!("median" %in% features)) {
        bxppars$medlty <- "blank"
        bxppars$medpch <- NA
    }
    if (!("box" %in% features)) {
        bxppars$boxlty <- "blank"
        bxppars$boxfill <- "transparent"
    }
    if (!("iqr" %in% features)) {
        bxppars$whisklty <- "blank"
        bxppars$staplelty <- "blank"
    } else {
        if (is.null(bxppars$whisklty))
            bxppars$whisklty <- "22"
        if (is.null(bxppars$staplelty))
            bxppars$staplelty <- "22"
    }

    pars$pars <- bxppars
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)

    plotsd <- function() {
        if ("sd" %in% features) {
            segments(at - bxppars$boxwex / 4, stats$means + stats$sds, at + bxppars$boxwex / 4, stats$means + stats$sds, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
            segments(at - bxppars$boxwex / 4, stats$means - stats$sds, at + bxppars$boxwex / 4, stats$means - stats$sds, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
            segments(at, stats$means + stats$sds, at, stats$means - stats$sds, lend='butt', col=pars$sdwhiskcol, lty=pars$sdwhisklty, lwd=pars$sdwhisklwd)
        }
    }

    havesd <- FALSE
    if (max(stats$means + stats$sds, na.rm=TRUE) > max(stats$boxmax, na.rm=TRUE) && min(stats$means - stats$sds, na.rm=TRUE) < min(stats$boxmin, na.rm=TRUE)) {
        plotsd()
        havesd <- TRUE
    }
    bxp.toreturn <- do.call(boxplot, list.merge(pars, list(x=data, at=at, xaxt="n", col=colors, yaxt='n', add=TRUE, range=stats$range)))
    if (!havesd) {
        plotsd()
        havesd <- TRUE
    }

    if ("mean" %in% features)
        segments(at - bxppars$boxwex / 2, stats$means, at + bxppars$boxwex / 2, stats$means, lend='butt', lty=pars$meanlty, lwd=pars$meanlwd, col=pars$meancol)
        points(at, stats$means, pch=pars$meanpch, cex=pars$meancex, col=pars$meancol)
    if ("sem" %in% features) {
        segments(at - bxppars$boxwex / 2, stats$means + stats$sems, at + bxppars$boxwex / 2, stats$means + stats$sems, lend='butt', lty=pars$semstaplelty, lwd=pars$semstaplelwd, col=pars$semstaplecol)
        segments(at - bxppars$boxwex / 2, stats$means - stats$sems, at + bxppars$boxwex / 2, stats$means - stats$sems, lend='butt', lty=pars$semstaplelty, lwd=pars$semstaplelwd, col=pars$semstaplecol)
        segments(at, stats$means +stats$sems, at, stats$means - stats$sems, lend='butt', lty=pars$semwhisklty, lwd=pars$semwhisklwd, col=pars$semwhiskcol)
    }
    if ("ci" %in% features) {
        segments(at - bxppars$boxwex / 2, stats$cimax, at + bxppars$boxwex / 2, stats$cimax, lend='butt', lty=pars$cistaplelty, lwd=pars$cistaplelwd, col=pars$cistaplecol)
        segments(at - bxppars$boxwex / 2, stats$cimin, at + bxppars$boxwex / 2, stats$cimin, lend='butt', lty=pars$cistaplelty, lwd=pars$cistaplelwd, col=pars$cistaplecol)
        segments(at, stats$cimax, 1:length(data), stats$cimin, lend='butt', lty=pars$ciwhisklty, lwd=pars$ciwhisklwd, col=pars$ciwhiskcol)
    }

    if (swarm) {
        args <- list(data=data, at=at, stats=stats, colors=swarmcols, features=NA, barwidth=barwidth)
        if (!is.null(beeswarmpars) && length(beeswarmpars))
            args <- list.merge(beeswarmpars, args)
        swarm.toreturn <- do.call(plotgroups.beeswarm$plot, args)
    } else {
        swarm.toreturn <- NULL
    }
    invisible(list(boxplot=bxp.toreturn, beeswarm=swarm.toreturn))
},
ylim=function(data, stats, features, ...) {
    dots <- list(...)
    if (!is.null(dots$swarm)) {
        swarm <- dots$swarm
    } else {
        swarm <- formals(plotgroups.boxplot$plot)$swarm
    }
    if (swarm)
        do.call(plotgroups.beeswarm$ylim, c(list(data=data, stats=stats, features=NA), dots$beeswarmpars))
    else {
        NULL
    }
},
features=allparamcheck
)
