#' @templateVar plottype boxplot (extended with mean, standard deviation, and standard error of the mean)
#' @templateVar additionalparams
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
#'                  \item{sdstaplelty, sdstaplelwd, sdstaplecol, sdstaplewex}{Staple (end of whisker) line type,
#'                        width, color (default: black), and length of standard deviation whiskers}
#'                  \item{semwhisklty, semwhisklwd, semwhiskcol}{Whisker line type(default:
#'                        \code{"solid"}), width, and color (default: \code{"#EDA217"})
#'                        of standard error of the mean whiskers.}
#'                  \item{semstaplelty, semstaplelwd, semstaplecol, semstaplewex}{Staple (end of whisker) line type,
#'                        width, color (default: \code{"#090E97"}), and length of standard error
#'                        of the mean whiskers}
#'                  \item{cistaplelty, cistaplelwd, cistaplecol, cistaplewex}{Staple (end of whisker) line type,
#'                        width, color (default: \code{"#EDA217"}), and length of confidence interval
#'                        whiskers}}}
#' @template plotgroups.-
#' @return Same as \code{\link[graphics]{boxplot}}
#' @seealso \code{\link[graphics]{boxplot}}
#' @export
#' @importFrom rlist list.merge
#' @importFrom graphics boxplot segments points
plotgroups.boxplot <- list(
plot=function(data, at, stats, colors, features, barwidth, swarm=FALSE, swarmcols='black', beeswarmpars=NULL, ...)
{
    lwd.base <- par("lwd")
    dots <- list(...)
    pars <- list(notch=TRUE, notch.frac=0.5, outpch=NA,
                 meanlty=1, meanlwd=3*lwd.base, meancol="red", meanpch=NA, meancex=1,
                 sdwhisklty=1, sdwhisklwd=lwd.base, sdwhiskcol="black",
                 sdstaplelty=1, sdstaplelwd=lwd.base, sdstaplecol="black", sdstaplewex=0.5,
                 semwhisklty=0, semwhisklwd=lwd.base, semwhiskcol="#090E97",
                 semstaplelty=1, semstaplelwd=lwd.base, semstaplecol="#090E97", semstaplewex=0.5,
                 ciwhisklty=0, ciwhisklwd=lwd.base, ciwhiskcol="#EDA217",
                 cistaplelty=1, cistaplelwd=lwd.base, cistaplecol="#EDA217", cistaplewex=1)
    pars$boxwex <- barwidth
    pars$boxlwd <- par('lwd')

    if (!("median" %in% features)) {
        pars$medlty <- "blank"
        pars$medpch <- NA
    }
    if (!("box" %in% features)) {
        pars$boxlty <- "blank"
        pars$boxfill <- "transparent"
    }
    if (!("iqr" %in% features)) {
        pars$whisklty <- "blank"
        pars$staplelty <- "blank"
    } else {
        if (is.null(pars$whisklty))
            pars$whisklty <- "22"
        if (is.null(pars$staplelty))
            pars$staplelty <- "22"
    }

    if (length(dots) > 0)
        pars <- list.merge(pars, dots)

    calc_notch_coords <- function(x, y, wex=1, stats=NULL, conf=NULL) {
        wex <- wex * pars$boxwex / 2
        if (is.null(stats) || is.null(conf))
            return(list(x1=x - wex, x2=x + wex, y=y))
        n <- ifelse(y < stats[3,], conf[1,], conf[2,])
        intersect <- (y - stats[3,]) * (pars$notch.frac * 0.5) / (n - stats[3,]) + 0.5 * (pars$boxwex * pars$notch.frac)
        intersect[intersect < 0] <- wex
        x1 <- pmax(x - wex, x - intersect)
        x2 <- pmin(x + wex, x + intersect)
        list(x1=x1, x2=x2, y=y)
    }

    plotsd <- function(bxpstats=NULL, conf=NULL) {
        if ("sd" %in% features) {
            uc <- calc_notch_coords(at, stats$means + stats$sds, pars$sdstaplewex, bxpstats, conf)
            lc <- calc_notch_coords(at, stats$means - stats$sds, pars$sdstaplewex, bxpstats, conf)
            segments(uc$x1, uc$y, uc$x2, uc$y, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
            segments(lc$x1, lc$y, lc$x2, lc$y, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
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
        plotsd(bxp.toreturn$stats, bxp.toreturn$conf)
        havesd <- TRUE
    }

    if ("mean" %in% features) {
        mc <- calc_notch_coords(at, stats$means, 1, bxp.toreturn$stats, bxp.toreturn$conf)
        segments(mc$x1, mc$y, mc$x2, mc$y, lend='butt', lty=pars$meanlty, lwd=pars$meanlwd, col=pars$meancol)
        points(at, stats$means, pch=pars$meanpch, cex=pars$meancex, col=pars$meancol)
    }
    if ("sem" %in% features) {
        uc <- calc_notch_coords(at, stats$means + stats$sems, pars$semstaplewex, bxp.toreturn$stats, bxp.toreturn$conf)
        lc <- calc_notch_coords(at, stats$means - stats$sems, pars$semstaplewex, bxp.toreturn$stats, bxp.toreturn$conf)
        segments(uc$x1, uc$y, uc$x2, uc$y, lend='butt', lty=pars$semstaplelty, lwd=pars$semstaplelwd, col=pars$semstaplecol)
        segments(lc$x1, lc$y, lc$x2, lc$y, lend='butt', lty=pars$semstaplelty, lwd=pars$semstaplelwd, col=pars$semstaplecol)
        segments(at, stats$means +stats$sems, at, stats$means - stats$sems, lend='butt', lty=pars$semwhisklty, lwd=pars$semwhisklwd, col=pars$semwhiskcol)
    }
    if ("ci" %in% features) {
        uc <- calc_notch_coords(at, stats$cimax, pars$cistaplewex, bxp.toreturn$stats, bxp.toreturn$conf)
        lc <- calc_notch_coords(at, stats$cimin, pars$cistaplewex, bxp.toreturn$stats, bxp.toreturn$conf)
        segments(uc$x1, uc$y, uc$x2, uc$y, lend='butt', lty=pars$cistaplelty, lwd=pars$cistaplelwd, col=pars$cistaplecol)
        segments(lc$x1, lc$y, lc$x2, lc$y, lend='butt', lty=pars$cistaplelty, lwd=pars$cistaplelwd, col=pars$cistaplecol)
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
