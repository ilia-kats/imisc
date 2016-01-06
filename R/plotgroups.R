#' @templateVar plottype boxplot (extended with mean, standard deviation, and standard error of the mean)
#' @template plotgroups.-
#' @param bxppars additional parameters passed to \code{\link[graphics]{boxplot}} as
#'        sig\code{pars} parameter
#' @param ... additional parameters passed to \code{\link[graphics]{boxplot}} as \code{...}. Can also
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
#'                        whiskers}}
#' @return Same as \code{\link[graphics]{boxplot}}
#' @seealso \code{\link[graphics]{boxplot}}
#' @export
#' @importFrom rlist list.merge
plotgroups.boxplot <- function(data, stats, colors, ylim, features, barwidth, bxppars, ...)
{
    if (!missing(ylim))
        return(NULL)
    if (missing(bxppars) || is.null(bxppars))
        bxppars <- list()
    lwd.base <- par("lwd")
    if (is.null(bxppars$boxwex))
        bxppars$boxwex <- barwidth
    dots <- list(...)
    pars <- list(notch=TRUE, las=1, notch.frac=0.9, outpch=NA,
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
        bxppars$whisklty <- "22"
        bxppars$staplelty <- "22"
    }

    pars$pars <- bxppars
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)

    plotsd <- function() {
        if ("sd" %in% features) {
            segments(1:length(data) - bxppars$boxwex / 4, stats$means + stats$sds, 1:length(data) + bxppars$boxwex / 4, stats$means + stats$sds, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
            segments(1:length(data) - bxppars$boxwex / 4, stats$means - stats$sds, 1:length(data) + bxppars$boxwex / 4, stats$means - stats$sds, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
            segments(1:length(data), stats$means + stats$sds, 1:length(data), stats$means - stats$sds, lend='butt', col=pars$sdwhiskcol, lty=pars$sdwhisklty, lwd=pars$sdwhisklwd)
        }
    }

    havesd <- FALSE
    if (max(stats$means + stats$sds) > max(stats$boxmax) && min(stats$means - stats$sds) < min(stats$boxmin)) {
        plotsd()
        havesd <- TRUE
    }
    toreturn <- do.call(boxplot, list.merge(pars, list(x=data, xaxt="n", col=colors, yaxt='n', add=TRUE, range=stats$range)))
    if (!havesd) {
        plotsd()
        havesd <- TRUE
    }

    if ("mean" %in% features)
        segments(1:length(data) - bxppars$boxwex / 2, stats$means, 1:length(data) + bxppars$boxwex / 2, stats$means, lend='butt', lty=pars$meanlty, lwd=pars$meanlwd, col=pars$meancol)
        points(1:length(data), stats$means, pch=pars$meanpch, cex=pars$meancex, col=pars$meancol)
    if ("sem" %in% features) {
        segments(1:length(data) - bxppars$boxwex / 2, stats$means + stats$sems, 1:length(data) + bxppars$boxwex / 2, stats$means + stats$sems, lend='butt', lty=pars$semstaplelty, lwd=pars$semstaplelwd, col=pars$semstaplecol)
        segments(1:length(data) - bxppars$boxwex / 2, stats$means - stats$sems, 1:length(data) + bxppars$boxwex / 2, stats$means - stats$sems, lend='butt', lty=pars$semstaplelty, lwd=pars$semstaplelwd, col=pars$semstaplecol)
        segments(1:length(data), stats$means +stats$sems, 1:length(data), stats$means - stats$sems, lend='butt', lty=pars$semwhisklty, lwd=pars$semwhisklwd, col=pars$semwhiskcol)
    }
    if ("ci" %in% features) {
        segments(1:length(data) - bxppars$boxwex / 2, stats$cimax, 1:length(data) + bxppars$boxwex / 2, stats$cimax, lend='butt', lty=pars$cistaplelty, lwd=pars$cistaplelwd, col=pars$cistaplecol)
        segments(1:length(data) - bxppars$boxwex / 2, stats$cimin, 1:length(data) + bxppars$boxwex / 2, stats$cimin, lend='butt', lty=pars$cistaplelty, lwd=pars$cistaplelwd, col=pars$cistaplecol)
        segments(1:length(data), stats$cimax, 1:length(data), stats$cimin, lend='butt', lty=pars$ciwhisklty, lwd=pars$ciwhisklwd, col=pars$ciwhiskcol)
    }
    invisible(toreturn)
}

threeparamsstats <- function(stats, features)
{
    bars <- list(m=NULL, u=NULL, l=NULL)
    if ("mean" %in% features)
        bars$m <- stats$means
    if ("median" %in% features)
        bars$m <- stats$medians
    if ("box" %in% features) {
        bars$u <- stats$boxmax
        bars$l <- stats$boxmin
    }
    if ("iqr" %in% features) {
        bars$u <- stats$iqrmax
        bars$l <- stats$iqrmin
    }
    if ("sd" %in% features) {
        bars$u <- bars$m + stats$sds
        bars$l <- bars$m - stats$sds
    }
    if ("sem" %in% features) {
        bars$u <- bars$m + stats$sems
        bars$l <- bars$m - stats$sems
    }
    if ("ci" %in% features) {
        bars$u <- stats$cimax
        bars$l <- stats$cimin
    }
    bars
}

threeparamcheck <- function(features)
{
    if ("mean" %in% features && "median" %in% features)
        stop("both mean and median present in features, only one can be plotted")
    if (as.integer("box" %in% features + "iqr" %in% features + "sd" %in% features + "sem" %in% features) > 1 + "ci" %in% features)
        stop("only one of 'box', 'iqr', 'sd', 'sem', 'ci' can be plotted, ajust features argument accordingly")
}

allfeatures <- c("median", "box", "iqr", "mean", "sd", "sem", "ci")

#' @templateVar plottype beeswarm plot
#' @templateVar additionaldesc Requires the \code{beeswarm} package.
#' @templateVar featuresdesc At the moment, either \code{mean} or \code{median} can be plotted. Also, only one of \code{box}, \code{iqr}, \code{sd}, \code{sem}, \code{ci} can be plotted at the moment.
#' @template plotgroups.-
#' @param palpha opacity of the individual points
#' @param bxplwd line width for the simplified boxplot
#' @param bxpcols colors for the simplified boxplot
#' @param ... additional parameters passed to \code{\link[beeswarm]{beeswarm}}
#' @return Same as \code{\link[beeswarm]{beeswarm}}
#' @seealso \code{\link[beeswarm]{beeswarm}}
#' @export
#' @importFrom rlist list.merge
plotgroups.beeswarm <- function(data, stats, colors, ylim, features, barwidth, palpha=1, bxplwd=par("lwd"), bxpcols=colors, ...)
{
    if (!requireNamespace("beeswarm", quietly = TRUE))
        stop("Please install the beeswarm package for this plot.")
    threeparamcheck(features)
    if (!missing(ylim))
        return(range(unlist(data)))

    library(beeswarm)
    dots <- list(...)
    pars <- list(method="swarm", corral="random", priority="random", pch=16)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)

    toreturn <- do.call(beeswarm::beeswarm, list.merge(pars, list(x=data, corralWidth=barwidth, add=TRUE, col=adjustcolor(colors, alpha.f=palpha), yaxs='i', xaxt='n')))

    bars <- threeparamsstats(stats, features)

    if (!is.null(bars$u) && !is.null(bars$l))
        segments(1:length(data), bars$l, 1:length(data), bars$u, col=bxpcols, lend='butt', lwd=bxplwd)

    for (b in bars) {
        if (!is.null(b))
            segments(1:length(data) - barwidth / 2, b, 1:length(data) + barwidth / 2, b, col=bxpcols, lend='butt', lwd=bxplwd)
    }
    invisible(toreturn)
}

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
plotgroups.barplot <- function(data, stats, colors, ylim, features, barwidth, whiskerswidth=barwidth, whiskerslwd=par("lwd"), whiskerscol="black", bordercol="black", ...)
{
    threeparamcheck(features)
    if (!missing(ylim))
        return(NULL)
    bars <- threeparamsstats(stats, features)
    dots <- list(...)
    pars <- list(names.arg=NULL)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(rect, list.merge(pars, list(xleft=1:length(data) - barwidth/2, ybottom=par("usr")[3], xright=1:length(data) + barwidth/2, ytop=bars$m, col=colors, border=bordercol)))

    if (!is.null(bars$u) && !is.null(bars$l))
        segments(1:length(data), bars$l, 1:length(data), bars$u, col=whiskerscol, lend='butt', lwd=whiskerslwd)

    for (b in bars[c("u", "l")]) {
        if (!is.null(b))
            segments(1:length(data) - whiskerswidth / 2, b, 1:length(data) + whiskerswidth / 2, b, col=whiskerscol, lend='butt', lwd=whiskerslwd)
    }
    invisible(NULL)
}

#' @templateVar plottype violin plot
#' @templateVar additionaldesc Requires the \code{vioplot} package.
#' @template plotgroups.-
#' @param boxpars parameters passed to \code{\link{plotgroups.boxplot}}
#' @param boxcol color of the boxes
#' @param boxwidth width of the boxes
#' @param ... addtional parameters passed to \code{\link[vioplot]{vioplot}}
#' @return List with the following components:
#'        \item{vioplot}{List containing the aggregated return values of \code{\link[vioplot]{vioplot}}}
#'        \item{boxplot}{Return value of \code{\link{plotgroups.boxplot}}}
#' @seealso \code{\link[vioplot]{vioplot}}
#' @export
#' @importFrom rlist list.merge
plotgroups.vioplot <- function(data, stats, colors, ylim, features, barwidth, boxpars, boxcol="white", boxwidth=barwidth/4, ...)
{
    if (!requireNamespace("vioplot", quietly = TRUE))
        stop("Please install the vioplot package for this plot.")
    if (!missing(ylim))
        return(range(unlist(data)))
    library("sm") # needed by vioplot
    colors <- rep_len(colors, length(data))
    dots <- list(...)
    pars <- list(drawRect=TRUE)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    vioplot.results <- mapply(function(data, color, i){
            vioplot.results <- do.call(vioplot::vioplot, list.merge(pars, list(x=data, col=color, at=i, add=TRUE, wex=barwidth, drawRect=FALSE)))
            vioplot.results
        }, data, colors, 1:length(data))
    vioplot.toreturn <- lapply(seq_len(nrow(vioplot.results)), function(i)unlist(vioplot.results[i,]))
    names(vioplot.toreturn) <- rownames(vioplot.results)
    if (missing(boxpars) || is.null(boxpars))
        boxpars <- list()
    if (is.null(boxpars$notch))
        boxpars$notch <- FALSE
    bxp.toreturn <- do.call(plotgroups.boxplot, list.merge(boxpars, list(data=data, stats=stats, colors=boxcol, features=features, barwidth=boxwidth)))
    invisible(list(vioplot=vioplot.toreturn, boxplot=bxp.toreturn))
}

plotgroups.ci <- function(data, mean, se, ndata, conf.level=0.95) {
    if (missing(data) && (missing(mean) || missing(se) || missing(ndata)))
        stop("need either the data set or mean and standard error estimates", call.=TRUE)
    if (!missing(data)) {
        mean <- mean(data)
        ndata <- length(data)
        se <- mean / sqrt(ndata)
    }
    Q <- qt(conf.level + (1 - conf.level) / 2, df=ndata - 1)
    c(mean - Q * se, mean + Q * se)
}

#' Plot several groups of repeated observations.
#'
#' Plot several groups of repeated observations, e.g. abundance/half-life of several
#' proteins each observed in several cell lines in several replicates. Observations can be grouped
#' either by protein (in which case cell lines will be annotated as X axis labels
#' and proteins above the plot) or by cell line. Related parameters can be plotted in separate plots
#' below each other, sharing the groupings and annotations (see examples)
#'
#' This is a wrapper function around \code{plot.fun}. It sets up the coordinate system, calls
#' \code{extrafun.before} followed by \code{plot.fun}, which does the actual plotting, and
#' \code{extrafun.after}. All three functions are passed the follwing arguments:
#' \describe{
#'           \item{data}{the \code{data} argument passed to \code{plotgroups}}
#'           \item{stats}{summary statistics of the data. List with the following components:
#'                        \describe{
#'                               \item{means}{means}
#'                               \item{sds}{standard deviations}
#'                               \item{sems}{standard errors of the mean}
#'                               \item{medians}{medians}
#'                               \item{boxmax}{third quartile}
#'                               \item{boxmin}{tirst quartile}
#'                               \item{iqrmax}{the maximal data point within \code{range} times
#'                                             the interquartile range of \code{boxmax}}
#'                               \item{iqrmin}{the minimal data point within \code{range} times
#'                                             the interquartile range of \code{boxmin}}
#'                               \item{cimax}{the upper confidence bound, computed by \code{ci.fun}
#'                                            according to \code{conf.level}}
#'                               \item{cimin}{the lower confidence bound, computed by \code{ci.fun}
#'                                            according to \code{conf.level}}
#'                               \item{range}{the range of the extreme data points within
#'                                            [\code{iqrmin}, \code{iqrmax}]}
#'                               \item{conf.level}{the confidence level at which \code{cimax},
#'                                                 \code{cimin} apply}}}
#'           \item{colors}{the \code{colors} argument passed to \code{plotgroups}}
#'           \item{features}{the \code{features} argument passed to \code{plotgroups}}
#'           \item{barwidth}{the \code{barwidth} argument passed to \code{plotgroups}}}
#' \code{plot.fun} is additionally passed the arguments given by \code{plot.fun.pars}.
#'
#' Significance testing is performed by calling \code{signif.test.fun} with two vector arguments
#' containing the samples to be compared. \code{signif.test.fun} must return a list containing
#' a \code{p.value} element. The p value is passed as single argument to \code{signif.test.text}
#' which returns a character vector (or anything usable by \code{\link[graphix]{text}}).
#'
#' @param data list, each element is a vector of replicates for one combination of parameters, or
#'             each element is a list containing a vector of replicates, in which case the data sets
#'             will be plotted below each other in separate plots
#' @param names character vector of X axis labels
#' @param colors colors for plotting
#' @param legend.text character vector of the same length as \code{data} giving the group names.
#'        A group of observations is identified by consecutive occurrence of the same name.
#' @param legend.col colors for group annotations. Defaults to plotting colors
#' @param legend.pars parameters for group annotation. Will be passed to \code{\link[base]{text}}
#' @param legend.lwd line width for grouping annotations. Defaults to \code{par("lwd")}
#' @param names.split character by which to split the \code{names}. Only useful in combination with
#'        \code{names.italicize} or \code{names.style='combinatorial'}
#' @param names.italicize if a part of a \code{name} is to be written in italic text, the part is
#'        identified by this character. I.e. The name is first split by \code{names.split}, each
#'        fragment containing \code{names.italicize} is rendered in italics
#' @param names.style how the \code{names} are to be rendered.
#'        \describe{
#'              \item{plain}{each name will be written as-is below the plot}
#'              \item{combinatorial}{names will be split by \code{names.split}, unique strings will be
#'                    printed at the bottom-left, and observations whose name contains the string will
#'                    be identified by prining \code{names.pch} below the respective bar. Useful if e.g.
#'                   assaying different combinations of single/double/triple knock-outs.}
#'                   }
#' @param names.adj text adjustment for \code{names} or \code{names.pch}, depending on
#'                  \code{names.style}.See \code{\link[base]{text}}. Defaults to 1 for
#'                  \code{names.style = 'plain'}, unless \code{names.rotate = 0}, in which case it
#'                  defaults to 0.5. Defaults to 0.5 for \code{names.style='combinatorial'}.
#' @param names.pch character to be used for annotation of observations when
#'        \code{names.style='combinatorial'}
#' @param names.pch.cex character expansion factor for \code{names.pch}
#' @param names.margin spacing between the bottom edge of the plot and the annotation, in inches
#' @param names.rotate only used when \code{names.style='plain'}. Degrees by which to rotate the
#'        annotation strings.
#' @param features which features of the sample distributions to plot. Availability of features
#'        depends on \code{plot.fun} Can contain any combination of the following:
#'        \describe{
#'                  \item{median}{the median}
#'                  \item{box}{the first and third quartiles}
#'                  \item{iqr}{the most extreme data point no more than \code{range} times the
#'                             interquartile range away from the \code{box}}
#'                  \item{mean}{the mean}
#'                  \item{sd}{mean \eqn{\pm} standard deviation}
#'                  \item{sem}{mean \eqn{\pm} standard error of the mean}}
#'        Can be a list containing character vectors, in which case the specified feature set will
#'        apply to the corresponding plot if multiple data sets are plotted (see examples). Will be
#'        recycled to the number of plots.
#' @param range determines how far the the \code{iqr} whiskers will extend out from the box,
#'        if they are to be plotted. Will be recycled to the number of plots.
#' @param conf.level Confidence level for plotting of confidence intervals. Will be recycled to the
#'        number of plots
#' @param ci.fun Function to compute confidence intervals. Will be recycled to the number of plots.
#'        Must accept five arguments:
#'        \describe{
#'                  \item{data}{Numeric vector containing data for one group}
#'                  \item{mean}{Precomputed mean of the sample}
#'                  \item{se}{Precomputed standard error of the mean of the sample}
#'                  \item{ndata}{Number of observations}
#'                  \item{conf.level}{Confidence level}}
#'        If \code{data} is given, \code{mean}, \code{se}, and \code{ndata} are not used,
#'        but calculated from the data. If \code{data} is omitted, all of \code{mean},
#'        \code{se}, and \code{ndata} must be given. Defaults to \code{plotgroups.ci}, which computes
#'        confidence intervals using the t statistics. Must return a numeric vector of length 2,
#'        containing the lower and upper confidence bounds.
#' @param cex.xlab character expansion factor for X axis annotation
#' @param ylim Y axis limits. Will be determined automatically if \code{NULL}. If not \code{NULL} but
#'        only one limit is finite, the other will be determined automatically. Can be a list containing
#'        numeric vectors, in which case the limits will apply to the corresponding plot if multiple
#'        data sets are plotted. Will be recycled to the number of plots.
#' @param legendmargin spacing between the upper-most data point/feature and the upper edge of the
#'        plot, required for group annotation. Will be determined automatically if \code{NULL}
#' @param plot.fun function to do the actual plotting. See \code{\link{plotgroups.boxplot}},
#'        \code{\link{plotgroups.beeswarm}}, \code{\link{plotgroups.barplot}},
#'        \code{\link{plotgroups.vioplot}}. Can be a list containing functions, in which case the
#'        functions will apply to the corresponding plot.
#' @param plot.fun.pars additional parameters to pass to \code{plot.fun}
#' @param barwidth width of the individual bars/boxes etc. as fraction of 1
#' @param main main title
#' @param ylab Y axis label. Will be recycled to the number of plots.
#' @param signif.test list of 2-element integer vectors giving the elements of \code{data} to be
#'        tested for significant differences. Can be a list of lists, in which case each element
#'        will apply to the corresponding plot if multiple data sets are plotted.
#' @param signif.test.fun function to perform the significance testing. Must accept 2 vectors and
#'        return a list containing at least the element \code{p.value}. Can be a list of functions,
#'        in which case each element will apply to the corresponding plot if multiple data sets
#'        are plotted.
#' @param signif.test.text function accepting a p-value and returning a formatted string to be used
#'        for plotting or \code{NULL} if this p-value is not to be plotted (e.g. if it is not
#'        significant). Can be a list of functions, in which case each element will apply to the
#'        corresponding plot if multiple data sets are plotted.
#' @param signif.test.lwd line width for p-value annotations. Can be a list, in which case the lwd
#'        will apply to the corresponding plot if multiple data sets are plotted.
#' @param signif.test.pars parameters for group annotation. Will be passed to \code{\link[base]{text}}.
#'        Can be a list of lists, in which case each element will apply to the corresponding
#'        plot if multiple data sets are plotted.
#' @param extrafun.before additional function to call after the coordinate system has been set up, but
#'        before plotting, e.g. to add a background grid to the plot. Can be a list of functions, in
#'        which case each element will apply to the corresponding plot if multiple data sets are
#'        plotted.
#' @param extrafun.after additional function to call after plotting, e.g. to add additional elements
#'        to the plot. Can be a list of functions, in which case each element will apply to the
#'        corresponding plot if multiple data sets are plotted.
#' @param ... additional parameters passed to \code{\link[base]{par}}
#' @return list with the following components:
#'         \item{stats}{summary statistics of the data.}
#'         \item{plotfun}{Return value of \code{plot.fun}}
#'         If significance testing was performed, also contains a component \code{signiftest}, which
#'         is a list with elements ordered by \code{signif.test} with the following components:
#'         \item{test}{return value of the testing function}
#'         \item{label}{return value of \code{signif.test.text}}
#' @examples
#' data <- list()
#' for (i in 1:14) data[[i]] <- rnorm(50, i, 0.5)
#' names <- rep(c('gene1', 'gene2', 'gene3', 'gene1 gene2', 'gene1 gene3', 'gene2 gene3', 'gene1 gene2 gene3'),
#'      times=2)
#' names2 <- as.character(rep(1:7,times=2))
#' names2[2] <- "abc\nefg"
#' colors <- c("green", "blue")
#' legend.text <- rep(c("protein1", "protein2"), each=7)
#' plotgroups(data, names, colors, legend.text,
#'            plot.fun=plotgroups.beeswarm, features=c('mean', 'sd'), ylim=c(0,Inf))
#' plotgroups(data, names2, colors, legend.text,plot.fun=plotgroups.vioplot, ylim=c(0,Inf),
#'            names.rotate=0, names.adj=c(0.5, 1))
#' plotgroups(data, names, colors, legend.text,
#'            plot.fun=plotgroups.beeswarm, features=c('mean', 'sd'),
#'            names.style='combinatorial', names.split=" ", names.pch='\u0394',
#'            plot.fun.pars=list(palpha=0.5, bxpcols="black"))
#' plotgroups(data, names, colors, legend.text,
#'            names.style='combinatorial', names.split=" ", names.pch='\u0394')
#' plotgroups(data, names, colors, legend.text,
#'            names.style='combinatorial', names.split=" ", names.pch=19,
#'            main="test", plot.fun=plotgroups.barplot, features=c("mean", "sd"),
#'            plot.fun.pars=list(whiskerswidth=0.6))
#'
#' names.pch <- rep('\u0394', 24)
#' names.adj <- names.adj <- rep(list(c(0.5, 0)), 24)
#' names.rotate <- rep(0, 24)
#' names.pch[2] <- 'S158T'
#' names.adj[[2]] <- c(0, 0.5)
#' names.rotate[2] <- 90
#' plotgroups(data, names, colors, legend.text,names.style='combinatorial', names.split=" ",
#'            names.pch=names.pch, names.rotate=names.rotate, names.adj=names.adj)
#' ## significance testing
#' plotgroups(data, names, colors, legend.text,names.style='combinatorial',
#'            names.split=" ", names.pch='\u0394',
#'            signif.test=list(c(1,3), c(2,5), c(5,8), c(3,10)))
#' plotgroups(data, names, colors, legend.text,names.style='combinatorial',
#'            names.split=" ", names.pch='\u0394',
#'            signif.test=list(c(1,3), c(2,5), c(5,8), c(3,10)),
#'            signif.test.text=function(p) {
#'                      if (p < 0.001) {
#'                          return('***')
#'                      } else if (p < 0.01) {
#'                          return('**')
#'                      } else if (p < 0.05) {
#'                          return('*')
#'                      } else {
#'                          return(NULL)
#'                      }})
#' ## multiple plots
#' plotgroups(list(data, rev(data)), names, colors, legend.text,names.style='combinatorial',
#'            names.split=" ",names.pch=names.pch, names.rotate=names.rotate, names.adj=names.adj,
#'            ylim=c(0,Inf), ylab=c("data1", "data2"), main="test", features=list(NULL,
#'            c("median", "box")), plot.fun=list(plotgroups.boxplot, plotgroups.beeswarm),
#'            signif.test=list(NULL,list(c(1,3), c(2,5), c(5,8), c(3,10))))
#' @export
#' @importFrom rlist list.merge
plotgroups <- function(
                        data,
                        names,
                        colors=NULL,
                        legend.text=NULL,
                        legend.col=NULL,
                        legend.pars=list(),
                        legend.lwd=NULL,
                        names.split=NULL,
                        names.italicize=NULL,
                        names.style=c("plain", "combinatorial"),
                        names.pch.cex=1,
                        names.pch=19,
                        names.adj=NA,
                        names.margin=0.5,
                        names.rotate=NULL,
                        features=NULL,
                        range=1.5,
                        conf.level=0.95,
                        ci.fun=plotgroups.ci,
                        cex.xlab=1,
                        ylim=NULL,
                        legendmargin=NULL,
                        plot.fun=plotgroups.boxplot,
                        plot.fun.pars=list(),
                        barwidth=0.8,
                        main=NULL,
                        ylab=NULL,
                        signif.test=NULL,
                        signif.test.fun=t.test,
                        signif.test.text=function(p)paste0("p=", formatC(p, digits=3, format="g")),
                        signif.test.col="black",
                        signif.test.lwd=legend.lwd,
                        signif.test.pars=legend.pars,
                        extrafun.before=NULL,
                        extrafun.after=NULL,
                        ...)
{
    names.style <- match.arg(names.style)
    dots <- list(...)

    stopifnot(is.list(data))
    stopifnot(is.list(plot.fun.pars))
    if (length(data) != length(names)) {
        if (all(sapply(data, is.list))) {
            nplots <- length(data)
            if (is.null(ylab)) {
                if (!is.null(names(data))) {
                    ylab <- names(data)
                } else {
                    ylab <- deparse(substitute(data))
                }
            }
        } else {
            stop("")
        }
    } else {
        if (is.null(ylab))
            ylab <- deparse(substitute(data))
        data <- list(data)
        nplots <- 1
    }
    ngroups <- length(names)
    cenv <- environment()
    for (arg in c("range", "conf.level", "ylab")) {
        cenv[[arg]] <- rep(cenv[[arg]], length.out=nplots)
    }
    for (arg in c("features", "ylim", "plot.fun", "signif.test.fun", "ci.fun", "signif.test.text", "signif.test.col", "signif.test.lwd", "extrafun.before", "extrafun.after")) {
        if (!is.list(cenv[[arg]])) {
            cenv[[arg]] <- rep(list(cenv[[arg]]), nplots)
        } else {
            cenv[[arg]] <- rep(cenv[[arg]], length.out=nplots)
        }
    }
#     for (arg in names(dots)) {
#         if (!is.list(arg)) {
#             dots[[arg]] <- rep(list(arg), nplots)
#         } else {
#             dots[[arg]] <- rep(arg, length.out=nplots)
#         }
#     }
    for (arg in c("plot.fun.pars", "signif.test.pars", "signif.test")) {
        if (!is.null(cenv[[arg]]) && length(cenv[[arg]]) && !all(sapply(cenv[[arg]], function(x)is.list(x) || is.null(x)))) {
            cenv[[arg]] <- rep(list(cenv[[arg]]), nplots)
        } else if (!is.null(cenv[[arg]]) && length(cenv[[arg]]) != nplots) {
            cenv[[arg]] <- rep(cenv[[arg]], length.out=nplots)
        }
    }
    pars <- list(oma=c(0,0,0,0), mar=c(0, 3, 0.2, 0.2), las=1, mgp=c(2, 0.5, 0), ljoin="mitre", lend="square", lwd=2)
    if (!is.null(main))
        pars$oma <- c(0, 0, 2, 0)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(par, pars)
    plot.new() # have to do this here, otherwise strwidth on cairo devices will crash
    lwd.base <- par("lwd")
    if (is.null(legend.lwd))
        legend.lwd <- lwd.base

    lheight <- par("lheight")
    origcex <- par("cex") #reset by layout()
    par(lheight=1)
    lineheight <- strheight("\n", units="inches", cex=cex.xlab)
    mai <- par("mai")
    if (names.style=="combinatorial") {
        if (is.null(names.rotate))
            names.rotate <- 0
        if (is.na(names.adj) || is.null(names.adj))
            names.adj <- 0.5
        if (!is.null(names.split)) {
            uniquegenes <- unique(unlist(strsplit(names, names.split, fixed=TRUE)))
        } else {
            uniquegenes <- unique(names)
        }

        uniquegenes <- uniquegenes[nchar(uniquegenes) > 0]
        uniquegenes <- uniquegenes[order(uniquegenes, decreasing=TRUE)]

        if (!is.null(names.split)) {
            npoints <- sum(sapply(strsplit(names, names.split, fixed=TRUE), function(x)length(x)))
        } else {
            npoints <- sum(length(names))
        }
        pch <- rep(names.pch, length.out=npoints)
        cex <- rep(names.pch.cex, length.out=npoints) * cex.xlab
        rotate <- rep(names.rotate, length.out=npoints)
        adj <- rep(names.adj, length.out=npoints)

        imat <- matrix(nrow=length(uniquegenes), ncol=length(names))
        currlength <- 0
        for (i in 1:length(names)) {
            if (nchar(names[i]) > 0) {
                if (!is.null(names.split)) {
                    genes <- strsplit(names[i], names.split, fixed=TRUE)[[1]]
                } else {
                    genes <- names[i]
                }
                newlength <- currlength + length(genes)
                currgenes <- which(uniquegenes %in% genes)
                for (j in 1:length(genes)) {
                    imat[currgenes[j],i] <- currlength + j
                }
                currlength <- newlength
            }
        }

        if (!is.list(adj))
           adj <- sapply(adj, function(x)c(x, 0), simplify=FALSE)
        heights <- apply(imat, 1, function(x) {
            x <- x[!is.na(x)]
            max(mapply(function(pch, cex, rotate, adj) {
                if (!is.character(pch)) {
                    lineheight
                } else {
                    height <- sin(rotate * pi / 180) * strwidth(pch, units="inches", cex=cex) + 0.5 * lineheight
                    if (rotate != 90)
                        height <- height + adj[1] * strheight(pch, units="inches", cex=cex)
                    height
                }
            }, pch[x], cex[x], rotate[x], adj[x], SIMPLIFY=TRUE), lineheight)
        })
        hfracs <- heights / sum(heights)
        hfracscs <- c(0,cumsum(hfracs))[1:length(hfracs)]
        ycoords <- hfracscs +  0.25 * min(hfracs)

        legend.height <- max(sum(heights), strheight(paste0(uniquegenes, collapse="\n"), units="inches", cex=cex.xlab))
        legend.width <- max(strwidth(uniquegenes, units="inches"))

        layout(matrix(c(2:(nplots + 1), 1), byrow=TRUE), heights=c(rep(1, nplots), lcm(cm(legend.height))))
        par(cex=origcex)

        mai[2] <- max(mai[2], legend.width)
        par(mai=c(0, mai[2], 0, mai[4]))
        plot.new()
        plot.window(xlim=c(0.5, ngroups + 0.5), ylim=c(0,1), xaxs='i', yaxs='i')

        mapply(function(x, y) {
            i <- imat[y,x]
            if (!is.na(i)) {
                if (is.character(pch[i])) {
                    pfun <- text
                    parg <- "labels"
                    cadj <- adj[[i]]
                } else {
                    pfun <- points
                    parg <- "pch"
                    cadj <- adj[[i]][1]
                }
                args <- list(x=x, y=ycoords[y], adj=cadj, cex=cex[i], srt=rotate[i], xpd=TRUE)
                args[[parg]] <- pch[i]
                do.call(pfun, args)
            }
        }, rep(1:length(names), times=length(uniquegenes)), rep(1:length(uniquegenes), each=length(names)))

        labels <- uniquegenes
        if (!is.null(names.italicize)) {
            if (!is.na(names.italicize)) {
                labels <- sapply(labels, function(x) {if (grepl(names.italicize, x, fixed=TRUE)) {x <- as.expression(substitute(italic(x), list(x=x)))}; x}, USE.NAMES=FALSE)
            } else {
                labels <- sapply(labels, function(x)as.expression(substitute(italic(x), list(x=x))), USE.NAMES=FALSE)
            }
        }
        text(0.5, ycoords, labels=labels, adj=c(1, 0), cex=cex.xlab, xpd=NA)

        plt <- par("plt")
        plt[1] <- plt[1] - max(strwidth(uniquegenes, units="figure", cex=cex.xlab))
        par(plt=plt)
        plot.window(xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
        do.call("clip", as.list(par("usr")))
        sapply(hfracscs[2:length(hfracscs - 1)], function(x)abline(h=x, lwd=lwd.base))
    } else {
        if (is.null(names.rotate))
            names.rotate <- 45
        if (is.na(names.adj) || is.null(names.adj)) {
            if (names.rotate == 0) {
                names.adj <- 0.5
            } else {
                names.adj <- 1
            }
        }
        labels <- names
        if (!is.null(names.italicize)) {
            if (!is.null(names.split)) {
                .split <- function(x){strsplit(x, names.split, fixed=TRUE)}
            } else {
                .split <- function(x){x}
            }
            labels <- sapply(labels, function(x){
                    xx <- .split(x)
                    parse(text=paste0("paste(", paste(sapply(xx[[1]], function(x) {
                            if (grepl(names.italicize, x, fixed=TRUE)) {
                                x <- deparse(substitute(italic(x), list(x=x)))
                            }
                            x
                        }, USE.NAMES=F), collapse='," ",'), ")"))
                }, USE.NAMES=F)
        }
        if (length(names.adj) == 2) {
            vadj <- names.adj[2]
        } else {
            vadj <- names.adj
        }
        legend.width <- max(strwidth(labels, units="inches", cex=cex.xlab))
        legend.height <- sin(names.rotate * pi / 180) * legend.width + vadj * max(strheight(labels, units="inches", cex=cex.xlab)) + 0.15 * lineheight
        layout(matrix(c(2:(nplots + 1), 1), byrow=TRUE), heights=c(rep(1, nplots), lcm(cm(legend.height))))
        par(cex=origcex)
        par(mai=c(0, mai[2], 0, mai[4]))
        plot.new()
        plot.window(xlim=c(0.5, ngroups + 0.5), ylim=c(0,1), xaxs='i', yaxs='i')
        text(1:ngroups, 1, srt=names.rotate, adj=names.adj, labels=labels, xpd=NA, cex=cex.xlab)
    }
    mai[1] <- names.margin * lineheight
    title(main=main, outer=TRUE)

    allstats <- list()
    allplotfunrets <- list()
    allsigniftestrets <- list()

    for (cplot in 1:nplots) {
        if (!is.null(features[[cplot]]) && length(features[[cplot]]) > 0) {
            features[[cplot]] <- match.arg(features[[cplot]], choices=allfeatures, several.ok=TRUE)
        } else {
            features[[cplot]] <- c("median", "box", "iqr", "mean", "sd", "ci")
        }

        cmai <- mai
        if (cplot > 1 && cplot < nplots) {
            cmai[c(1,3)] <- 0
        } else if (cplot == 1 && cplot < nplots) {
            cmai[1] <- 0
        } else if (cplot == nplots && cplot > 1) {
            cmai[3] <- 0
        }
        par(mai=cmai)
        plot.new()
        emptyvec <- vector("numeric", length=ngroups)
        stats <- list(means=emptyvec, sds=emptyvec, sems=emptyvec, medians=emptyvec, boxmax=emptyvec, iqrmax=emptyvec, boxmin=emptyvec, iqrmin=emptyvec, cimin=emptyvec, cimax=emptyvec, range=range[cplot], conf.level=conf.level[cplot])
        for (i in 1:ngroups) {
            ndata <- length(data[[cplot]][[i]])
            stats$means[i] <- mean(data[[cplot]][[i]])
            stats$sds[i] <- sd(data[[cplot]][[i]])
            stats$sems[i] <- stats$sds[i] / sqrt(ndata)
            ci <- ci.fun[[cplot]](mean=stats$means[i], se=stats$se[i], ndata=ndata, conf.level=conf.level[cplot])
            stats$cimin[i] <- ci[1]
            stats$cimax[i] <- ci[2]
            bstats <- boxplot.stats(data[[cplot]][[i]], coef=range[cplot], do.conf=F, do.out=F)$stats
            stats$medians[i] <- bstats[3]
            stats$boxmax[i] <- bstats[4]
            stats$iqrmax[i] <- bstats[5]
            stats$boxmin[i] <- bstats[2]
            stats$iqrmin[i] <- bstats[1]
        }
        ylim.usr <- NULL
        cylim <- ylim[[cplot]]
        if (!is.null(cylim) && (!is.finite(cylim[1]) || !is.finite(cylim[2]))) {
            ylim.usr <- cylim
            cylim <- NULL
        }
        if (is.null(cylim)) {
            cylim <- plot.fun[[cplot]](data=data[[cplot]], features=features[[cplot]], ylim=TRUE)
            if (!is.null(cylim) && all(is.finite(cylim)))
                cylim <- extendrange(cylim, f=0.04)
        }
        if (is.null(cylim)) {
            cylim <- c(Inf, 0)
            if ("median" %in% features[[cplot]]) {
                cylim[1] <- min(cylim[1], stats$medians, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$medians, na.rm=TRUE)
            }
            if ("box" %in% features[[cplot]]) {
                cylim[1] <- min(cylim[1], stats$boxmin, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$boxmax, na.rm=TRUE)
            }
            if ("iqr" %in% features[[cplot]]) {
                cylim[1] <- min(cylim[1], stats$iqrmin, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$iqrmax, na.rm=TRUE)
            }
            if ("mean" %in% features[[cplot]]) {
                cylim[1] <- min(cylim[1], stats$means, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$means, na.rm=TRUE)
            }
            if ("sd" %in% features[[cplot]]) {
                cylim[1] <- min(cylim[1], stats$means - stats$sds, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$means + stats$sds, na.rm=TRUE)
            }
            if ("sem" %in% features[[cplot]]) {
                cylim[1] <- min(cylim[1], stats$means - stats$sems, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$means + stats$sems, na.rm=TRUE)
            }
            if ("ci" %in% features[[cplot]]) {
                cylim[1] <- min(cylim[1], stats$means - stats$ci, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$means + stats$ci, na.rm=TRUE)
            }
            if (!is.null(cylim) && all(is.finite(cylim)))
                cylim <- extendrange(cylim, f=0.04)
        }

        if (!is.null(ylim.usr)) {
            if (is.finite(ylim.usr[1]))
                cylim[1] <- ylim.usr[1]
            if (is.finite(ylim.usr[2]))
                cylim[2] <- ylim.usr[2]
        }

        if (is.null(colors))
            colors <- "grey"
        # can't use grconvertY here because plot.window has not been called yet, have
        # to do the conversion manually
        inchestouser <- (cylim[2] - cylim[1]) / par("pin")[2]
        lineheight <- strheight("\n", units="inches") * inchestouser
        signifheight <- 0.3 * lineheight
        legendbase <- cylim[2]
        if (cplot == 1 && !is.null(legend.text)) {
            grouplength <- rle(legend.text)$lengths
            if (length(colors) != ngroups) {
                colors <- rep(colors, length.out=length(grouplength))
                colors <- rep(colors, times=grouplength)
            }
            if (is.null(legendmargin))
                legendmargin <- max(max(strheight(legend.text, units="inches")) * inchestouser, lineheight)
        } else {
            legendmargin <- 0
        }
        signifmargin <- 0
        if (!is.null(signif.test[[cplot]])) {
            if (!requireNamespace("IRanges", quietly = TRUE))
                stop("Please install the IRanges package if significance testing is to be performed.")
            intervals.start <- sapply(signif.test[[cplot]], function(x)min(x))
            intervals.stop <- sapply(signif.test[[cplot]], function(x)max(x))
            intervals.order <- order(intervals.start, intervals.stop)
            signif.test[[cplot]] <- signif.test[[cplot]][intervals.order]
            query <- IRanges::IRanges(intervals.start[intervals.order], intervals.stop[intervals.order])
            signifoverlaps <- S4Vectors::as.matrix(IRanges::findOverlaps(query, minoverlap=2))
            signifoverlaps <- signifoverlaps[which(signifoverlaps[,1] != signifoverlaps[,2]),]
            if (nrow(signifoverlaps)) {
                signifoverlaps <- t(apply(signifoverlaps, 1, function(x)c(min(x),max(x))))
                signifoverlaps <- signifoverlaps[!duplicated(signifoverlaps),]

                maxsignifoverlaps <- max(rle(signifoverlaps[,1])$length)
            } else {
                maxsignifoverlaps <- 0
            }
            signifmargin <- (maxsignifoverlaps + 1) * lineheight + signifheight
            signifbase <- legendbase + signifheight
            legendbase <- signifbase + signifmargin

            if (maxsignifoverlaps == 0) {
                signiflines <- rep(0, length(signif.test[[cplot]]))
            } else {
                signiflines <- rep(-1, length(signif.test[[cplot]]))
                currline <- 0
                while (currline <= maxsignifoverlaps) {
                    i <- which(signiflines == -1)[1]
                    while (i <= length(signif.test[[cplot]])) {
                        signiflines[i] <- currline
                        overlaps <- which(signifoverlaps[,1] == i)
                        if (length(overlaps) > 0) {
                            i <- max(signifoverlaps[overlaps,]) + 1
                        } else {
                            i <- i + 1
                        }
                    }
                    currline <- currline + 1
                }
            }
        }

        plot.window(xlim=c(0.5, ngroups + 0.5), ylim=c(cylim[1], cylim[2] + legendmargin + signifmargin), xaxs='i', yaxs='i')

        if (!is.null(extrafun.before[[cplot]]))
            extrafun.before[[cplot]](data[[cplot]], stats, colors, features, barwidth)
        plotfunret <- do.call(plot.fun[[cplot]], c(list(data=data[[cplot]], stats=stats, colors=colors, features=features[[cplot]], barwidth=barwidth), plot.fun.pars[[cplot]]))
        if (!is.null(extrafun.after[[cplot]]))
            extrafun.after[[cplot]](data[[cplot]], stats, colors, features, barwidth)

        # try to avoid axis ticks to close to the upper edge
        # probably need better logic here (what about the lower edge? current assumption is
        # we ignore it because we have space due to names and names.margin. Just skipping the upper
        # tick also works for multiple plots
        ticks <- axTicks(side=2)
        lticks <- length(ticks)
        if (ticks[lticks] > cylim[2] - lineheight)
            ticks <- ticks[-lticks]
        do.call(axis, list.merge(pars, list(side=2, at=ticks)))
        title(ylab=ylab[cplot])
        do.call(box, pars)

        if (cplot == 1 && !is.null(legend.text)) {
            segs.begin <- c(1, cumsum(grouplength)[-length(grouplength)] + 1) - barwidth / 2
            segs.end <- cumsum(grouplength) + barwidth / 2
            if (is.null(legend.col)) {
                cols <- unique(colors)
            } else {
                cols <- legend.col
            }
            segments(segs.begin, legendbase, segs.end, legendbase, lwd=legend.lwd, col=cols, lend="butt")

            mids <- (segs.end - segs.begin) / 2 + segs.begin
            do.call(text, c(list(x=mids, y=legendbase + 0.2 * lineheight, labels=unique(legend.text), adj=c(0.5, 0), col=cols), legend.pars))
        }
        if (!is.null(signif.test[[cplot]])) {
            signif.test.ret <- vector("list", length(signif.test[[cplot]]))
            for (i in 1:length(signif.test[[cplot]])) {
                signif.test.ret[[i]]$test <- signif.test.fun[[cplot]](data[[cplot]][[signif.test[[cplot]][[i]][1]]], data[[cplot]][[signif.test[[cplot]][[i]][2]]])
                p <- signif.test.ret[[i]]$test$p.value
                label <- signif.test.text[[cplot]](p)
                if (!is.null(label)) {
                    begin <- signif.test[[cplot]][[i]][1] + (1 - barwidth) / 2
                    end <- signif.test[[cplot]][[i]][2] - (1 - barwidth) / 2
                    mid <- (end - begin) / 2 + begin
                    base <- signifbase + signiflines[i] * lineheight
                    lines(c(begin, begin, end, end), c(base - signifheight, base, base, base - signifheight), lwd=signif.test.lwd[[cplot]], col=signif.test.col[[cplot]], lend="butt")
                    do.call(text, c(list(x=mid, y=base + 0.2 * lineheight, labels=label, adj=c(0.5, 0), col=signif.test.col[[cplot]]), signif.test.pars[[cplot]]))
                }
                signif.test.ret[[i]]$label <- label
            }
            allsigniftestrets[[cplot]] <- signif.test.ret
        }
        allstats[[cplot]] <- stats
        allplotfunrets <- plotfunret
    }

#     if (!is.null(labels) && names.style=="plain") {
#         text(1:length(data), grconvertY(grconvertY(par("usr")[3], from="user", to="inches") - names.margin, from="inches", to="user"), srt=names.rotate, adj=names.adj, labels=labels, xpd=TRUE, cex=cex.xlab)
#     }
    toreturn <- list(stats=allstats, plotfun=allplotfunrets, signiftest=allsigniftestrets)
    invisible(toreturn)
}
