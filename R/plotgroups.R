allfeatures <- c("median", "box", "iqr", "mean", "sd", "sem", "ci")

threeparamcheck <- function(features)
{
    if (is.na(features) || !length(features))
        features <- c('mean', 'ci')
    if ("mean" %in% features && "median" %in% features)
        stop("both mean and median present in features, only one can be plotted")
    if (as.integer("box" %in% features + "iqr" %in% features + "sd" %in% features + "sem" %in% features) > 1 + "ci" %in% features)
        stop("only one of 'box', 'iqr', 'sd', 'sem', 'ci' can be plotted, ajust features argument accordingly")
    features
}

allparamcheck <- function(features)
{
    if (!is.na(features) && length(features) > 0) {
        features <- match.arg(features, choices=allfeatures, several.ok=TRUE)
    } else {
        features <- c("median", "box", "iqr", "mean", "sd", "ci")
    }
    features
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

#' @importFrom stats qt
plotgroups.ci <- function(data, mean, se, ndata, conf.level=0.95) {
    if (missing(data) && (missing(mean) || missing(se) || missing(ndata)))
        stop("need either the data set or mean and standard error estimates", call.=TRUE)
    if (!missing(data)) {
        mean <- mean(data)
        ndata <- length(data)
        se <- sd(data) / sqrt(ndata)
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
#' This is a wrapper function around \code{plot.type$plot}. It sets up the coordinate system, calls
#' \code{extrafun.before} followed by \code{plot.type$plot}, which does the actual plotting, and
#' \code{extrafun.after}. All three functions are passed the following arguments:
#' \describe{
#'           \item{data}{the \code{data} argument passed to \code{plotgroups}}
#'           \item{at}{X coordinates of the data. Particularly important when groups.spacing != 0}
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
#' \code{plot.type$plot} is additionally passed the arguments given by \code{plot.fun.pars}.
#'
#' Significance testing is performed by calling \code{signif.test.fun} with two vector arguments
#' containing the samples to be compared. \code{signif.test.fun} must return a list containing
#' a \code{p.value} element. The p value is passed as single argument to \code{signif.test.text}
#' which returns a character vector (or anything usable by \code{\link[graphics]{text}}).
#'
#' @param data list, each element is a vector of replicates for one combination of parameters, or
#'             each element is a list containing a vector of replicates, in which case the data sets
#'             will be plotted below each other in separate plots
#' @param names character vector of X axis labels
#' @param colors colors for plotting
#' @param legend.text character vector of the same length as \code{data} giving the group names.
#'        A group of observations is identified by consecutive occurrence of the same name.
#' @param legend.col colors for group annotations. Defaults to plotting colors
#' @param legend.pars parameters for group annotation. Will be passed to \code{\link[graphics]{text}}
#' @param legend.lwd line width for grouping annotations. Defaults to \code{par("lwd")}
#' @param groups.spacing extra space between the groups in user coordinates.
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
#'                    be identified by printing \code{names.pch} below the respective bar. Useful if e.g.
#'                   assaying different combinations of single/double/triple knock-outs.}
#'                   }
#' @param names.adj text adjustment for \code{names} or \code{names.pch}, depending on
#'                  \code{names.style}.See \code{\link[graphics]{text}}. Defaults to 1 for
#'                  \code{names.style = 'plain'}, unless \code{names.rotate = 0}, in which case it
#'                  defaults to 0.5. Defaults to 0.5 for \code{names.style='combinatorial'}.
#' @param names.pch character to be used for annotation of observations when
#'        \code{names.style='combinatorial'}
#' @param names.pch.cex character expansion factor for \code{names.pch}
#' @param names.margin spacing between the bottom edge of the plot and the annotation, in inches
#' @param names.rotate Degrees by which to rotate the annotation strings.
#' @param names.placeholder Only used when \code{names.style='combinatorial'}. Placeholder character
#'        to use when no annotation is present for the current sample and row. See examples.
#' @param names.map.fun Function mapping between names string and pch/cex/adj/rotate for the respective
#'        combination. Useful for more complicated experimental layouts where different names.pch
#'        must be used for different genes, see examples. Must accept six arguments:
#'        \describe{
#'                  \item{n}{String with the names combination to process}
#'                  \item{split}{Default pattern to split by, as given by \code{names.split}}
#'                  \item{pch}{Default pch, as given by \code{names.pch}}
#'                  \item{cex}{Default cex, as given by \code{names.pch.cex}}
#'                  \item{rotate}{Default rotate, as given by \code{names.rotate}}
#'                  \item{adj}{Default adj, as given by \code{names.adj}}}
#'        Must return a named list, with the names being the split genes that should be used to
#'        label the rows, each element being itself a named list containing the plotting
#'        parameters for that particular annotation, i.e. \code{pch}, \code{cex}, \code{rotate},
#'        \code{adj}.
#' @param features which features of the sample distributions to plot. Availability of features
#'        depends on \code{plot.type} Can contain any combination of the following:
#'        \describe{
#'                  \item{median}{the median}
#'                  \item{box}{the first and third quartiles}
#'                  \item{iqr}{the most extreme data point no more than \code{range} times the
#'                             interquartile range away from the \code{box}}
#'                  \item{mean}{the mean}
#'                  \item{sd}{mean \eqn{\pm} standard deviation}
#'                  \item{sem}{mean \eqn{\pm} standard error of the mean}
#'                  \item{ci}{confidence interval at \code{conf.level}}}
#'        Can be a list containing character vectors, in which case the specified feature set will
#'        apply to the corresponding plot if multiple data sets are plotted (see examples). Will be
#'        recycled to the number of plots.
#' @param log Whether to plot the Y axis on log scale
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
#' @param plot.type list containint three functions:
#'        \describe{
#'                  \item{plot}{function to do the actual plotting. See
#'                              \code{\link{plotgroups.boxplot}}, \code{\link{plotgroups.beeswarm}},
#'                              \code{\link{plotgroups.barplot}}, \code{\link{plotgroups.vioplot}}.}
#'                  \item{ylim}{Function to calculate Y axis limits based on data and features.
#'                              Takes three arguments:\describe{
#'                                      \item{data}{List of numeric vectors with data}
#'                                      \item{stats}{Precomputed statistics}
#'                                      \item{features}{Features to plot}}
#'                              Returns either a 2-element vector with Y limits or \code{NULL}, in
#'                              which case Y limits will be computed based on sensible defaults.}
#'                  \item{features}{Function to check user-supplied feature lists for correctness
#'                              and compute default features, if necessary. Takes one argument
#'                              (the user-supplied feature character vector) and returns a
#'                              character vector with features to plot.}}
#'        Can be a list of lists, in which case the elements will apply to the corresponding plot.
#' @param plot.fun.pars additional parameters to pass to \code{plot.type$plot}
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
#' @param signif.test.col color of p-value annotations.
#' @param signif.test.lwd line width for p-value annotations. Can be a list, in which case the lwd
#'        will apply to the corresponding plot if multiple data sets are plotted.
#' @param signif.test.pars parameters for group annotation. Will be passed to \code{\link[graphics]{text}}.
#'        Can be a list of lists, in which case each element will apply to the corresponding
#'        plot if multiple data sets are plotted.
#' @param extrafun.before additional function to call after the coordinate system has been set up, but
#'        before plotting, e.g. to add a background grid to the plot. Can be a list of functions, in
#'        which case each element will apply to the corresponding plot if multiple data sets are
#'        plotted.
#' @param extrafun.after additional function to call after plotting, e.g. to add additional elements
#'        to the plot. Can be a list of functions, in which case each element will apply to the
#'        corresponding plot if multiple data sets are plotted.
#' @param ... additional parameters passed to \code{\link[graphics]{par}}
#' @return list with the following components:
#'         \describe{
#'                  \item{stats}{summary statistics of the data.}
#'                  \item{features}{Character vector of features actually plotted.}
#'                  \item{plotfun}{Return value of \code{plot.type$plot}}
#'                  \item{xcoords}{X coordinates of the data.}
#'                  \item{annotation.height}{Height of the annotation in inches.}
#'                  \item{annotation.width}{Width of the annotation in inches. If
#'                      \code{names.style='combinatorial'} this is the width of the left margin.}
#'                  \item{legendmargin}{Top margin required for the legend, in user coordinates.}
#'                  If significance testing was performed, also contains a component
#'                  \code{signiftest}, which is a list with elements ordered by
#'                  \code{signif.test} with the following components:
#'                  \describe{
#'                          \item{test}{return value of the testing function}
#'                          \item{label}{return value of \code{signif.test.text}}}}
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
#'            plot.type=plotgroups.beeswarm, features=c('mean', 'sd'), ylim=c(0,Inf))
#' plotgroups(data, names2, colors, legend.text,plot.type=plotgroups.vioplot, ylim=c(0,Inf),
#'            names.rotate=0, names.adj=c(0.5, 1))
#' plotgroups(data, names, colors, legend.text, log=TRUE,
#'            plot.type=plotgroups.beeswarm, features=c('mean', 'sd'),
#'            names.style='combinatorial', names.split=" ", names.pch='\u0394',
#'            plot.fun.pars=list(palpha=0.5, bxpcols="black"))
#' plotgroups(data, names, colors, legend.text,
#'            names.style='combinatorial', names.split=" ", names.pch='\u0394',
#'            names.placeholder='+')
#' plotgroups(data, names, colors, legend.text,
#'            names.style='combinatorial', names.split=" ", names.pch=19,
#'            main="test", plot.type=plotgroups.barplot, features=c("mean", "sd"),
#'            plot.fun.pars=list(whiskerswidth=0.6))
#'
#' map.fun <- function(n, split, pch, cex, rotate, adj) {
#'                n <- strsplit(n, split, fixed=TRUE)[[1]]
#'                nlist <- lapply(n, function(x){
#'                                       if (x != "gene2") {
#'                                           list(pch=pch, cex=cex, rotate=rotate, adj=adj)
#'                                        } else {
#'                                            list(pch='S158T', cex=cex, rotate=90, adj=c(0,0.5))
#'                                        }
#'                                     })
#'                 names(nlist) <- n
#'                 nlist
#' }
#' plotgroups(data, names, colors, legend.text,names.style='combinatorial', names.split=" ",
#'            names.pch='\u0394', names.map.fun=map.fun)
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
#'            names.split=" ",names.pch='\u0394', names.map.fun=map.fun,
#'            ylim=c(0,Inf), ylab=c("data1", "data2"), main="test", features=list(NULL,
#'            c("median", "box")), plot.type=list(plotgroups.boxplot, plotgroups.beeswarm),
#'            signif.test=list(NULL,list(c(1,3), c(2,5), c(5,8), c(3,10))))
#' @export
#' @importFrom rlist list.merge
#' @importFrom grDevices boxplot.stats cm dev.cur extendrange
#' @importFrom graphics abline axTicks axis box layout lcm lines par plot.new plot.window
#'             points segments strheight strwidth text title
#' @importFrom stats sd t.test
#' @importFrom utils assignInNamespace
plotgroups <- function(
                        data,
                        names,
                        colors=NULL,
                        legend.text=NULL,
                        legend.col=NULL,
                        legend.pars=list(),
                        legend.lwd=NULL,
                        groups.spacing=0,
                        names.split=NULL,
                        names.italicize=NULL,
                        names.style=c("plain", "combinatorial"),
                        names.pch.cex=1,
                        names.pch=19,
                        names.adj=NA,
                        names.map.fun=NULL,
                        names.margin=0.5,
                        names.rotate=NULL,
                        names.placeholder=NA,
                        features=NA,
                        log=FALSE,
                        range=1.5,
                        conf.level=0.95,
                        ci.fun=plotgroups.ci,
                        cex.xlab=1,
                        ylim=NULL,
                        legendmargin=NULL,
                        plot.type=plotgroups.boxplot,
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
    for (arg in c("log", "range", "conf.level", "ylab")) {
        cenv[[arg]] <- rep(cenv[[arg]], length.out=nplots)
    }
    for (arg in c("features", "ylim", "signif.test.fun", "ci.fun", "signif.test.text", "signif.test.col", "signif.test.lwd", "extrafun.before", "extrafun.after")) {
        if (!is.list(cenv[[arg]])) {
            cenv[[arg]] <- rep(list(cenv[[arg]]), nplots)
        } else {
            cenv[[arg]] <- rep(cenv[[arg]], length.out=nplots)
        }
    }

    for (arg in c("plot.type", "plot.fun.pars", "signif.test.pars", "signif.test")) {
        if (!is.null(cenv[[arg]])
            &&length(cenv[[arg]])
            &&!all(sapply(cenv[[arg]], function(x)is.list(x) || is.null(x)))
            # we need to deal with parameter lists containing lists if there is only one plot, i.e.
            # something like plot.fun.pars=list(test=list(a=1,c=2))
            # assume that top-level lists for multiple plots are unnamed
            || !is.null(names(cenv[[arg]]))) {
            cenv[[arg]] <- rep(list(cenv[[arg]]), nplots)
        } else if (!is.null(cenv[[arg]]) && length(cenv[[arg]]) != nplots) {
            cenv[[arg]] <- rep(cenv[[arg]], length.out=nplots)
        }
    }
    haveMagicAxis <- requireNamespace("magicaxis", quietly = TRUE)

    if (!is.null(legend.text)) {
        grouplength <- rle(legend.text)$lengths
    } else {
        grouplength <- ngroups
    }
    if (!is.null(colors) && length(colors) != ngroups) {
        colors <- rep(colors, length.out=length(grouplength))
        colors <- rep(colors, times=grouplength)
    }
    xcoords <- 1:ngroups + rep(cumsum(c(0, rep(groups.spacing, times=length(grouplength) - 1))), times=grouplength)
    xlim <- c(0.5 - 0.5 * groups.spacing, max(xcoords) + 0.5 + 0.5 * groups.spacing)

    pars <- list(oma=c(0,0,0,0), mar=c(0, 3, 0.2, 0.2), las=1, mgp=c(2, 0.5, 0), ljoin="mitre", lend="square")
    if (!is.null(main))
        pars$oma <- c(0, 0, 2, 0)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(par, pars)

    if (grepl('cairo', names(dev.cur()), ignore.case=TRUE))
        plot.new() # have to do this here, otherwise strwidth on cairo devices will crash
    lwd.base <- par("lwd")
    if (is.null(legend.lwd))
        legend.lwd <- lwd.base

    lheight <- par("lheight")
    origcex <- par("cex") #reset by layout()
    par(lheight=1)
    lineheight <- strheight("\n", units="inches", cex=cex.xlab)
    mai <- par("mai")
    if (is.null(names.split))
        names.split <- NA
    if (names.style=="combinatorial") {
        if (is.null(names.rotate))
            names.rotate <- 0
        if (is.na(names.adj) || is.null(names.adj))
            names.adj <- 0.5

        if (is.null(names.map.fun))
            names.map.fun <- function(n, split, pch, cex, rotate, adj) {
                n <- strsplit(n, split, fixed=TRUE)[[1]]
                nlist <- lapply(n, function(x)list(pch=pch, cex=cex, rotate=rotate, adj=adj))
                names(nlist) <- n
                nlist
            }

        fixmappednamesadj <- function(y) {if (length(y$adj) != 2) {y$adj <- c(y$adj[1], 0)}; y}
        names.mapped <- lapply(lapply(names, names.map.fun, names.split, names.pch, names.pch.cex, names.rotate, names.adj), function(x) {
            x <- x[nchar(names(x))>0]
            x <- lapply(x, fixmappednamesadj)
            x
        });
        uniquegenes <- unique(unlist(lapply(names.mapped, function(x)names(x))))
        uniquegenes <- uniquegenes[order(uniquegenes, decreasing=TRUE)]

        if (!is.na(names.placeholder) && !is.null(names.placeholder)) {
            names.placeholder <- fixmappednamesadj(list(pch=names.placeholder, cex=names.pch.cex, rotate=names.rotate, adj=names.adj))
        }
        heights <- sapply(uniquegenes, function(gene) {
            max(sapply(names.mapped, function(nm) {
                x <- nm[[gene]]
                if (is.null(x) && !is.na(names.placeholder) && !is.null(names.placeholder)) {
                    x <- names.placeholder
                }
                if (!is.null(x)) {
                    if (!is.character(x$pch)) {
                        lineheight
                    } else {
                        height <- sin(x$rotate * pi / 180) * strwidth(x$pch, units="inches", cex=x$cex) + 0.5 * lineheight
                        if (x$rotate != 90)
                            height <- height + x$adj[1] * strheight(x$pch, units="inches", cex=x$cex)
                        height
                    }
                } else {
                    -Inf
                }
            }))
        })

        hfracs <- heights / sum(heights)
        hfracscs <- c(0,cumsum(hfracs))[1:length(hfracs)]
        ycoords <- hfracscs +  0.15 * (lineheight / sum(heights))
        names(ycoords) <- uniquegenes

        legend.height <- max(sum(heights), strheight(paste0(uniquegenes, collapse="\n"), units="inches", cex=cex.xlab)) + names.margin * lineheight
        legend.width <- max(strwidth(uniquegenes, units="inches"))

        layout(matrix(c(2:(nplots + 1), 1), byrow=TRUE), heights=c(rep(1, nplots), lcm(cm(legend.height))))
        par(cex=origcex)

        mai[2] <- max(mai[2], legend.width)
        par(mai=c(0, mai[2], names.margin * lineheight, mai[4]))
        plot.new()
        plot.window(xlim=xlim, ylim=c(0,1), xaxs='i', yaxs='i')
        mapply(function(nm, x) {
            lapply(uniquegenes, function(g, x, nm) {
                n <- nm[[g]]
                if (is.null(n) && !is.na(names.placeholder) && !is.null(names.placeholder)) {
                    n <- names.placeholder
                }
                if (!is.null(n)) {
                    if (is.character(n$pch)) {
                        pfun <- text
                        parg <- "labels"
                        cadj <- n$adj
                    } else {
                        pfun <- points
                        parg <- "pch"
                        cadj <- n$adj[1]
                    }
                    args <- list(x=x, y=ycoords[g], adj=cadj, cex=n$cex, srt=n$rotate, xpd=TRUE)
                    args[[parg]] <- n$pch
                    do.call(pfun, args)
                }
            }, x, nm)
        }, names.mapped, xcoords)

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
        plt[1] <- max(0, plt[1] - max(strwidth(uniquegenes, units="figure", cex=cex.xlab)))
        par(plt=plt)
        plot.window(xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
        do.call("clip", as.list(par("usr")))
        if (length(heights) > 1) {
            sapply(hfracscs[2:length(hfracscs - 1)], function(x)abline(h=x, lwd=lwd.base))
        }
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
                            } else {
                                x <- paste0('"', x, '"')
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
        legend.height <- sin(names.rotate * pi / 180) * legend.width + vadj * max(strheight(labels, units="inches", cex=cex.xlab)) + 0.15 * lineheight + names.margin * lineheight

        layout(matrix(c(2:(nplots + 1), 1), byrow=TRUE), heights=c(rep(1, nplots), lcm(cm(legend.height))))
        par(cex=origcex)
        par(mai=c(0, mai[2], names.margin * lineheight, mai[4]))
        plot.new()
        plot.window(xlim=xlim, ylim=c(0,1), xaxs='i', yaxs='i')
        text(xcoords, 1, srt=names.rotate, adj=names.adj, labels=labels, xpd=NA, cex=cex.xlab)
    }
    title(main=main, outer=TRUE)

    allstats <- list()
    allplotfunrets <- list()
    allsigniftestrets <- list()

    for (cplot in 1:nplots) {
        if (is.null(features[[cplot]]))
            features[[cplot]] <- NA
        features[[cplot]] <- plot.type[[cplot]]$features(features[[cplot]])
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
            ndata <- length(data[[cplot]][[i]]) - length(which(is.na(data[[cplot]][[i]])))
            stats$means[i] <- mean(data[[cplot]][[i]], na.rm=TRUE)
            stats$sds[i] <- sd(data[[cplot]][[i]], na.rm=TRUE)
            stats$sems[i] <- stats$sds[i] / sqrt(ndata)
            ci <- ci.fun[[cplot]](mean=stats$means[i], se=stats$sems[i], ndata=ndata, conf.level=conf.level[cplot])
            stats$cimin[i] <- ci[1]
            stats$cimax[i] <- ci[2]
            bstats <- boxplot.stats(data[[cplot]][[i]], coef=range[cplot], do.conf=F, do.out=F)$stats
            stats$medians[i] <- bstats[3]
            stats$boxmax[i] <- bstats[4]
            stats$iqrmax[i] <- bstats[5]
            stats$boxmin[i] <- bstats[2]
            stats$iqrmin[i] <- bstats[1]
        }
        allstats[[cplot]] <- stats
        ylim.usr <- cylim <- ylim[[cplot]]
        if (!is.null(cylim) && (!is.finite(cylim[1]) || !is.finite(cylim[2]))) {
            cylim <- NULL
        }
        if (is.null(cylim))
            cylim <- do.call(plot.type[[cplot]]$ylim, c(list(data=data[[cplot]], stats=stats, features=features[[cplot]]), plot.fun.pars[[cplot]]))
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
                cylim[1] <- min(cylim[1], stats$cimin, na.rm=TRUE)
                cylim[2] <- max(cylim[2], stats$cimax, na.rm=TRUE)
            }

        }

        if (log[cplot]) {
            cylim <- log10(cylim)
            cylim[is.na(cylim)] <- 0
        }

        ylim.extended <- FALSE
        if (!is.null(ylim.usr)) {
            if (is.finite(ylim.usr[1])) {
                if (log[cplot]) {
                    cylim[1] <- log10(ylim.usr[1])
                } else {
                    cylim[1] <- ylim.usr[1]
                }
            }
            if (is.finite(ylim.usr[2])) {
                if (log[cplot]) {
                    cylim[2] <- log10(ylim.usr[2])
                } else {
                    cylim[2] <- ylim.usr[2]
                }
            }
            # this needs to be after the full range has been set up
            datarange <- diff(cylim)
            if (!is.finite(ylim.usr[1]))
                cylim[1] <- cylim[1] - 0.04 * datarange
            if (!is.finite(ylim.usr[2])) {
                cylim[2] <- cylim[2] + 0.04 * datarange
                ylim.extended <- TRUE
            }
        } else {
            if (!is.null(cylim) && all(is.finite(cylim))) {
                cylim <- extendrange(cylim, f=0.04)
                ylim.extended <- TRUE
            }
        }

        if (is.null(colors))
            colors <- "grey"
        # can't use grconvertY here because plot.window has not been called yet, have
        # to do the conversion manually
        lineheight <- strheight("", units="inches", cex=par("cex")) * 2
        signifheight <- 0.2 * lineheight
        legendbase <- cylim[2]
        if (cplot == 1) {
            if (!is.null(legend.text)) {
                if (is.null(legendmargin))
                    legendmargin <- max(max(strheight(legend.text, units="inches", cex=par("cex"))), lineheight)
                if (!ylim.extended)
                    legendmargin <- legendmargin + signifheight
            } else if (!is.null(signif.test[[cplot]]) && !ylim.extended) {
                legendmargin <- signifheight
            } else {
                legendmargin <- 0
            }
        } else {
            legendmargin <- 0
        }
        signifmargin <- 0
        if (!is.null(signif.test[[cplot]])) {
            for (p in c('IRanges', 'S4Vectors')) {
                if (!requireNamespace(p, quietly = TRUE))
                    stop(paste0("Please install the ", p, " package if significance testing is to be performed."))
            }
            intervals.start <- sapply(signif.test[[cplot]], function(x)min(x))
            intervals.stop <- sapply(signif.test[[cplot]], function(x)max(x))
            intervals.order <- order(intervals.start, intervals.stop)
            signif.test[[cplot]] <- signif.test[[cplot]][intervals.order]
            query <- IRanges::IRanges(intervals.start[intervals.order], intervals.stop[intervals.order])
            signifoverlaps <- S4Vectors::as.matrix(IRanges::findOverlaps(query, minoverlap=2))
            signifoverlaps <- signifoverlaps[which(signifoverlaps[,1] != signifoverlaps[,2]),]
            if (nrow(signifoverlaps)) {
                signifoverlaps <- t(apply(signifoverlaps, 1, function(x)c(min(x),max(x))))
                signifoverlaps <- signifoverlaps[!duplicated(signifoverlaps),, drop=FALSE]

                maxsignifoverlaps <- max(rle(signifoverlaps[,1])$length)
            } else {
                maxsignifoverlaps <- 0
            }

            if (maxsignifoverlaps == 0) {
                signiflines <- rep(0, length(signif.test[[cplot]]))
            } else {
                signiflines <- rep(-1, length(signif.test[[cplot]]))
                currline <- 0
                while (currline <= maxsignifoverlaps) {
                    i <- which(signiflines == -1)[1]
                    if (is.na(i)) {
                        break
                    }
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
                maxsignifoverlaps <- currline - 1
            }

            signifmargin <- (maxsignifoverlaps + 1) * lineheight
            if (!ylim.extended)
                legendmargin <- legendmargin + signifheight
            if (is.null(legend.text))
                legendmargin <- legendmargin + signifheight
        }

        margin <- legendmargin + signifmargin
        inchestouser <- (cylim[2] - cylim[1]) / (par("pin")[2] - margin)
        cylim <- c(cylim[1], cylim[2] + margin * inchestouser)

        if (log[cplot]) {
            plot.window(xlim=xlim, ylim=10^cylim, xaxs='i', yaxs='i', log='y')
        } else {
            plot.window(xlim=xlim, ylim=cylim, xaxs='i', yaxs='i')
        }
        inchestouser <- (cylim[2] - cylim[1]) / par("pin")[2]
        lineheight <- lineheight * inchestouser
        signifheight <- signifheight * inchestouser
        if (!ylim.extended)
            legendbase <- legendbase + signifheight
        if (!is.null(signif.test[[cplot]])) {
            signifbase <- legendbase
            if (!ylim.extended)
                signifbase <- signifbase + signifheight
            legendbase <- signifbase + signifmargin * inchestouser
        }

        if (!is.null(extrafun.before[[cplot]]))
            extrafun.before[[cplot]](data[[cplot]], xcoords, stats, colors, features, barwidth)
        plotfunret <- do.call(plot.type[[cplot]]$plot, c(list(data=data[[cplot]], at=xcoords, stats=allstats[[cplot]], colors=colors, features=features[[cplot]], barwidth=barwidth), plot.fun.pars[[cplot]]))
        if (!is.null(extrafun.after[[cplot]]))
            extrafun.after[[cplot]](data[[cplot]], xcoords, stats, colors, features, barwidth)

        if (log[cplot] && haveMagicAxis) {
            maglab.old <- magicaxis::maglab
            if (substr(names(dev.cur()), 1, 4) == "tikz") {
                assignInNamespace("maglab", function(...) {
                    ret <- maglab.old(...)
                    ret$exp <- lapply(ret$exp, function(x)paste0('$',sub('\\s+', ' ', sub('*', '\\cdot ', paste0(deparse(x, control=NULL), collapse=""), fixed=TRUE)), '$'))
                    ret
                }, "magicaxis")
            }
            do.call(magicaxis::magaxis, list.merge(pars, list(side=2, usepar=TRUE, minorn='auto', majorn=abs(ceiling(diff(cylim))), family=par('family'))))
            assignInNamespace("maglab", maglab.old, "magicaxis")
        } else {
            # try to avoid axis ticks to close to the upper edge
            # probably need better logic here (what about the lower edge? current assumption is
            # we ignore it because we have space due to names and names.margin. Just skipping the upper
            # tick also works for multiple plots
            ticks <- axTicks(side=2)
            lticks <- length(ticks)
            if ((cplot > 1 || !is.null(legend.text)) && ticks[lticks] > cylim[2] - 0.5 * lineheight)
                ticks <- ticks[-lticks]
            do.call(axis, list.merge(pars, list(side=2, at=ticks)))
        }

        title(ylab=ylab[cplot])
        do.call(box, pars)

        if (log[cplot]) {
            par(ylog=FALSE)
            plot.window(xlim=xlim, ylim=cylim, xaxs='i', yaxs='i')
        }
        if (cplot == 1 && !is.null(legend.text)) {
            segs.begin <- c(1, xcoords[cumsum(grouplength)[-length(grouplength)] + 1]) - barwidth / 2
            segs.end <- xcoords[cumsum(grouplength)] + barwidth / 2
            if (is.null(legend.col)) {
                cols <- unique(colors)
            } else {
                cols <- legend.col
            }
            segments(segs.begin, legendbase, segs.end, legendbase, lwd=legend.lwd, col=cols)

            mids <- (segs.end - segs.begin) / 2 + segs.begin
            do.call(text, c(list(x=mids, y=legendbase + signifheight, labels=unique(legend.text), adj=c(0.5, 0), col=cols), legend.pars))
        }
        if (!is.null(signif.test[[cplot]])) {
            signif.test.ret <- vector("list", length(signif.test[[cplot]]))
            for (i in 1:length(signif.test[[cplot]])) {
                signif.test.ret[[i]]$test <- signif.test.fun[[cplot]](data[[cplot]][[signif.test[[cplot]][[i]][1]]], data[[cplot]][[signif.test[[cplot]][[i]][2]]])
                p <- signif.test.ret[[i]]$test$p.value
                label <- signif.test.text[[cplot]](p)
                if (!is.null(label)) {
                    begin <- xcoords[signif.test[[cplot]][[i]][1]] + (1 - barwidth) / 2
                    end <- xcoords[signif.test[[cplot]][[i]][2]] - (1 - barwidth) / 2
                    mid <- (end - begin) / 2 + begin
                    base <- signifbase + signiflines[i] * lineheight
                    lines(c(begin, begin, end, end), c(base - signifheight, base, base, base - signifheight), lwd=signif.test.lwd[[cplot]], col=signif.test.col[[cplot]])
                    do.call(text, c(list(x=mid, y=base + 0.2 * lineheight, labels=label, adj=c(0.5, 0), col=signif.test.col[[cplot]]), signif.test.pars[[cplot]]))
                }
                signif.test.ret[[i]]$label <- label
            }
            allsigniftestrets[[cplot]] <- signif.test.ret
        }
        allplotfunrets <- plotfunret
    }
    toreturn <- list(stats=allstats, features=features, plotfun=allplotfunrets, signiftest=allsigniftestrets, annotation.height=legend.height, annotation.width=legend.width, legendmargin=legendmargin)
    invisible(toreturn)
}
