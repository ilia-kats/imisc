#' @export
#' @importFrom rlist list.merge
plotgroups.boxplot <- function(data, stats, colors, ylim, features, barwidth, bxppars, ...)
{
    if (!missing(ylim))
        return(NULL)
    if (missing(bxppars))
        bxppars <- list()
    lwd.base <- par("lwd")
    if (is.null(bxppars$boxwex))
        bxppars$boxwex <- barwidth
    dots <- list(...)
    pars <- list(notch=TRUE, las=1, notch.frac=0.9, outpch=NA,
                 meanlty=1, meanlwd=3*lwd.base, meancol="red", meanpch=NA, meancex=1,
                 sdwhisklty=1, sdwhisklwd=lwd.base, sdwhiskcol="black",
                 sdstaplelty=1, sdstaplelwd=lwd.base, sdstaplecol="black",
                 semlty=1, semlwd=lwd.base, semcol="#EDA217", sempch=NA, semcex=1)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots, list(pars=bxppars))

    if ("sd" %in% features) {
        segments(1:length(data) - bxppars$boxwex / 4, stats$means + stats$sds, 1:length(data) + bxppars$boxwex / 4, stats$means + stats$sds, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
        segments(1:length(data) - bxppars$boxwex / 4, stats$means - stats$sds, 1:length(data) + bxppars$boxwex / 4, stats$means - stats$sds, lend='butt', col=pars$sdstaplecol, lwd=pars$sdstaplelwd, lty=pars$sdstaplelty)
        segments(1:length(data), stats$means + stats$sds, 1:length(data), stats$means - stats$sds, lend='butt', col=pars$sdwhiskcol, lty=pars$sdwhisklty, lwd=pars$sdwhisklwd)
    }

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

    toreturn <- do.call(boxplot, list.merge(pars, list(x=data, xaxt="n", col=colors, yaxt='n', add=TRUE)))
    if ("mean" %in% features)
        segments(1:length(data) - bxppars$boxwex / 2, stats$means, 1:length(data) + bxppars$boxwex / 2, stats$means, lend='butt', lty=pars$meanlty, lwd=pars$meanlwd, col=pars$meancol)
        points(1:length(data), stats$means, pch=pars$meanpch, cex=pars$meancex, col=pars$meancol)
    if ("sem" %in% features) {
        segments(1:length(data) - bxppars$boxwex / 2, stats$means + stats$sems, 1:length(data) + bxppars$boxwex / 2, stats$means +stats$sems, lend='butt', lty=pars$semlty, lwd=pars$semlwd, col=pars$semcol)
        segments(1:length(data) - bxppars$boxwex / 2, stats$means - stats$sems, 1:length(data) + bxppars$boxwex / 2, stats$means -stats$sems, lend='butt', lty=pars$semlty, lwd=pars$semlwd, col=pars$semcol)
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
        bars$u <- stats$boxmin
        bars$l <- stats$boxmax
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
    bars
}

#' @export
#' @importFrom rlist list.merge
plotgroups.beeswarm <- function(data, stats, colors, ylim, features, barwidth, palpha=1, bxplwd=par("lwd"), bxpcols=colors, ...)
{
    if (!requireNamespace("beeswarm", quietly = TRUE))
        stop("Please install the beeswarm package for this plot.")
    if ("mean" %in% features && "median" %in% features)
        stop("both mean and median present in features, only one can be plotted")
    if (as.integer("box" %in% features + "iqr" %in% features + "sd" %in% features + "sem" %in% features) > 1)
        stop("only one of 'box', 'iqr', 'sd', 'sem' can be plotted, ajust features argument accordingly")
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

#' @export
#' @importFrom rlist list.merge
plotgroups.barplot <- function(data, stats, colors, ylim, features, barwidth, whiskerswidth=barwidth, whiskerslwd=par("lwd"), whiskerscols="black", bordercols="black", ...)
{
    if ("mean" %in% features && "median" %in% features)
        stop("both mean and median present in features, only one can be plotted")
    if (as.integer("box" %in% features + "iqr" %in% features + "sd" %in% features + "sem" %in% features) > 1)
        stop("only one of 'box', 'iqr', 'sd', 'sem' can be plotted, ajust features argument accordingly")
    if (!missing(ylim))
        return(NULL)
    bars <- threeparamsstats(stats, features)
    dots <- list(...)
    pars <- list(names.arg=NULL)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(rect, list.merge(pars, list(xleft=1:length(data) - barwidth/2, ybottom=par("usr")[3], xright=1:length(data) + barwidth/2, ytop=bars$m, col=colors, border=bordercols)))

    if (!is.null(bars$u) && !is.null(bars$l))
        segments(1:length(data), bars$l, 1:length(data), bars$u, col=whiskerscols, lend='butt', lwd=whiskerslwd)

    for (b in bars[c("u", "l")]) {
        if (!is.null(b))
            segments(1:length(data) - whiskerswidth / 2, b, 1:length(data) + whiskerswidth / 2, b, col=whiskerscols, lend='butt', lwd=whiskerslwd)
    }
    invisible(NULL)
}

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
    if (missing(boxpars))
        boxpars <- list()
    if (is.null(boxpars$notch))
        boxpars$notch <- FALSE
    bxp.toreturn <- do.call(plotgroups.boxplot, list.merge(boxpars, list(data=data, stats=stats, colors=boxcol, features=features, barwidth=boxwidth)))
    invisible(list(vioplot=vioplot.toreturn, boxplot=bxp.toreturn))
}

#' Plot several groups of repeated observations.
#'
#' Plot serveral groups of repeated observations, e.g. abundance/half-life of several
#' proteins each observed in several cell lines in several replicates. Observations can be grouped
#' either by protein (in which case cell lines will be annotated as X axis labels
#' and proteins above the plot) or by cell line.
#' @param data List, each element is a vector of replicates for one combination of parameters
#' @param names Character vector of X axis labels
#' @param colors Colors for plotting. Note that a group of observations is identified by consecutive
#'        occurrence of the same color
#' @param legend.text Character vector of group names
#' @param legend.col Colors for group annotations. Defaults to plotting colors
#' @param legend.pars Parameters for group annotation. Will be passed to \code{\link[base]{text}}
#' @param legend.lwd Line width for grouping annotations. Defaults to \code{par("lwd")}
#' @param names.split Character by which to split the \code{names}. Only useful in combination with
#'        \code{names.italicize} or \code{names.style='combinatorial'}
#' @param names.italicize If a part of a \code{name} is to be written in italic text, the part is
#'        identified by this character. I.e. The name is first split by \code{names.split}, each
#'        fragment containing \code{names.italicize} is rendered in italics
#' @param names.style How the \code{names} are to be rendered.
#'        \describe{
#'              \item{plain}{Each name will be written as-is below the plot}
#'              \item{combinatorial}{Names will be split by \code{names.split}, unique strings will be
#'                    printed at the bottom-left, and observations whose name contains the string will
#'                    be identified by prining \code{names.pch} below the respective bar. Useful if e.g.
#'                   assaying different combinations of single/double/triple knock-outs.}
#'                   }
#' @param names.pch Character to be used for annotation of observations when
#'        \code{names.style='combinatorial'}
#' @param names.pch.cex Character expansion factor for \code{names.pch}
#' @param names.pch.adj Text adjustment for \code{names.pch}. See \code{\link[base]{text}}
#' @param names.margin Spacing between the bottom edge of the plot and the annotation, in inches
#' @param names.rotate Only used when \code{names.style='plain'}. Degrees by which to rotate the
#'        annotation strings.
#' @param features Which features of the sample distributions to plot. Availability of features
#'        depends on \code{plot.fun}
#' @param cex.xlab Character expansion factor for X axis annotation
#' @param ylim Y axis limits. Will be determined automatically if \code{NULL}. If not \code{NULL} but
#'        only one limit is finite, the other will be determined automatically.
#' @param legendmargin Spacing between the upper-most data point/feature and the upper edge of the
#'        plot, required for group annotation. Will be determined automatically if \code{NULL}
#' @param plot.fun Function to do the actual plotting. See \code{\link{plot.groups.boxplot}},
#'        \code{\link{plot.groups.beeswarm}}
#' @param plot.fun.pars Additional parameters to pass to \code{plot.fun}
#' @param barwidth Width of the individual bars/boxes etc. as fraction of 1
#' @param main Main title
#' @param ylab Y axis label
#' @param signif.test List of 2-element integer vectors giving the elements of \code{data} to be
#'        tested for significant differences.
#' @param signif.test.fun Function to perform the significance testing. Must accept 2 vectors and
#'        return a list containing at least the element \code{p.value}
#' @param signif.test.text Function accepting a p-value and returning a formatted string to be used
#'        for plotting or \code{NULL} if this p-value is not to be plotted (e.g. if it is not
#'        significant)
#' @param signif.test.lwd Line width for p-value annotations.
#' @param signif.test.pars Parameters for group annotation. Will be passed to \code{\link[base]{text}}
#' @param extrafun Additional function to call after plotting, e.g. to add addtional elements
#'        to the plot
#' @param ... Additional parameters passed to \code{\link[base]{par}}
#' @examples
#' data <- list()
#' for (i in 1:14) data[[i]] <- rnorm(50, i, 0.5)
#' names <- rep(c('gene1', 'gene2', 'gene3', 'gene1 gene2', 'gene1 gene3', 'gene2 gene3', 'gene1 gene2 gene3'),
#'      times=2)
#' colors <- rep(c("green", "blue"), each=7)
#' legend.text <- c("protein1", "protein2")
#' plotgroups(data, names, colors, legend.text,
#'            plot.fun=plotgroups.beeswarm, features=c('mean', 'sd'), ylim=c(0,Inf))
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
#' ## significance testing
#' plotgroups(data, names, colors, legend.text,names.style='combinatorial',
#'            names.split=" ", names.pch='\u0394',
#'            signif.test=list(c(1,3), c(2,5), c(5,8), c(3,10)))
#' plotgroups(data, names, colors, legend.text,names.style='combinatorial',
#'            names.split=" ", names.pch='\u0394',
#'            signif.test=list(c(1,3), c(2,5), c(5,8), c(3,10)), signif.test.text=function(p) {
#'                      if (p < 0.001) {
#'                          return('***')
#'                      } else if (p < 0.01) {
#'                          return('**')
#'                      } else if (p < 0.05) {
#'                          return('*')
#'                      } else {
#'                          return(NULL)
#'                      }})
#' @export
#' @importFrom rlist list.merge
plotgroups <- function(
                        data,
                        names,
                        colors=NULL,
                        legend.text=NULL,
                        legend.col=NULL,
                        legend.pars=list(font=2),
                        legend.lwd=NULL,
                        names.split=NULL,
                        names.italicize=NULL,
                        names.style=c("plain", "combinatorial"),
                        names.pch=19,
                        names.pch.cex=1,
                        names.pch.adj=0.5,
                        names.margin=0.5,
                        names.rotate=NULL,
                        features=c("median", "box", "iqr", "mean", "sd", "sem"),
                        cex.xlab=1,
                        ylim=NULL,
                        legendmargin=NULL,
                        plot.fun=plotgroups.boxplot,
                        plot.fun.pars=list(),
                        barwidth=0.8,
                        main=NULL,
                        ylab=deparse(substitute(data)),
                        signif.test=NULL,
                        signif.test.fun=t.test,
                        signif.test.text=function(p)paste0("p=", formatC(p, digits=3, format="g")),
                        signif.test.col="black",
                        signif.test.lwd=legend.lwd,
                        signif.test.pars=legend.pars,
                        extrafun=NULL,
                        ...)
{
    names.style <- match.arg(names.style)
    if (!is.null(features) && length(features) > 0) {
        features <- match.arg(features, several.ok=TRUE)
    } else {
        features <- character(0)
    }

    stats <- list(means=c(), sds=c(), sems=c(), medians=c(), boxmax=c(), iqrmax=c(), boxmin=c(), iqrmin=c())
    for (i in 1:length(data)) {
        stats$means[i] <- mean(data[[i]])
        stats$sds[i] <- sd(data[[i]])
        stats$sems[i] <- stats$sds[i] / sqrt(length(data[[i]]))
        bstats <- boxplot.stats(data[[i]], do.conf=F, do.out=F)$stats
        stats$medians[i] <- bstats[3]
        stats$boxmax[i] <- bstats[4]
        stats$iqrmax[i] <- bstats[5]
        stats$boxmin[i] <- bstats[1]
        stats$iqrmin[i] <- bstats[2]
    }

    ylim.usr <- NULL
    if (!is.null(ylim) && (!is.finite(ylim[1]) || !is.finite(ylim[2]))) {
        ylim.usr <- ylim
        ylim <- NULL
    }
    if (is.null(ylim)) {
        ylim <- plot.fun(data=data, features=features, ylim=TRUE)
        if (!is.null(ylim))
            ylim <- extendrange(ylim, f=0.04)
    }
    if (is.null(ylim)) {
        ylim <- c(Inf, 0)
        if ("median" %in% features) {
            ylim[1] <- min(ylim[1], stats$medians, na.rm=TRUE)
            ylim[2] <- max(ylim[2], stats$medians, na.rm=TRUE)
        }
        if ("box" %in% features) {
            ylim[1] <- min(ylim[1], stats$boxmin, na.rm=TRUE)
            ylim[2] <- max(ylim[2], stats$boxmax, na.rm=TRUE)
        }
        if ("iqr" %in% features) {
            ylim[1] <- min(ylim[1], stats$iqrmin, na.rm=TRUE)
            ylim[2] <- max(ylim[2], stats$iqrmax, na.rm=TRUE)
        }
        if ("mean" %in% features) {
            ylim[1] <- min(ylim[1], stats$means, na.rm=TRUE)
            ylim[2] <- max(ylim[2], stats$means, na.rm=TRUE)
        }
        if ("sd" %in% features) {
            ylim[1] <- min(ylim[1], stats$means - stats$sds, na.rm=TRUE)
            ylim[2] <- max(ylim[2], stats$means + stats$sds, na.rm=TRUE)
        }
        if ("sem" %in% features) {
            ylim[1] <- min(ylim[1], stats$means - stats$sems, na.rm=TRUE)
            ylim[2] <- max(ylim[2], stats$means + stats$sems, na.rm=TRUE)
        }
        ylim <- extendrange(ylim, f=0.04)
    }
    if (!is.null(ylim.usr)) {
        if (is.finite(ylim.usr[1]))
            ylim[1] <- ylim.usr[1]
        if (is.finite(ylim.usr[2]))
            ylim[2] <- ylim.usr[2]
    }

    dots <- list(...)
    pars <- list(oma=c(0,0,0,0), las=1, mgp=c(2, 0.5, 0), ljoin="mitre", lend="square", lwd=2, lheight=1.2)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(par, pars)
    plot.new()
    lwd.base <- par("lwd")
    if (is.null(legend.lwd))
        legend.lwd <- lwd.base

    if (names.style=="combinatorial") {
        if (is.null(names.rotate))
            names.rotate <- 0
        if (!is.null(names.split)) {
            uniquegenes <- unique(unlist(strsplit(names, names.split, fixed=TRUE)))
        } else {
            uniquegenes <- unique(names)
        }
        uniquegenes <- uniquegenes[nchar(uniquegenes) > 0]
        uniquegenes <- uniquegenes[order(uniquegenes, decreasing=TRUE)]
        legend.height <- strheight(paste0(uniquegenes, collapse="\n"), units="inches", cex=cex.xlab)
        legend.width <- max(strwidth(uniquegenes, units="inches"))
        mar <- par("mar")
        mar[1] <- 0
        par(mar=mar)
        mai <- par("mai")
        mai[1] <- legend.height + names.margin * legend.height / length(uniquegenes)
        mai[2] <- max(mai[2], legend.width)
        par(mai=mai)
    } else {
        if (is.null(names.rotate))
            names.rotate <- 45
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
        lineheight <- strheight("\n", units="inches", cex=cex.xlab)
        names.margin <- names.margin * lineheight
        legend.width <- max(strwidth(labels, units="inches", cex=cex.xlab))
        legend.height <- sin(names.rotate * pi / 180) * legend.width + names.margin
        mai <- par("mai")
        mai[1] <- legend.height + names.margin
        par(mai=mai)
    }

    if (is.null(colors))
        colors <- "grey"

    # can't use grconvertY here because plot.window has not been called yet, have
    # to do the conversion manually
    inchestouser <- (ylim[2] - ylim[1]) / par("pin")[2]
    legendmarginfactor <- (ylim[2] - ylim[1]) / par("pin")[2] * 1.8
    lineheight <- strheight("\n", units="inches") * inchestouser
    if (is.null(legendmargin)) {
        legendmargin <- max(max(strheight(legend.text, units="inches")) * inchestouser, lineheight)
        legendbase <- ylim[2]
    }
    signifmargin <- 0
    if (!is.null(signif.test)) {
        if (!requireNamespace("IRanges", quietly = TRUE))
            stop("Please install the IRanges package if significance testing is to be performed.")
        intervals.start <- sapply(signif.test, function(x)x[1])
        intervals.stop <- sapply(signif.test, function(x)x[2])
        intervals.order <- order(intervals.start, intervals.stop)
        signif.test <- signif.test[intervals.order]
        query <- IRanges::IRanges(intervals.start[intervals.order], intervals.stop[intervals.order])
        signifoverlaps <- S4Vectors::as.matrix(IRanges::findOverlaps(query, minoverlap=2))
        signifoverlaps <- signifoverlaps[which(signifoverlaps[,1] != signifoverlaps[,2]),]

        signifoverlaps <- t(apply(signifoverlaps, 1, function(x)c(min(x),max(x))))
        signifoverlaps <- signifoverlaps[!duplicated(signifoverlaps),]

        maxsignifoverlaps <- max(rle(signifoverlaps[,1])$length)
        signifmargin <- (maxsignifoverlaps + 1) * lineheight
        signifbase <- legendbase
        legendbase <- signifbase + signifmargin

        if (maxsignifoverlaps == 0) {
            signiflines <- rep(0, length(signif.test))
        } else {
            signiflines <- rep(-1, length(signif.test))
            currline <- 0
            while (currline <= maxsignifoverlaps) {
                i <- which(signiflines == -1)[1]
                while (i <= length(signif.test)) {
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

    plot.window(xlim=c(0.5, length(data) + 0.5), ylim=c(ylim[1], ylim[2] + legendmargin + signifmargin), xaxs='i', yaxs='i')

    plotfunret <- do.call(plot.fun, c(list(data=data, stats=stats, colors=colors, features=features, barwidth=barwidth), plot.fun.pars))
    if (!is.null(extrafun))
        extrafun(data, stats, colors, features, barwidth)
    axis(2, lwd=lwd.base, ...)
    title(main=main, ylab=ylab)
    box(lwd=par("lwd"))

    if (!is.null(legend.text)) {
        rlength <- rle(colors)$lengths
        segs.begin <- c(1, cumsum(rlength)[-length(rlength)] + 1) - barwidth / 2
        segs.end <- cumsum(rlength) + barwidth / 2
        if (is.null(legend.col)) {
            cols <- unique(colors)
        } else {
            cols <- legend.col
        }
        segments(segs.begin, legendbase, segs.end, legendbase, lwd=legend.lwd, col=cols, lend="butt")

        mids <- (segs.end - segs.begin) / 2 + segs.begin
        do.call(text, c(list(x=mids, y=legendbase + 0.3 * legendmargin, labels=legend.text, adj=c(0.5, 0), col=cols), legend.pars))
    }

    if (!is.null(signif.test)) {
            signif.test.ret <- vector("list", length(signif.test))
        for (i in 1:length(signif.test)) {
            signif.test.ret[[i]]$test <- signif.test.fun(data[[signif.test[[i]][1]]], data[[signif.test[[i]][2]]])
            p <- signif.test.ret[[i]]$test$p.value
            label <- signif.test.text(p)
            if (!is.null(label)) {
                begin <- signif.test[[i]][1] + (1 - barwidth) / 2
                end <- signif.test[[i]][2] - (1 - barwidth) / 2
                mid <- (end - begin) / 2 + begin
                base <- signifbase + signiflines[i] * lineheight
                lines(c(begin, begin, end, end), c(base - 0.3 * legendmargin, base, base, base - 0.3 * legendmargin), lwd=signif.test.lwd, col=signif.test.col, lend="butt")
                do.call(text, c(list(x=mid, y=base + 0.2 * legendmargin, labels=label, adj=c(0.5, 0), col=signif.test.col), signif.test.pars))
            }
            signif.test.ret[[i]]$label <- label
        }
    }

    if (!is.null(labels) && names.style=="plain") {
        text(1:length(data), grconvertY(grconvertY(par("usr")[3], from="user", to="inches") - names.margin, from="inches", to="user"), srt=names.rotate, adj=1, labels=labels, xpd=TRUE, cex=cex.xlab)
    } else if (!is.null(names)) {
        plt <- par("plt")
        par(plt=c(plt[1],plt[2],0, grconvertY(legend.height, from="inches", to="nfc")))
        plot.window(xlim=c(0.5, length(data) + 0.5), ylim=c(0.5, length(uniquegenes) + 0.5), xaxs='i', yaxs='i')
        labels <- uniquegenes
        if (!is.null(names.italicize)) {
            if (!is.na(names.italicize)) {
                labels <- sapply(labels, function(x) {if (grepl(names.italicize, x, fixed=TRUE)) {x <- as.expression(substitute(italic(x), list(x=x)))}; x}, USE.NAMES=FALSE)
            } else {
                labels <- sapply(labels, function(x)as.expression(substitute(italic(x), list(x=x))), USE.NAMES=FALSE)
            }
        }
        text(0.5, 1:length(uniquegenes), labels=labels, adj=1, cex=cex.xlab, xpd=NA)
        if (!is.null(names.split)) {
            npoints <- sum(sapply(strsplit(names, names.split, fixed=TRUE), function(x)length(x)))
        } else {
            npoints <- sum(length(names))
        }
        pch <- rep(names.pch, length.out=npoints)
        cex <- rep(names.pch.cex, length.out=npoints)
        rotate <- rep(names.rotate, length.out=npoints)
        adj <- rep(names.pch.adj, length.out=npoints)
        if (is.character(pch)) {
            pfun <- text
            parg <- "labels"
        } else {
            pfun <- points
            parg <- "pch"
        }
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
                    args <- list(x=i, y=currgenes[j], adj=adj[[currlength + j]], cex=cex.xlab * cex[currlength + j], srt=rotate[currlength + j], xpd=T)
                    args[[parg]] <- pch[currlength + j]
                    do.call(pfun, args)
                }
                currlength <- newlength
            }
        }
        plt <- par("plt")
        plt[1] <- plt[1] - max(strwidth(uniquegenes, units="figure", cex=cex.xlab))
        par(plt=plt)
        plot.window(xlim=c(0,1), ylim=c(0.5, length(uniquegenes) + 0.5), xaxs='i', yaxs='i')
        do.call("clip", as.list(par("usr")))
        sapply(seq(from=1.5, length.out=length(uniquegenes) - 1, by=1), function(x)abline(h=x, lwd=lwd.base))
    }
    toreturn <- list(stats=stats, plotfun=plotfunret)
    if (length(signif.test.ret))
        toreturn$signiftest <- signif.test.ret[order(intervals.order)]
    invisible(toreturn)
}
