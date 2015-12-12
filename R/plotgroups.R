#' @export
#' @importFrom rlist list.merge
plotgroups.boxplot <- function(data, stats, ylab, colors, ylim, features, barwidth, legendmargin, bxppars, ...)
{
    if (!missing(ylim))
        return(NULL)
    if (missing(bxppars))
        bxppars <- list()
    lwd.base <- par("lwd")
    if (is.null(bxppars$boxwex))
        bxppars$boxwex <- barwidth

    lwd.mean <- 3 * lwd.base
    lwd.sem <- 1.5 * lwd.base
    if ("sd" %in% features) {
        segments(1:length(data) - bxppars$boxwex / 4, stats$means + stats$sds, 1:length(data) + bxppars$boxwex / 4, stats$means + stats$sds, col="black", lwd=1 * lwd.base)
        segments(1:length(data) - bxppars$boxwex / 4, stats$means - stats$sds, 1:length(data) + bxppars$boxwex / 4, stats$means - stats$sds, col="black", lwd=1 * lwd.base)
        segments(1:length(data), stats$means + stats$sds, 1:length(data), stats$means - stats$sds, col="black", lwd=1 * lwd.base)
    }

    if (!("median" %in% features))
        bxppars$medlty="blank"
    if (!("box" %in% features)) {
        bxppars$boxlty="blank"
        bxppars$boxfill="transparent"
    }
    if (!("iqr" %in% features)) {
        bxppars$whisklty="blank"
        bxppars$staplelty="blank"
    } else {
        bxppars$whisklty="22"
        bxppars$staplelty="22"
    }
    dots <- list(...)
    pars <- list(notch=TRUE, las=1, notch.frac=0.9, outpch=NA)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(boxplot, list.merge(pars, list(x=data, ylab=ylab, xaxt="n", col=colors, yaxs="i", pars=bxppars, add=TRUE)))
    axis(2, lwd=lwd.base)
    if ("mean" %in% features)
        segments(1:length(data) - bxppars$boxwex / 2, stats$means, 1:length(data) + bxppars$boxwex / 2, stats$means, col="red", lwd=lwd.mean, lend='butt')
    if ("sem" %in% features) {
        segments(1:length(data) - bxppars$boxwex / 2, stats$means + stats$sems, 1:length(data) + bxppars$boxwex / 2, stats$means +stats$sems, col="#EDA217", lwd=lwd.sem, lend='butt')
        segments(1:length(data) - bxppars$boxwex / 2, stats$means - stats$sems, 1:length(data) + bxppars$boxwex / 2, stats$means -stats$sems, col="#EDA217", lwd=lwd.sem, lend='butt')
    }
}

#' @export
#' @importFrom rlist list.merge
plotgroups.beeswarm <- function(data, stats, ylab, colors, ylim, features, barwidth, legendmargin, palpha=1, bxplwd=par("lwd"), bxpcols=colors, ...)
{
    if (!requireNamespace("beeswarm", quietly = TRUE))
        stop("Please install the beeswarm package for this plot.")
    if ("mean" %in% features && "median" %in% features)
        stop("both mean and median present in features, only one can be plotted")
    if (as.integer("box" %in% features + "iqr" %in% features + "sd" %in% features + "sem" %in% features) > 1)
        stop("only one of 'box', 'iqr', 'sd', 'sem' can be plotted, ajust features argument accordingly")
    if (!missing(ylim))
        return(c(0,max(sapply(data, max))))
        
    library(beeswarm)
    dots <- list(...)
    pars <- list(method="swarm", corral="random", priority="random", pch=16)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    
    lims <- par("usr")
    do.call(beeswarm::beeswarm, list.merge(pars, list(x=data, corralWidth=barwidth, add=TRUE, ylab=ylab, col=rgb(t(col2rgb(colors)), alpha=palpha*255, maxColorValue=255), yaxs='i', xaxt='n')))
    
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
    
    if (!is.null(bars$u) && !is.null(bars$l))
        segments(1:length(data), bars$l, 1:length(data), bars$u, col=bxpcols, lend='butt', lwd=bxplwd)
    
    for (b in bars) {
        if (!is.null(b))
            segments(1:length(data) - barwidth / 2, b, 1:length(data) + barwidth / 2, b, col=bxpcols, lend='butt', lwd=bxplwd)
    }
    axis(2, ...)
    title(ylab=ylab)
    box(lwd=par("lwd"))
}

#' Plot several groups of repeated observations, e.g. abundance/half-life of several
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
#' @param lwd.legend Line width for grouping annotations. Defaults to \code{par("lwd")}
#' @param features Which features of the sample distributions to plot. Availability of features
#'        depends on \code{plot.fun}
#' @param cex.xlab Character expansion factor for X axis annotation
#' @param ylim Y axis limits. Will be determined automatically if \code{NULL}
#' @param legendmargin Spacing between the upper-most data point/feature and the upper edge of the
#'        plot, required for group annotation. Will be determined automatically if \code{NULL}
#' @param plot.fun Function to do the actual plotting. See \code{\link{plot.groups.boxplot}},
#'        \code{\link{plot.groups.beeswarm}}
#' @param plot.fun.pars Additional parameters to pass to \code{plot.fun}
#' @param barwidth Width of the individual bars/boxes etc. as fraction of 1
#' @param ylab Y axis label
#' @param ... Additional parameters passed to \code{\link[base]{par}}
#' @examples
#' data <- list()
#' for (i in 1:14) data[[i]] <- rnorm(50, i, 0.5)
#' names <- rep(c('gene1', 'gene2', 'gene3', 'gene1 gene2', 'gene1 gene3', 'gene2 gene3', 'gene1 gene2 gene3'), times=2)
#' colors <- rep(c("green", "blue"), each=7)
#' legend.text <- c("protein1", "protein2")
#' plotgroups(data, names, colors, legend.text, plot.fun=plotgroups.beeswarm, features=c('mean', 'sd'))
#' plotgroups(data, names, colors, legend.text, plot.fun=plotgroups.beeswarm, features=c('mean', 'sd'), names.style='combinatorial', names.split=" ", names.pch='Δ', plot.fun.pars=list(palpha=0.5, bxpcols="black"))
#'plotgroups(data, names, colors, legend.text, names.style='combinatorial', names.split=" ", names.pch='Δ')
#' @export
#' @importFrom rlist list.merge
plotgroups <- function(data, names, colors=NULL, legend.text=NULL, legend.col=NULL, legend.pars=list(font=2), names.split=NULL, names.italicize=NULL, names.style=c("plain", "combinatorial"), names.pch=19, names.pch.cex=1, names.pch.adj=0.5, names.margin=0.5, names.rotate=NULL, lwd.legend=NULL, features=c("median", "box", "iqr", "mean", "sd", "sem"), cex.xlab=1, ylim=NULL, legendmargin=NULL, plot.fun=plotgroups.boxplot, plot.fun.pars=list(), barwidth=0.8, ylab=deparse(substitute(data)), ...)
{
    names.style=match.arg(names.style)
    
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

    if (is.null(ylim)) {
        ylim <- plot.fun(data=data, features=features, ylim=TRUE)
        ylim[2] <- ylim[2] + 0.02 * ylim[2]
    }
    if (is.null(ylim)) {
        ylim <- c(0, 0)
        if ("median" %in% features)
            ylim[2] <- max(ylim[2], stats$medians, na.rm=TRUE)
        if ("box" %in% features)
            ylim[2] <- max(ylim[2], stats$boxmax, na.rm=TRUE)
        if ("iqr" %in% features)
            ylim[2] <- max(ylim[2], stats$iqrmax, na.rm=TRUE)
        if ("mean" %in% features)
            ylim[2] <- max(ylim[2], stats$means, na.rm=TRUE)
        if ("sd" %in% features)
            ylim[2] <- max(ylim[2], stats$means + stats$sds, na.rm=TRUE)
        if ("sem" %in% features)
            ylim[2] <- max(ylim[2], stats$means + stats$sems, na.rm=TRUE)
        ylim[2] <- ylim[2] + 0.02 * ylim[2]
    }

    dots <- list(...)
    pars <- list(oma=c(0,0,0,0), las=1, mgp=c(2, 0.5, 0), ljoin="mitre", lend="square", lwd=2, lheight=1.2)
    if (length(dots) > 0)
        pars <- list.merge(pars, dots)
    do.call(par, pars)
    plot.new()
    lwd.base <- par("lwd")
    if (is.null(lwd.legend))
        lwd.legend <- lwd.base
        
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

    if (is.null(legendmargin)) {
        legendmargin <- max(strheight(legend.text, units="inches"))
        legendmargin <- legendmargin * (ylim[2] - ylim[1]) / par("pin")[2] * 1.8
    }
    plot.window(xlim=c(0.5, length(data) + 0.5), ylim=c(ylim[1], ylim[2] + legendmargin), xaxs='i', yaxs='i')
    
    do.call(plot.fun, c(list(data=data, stats=stats, ylab=ylab, colors=colors, features=features, barwidth=barwidth, legendmargin=legendmargin), plot.fun.pars))
    
    if (!is.null(legend.text)) {
        rlength <- rle(colors)$lengths
        segs.begin <- c(1, cumsum(rlength)[-length(rlength)] + 1) - barwidth / 2
        segs.end <- cumsum(rlength) + barwidth / 2
        if (is.null(legend.col)) {
            cols <- unique(colors)
        } else {
            cols <- legend.col
        }
        segments(segs.begin, ylim[2], segs.end, ylim[2], lwd=lwd.legend * lwd.base, col=cols, lend="butt")

        mids <- (segs.end - segs.begin) / 2 + segs.begin
        do.call(text, c(list(x=mids, y=ylim[2] + 0.3 * legendmargin, labels=legend.text, adj=c(0.5, 0), col=cols), legend.pars))
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
                    text(x=i, y=currgenes[j], labels=pch[currlength + j], adj=adj[[currlength + j]], cex=cex.xlab * cex[currlength + j], srt=rotate[currlength + j], xpd=T)
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
    invisible(NULL)
} 
