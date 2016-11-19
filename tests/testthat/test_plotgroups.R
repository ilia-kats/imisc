context('plotgroups')

set.seed(42)
data <- list()
for (i in 1:14) data[[i]] <- rnorm(50, i, 0.5)
names <- rep(c('gene1', 'gene2', 'gene3', 'gene1 gene2', 'gene1 gene3', 'gene2 gene3', 'gene1 gene2 gene3'),times=2)
names2 <- as.character(rep(1:7,times=2))
names2[2] <- "abc\nefg"
colors <- c("green", "blue")
legend.text <- rep(c("protein1", "protein2"), each=7)

ylab='abc'

map.fun <- function(n, split, pch, cex, rotate, adj) {
               n <- strsplit(n, split, fixed=TRUE)[[1]]
               nlist <- lapply(n, function(x){
                                      if (x != "gene2") {
                                          list(pch=pch, cex=cex, rotate=rotate, adj=adj)
                                       } else {
                                           list(pch='S158T', cex=cex, rotate=90, adj=c(0,0.5))
                                       }
                                    })
                names(nlist) <- n
                nlist
}

plot.type <- plotgroups.boxplot

test_that('no legend works', {
    expect_doppelganger('no legend single', function() {
        plotgroups(data, names, colors, NULL, plot.type=plot.type, ylab=ylab)
    })
    expect_doppelganger('no legend double', function() {
        plotgroups(list(data, rev(data)), names, colors, NULL, plot.type=plot.type, ylab=ylab)
    })
    expect_doppelganger('no legend double log', function() {
        plotgroups(list(data, rev(data)), names, colors, NULL, plot.type=plot.type, ylab=ylab, log=c(TRUE, FALSE))
    })
})

test_that('legend works', {
    expect_doppelganger('legend single', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plot.type, ylab=ylab)
    })
    expect_doppelganger('legend double', function() {
        plotgroups(list(data, rev(data)), names, colors, legend.text, plot.type=plot.type, ylab=ylab)
    })
    expect_doppelganger('legend double log', function() {
        plotgroups(list(data, rev(data)), names, colors, legend.text, plot.type=plot.type, ylab=ylab, log=c(TRUE, FALSE))
    })
})

test_that('combinatorial naming works', {
    expect_doppelganger('legend combinatorial single', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394')
    })
    expect_doppelganger('legend combinatorial double', function() {
        plotgroups(list(data, rev(data)), names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394')
    })
})

test_that('combinatorial naming with placeholder works', {
    expect_doppelganger('combinatorial placeholder', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', names.placeholder='+')
    })
})

test_that('combinatorial naming with numeric pch works', {
    expect_doppelganger('combinatorial numeric pch', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch=19, names.placeholder=16)
    })
})

test_that('custom map.fun works', {
    expect_doppelganger('map.fun', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', names.map.fun=map.fun)
    })
})

test_that('significance testing with legend', {
    expect_doppelganger('significance testing single', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(c(1,2), c(3,5)))
    })
    expect_doppelganger('significance testing multi', function() {
        plotgroups(list(data, rev(data), data), names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(list(c(1,2), c(3,5)), list(c(1, 2)), NULL))
    })
    expect_doppelganger('significance testing multi overlap', function() {
        plotgroups(list(data, rev(data), data), names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(list(c(1,3), c(2,5), c(5, 8), c(3, 10)), list(c(1, 5), c(1, 2), c(2, 4)), NULL))
    })
    expect_doppelganger('significance testing multi overlap log', function() {
        plotgroups(list(data, rev(data), data), names, colors, legend.text, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(list(c(1,3), c(2,5), c(5, 8), c(3, 10)), list(c(1, 5), c(1, 2), c(2, 4)), NULL), log=c(TRUE, FALSE, TRUE))
    })
})

test_that('significance testing without legend', {
    expect_doppelganger('significance testing single w/o legend', function() {
        plotgroups(data, names, colors, NULL, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(c(1,3), c(2,5)))
    })
    expect_doppelganger('significance testing multi w/o legend', function() {
        plotgroups(list(data, rev(data), data), names, colors, NULL, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(list(c(1,3), c(2,5)), list(c(1, 2)), NULL))
    })
    expect_doppelganger('significance testing multi overlap w/o legend', function() {
        plotgroups(list(data, rev(data), data), names, colors, NULL, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(list(c(1,3), c(2,5), c(5, 8), c(3, 10)), list(c(1, 5), c(1, 2), c(2, 4)), NULL))
    })
    expect_doppelganger('significance testing multi overlap w/o legend log', function() {
        plotgroups(list(data, rev(data), data), names, colors, NULL, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(list(c(1,3), c(2,5), c(5, 8), c(3, 10)), list(c(1, 5), c(1, 2), c(2, 4)), NULL), log=c(TRUE, FALSE, TRUE))
    })
})

test_that('italicize', {
    expect_doppelganger('italic combinatorial', function() {
        plotgroups(data, names, colors, NULL, plot.type=plot.type, names.style='combinatorial', names.split=" ", names.pch='\u0394', names.italicize='')
    })
    expect_doppelganger('italic plain', function() {
        plotgroups(data, names, colors, NULL, plot.type=plot.type, names.pch='\u0394', names.split=' ', names.italicize='1')
    })
})

test_that('custom significance text', {
    stars <- function(p) {
        if (p < 0.001) {
            return('***')
        } else if (p < 0.01) {
            return('**')
        } else if (p < 0.05) {
            return('*')
        } else {
            return(NULL)
        }
    }
    expect_doppelganger('custom significance text', function() {
        plotgroups(list(data, rev(data), data), names, colors, NULL, plot.type=plot.type, ylab=ylab, names.style='combinatorial', names.split=" ", names.pch='\u0394', signif.test=list(list(c(1,3), c(2,5), c(5, 8), c(3, 10)), list(c(1, 5), c(1, 2), c(2, 4)), NULL), signif.test.text=stars)
    })
})

test_that('boxplot arguments', {
    expect_doppelganger('boxplot arguments', function() {
        plotgroups(list(data, rev(data), data), names, colors, legend.text, plot.type=plotgroups.boxplot, ylab=ylab, features=list(c('median', 'box'), c('median', 'mean', 'ci', 'box', 'iqr', 'sem', 'sd'), c('median', 'box', 'iqr')), plot.fun.pars=list(list(notch=FALSE, medpch=19, medcol='red'), list(meanpch=19, semwhisklty=1, semstaplecol='orange', border='darkgreen'), list(bxppars=list(whisklty=1, staplelty=1))))
    })
})

test_that('vioplot', {
    expect_doppelganger('pure vioplot', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plotgroups.vioplot, ylab=ylab, features=c())
    })
    expect_doppelganger('vioplot with overlaid boxplot', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plotgroups.vioplot, ylab=ylab, features=c('median', 'box', 'mean', 'sd'), plot.fun.pars=list(boxpars=list(meanpch=19, meanlty=0)))
    })
})

test_that('beeswarm', {
    expect_doppelganger('pure beeswarm', function() {
        set.seed(42)
        plotgroups(data, names, colors, legend.text, plot.type=plotgroups.beeswarm, ylab=ylab, features=c())
    })
    expect_doppelganger('beeswarm with stats', function() {
        set.seed(42)
        plotgroups(list(data, data, data), names, colors, legend.text, plot.type=plotgroups.beeswarm, ylab=ylab, features=list(c('mean', 'ci'), c('median', 'box'), c('mean', 'sd')), plot.fun.pars=list(palpha=0.5, bxpcols='black'))
    })
    expect_doppelganger('beeswarm with stats and params', function() {
        set.seed(42)
        plotgroups(list(data, data, data), names, colors, legend.text, plot.type=plotgroups.beeswarm, ylab=ylab, features=list(c('mean', 'ci'), c('median', 'box'), c('mean', 'sd')), plot.fun.pars=list(list(palpha=0.5, bxpcols='black'), list(method='hex', priority='ascending', bxpcols='red'), list(corral='gutter', bxpcols=rep_len(c('black', 'red'), length(data)))))
    })
})

test_that('barplot', {
    expect_doppelganger('pure barplot', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plotgroups.barplot, ylab=ylab, features=c('mean'))
    })
    expect_doppelganger('barplot with error bars', function() {
        plotgroups(data, names, colors, legend.text, plot.type=plotgroups.barplot, ylab=ylab, features=c('mean', 'ci'), plot.fun.pars=list(whiskerswidth=0.6, whiskerslwd=3, whiskerslty='21', bordercol=rep_len(c('red', 'yellow'), length(data))))
    })
})
