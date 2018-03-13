#' Plot Annotation
#'
#' This function can be used to add labels to a plot. Its main difference to \code{\link[graphics]{title}}
#' is that it allows to specify a different drawing position for each annotation. The defaults are the same
#' as for \code{\link[graphics]{title}}, but can be changed by a call to \code{\link[base]{options}}.
#'
#' @param main Main plot title.
#' @param sub Plot subtitle.
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param main.line The margin line for the main title. Defaults to \code{getOption('imisc.mgp.main')} or,
#'                  if the option is not set, 3.
#' @param sub.line The margin line for the sub-title. Defaults to \code{getOption('imisc.mgp.sub')} or,
#'                  if the option is not set, \code{par('mgp')[1] + 1}.
#' @param xlab.line The margin line for the X axis label. Defaults to \code{getOption('imisc.mgp.xlab')} or,
#'                  if the option is not set, \code{par('mgp')[1]}.
#' @param ylab.line The margin line for the Y axis label. Defaults to \code{getOption('imisc.mgp.xlab')} or,
#'                  if the option is not set, \code{par('mgp')[1]}.
#' @param use.default.line Use \code{\link[graphics]{title}} defaults instead of \code{getOption('imisc.mgp')},
#'                  even if the options are set.
#' @param ... Additional parameters passed to \code{\link[graphics]{title}}
#'
#' @export
#' @importFrom graphics par title

title <- function(main=NULL,
                  sub=NULL,
                  xlab=NULL,
                  ylab=NULL,
                  main.line=NULL,
                  sub.line=NULL,
                  xlab.line=NULL,
                  ylab.line=NULL,
                  use.default.line=FALSE,
                  ...)
{
    getline <- function(x) {
        env <- parent.frame()
        argname <- paste0(x, '.line')
        if (is.null(env[[argname]])) {
            globalmgp <- getOption(paste0('imisc.mgp.', x))
            if (use.default.line || is.null(globalmgp)) {
                if (x == 'main') {
                    3
                } else if (x == 'sub') {
                    par('mgp')[1] + 1
                } else {
                    par('mgp')[1]
                }
            } else {
                globalmgp
            }
        } else {
            env[[argname]]
        }
    }

    env <- environment()
    for (t in c('main', 'sub', 'xlab', 'ylab')) {
        if (!is.null(env[[t]])) {
            args <- list(...)
            args[[t]] <- env[[t]]
            args$line <- getline(t)
            do.call(graphics::title, args)
        }
    }
    invisible()
}

