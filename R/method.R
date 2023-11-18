# init
iLAD <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste0("  median=", format(signif(yval, digits)),
          ", LAD=" , format(signif(dev/wt, digits)))
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}

# eval
eLAD <- function(y, wt, parms) {
  wmed <- wmedian(y, wt)
  rt <- sum(wt * abs(y - wmed))
  list(label = wmed, deviance = rt)
}

# split
sLAD <- function(y, wt, x, parms, continuous) {
  n <- length(y)

  if (continuous) {
    sy <- sort_index(y) + 1
    y.sorted <- y[sy]
    wt.sorted <- wt[sy]
    sy.i <- sort_index(sy) + 1
    medians <- getMedians(y.sorted, wt.sorted, sy.i)
    if (n < 100) {
      goodness <- getGoodness(y, wt, medians)
    } else {
      goodness <- getGoodnessOMP(y, wt, medians)
    }
    direction <- sign(medians[1:{n - 1}] - medians[2:n + n - 2])
  } else {
    # Categorical X variable
    ux <- sort(unique(x))
    medians <- sapply(ux, function(idx) {
      filter <- (x == idx)
      wmedian(y[filter], wt[filter])
    })

    # For anova splits, we can order the categories by their means
    #  then use the same code as for a non-categorical
    ord <- order(medians)
    n <- length(ord)
    ux.ord <- ux[ord]
    filters <- lapply(1:n, function(i) {x == ux.ord[i]})

    lmedian <- sum(wt[filters[[1]]]*abs(y[filters[[1]]] - medians[ord[1]]))
    rmedian <- sum(wt[filters[[n]]]*abs(y[filters[[n]]] - medians[ord[n]]))
    if (n > 2) {
      lmedian <- c(lmedian, sapply(2:(n - 1), function(pos) {
        filter <- (rowSums(do.call("cbind", filters[1:pos])) > 0)
        sum(wt[filter]*abs(y[filter] - wmedian(y[filter], wt[filter])))
      }))
      rmedian <- c(sapply(2:(n - 1), function(pos) {
        filter <- (rowSums(do.call("cbind", filters[pos:n])) > 0) #filter <- x %in% ux.ord[pos:n]
        sum(wt[filter]*abs(y[filter] - wmedian(y[filter], wt[filter])))
      }), rmedian)
    }

    goodness <- (lmedian + rmedian) / sum(wt)
    direction <- ux.ord
  }
  goodness <- max(goodness) - goodness

  list(goodness = goodness, direction = direction)
}

# text
tLAD <- function(yval, dev, wt, ylevel, digits, n, use.n) {
  if (use.n)
    paste0(formatg(yval, digits), "\nn=", n)
  else formatg(yval, digits)
}


#' 'rpart'-method: List of required functions for inducing 'rpart'-like LAD regression trees
#' 
#' @export
#' 
#' @examples
#'   mystate <- data.frame(state.x77, region = state.region)
#'   names(mystate) <- casefold(names(mystate)) #remove mixed case
#'
#'   fit <-  rpart(murder ~ ., data = mystate, minsplit = 10, method = LAD)
#'   plot(fit); text(fit)
#'
LAD <- list(eval = eLAD, split = sLAD, init = iLAD, text = tLAD)
