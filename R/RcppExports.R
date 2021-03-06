# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

getMedians <- function(x, wt, idx) {
    .Call('_rpart_LAD_getMedians', PACKAGE = 'rpart.LAD', x, wt, idx)
}

getGoodnessOMP <- function(x, wt, medians) {
    .Call('_rpart_LAD_getGoodnessOMP', PACKAGE = 'rpart.LAD', x, wt, medians)
}

getGoodness <- function(x, wt, medians) {
    .Call('_rpart_LAD_getGoodness', PACKAGE = 'rpart.LAD', x, wt, medians)
}

sort_index <- function(x) {
    .Call('_rpart_LAD_sort_index', PACKAGE = 'rpart.LAD', x)
}

wmedian <- function(x, weight) {
    .Call('_rpart_LAD_wmedian', PACKAGE = 'rpart.LAD', x, weight)
}

