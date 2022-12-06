

#' @export
`cards` <- function (jokers = FALSE, makespace = FALSE){
    x <- c(2:10, "J", "Q", "K", "A")
    y <- c("Club", "Diamond", "Heart", "Spade")
    res <- expand.grid(rank = x, suit = y)
    if (jokers) {
        levels(res$rank) <- c(levels(res$rank), "Joker")
        res <- rbind(res, data.frame(rank = c("Joker", "Joker"), 
            suit = c(NA, NA)))
    }
    if (makespace) {
        res$probs <- rep(1, dim(res)[1])/dim(res)[1]
    }
    return(res)
}

#' @export
`euchredeck` <- function (benny = FALSE, makespace = FALSE){
    x <- c(9:10, "J", "Q", "K", "A")
    y <- c("Club", "Diamond", "Heart", "Spade")
    res <- expand.grid(value = x, suit = y)
    if (benny) {
        levels(res$value) <- c(levels(res$value), "Joker")
        res <- rbind(res, data.frame(value = c("Joker"), suit = NA))
    }
    if (makespace) {
        res$probs <- rep(1, dim(res)[1])/dim(res)[1]
    }
    return(res)
}


#' @export
`rolldie` <- function (times, nsides = 6, makespace = FALSE){
    temp = list()
    for (i in 1:times) {
        temp[[i]] <- 1:nsides
    }
    res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
    names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
    if (makespace) 
        res$probs <- rep(1, nsides^times)/nsides^times
    return(res)
}


#' @export
`roulette` <- function (european = FALSE, makespace = FALSE){
    if (european) {
        num = c("0", "26", "3", "35", "12", "28", "7", "29", 
            "18", "22", "9", "31", "14", "20", "1", "33", "16", 
            "24", "5", "10", "23", "8", "30", "11", "36", "13", 
            "27", "6", "34", "17", "25", "2", "21", "4", "19", 
            "15", "32")
        color <- c("Green", rep(c("Black", "Red"), 18))
    }
    else {
        num = c("27", "10", "25", "29", "12", "8", "19", "31", 
            "18", "6", "21", "33", "16", "4", "23", "35", "14", 
            "2", "0", "28", "9", "26", "30", "11", "7", "20", 
            "32", "17", "5", "22", "34", "15", "3", "24", "36", 
            "13", "1", "00")
        color <- c(rep(c("Red", "Black"), 9), "Green", rep(c("Black", 
            "Red"), 9), "Green")
    }
    res <- data.frame(num = num, color = color)
    if (makespace) {
        res$probs <- rep(1, length(num))/length(num)
    }
    return(res)
}


#' Tossing a coin
#' 
#' Sets up a sample space for the experiment of tossing a coin repeatedly with the outcomes "H" or "T".
#' 
#' The function uses `expand.grid()` to generate all possible sequences of flips resulting from 
#' the experiment of tossing a coin.  Columns of the dataframe are denoted `toss1`, 
#' `toss2`, up to `tosstimes`.
#' @param times number of tosses.
#' @param makespace logical.
#' @return  A data frame, with an equally likely `probs` column if `makespace` is `TRUE`.
#' @examples
#' tosscoin(2)
#' tosscoin(3, makespace = TRUE)
#' @export
`tosscoin` <- function (times, makespace = FALSE){
    temp <- list()
    for (i in 1:times) {
        temp[[i]] <- c("H", "T")
    }
    res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
    names(res) <- c(paste(rep("toss", times), 1:times, sep = ""))
    if (makespace) 
        res$probs <- rep(1, 2^times)/2^times
    return(res)
}


#' Sampling from urns
#' 
#' This function creates a sample space associated with the experiment of sampling 
#' distinguishable objects from an urn.
#' 
#' The function operates on the indices of the urn (or rows, in the case `urn` 
#' is a data frame).  It then takes those samples and substitutes back into `urn` 
#' to generate the entries of the data frame (or list, respectively).  In the 
#' case that `urn` has repeated values, the result will be repeated values 
#' in the output.
#' 
#' Note that `urnsamples` strips `x` of any existing `probs` 
#' column before sampling.
#' @param x a vector or data frame from which sampling should take place.
#' @param size number indicating the sample size.
#' @param replace logical indicating whether sampling should be done with replacement.
#' @param ordered logical indicating whether order among samples is important.
#' @param ... further arguments to be passed to or from other methods.
#' @return A data frame if `urn` is a vector, and a list if `urn` is a data frame.
#' @examples
#' urnsamples(1:10, size = 5)
#' S <- cards()
#' urnsamples(S, size = 2)
#' @export
`urnsamples` <- function (x, ...)
UseMethod("urnsamples")


#' @method urnsamples data.frame
#' @export
`urnsamples.data.frame` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
    nurn <- dim(x)[1]
    if (isTRUE(replace)) {
        if (isTRUE(ordered)) {
            temp <- list()
            for (i in 1:size) {
                temp[[i]] <- 1:nurn
            }
            ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
        }
        else {
            temp <- list()
            for (i in 1:size) {
                temp[[i]] <- 1:nurn
            }
            res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
            ind <- t(unique(t(apply(res, 1, sort))))
        }
    }
    else {
        if (size > nurn) 
            stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
        if (isTRUE(ordered)) {
            ind <- permsn(1:nurn, size)
        }
        else {
            ind <- combn(1:nurn, size)
        }
    }
    if (!is.null(x$probs)) 
        x$probs <- NULL
    nss <- dim(ind)[2]
    out <- list()
    for (i in 1:nss) {
        out[[i]] <- x[ind[, i], ]
    }
    return(out)
}


#' @method urnsamples default
#' @export
`urnsamples.default` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
    nurn <- length(x)
    if (isTRUE(replace)) {
        if (isTRUE(ordered)) {
            temp = list()
            for (i in 1:size) {
                temp[[i]] <- 1:nurn
            }
            ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
        }
        else {
            temp = list()
            for (i in 1:size) {
                temp[[i]] <- 1:nurn
            }
            res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
            ind <- t(unique(t(apply(res, 1, sort))))
        }
    }
    else {
        if (size > nurn) 
            stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
        if (isTRUE(ordered)) {
            ind <- permsn(1:nurn, size)
        }
        else {
            ind <- combn(1:nurn, size)
        }
    }
    nss <- dim(ind)[2]
    out <- matrix(nrow = nss, ncol = size)
    for (i in 1:nss) {
        out[i, ] <- x[ind[, i]]
    }
    return(data.frame(out))
}
