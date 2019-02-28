#' Bouricez dataset
#'
#' Bourciez dataset to be used in the examples.
#' List containing an object of class diaLongLat and a object containing the neighbours.
#'
#' @name bourciezDiaData
#' @docType data
NULL

##--
## Measures
##--

#' Obtain get the measures
#'
#' Applies certain ad-hoc measures used in Dialektometrics.
#'
#' @param set object for method to be applied
#' @param measure measure to be used
#' @param ... arguments passing to other methods
#' 
#' @export
#'
getMeasures <- function(set, measure, ...)  UseMethod('getMeasures')

#' Get geographical distance measures
#'
#' Get geographical distance measures
#'
#' @param set object of class diaGeoDistance
#' @param labelsCol column name of set that will be used as labels
#' @param ... arguments passing to other methods
#'
#' @import sp diaMeasures
#' @export
#'
#' @examples
#'
#' data(bourciezDiaData)
#' set <- bourciezDiaData$lonlat
#' measure <- 'euclidean'
#' 
#' allEuclideanDistances <- getMeasures(set, 'euclidean')
#'
getMeasures.diaLongLat <- function(set, labelsCol = 'id', ...){
    mat <- sp::spDists(as.matrix(set[, c('longitude', 'latitude')]))
    colnames(mat) <- rownames(mat) <- as.character(set[[labelsCol]])
    dm <- diaMeasures::as.diaMeasure(mat)
    return (dm)
}

#' Get linguistic distance measures
#'
#' Get linguistic distance measures
#'
#' @param set object of class diaGeoDistance
#' @param measure linguistic distance or similarity index to be used
#' @param binaryIndex binary index to be used for multiple answers
#' @param ... arguments passing to other methods
#'
#' @import sp diaMeasures
#' @export
#'
#' @examples
#'
#' data(bourciezDiaData)
#' set <- bourciezDiaData$lingdata
#' 
#' result <- getMeasures(
#'   set, formula = id ~ question, value.var = 'answer', measure = 'iri', binaryIndex = 'dice')
#'
getMeasures.diaLingData <- function(set, ...)
    diaMeasures::diaMeasure(set,  ...)

##--
## Statistics
##--

#' Compute Statitics.
#'
#' Get statistics related to the Diatech project
#'
#' @param measure object to pass to methods
#' @param stat statistic to be calculated
#' @param ... other arguments passing to methods
#'
#' @export
#' 
getStatistic <- function(measure, stat, ...) UseMethod('getStatistic')


#' Compute Statistics
#'
#' Get statistics for diaMeasures.
#'
#' @param measure object of class diaMeasures.
#' @param stat statistic to be calculated.
#' @param measure2 another object of class diaMeasures used for the correlation statistic.
#' @param ... other arguments passing to methods (currently unused)
#' @export
#' @examples
#'
#' data(bourciezDiaData)
#' set <- bourciezDiaData$lonlat
#' measure <- getMeasures(set, 'euclidean')
#'
#' sd <- getStatistic(measure, 'sd')
#' minm <- getStatistic(measure, 'min')
#' maxm <- getStatistic(measure, 'max')
#' medianm <- getStatistic(measure, 'median')
#' skewness <- getStatistic(measure, 'skewness')
#' correlation <- getStatistic(measure, 'cor', measure)
#' 
getStatistic.diaMeasure <- function(measure, stat = c('sd', 'min', 'max', 'median', 'skewness', 'cor'), measure2, ...){

    skewness <- function(x, na.rm = TRUE){
        if (na.rm) x <- x[!is.na(x)]
        n <- length(x); mn <- mean(x)
        return ((sum((x - mn)^3) / n)/(sum((x - mn)^2) / n)^(3 / 2))
    }
    removeDiag <- function(x){
        d <- ncol(x)
        return (matrix(x[row(x) != col(x)], d - 1L, d, FALSE))
    }

    fun <- match.arg(stat)
    m <- removeDiag(as.matrix(measure))
    
    if (fun == 'cor'){
        if (missing(measure2)) stop ('For correlation a second measure has to be provided')
        m2 <- removeDiag(as.matrix(measure2))
        sameNames <- identical(colnames(m), colnames(m2))
        if (sameNames == FALSE) stop ('linguistical variables not equal')
        d <- ncol(m2)
        result <- vector('numeric', d)
        for (i in 1L:d)
            result[i] <- cor(m[, i], m2[, i], "pairwise.complete.obs", "pearson")
    } else {
        result <- apply(m, 2, fun)
    }

    attributes(result) <- attributes(measure)
    class(result) <- 'diaStatistic'
    return (result)
}

##--
## get Classification
##--

#' Compute Classifications
#'
#' Apply classification methods related to the Diatech project.
#'
#' @param measure object to pass to other methods
#' @param ... other arguments passing to methods
#'
#' @export
#' 
getClassification <- function(measure,  ...) UseMethod('getClassification')


#' Compute Classifications
#'
#' Apply classification methods related to the Diatech project.
#'
#' @param measure object to pass to other methods.
#' @param type type of classification requried.
#' @param method classification method to be used: Med, MinMwMax, MedMw.
#' @param ids ids from the measure for which the classification method must be applied.
#' @param ns integer vector indicating in how many groups must the classification happen.
#' @param nb optional neighbourhood object returned from 'generateMaps'.
#' @param ... unused
#'
#' @import diaMeasures
#'
#' @export
#'
#' @examples
#'
#' library(diaMeasures)
#' data(bourciezDiaData)
#'
#' set <- bourciezDiaData$lonlat
#' labels <- set[c('id', 'location')]
#' measure <- getMeasures(set, 'euclidean')
#'
#' result <- getClassification(measure, 'classic', 'Med', '107', 2:4)
#'
#' result2 <- getClassification(measure, 'classic', 'Med', '107', 2:4, bourciezDiaData$nb)
#'
getClassification.diaMeasure <- function(measure, type = c('classic'), method, ids, ns, nb, ...){
    type <- match.arg(type)
    if (type == 'classic'){
        if (missing(nb)){
            classList <- lapply(
                ns,
                diaMeasures::diaClassification,
                measure = measure,
                method = method,
                ids = ids)
        } else {
            mat <- as.matrix(measure)
            t1 <- rep(names(nb), sapply(nb, length))
            t2 <- unlist(nb)
            m1 <- match(t1, rownames(mat))
            m2 <- match(t2, colnames(mat))
            matSubset <- mat[(m1 - 1L) * nrow(mat) + m2]
            names(matSubset) <- paste(
                rownames(mat)[m1],
                colnames(mat)[m2],
                sep = '_')
            classList <- lapply(ns, function(n){
                                    m <- diaMeasures::diaClassification(
                                        measure =  matSubset,
                                        method = method,
                                        n = n)
                                    names(m) <- paste(
                                        rownames(mat)[m1],
                                        colnames(mat)[m2],
                                        sep = '_')
                                    attr(m, 'range') <- seq(1, n)
                                    return (m)
                                    })
            attr(classList, 'Labels') <- attr(measure, 'Labels')
            attr(classList, 'Values') <- matSubset
        }
        class(classList) <- 'diaClassificationList'
        return (classList)
    }
}

#' @export
getClassification.diaStatistic <- function(statistic, type = c('classic'), method, ns, ...){
    type <- match.arg(type)
    if (type == 'classic'){
        class(statistic) <- 'numeric'
        classList <- lapply(ns, diaMeasures::diaClassification, measure = statistic, method = method)
        attributes(classList) <- attributes(statistic)
    }
    class(classList) <- 'diaStatClassificationList'
    attr(classList, 'range') <- ns
    return (classList)
}
