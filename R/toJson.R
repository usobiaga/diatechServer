
#' Turn object to diatech JSON
#'
#' Turn object to diatech JSON. See specific method
#'
#' @param x object to be turned into JSON
#' @param ... other paramters passing to methods
#'
#' @export
#' 
as.diaJson <- function(x, ...) UseMethod('as.diaJson')

    
#' Turn object to diatech JSON
#'
#' Turn object to diatech JSON. See specific method
#'
#' @param x object to be turned into JSON.
#' @param labels data.frame containg the id column and extra variables to be added to the JSON.
#' @param nb list with the neighbouring polygons. Gotten from the function generateMaps in this package.
#' @param ... other paramters passing to methods.
#'
#' @export
#' @import rjson
#'
#' @examples
#'
#' \dontrun{
#'
#' library(diaMeasures)
#' 
#' data(bourciezDiaData)
#'
#' set <- bourciezDiaData$lonlat
#'
#' measure <- getMeasures(set, 'euclidean')
#' classification <- getClassification(measure, 'classic', 'Med', '107', 2:4)
#' result <- as.diaJson(classification, measure)
#'
#' ## statistics
#'
#' measure <- getMeasures(set, 'euclidean')
#' stat <- getStatistic(measure, stat = 'sd')
#' classification <- getClassification(stat, 'classic', 'Med', 2:4)
#' json <- as.diaJson(classification, stat)
#'
#' ## with boundary
#'
#' classification <- getClassification(measure, 'classic', 'Med', '107', 2:4, nb = bourciezDiaData$nb)
#' json <- as.diaJson(classification, bourciezDiaData$nb)
#' 
#' }
#' 
as.diaJson.diaClassificationList <- function(cls, mes, nb, ...){
    
    if (missing(nb)){

        mes <- as.matrix(mes)
        id <- names(cls[[1]])
        if (length(id) > 1)
            stop ('If no nb argument is given only one reference location is used')
        density <- density(mes[, id])

        tojson <- lapply(cls, function(x){
                             id <- names(x)
                             if (length(id) > 1)
                                 stop ('If no nb argument is given only one reference location is used')
                             
                             ## histogram
                             breaks <- attr(x, 'breaks')
                             h <- diff(breaks)
                             counts <- table(x)
                             n <- length(x)
                             if (sum(counts) < n) 
                                 stop('error when creating histogram. breaks in does not span x')
                             dens <- counts / (n * h)
                             
                             ## make result
                             x[[id]][id] <- 0
                             result <- list(
                                 colors = x[[id]],
                                 histogramHeights = dens,
                                 histogramWidths = breaks)
                             return (result)
                         })
        
        names(tojson) <- sapply(cls, function(x) max(attr(x, 'range')))
        
        return (rjson::toJSON(c(tojson, kernel = list(x = density$x, y = density$y))))
        
    } else {
        
        tojson <- lapply(cls, function(x){
                             
                             ids <- unlist(strsplit(names(x), '_'))
                             ids1 <- ids[rep(c(TRUE, FALSE), length(ids) / 2L)]
                             ids2 <- ids[rep(c(FALSE, TRUE), length(ids) / 2L)]
                             
                             ## histogram
                             breaks <- attr(x, 'breaks')
                             h <- diff(breaks)
                             counts <- .Call(graphics:::C_BinCount, as.vector(attr(cls, 'mat')), breaks, TRUE, TRUE)
                             n <- length(x)
                             if (sum(counts) < n) 
                                 stop('error when creating histogram. breaks in does not span x')
                             dens <- counts / (n * h)

                             ## make result
                             list(
                                 id1 = ids1,
                                 id2 = ids2,
                                 color = x,
                                 histogramHeights = dens,
                                 histogramWidths = breaks)
                         })
        
        names(tojson) <- sapply(cls, function(x) max(attr(x, 'range')))
        return (rjson::toJSON(tojson))
    }
}


#' @export
#' @import rjson 
as.diaJson.diaStatClassificationList <- function(cls, stat, ...){
    
    tojson <- lapply(cls, function(x){
                         names(x) <- attr(stat, 'Labels')
                         
                         ## histogram
                         breaks <- attr(x, 'breaks')
                         h <- diff(breaks)
                         counts <- .Call(graphics:::C_BinCount, stat, breaks, TRUE, TRUE)
                         n <- length(x)
                         if (sum(counts) < n) 
                             stop('error when creating histogram. breaks in does not span x')
                         dens <- counts / (n * h)
                         
                         return (
                             list(
                                 colors = x,
                                 histogramHeights = dens,
                                 histogramWidths = breaks
                                 ))
                     })
    names(tojson) <- attr(cls, 'range')
    return (rjson::toJSON(tojson))
}

#' @export
#' @import rjson 
as.diaJson.hclust <- function(x, polygons, ...){
    
    
    tojson <- lapply(x, function(y){
                         names(y) <- attr(x, 'Labels')
                         matched <- match(labels$id, names(y))
                         if (anyNA(matched))
                             stop ('Ids in the given "labels" do not match with the ids of "x"')
                         return (Map(list,
                                     ## location = labels$location[matched],
                                     color = y,
                                     id = names(y)))
                     })
    names(tojson) <- attr(x, 'range')
    return (rjson::toJSON(tojson))
}


## histogram

#' Histogram breaks and density
#'
#' Returns the histogram breaks and  density estimation base on a diaClassification for the
#' graphical representation.
#'
histBreaks <- function(x){
    if (is.list(x)){
        breaks <- attr(x[[id]], 'breaks')
        h <- diff(breaks)
        counts <- .Call(graphics:::C_BinCount, mes[, id], breaks, TRUE, TRUE)
        n <- length(x[[id]])
        if (sum(counts) < n) 
            stop('error when creating histogram. breaks in does not span x')
        dens <- counts / (n * h)
        return (list(density = density, breaks = breaks))
    } else {
        breaks <- attr(x, 'breaks')
        h <- diff(breaks)
        counts <- .Call(graphics:::C_BinCount, mes[, id], breaks, TRUE, TRUE)
        n <- length(x)
        if (sum(counts) < n) 
            stop('error when creating histogram. breaks in does not span x')
        dens <- counts / (n * h)
    }
    
}
