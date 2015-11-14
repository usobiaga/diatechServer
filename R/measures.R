
#' Obtain get the measures
#'
#' Applies certain ad-hoc measures used in Dialektometrics. 
#'
#' The available classification methods are Med MinMwMax and MedMw.
#'
#' @import diaMeasures
#'
#' @export
#'
getMeasures <- function(set, measure, ...)  UseMethod('getMeasures')

#' Get geographical distance measures
#'
#' Get geographical distance measures
#'
#' @param set object of class diaGeoDistance
#' @param measure measure to be used
#' @param id numeric of length one indicating for which location id compute the distance. If missing all will be computed.
#'
#' @import sp
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
getMeasures.diaLongLat <- function(set, measure = 'euclidean'){
    mat <- sp::spDists(as.matrix(set[, c('longitude', 'latitude')]))
    colnames(mat) <- rownames(mat) <- as.character(set$id)
    dm <- as.diaMeasure(mat)
    return (dm)
}
