
#' Turn object to diatech JSON
#'
#' Turn object to diatech JSON. See specific method
#'
#' @param x object to be turned into JSON
#' @param ... other paramters passing to methods
#' 
as.diaJson <- function(x, ...) UseMethod('as.diaJson')
    
#' Turn object to diatech JSON
#'
#' Turn object to diatech JSON. See specific method
#'
#' @param x object to be turned into JSON
#' @param ... other paramters passing to methods
#'
#' @import rjson
#'
#' @examples
#'
#' library(diaMeasures)
#' data(bourciezDiaData)
#'
#' set <- bourciezDiaData$lonlat
#' allEuclideanDistances <- getMeasures(set, 'euclidean')
#' classification <- diaClassification(allEuclideanDistances, 'Med', n = 6, ids = '107')
#' 
as.diaJson.diaClassification <- function(x, ...){
    rjson::toJSON(list('values' = x, 'range' = attr(x, 'range')))
}









