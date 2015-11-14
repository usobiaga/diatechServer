#' Write generated maps
#'
#' Write generated maps
#'
#' @import gridSVG grid
#'
#' @examples
#'
#' \dontrun{ ## Not run:
#' 
#' data(polydata)
#'
#' ## bourciez bounded
#' set <- polydata$bourciez$bourciez
#' bounds <- polydata$bourciez$bourciezBounds
#' map <- generateMaps(set, bounds)
#' writeSVGmaps(map, 'map.jpeg')
#'
#'
#' ## End(**Not run**)
#' }
#' 
writeMaps <- function(map, jpegPath, polySvgPath){

    ## jpeg
    jpeg(filename = jpegPath, width = 1600, height = 1200, quality = 90)
    plot(map, usePolypath = FALSE)
    dev.off()

    ## polygon svg
    spplot(
        map,
        colorkey = FALSE,
        col.regions = 'white',
        par.settings = list(axis.line = list(col = 'transparent')))
    grobs <- grid.ls()
    gpaths <- grep('^GRID\\.pathgrob\\.[0-9]+$', grobs$name, value = TRUE)
    rn <- rownames(map@data)
    counter <- 0L
    for (gpath in gpaths){
        counter <- counter + 1L
        grid.garnish(gpath, id = rn[counter])
    }
    grid.export(polySvgPath)
}


#' Generate spatial content for Diatech
#'
#' This function will generate the maps and spatial content used in Diatech
#'
#' @param set list containing the data to create the polygons
#' @param bounded  is there a boundary to clip the polygons
#'
#' @import sp rgeos maptools
#'
#' @examples
#'
#' data(polydata)
#'
#' ## bourciez bounded
#' set <- polydata$bourciez$bourciez
#' bounds <- polydata$bourciez$bourciezBounds
#' bourciezMap <- generateMaps(set, bounds)
#'
#' ## dialeb Bounded
#' set <- polydata$dialeb$dialeb
#' bounds <- polydata$dialeb$dialebBounds
#' dialebMap <- generateMaps(set, bounds)
#'
#' ## bourciez non bounded
#' set <- polydata$bourciez$bourciez
#' bourciezMap <- generateMaps(set)
#'
generateMaps <- function(set, bounds){
    names(set) <- c('id', 'x', 'y')
    if (missing(bounds)){ ## not bounded
        polygonMap <- generateVoronoiPolygons(set)
    } else { ## bounded
        spdt <- vector(mode = 'list', length(bounds))
        counter <- 0L
        for (bound in bounds){
            counter <- counter + 1L
            names(bound) <- c('x', 'y')
            boundPolys <- SpatialPolygons(
                list(Polygons(list(Polygon(bound)), 1L)),
                proj4string = CRS('+proj=merc'))
            inPip <- splancs::inpip(set[c(2L, 3L)], bound)
            v <- generateVoronoiPolygons(set[inPip, ], boundPolys)
            idsOut <- sapply(slot(v, "polygons"), slot, name = "ID")
            v <- rgeos::gIntersection(v, boundPolys, byid = TRUE, id = idsOut)
            spdt[[counter]] <- as(v, "SpatialPolygonsDataFrame")
        }
        polygonMap <- Reduce(maptools::spRbind, spdt)
    }
    return (polygonMap)
}

#' Apply deldir for polygons
#'
#' @import deldir sp
generateVoronoiPolygons <- function(mat, bound){
    ids <- as.character(mat$id)
    mat <- as.matrix(mat[c(2, 3)])
    if (!missing(bound)){
        rw <- as.numeric(t(bbox(bound)))
        tl <- tile.list(deldir::deldir(mat[, 1], mat[, 2], rw = rw))
    } else {
        tl <- tile.list(deldir::deldir(mat[, 1], mat[, 2]))
    }
    polys <- vector(mode = 'list', length = length(tl))
    for (nbr in seq_along(tl)){
        xy <- tl[[nbr]]
        p <- matrix(c(xy$x, xy$x[1], xy$y, xy$y[1]), length(xy$x) + 1, 2)
        polys[[nbr]] <- sp::Polygons(list(sp::Polygon(p)), ID = as.character(ids[nbr]))
    }
    spPolys <- sp::SpatialPolygons(polys, proj4string = CRS('+proj=merc'))
    v <- SpatialPolygonsDataFrame(spPolys,
                                  data = data.frame(
                                      x = mat[,1],
                                      y = mat[,2],
                                      row.names = sapply(slot(spPolys, 'polygons'), slot, 'ID')))
    return (v)
}


#' Read Spatial Data
#'
#' Reads spatial data from the Diatech database
#'
#' @param con Connection to the Diatech database.
#' @param proId Project Id from which to be taken the data.
#'
#' @import RMySQL
#'
#' @examples
#'
#' \dontrun{
#' ## Not run:
#' 
#' con <- dbConnect(dbDriver("MySQL"), user = 'user', password = 'password', 'diatech')
#' tb <- readSpatialDataset(con, '179')
#' 
#' ## End(**Not run**)
#' }
#' 
readSpatialDataset <- function(con, proId){
    RMySQL::dbGetQuery(con, "set names 'utf8'")
    spatialquery <- paste0('SELECT id, longitude, latitude from answers_location where project_id = ', proId)
    RMySQL::dbGetQuery(con,spatialquery)
}
