#' Generate spatial content for Diatech
#'
#' This function will generate the maps and spatial content used in Diatech
#'
#' @param set list containing the data to create the polygons, see examples.
#' @param bounds  Boundary matrix, see examples.
#'
#' @export
#' @import sp rgeos maptools spdep methods
#' @examples
#'
#' data(polydata)
#'
#' ## bourciez bounded
#' set <- polydata$bourciez$bourciez
#' names(set) <- c('id', 'x', 'y')
#' bounds <- polydata$bourciez$bourciezBounds
#' bourciezMap <- generateMaps(set, bounds)
#'
#' ## dialeb Bounded
#' set <- polydata$dialeb$dialeb
#' names(set) <- c('id', 'x', 'y')
#' bounds <- polydata$dialeb$dialebBounds
#' bounds <- polydata$dialeb$dialebBounds[c(3, 2, 1, 4, 5, 6, 7, 8)]
#' dialebMap <- generateMaps(set, bounds)
#'
#' ## bourciez non bounded
#' set <- polydata$bourciez$bourciez
#' names(set) <- c('id', 'x', 'y')
#' bourciezMap <- generateMaps(set)
#'
generateMaps <- function(set, bounds){
    incorrectNames <- !identical(names(set), c('id', 'x', 'y'))
    if (incorrectNames) stop ('The names of "set" must be c("id", "x", "y")')
    if (missing(bounds)){ ## not bounded
        return (generateVoronoi(set))
    } else { ## bounded
        ## polygons
        polygons <- vector(mode = 'list', length(bounds))
        honeycombs <- vector(mode = 'list', length(bounds))
        baricenters <- vector(mode = 'list', length(bounds))
        nbs <- vector(mode = 'list', length(bounds))
        for (counter in seq_along(bounds)){
            bound <- bounds[[counter]]
            names(bound) <- c('x', 'y')
            boundId <- paste0('bound_', counter)
            ## declare bounds
            boundPolys <- sp::SpatialPolygons(
                list(sp::Polygons(list(sp::Polygon(bound)), boundId)),
                proj4string = sp::CRS('+proj=merc'))
            
            boundLines <- sp::SpatialLines(
                list(sp::Lines(list(sp::Line(bound)), boundId)),
                proj4string = sp::CRS('+proj=merc'))
            
            ## obtain voronoi
            inPip <- splancs::inpip(set[c(2L, 3L)], bound)
            if (length(inPip) == 0L){
                warning ('One of the bounds has no locations inside')
                next
            }
            vs <- generateVoronoi(set[inPip, ], boundPolys, boundLines)
            polygons[[counter]] <- vs$polygon
            honeycombs[[counter]] <- vs$honeycomb
            baricenters[[counter]] <- vs$baricenter
            nbs[[counter]] <- vs$nb[sapply(vs$nb, length) > 0]
        }

        result <- list()
        result$polygon <- Reduce(maptools::spRbind, polygons)
        result$honeycomb <- Reduce(maptools::spRbind, honeycombs)
        result$baricenter <- Reduce(maptools::spRbind, baricenters)
        result$nb <- unlist(nbs, recursive = FALSE)
        
        return (result)
    }
}


do_voronoi <- function(mat, ids, bound){
    if (anyDuplicated(mat) != 0L) stop ('duplicated points in location coordinates')
    if (nrow(mat) == 1L & missing(bound) == FALSE){
        return (bound)
    }
    if (nrow(mat) == 1L) stop ('single point with no bounds')
    if (!missing(bound)){
        rw <- as.numeric(t(sp::bbox(bound)))
        tl <- try(deldir::tile.list(deldir::deldir(mat[, 1L], mat[, 2L], rw = rw)))
        if (class(tl) == 'try-error') {stop('deldir did not finish'); 0L}
        
    } else {
        tl <- deldir::tile.list(deldir::deldir(mat[, 1L], mat[, 2L]))
    }
    ## polygons
    polys <- vector(mode = 'list', length = length(tl))
    mats <- vector(mode = 'list', length = length(tl))
    for (nbr in seq_along(tl)){
        xy <- tl[[nbr]]
        p <- matrix(c(xy$x, xy$x[1L], xy$y, xy$y[1L]), length(xy$x) + 1L, 2L)
        mats[[nbr]] <- p
        polys[[nbr]] <- sp::Polygons(list(sp::Polygon(p)), ID = as.character(ids[nbr]))
    }
    spPolys <- sp::SpatialPolygons(polys, proj4string = sp::CRS('+proj=merc'))
    vdf <- data.frame(
        x = mat[,1],
        y = mat[,2],
        row.names = sapply(methods::slot(spPolys, 'polygons'), slot, 'ID'))
    v <- sp::SpatialPolygonsDataFrame(spPolys, data = vdf)

    if (!missing(bound)){
        v <- rgeos::gIntersection(
            as(v, 'SpatialPolygons'),
            as(bound, 'SpatialPolygons'),
            byid = TRUE,
            id = rownames(v@data))
        v <- as(v, "SpatialPolygonsDataFrame")
        v@data <- vdf
    }
    return (v)
}

splitSpatialPolygonsDataFrame <- function(polydf){
    polygonCount <- sapply(polydf@polygons, function(x) length(x@Polygons))
    mPolygonInd <- which(polygonCount > 1L)
    if (any(polygonCount > 1L)){
        leftovers <- vector('list', sum(polygonCount[mPolygonInd]) - length(mPolygonInd))
        counter <- 0L
        for (i in mPolygonInd){
            counter <- counter + 1L
            spPolySubset <- polydf[i, ]
            polygons <- spPolySubset@polygons[[1]]@Polygons
            baricenter <- sp::coordinates(spPolySubset)
            whereIsBaricenter <- sapply(polygons, function(x) splancs::inout(baricenter, x@coords))
            frames <- split(polygons, whereIsBaricenter)
            id <- spPolySubset@polygons[[1]]@ID
            polydf@polygons[i][[1]] <- sp::Polygons(frames[['TRUE']], spPolySubset@polygons[[1]]@ID)
            for (leftover in frames[['FALSE']]){
                leftoverId <- paste0('leftover_', counter, '__ID_', id)
                leftovers[[counter]] <- sp::Polygons(list(leftover), leftoverId)
            }
        }
        leftoverPolys <- sp::SpatialPolygons(leftovers, proj4string = sp::CRS('+proj=merc'))
        polydf <- maptools::spRbind(polydf, leftoverPolys)
        return (list(v = polydf, ids = sapply(leftoverPolys@polygons, function(x) x@ID)))
    }
    return (list(v = polydf, ids = character(0)))
}

integrateLeftovers <- function(polydf, ids){
    for (id in ids){
        nb <- spdep::poly2nb(polydf)
        pos <- match(id, attr(nb, 'region.id'))
        polyNbs <- nb[[pos]]
        polyNbsIds <- attr(nb, 'region.id')[polyNbs]
        polysSubset <- polydf[c(pos, nb[[pos]]), ]
        boundaryFrame <- rgeos::gUnionCascaded(polydf[nb[[pos]], ])
        boundaryPoints <- boundaryFrame@polygons[[1]]@Polygons[[1]]@coords
        if (length(nb[[pos]]) == 1L) boundName <- polyNbsIds  else boundName <- paste0('bound_', id)
        bounds <- sp::SpatialPolygons(
            list(sp::Polygons(list(sp::Polygon(boundaryPoints)), boundName)),
            proj4string = sp::CRS('+proj=merc'))
        newPolygons <- do_voronoi(sp::coordinates(polydf[nb[[pos]], ]), polyNbsIds, bounds)
        polydf@polygons[nb[[pos]]] <- newPolygons@polygons
    }
    allIds <- sapply(polydf@polygons, slot, 'ID')
    return (polydf[which(!(allIds %in% ids)), ])
}


generateVoronoi <- function(mat, bound, boundl){
    if (missing(bound) != missing(boundl)) stop ('Either both bounds are missing or none') 
    ids <- as.character(mat$id)
    mat <- as.matrix(mat[2:3])
    v <- do_voronoi(mat, ids, bound)
    vlist <- splitSpatialPolygonsDataFrame(v)
    v <- vlist$v
    leftoverIds <- vlist$ids
    mats <- lapply(v@polygons, function(x) x@Polygons[[1]]@coords)
    if (length(leftoverIds) > 0L)
        v <- integrateLeftovers(v, leftoverIds)
    vdf <- data.frame(
        x = 1L:length(v),
        row.names = sapply(methods::slot(v, 'polygons'), slot, 'ID'))
    v <- sp::SpatialPolygonsDataFrame(v, data = vdf)
    ## lines for honeycomb maps
    coords <- sp::coordinates(v)
    if (nrow(coords) == 1){
        v2 <- as(boundl, 'SpatialLinesDataFrame')
        vdf <- data.frame(empty = '', row.names = boundl@lines[[1]]@ID)
        v2@data <- vdf
        return (list(polygon = v, honeycomb = v2, baricenter = v2))
    }
    nb <- spdep::poly2nb(v)
    vl <- as(v, 'SpatialLinesDataFrame')
    lines2 <- lines <- vector(mode = 'list', length = nrow(coords))
    coords <- sp::coordinates(v)
    
    for (nbr in seq_along(nb)){
        neighNbr <- nb[[nbr]][nb[[nbr]] > nbr]
        nb[[nbr]] <- ids[neighNbr]
        if (length(neighNbr) == 0) next
        lines2[[nbr]] <- lines[[nbr]] <- vector(mode = 'list', length = length(neighNbr))
        for (nbr2 in seq_along(neighNbr)){
            z <- v[c(nbr, neighNbr[nbr2]), ]
            id <- paste(ids[nbr], ids[neighNbr][nbr2], sep = '_')
            x <- rgeos::gBuffer(vl[nbr, ], width = 1e-4)@polygons[[1]]@Polygons[[1]]@coords
            y <- vl[neighNbr[nbr2], ]@lines[[1]]@Lines[[1]]@coords
            x2 <- rgeos::gBuffer(vl[neighNbr[nbr2], ], width = 1e-4)@polygons[[1]]@Polygons[[1]]@coords
            y2 <- vl[nbr, ]@lines[[1]]@Lines[[1]]@coords
            intersection <- unique(rbind(y[splancs::inpip(y, x), ], y2[splancs::inpip(y2, x2), ]))
            lines[[nbr]][[nbr2]] <- sp::Lines(Line(intersection), ID = id)
            centroids <- sp::coordinates(v[c(nbr, neighNbr[nbr2]), ])
            lines2[[nbr]][[nbr2]] <- sp::Lines(list(sp::Line(centroids)), ID = id)
        }
    }
    makeLineMap <- function(lines, bound, boundl){
        lines <- unlist(lines, recursive = FALSE)
        lines <- lines[!is.null(lines)]
        spLines <- sp::SpatialLines(lines, proj4string = sp::CRS('+proj=merc'))
        ids <- sapply(lines, slot, 'ID')
        vdf <- data.frame(
            do.call(rbind, strsplit(ids, ' ')), 
            row.names = ids)
        v <- sp::SpatialLinesDataFrame(spLines, vdf)
        
        if (!missing(bound)){
            v <- rgeos::gIntersection(
                as(bound, 'SpatialPolygons'),
                as(v, 'SpatialLines'),
                byid = TRUE,
                id = ids,
                drop_lower_td = TRUE)
            vids <- sapply(v@lines, slot, 'ID')
            v <- maptools::spRbind(boundl, v)
            v <- as(v, "SpatialLinesDataFrame")
            vdf <- data.frame(
                empty = rep('', length(vids) + 1),
                row.names = c(boundl@lines[[1]]@ID, vids))
            v@data <- vdf
        }
        return (v)
    }
    
    v2 <- makeLineMap(lines, bound, boundl)
    v3 <- makeLineMap(lines2, bound, boundl)
    names(nb) <- ids
    
    return (list(polygon = v, honeycomb = v2, baricenter = v3, nb = nb))
}


#' Write generated maps
#'
#' Write generated maps
#'
#' @param map resulting from generateMaps function.
#' @param neighPath path where the binary data for neighbours will be saved.
#' @param jpegPath path where the jpeg map will be saved.
#' @param polySvgPath path where the svg with the Polygon map will be saved.
#' @param centSvgPath path where the svg with the Centroid map will be saved.
#' @param honeSvgPath path where the svg witht he Honecomb map will be saved.
#'
#' @export
#'
#' @import gridSVG grid sp
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
#' names(set) <- c('id', 'x', 'y')
#' map <- generateMaps(set, bounds)
#' writeMaps(map, 'nb.rda', 'map.jpeg', 'polygon.svg', 'cetroid.svg', 'honeycomb.svg')
#' 
#' ## dialeb Bounded
#' set <- polydata$dialeb$dialeb
#' names(set) <- c('id', 'x', 'y')
#' bounds <- polydata$dialeb$dialebBounds
#' map <- generateMaps(set, bounds)
#' writeMaps(map, 'nb.rda', 'map.jpeg', 'polygon.svg', 'cetroid.svg', 'honeycomb.svg')
#'
#' ## End(**Not run**)
#' }
#' 
writeMaps <- function(map, neighPath, jpegPath, polySvgPath, centSvgPath, honeSvgPath){

    ## neighbour
    nb <- map$nb
    save(list = 'nb', file = neighPath)

    ## jpeg
    jpeg(filename = jpegPath, width = 1600, height = 1200, quality = 90)
    plot(map$polygon)
    dev.off()
    
    ## polygon svg
    p <- sp::spplot(
        map$polygon,
        'x',
        colorkey = FALSE,
        col.regions = 'white',
        par.settings = list(axis.line = list(col = 'transparent')))
    
    print(p)
    grobs <- grid::grid.ls(print = FALSE)
    gpaths <- grep('^GRID\\.pathgrob\\.[0-9]+$', grobs$name, value = TRUE)
    rn <- rownames(map$polygon@data)
    plotOrder <- map$polygon@plotOrder
    counter <- 0L
    for (gpath in gpaths){
        counter <- counter + 1L
        gridSVG::grid.garnish(gpath, id = rn[plotOrder[counter]])
    }
    gridSVG::grid.export(polySvgPath)
    dev.off()
    
    ## line maps (honeycomb and centroid)
    myLinesPanel <- deparse(sp::panel.polygonsplot)
    myLinesPanel <- sub('lty = lty,', 'lty = lty, identifier = as.character(i),',
                        myLinesPanel, fixed = TRUE)
    panel <- eval(
        parse(text = myLinesPanel),
        envir = environment(sp::panel.polygonsplot))
    
    plotLinesMaps <- function(map, path){
        p <- sp::spplot(
            map,
            colorkey = FALSE,
            col.regions = 'black',
            par.settings = list(axis.line = list(col = 'transparent')),
            panel = panel)
        print(p)
        grobs <- grid::grid.ls(print = FALSE)
        dt <- map@data
        ids <- rownames(dt)
        greped <- regexpr('plot_01\\.[0-9]+', grobs$name)
        regmatched <- regmatches(grobs$name, greped)
        datarow <- as.numeric(substr(regmatched, nchar('plot_01.') + 1, nchar(regmatched)))
        names2change <- grobs$name[greped != -1]
        
        for (counter in seq_along(names2change)){
            id <- ids[datarow[counter]]
            gridSVG::grid.garnish(names2change[counter], id = id)

            if (grepl('^[0-9]+_[0-9]+$', names2change[counter])){
                indIds <- strsplit(id, '_')[[1]]
                gridSVG::grid.garnish(names2change[counter], id1 = indIds[1])
                gridSVG::grid.garnish(names2change[counter], id2 = indIds[2])
            }
        }
        
        ## change the warning value for grid export
        options(warn = -1)
        gridSVG::grid.export(path)
        options(warn = 1)
        dev.off()
        return (TRUE)
    }
    
    plotLinesMaps(map$baricenter, centSvgPath)
    plotLinesMaps(map$honeycomb, honeSvgPath)

    return (invisible(TRUE))
}



#' Read Spatial Data
#'
#' Reads spatial data from the Diatech database
#'
#' @param con Connection to the Diatech database.
#' @param proId Project Id from which to be taken the data.
#'
#' @export
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
