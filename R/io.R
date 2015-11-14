#' Prototype of load data
#'
#' This function loads data from the database. Its currently ineficients and has to be rewritten
#' with subqueries.
#' 
#' @export
#' 
#' @import DBI RMySQL
#'
#' @examples
#' 
#' ## sample call from server
#' \dontrun{
#'   set <- load('thedatabase', 'user', 'thepassword', '1', type = 'geodist')
#' }
#' 
load <- function(database, user, password, anstype, proid, linDomain, type = c('geodist', 'lingdata', 'lemmadata')){
    write('Reading data', stdout())
    requestType <- match.arg(type)
    on.exit(DBI::dbDisconnect(con))
    con <- DBI::dbConnect(DBI::dbDriver('MySQL'), user = user, password = password, database)
    DBI::dbGetQuery(con, 'SET NAMES "utf8"')
    if (requestType == 'geodist'){
        query <- paste0(
         'SELECT
               id,
               longitude,
               latitude
         FROM answers_location
         WHERE project_id = ', proid)
        result <- DBI::dbGetQuery(con, query)
        class(result) <- c('diaLongLat', 'data.frame')
    } else if (requestType == 'lingdata'){
        stop ('not programmed yet')
    } else { ## lemmadata
        stop ('not programmed yet')
    }
    DBI::dbDisconnect(con)
    on.exit(NULL)
    return (result)
}


