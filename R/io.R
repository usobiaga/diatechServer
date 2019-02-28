#' Prototype of load data
#'
#' This function loads data from the database.
#'
#' @param database database name.
#' @param user database user.
#' @param password password for database.
#' @param anstype '1' for orthographic answer, '2' for phonetic answer, '3' for all answer.
#' @param proid project id to be queried
#' @param linDomain linguistic domain to be used.
#' @param type data type to be queried.
#' 
#' @export
#' 
#' @import DBI RMySQL
#'
#' @examples
#' 
#' ## sample call from server
#' \dontrun{
#'   set <- loadDiatech('thedatabase', 'user', 'thepassword', '1', type = 'geodist')
#' }
#' 
loadDiatech <- function(database, user, password, parts, type = c('geodist', 'lingdata', 'lemmadata')){
    write('Reading data', stdout())
    requestType <- match.arg(type)
    on.exit(DBI::dbDisconnect(con))
    con <- DBI::dbConnect(DBI::dbDriver('MySQL'), user = user, password = password, database)
    DBI::dbGetQuery(con, 'SET NAMES "utf8"')
    if (requestType == 'geodist'){
        
        query <- paste0('SELECT
           dl.id,
           dl.location,
           dl.longitude,
           dl.latitude
         FROM diatech_location AS dl
         INNER JOIN diatech_section AS ds ON ds.project_id = dl.project_id
         WHERE ds.id IN (', paste(parts, collapse = ', '), ')')

        result <- DBI::dbGetQuery(con, query)
        class(result) <- c('diaLongLat', 'data.frame')

    } else if (requestType == 'lingdata'){

        query <- paste0(
         'SELECT
           dl.id,
           dl.location,
           dq.question,
           da.answer
         FROM diatech_answer AS da
         INNER JOIN diatech_question AS dq ON da.question_id = dq.id 
         INNER JOIN diatech_section AS ds ON dq.section_id = ds.id
         INNER JOIN diatech_location AS dl ON da.location_id = dl.id
         WHERE ds.id IN (', paste(parts, collapse = ', '), ')')
        
        result <- DBI::dbGetQuery(con, query)
        class(result) <- c('diaLingData', 'data.frame')
        
    } else { ## lemmadata
        
        stop ('deprecated')
        
    } 
    DBI::dbDisconnect(con)
    on.exit(NULL)
    if (nrow(result) == 0L) stop ('The lodaded dataset is empty')
    write('Data read', stdout())
    return (result)
}
