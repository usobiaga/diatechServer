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
#'   set <- load('thedatabase', 'user', 'thepassword', '1', '10')
#' }
#' 
#' 
load <- function(database, user, password, anstype, proid, linDomain = ''){
    message ('Reading data')
    on.exit(dbDisconnect(con))
    con <- dbConnect(dbDriver('MySQL'), dbname = database, user = user, password = password)
    anstype <- c("orthographic_answer","phonetic_answer","lemma")[as.numeric(anstype)]
    
    if (linDomain != ''){
        query <- paste0("select answers_location.location,\
    answers_answer.question_id, answers_answer.",anstype,"\
    from answers_location\
    inner join answers_informant on answers_location.id = answers_informant.location_id\
    inner join answers_answer on answers_informant.id = answers_answer.informant_id\
    inner join answers_question on answers_question.id = answers_answer.question_id\
    where answers_answer.",anstype," != ''\
    and answers_question.linguistic_domain =", linDomain,"\
    and answers_location.project_id =", proid," order by answers_location.location,\
    answers_answer.question_id,\
    answers_answer.", anstype)
        
    } else {
        query <- paste0("select answers_location.location,\
    answers_answer.question_id, answers_answer.", anstype,"\
    from answers_location\
    inner join answers_informant on answers_location.id = answers_informant.location_id\
    inner join answers_answer on answers_informant.id = answers_answer.informant_id\
    where answers_answer.", anstype," != ''\
    and answers_location.project_id =", proid," order by answers_location.location,\
    answers_answer.question_id,\
    answers_answer.", anstype)
    }
    dataset <- dbGetQuery(con,query)
    return (dataset)
}
