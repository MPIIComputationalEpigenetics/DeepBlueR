#' @export 
#' 
#' @title extract_ids 
#' @description A utility command that returns a list of IDs extracted 
#' from a data frame of ID and names.
#' @family Utilities for connecting operations
#' 
#' @param df - A array of IDs and names
#'
#' @return ids - A vector containing the extracted IDs)
#'
#' @examples
#' deepblue_extract_ids(
#'     df = data.frame(id = c("a124", "a1235"),
#'     name = c("Annotation 1", "Annotation 2")))
#'
deepblue_extract_ids <- function(df = NULL) {
    
    if(is.null(df)){
        warning("you need to supply a data frame")
        return(NULL)  
    } 
    else if(!is.data.frame(df)){
        warning("argument not a data frame")
        return(NULL)  
    } 
    
    return(as.character(df$id))
}



#' @export 
#' 
#' @title extract_names 
#' @description A utility command that returns a list of names extracted from a list of ID and names.
#' @family Utilities for connecting operations
#' 
#' @param df - A array of IDs and Names
#' 
#' @return names - A vector containing the extracted names
#' @examples
#' deepblue_extract_ids(
#'     df = data.frame(id = c("a124", "a1235"),
#'     name = c("Annotation 1", "Annotation 2")))
#'
deepblue_extract_names <- function(df = NULL) {
    
    if(is.null(df)){
        warning("you need to supply a data frame")
        return(NULL)  
    } 
    else if(!is.data.frame(df)){
        warning("argument not a data frame")
        return(NULL)  
    } 
    
    return(as.character(df$name))
}