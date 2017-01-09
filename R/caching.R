db.file.name <- "DeepBlueR.cache"

#' Sets up the DeepBlueR cache and returns a filehash db object
#' @importFrom filehash dbCreate
#' @importFrom filehash dbInit
#' @return A filehash package database
#'
deepblue_get_db <- function(){
    if(!file.exists(db.file.name)){
        dbCreate(db.file.name)
    }
    dbInit(db.file.name)
}

#' Clear cache
#'
#' @return TRUE if successful
#' @importFrom filehash dbUnlink
#' @export
#'
#' @examples deepblue_clear_cache()
deepblue_clear_cache <- function(){
    db <- deepblue_get_db()
    dbUnlink(db)
    return(TRUE)
}

#' List cached requests
#'
#' @return list of request ids that are cached
#' @importFrom filehash dbList
#' @export
#'
#' @examples deepblue_list_cached_requests()
deepblue_list_cached_requests <- function(){
    db <- deepblue_get_db()
    dbList(db)
}

#' Report on the cache size and status
#'
#' @return cache size in byte
#' @export
#'
#' @examples deepblue_cache_status()
deepblue_cache_status <- function(){
    if(!file.exists(db.file.name)){
        message("No cache file found in the current working directory")
        return(NULL)
    }
    else{
        cache_size <- file.size(db.file.name)
        size <- deepblue_format_object_size(cache_size, "auto")
        cached_requests <- deepblue_list_cached_requests()
        num_of_requests <- length(cached_requests)

        message(paste("DeepBlueR caches", num_of_requests,
        "request(s) in the current working directory. The cache file occupies",
        size))

        return(cache_size)
    }
}

#' Delete a specific request from the cache
#'
#' @param request_id the request to delete from the cache
#'
#' @return TRUE if the request was successfully deleted, FALSE otherwise
#' @importFrom filehash dbExists
#' @importFrom filehash dbDelete
#' @export
#'
#' @examples deepblue_delete_request_from_cache("non-existing-request-id")
#' # returns FALSE
deepblue_delete_request_from_cache <- function(request_id){
    db <- deepblue_get_db()
    if(!dbExists(db, request_id)){
        warning("request was not found in cache.")
        return(FALSE)
    }
    else{
        return(dbDelete(db, request_id))
    }
}
