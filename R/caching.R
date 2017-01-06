db.file.name <- "DeepBlueR.cache"

#' Sets up the DeepBlueR cache and returns a filehash db object
#'
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
#' @return new empty cache database
#' @export
#'
#' @examples deepblue_clear_cache()
deepblue_clear_cache <- function(){
    db <- dbInit(db.file.name)
    dbUnlink(db)
    return(deepblue_get_db)
}

#' List cached requests
#'
#' @return list of request ids that are cached
#' @export
#'
#' @examples deepblue_list_cached_requests()
deepblue_list_cached_request <- function(){
    db <- dbInit(db.file.name)
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
        size <- utils:::format.object_size(cache_size, "auto")
        cached_requests <- deepblue_list_cached_request()
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
#' @export
#'
#' @examples deepblue_delete_request_from_cache("non-existing-request-id")
#' # returns FALSE
deepblue_delete_request_from_cache <- function(request_id){
    db <- deepblue_get_db()
    if(!dbExists(request_id)){
        warning("request was not found in cache.")
        return(FALSE)
    }
    else{
        return(dbDelete(db, request_id))
    }
}
