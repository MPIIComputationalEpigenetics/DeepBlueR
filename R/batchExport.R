#'@export
#'@title batch_export_results
#'@description Write results from DeepBlue to disk as they become available
#'@param requests A list of request objects
#'@param target.directory Where the results should be saved
#'@param suffix File names suffix
#'@param prefix File names prefix
#'@param sleep.time How long this function will wait after the requests
#'verification
#'@param user_key A string used to authenticate the user
#'@return A list containing the requests IDs data
#' @examples
#' data_id = deepblue.select_experiments(
#' experiment_name="E002-H3K9ac.narrowPeak.bed", chromosome="chr1")
#' request_id = deepblue.get_regions(query_id =data_id,
#'   output_format = "CHROMOSOME,START,END")
#' request_data = deepblue.batch_export_results(list(request_id))
deepblue.batch_export_results <- function(requests,
                                          target.directory=NULL,
                                          suffix="result",
                                          prefix="DeepBlue",
                                          sleep.time = 1,
                                          user_key = deepblue.USER_KEY){
    #to store results
    all.results <- list()
    if(is.na(requests) || is.null(requests))
        stop("A list of request_info objects or request identifiers is needed.")

    #make sure we have a list
    requests <- as.list(requests)

    #check if there is at least one elt
    if(length(requests) < 1)
        stop("Provide a list of at least one request_info
             object or request identifier.")

    #get ids in case we are given request_info objects
    if(length(requests[[1]]) > 1){
        for(request in 1:length(requests)){
            requests[[request]] <- requests[[request]]$`_id`
        }
    }

    #remove duplicates
    requests <- unique(requests)

    #keep track of which instances were already downloaded
    need.saving <- rep(TRUE, length(requests))

    #count errors
    errors <- 0
    error_commands <- list()

    #while any request is not saved
    while(any(need.saving)){
        anything.done <- FALSE
        #go through unsaved requests
        for(request in which(need.saving)){
            request_id <- requests[[request]]

            #update info
            request_info <- deepblue.info(request_id, user_key = user_key)[[1]]

            if(request_info$state == "done" && need.saving[request])
            {
                #download data
                message(paste("Downloading results for id",
                              request_id@query_id))
                result <- deepblue.download_request_data(request_id)
                all.results[[request_id@query_id]] <- result

                if(!is.null(target.directory)){
                    #save to disk
                    dir.create(target.directory,
                               showWarnings=FALSE,
                               recursive = TRUE)
                    result <- GenomicRanges::as.data.frame(result)
                    write.table(result, sep = "\t",
                                row.names = FALSE,
                                quote=FALSE,
                                file = file.path(target.directory,
                                                 paste(paste(
                                                     prefix,
                                                     request_id@query_id,
                                                     suffix, sep="_"),
                                                     ".txt",
                                                     sep="")))
                }
                anything.done <- TRUE
                need.saving[request] <- FALSE
            }
            else if(request_info$state %in% c("failed", "error")
                    && need.saving[request]){
                need.saving[request] <- FALSE
                error_commands[[request_id@query_id]] <- request_id
                errors <- errors + 1
            }
        }
        #give DeepBlue some time to make progress
        if(!anything.done) Sys.sleep(sleep.time)
    }
    attr(all.results, "errors") <- error_commands
    message(paste("Processing of", length(requests),
                  "requests completed with", errors, "errors"))
    if(!is.null(target.directory)) message(
        paste("All result files saved to directory", target.directory))
    return(all.results)
}