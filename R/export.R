#' Export a DeepBlue result as ordinary tab delimited file
#'
#' @param result A result from a DeepBlue request such as a set of genomic
#' regions.
#' @param target.directory The directory to save the file to
#' @param file.name The name of the file without suffix
#'
#' @importFrom withr with_options
#' @return return value of write.table
#'
#' @export
#'
#' @examples query_id = deepblue_select_experiments (
#' experiment=c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig"),
#' chromosome="chr1", start=0, end=50000000)
#'cpg_islands = deepblue_select_annotations(annotation_name="CpG Islands",
#' genome="GRCh38", chromosome="chr1", start=0, end=50000000)
#' overlapped = deepblue_aggregate (data_id=query_id, ranges_id=cpg_islands,
#'                                 column="VALUE" )
#'request_id = deepblue_get_regions(query_id=overlapped,
#'                                 output_format=
#'                                      "CHROMOSOME,START,END,@AGG.MIN,@AGG.MAX,@AGG.MEAN,@AGG.VAR")
#' regions = deepblue_download_request_data(request_id=request_id)
#' temp_dir = tempdir()
#' deepblue_export_tab(regions, target.directory = temp_dir,
#'                   file.name = "GC_T14_10.CpG_islands")
deepblue_export_tab <- function(result,
                                  target.directory = "./",
                                  file.name){
    withr::with_options(c(scipen = 10),
            write.table(result, sep = "\t",
                row.names = FALSE,
                quote=FALSE,
                file = file.path(target.directory,
                                 paste(file.name,
                                     ".txt",
                                     sep=""))))
}


#' Export GenomicRanges result as BED file
#'
#' @import GenomicRanges
#' @importFrom withr with_options
#' @importFrom stringr str_replace_all
#'
#' @param result A result from a DeepBlue request such as a set of genomic
#' regions.
#' @param target.directory The directory to save the file to
#' @param file.name The name of the file without suffix
#' @param score.field Which column of the results should be used to populate
#' the score column of the BED file (optional)
#' @export
#' @return return value of write.table
#'
#' @examples query_id = deepblue_select_experiments (
#' experiment=c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig"),
#' chromosome="chr1", start=0, end=50000000)
#'cpg_islands = deepblue_select_annotations(annotation_name="CpG Islands",
#' genome="GRCh38", chromosome="chr1", start=0, end=50000000)
#' overlapped = deepblue_aggregate (data_id=query_id, ranges_id=cpg_islands,
#'                                 column="VALUE" )
#'request_id = deepblue_get_regions(query_id=overlapped,
#'                                 output_format=
#'                                      "CHROMOSOME,START,END,@AGG.MIN,@AGG.MAX,@AGG.MEAN,@AGG.VAR")
#' regions = deepblue_download_request_data(request_id=request_id)
#' temp_dir = tempdir()
#' deepblue_export_bed(regions, target.directory = temp_dir,
#'                   file.name = "GC_T14_10.CpG_islands")
deepblue_export_bed <- function(result,
                                target.directory = "./",
                                file.name,
                                score.field = NULL){
    if(class(result) != "GRanges" && class(result) == "data.frame"){
        stop("Results are not in GenomicRanges format. Use deepblue_export_tab instead")
    }
    else if(class(result) != "GRanges" && class(result) != "data.frame"){
        stop(paste("Do not know how to deal with object of class", class(result)))
    }

    scores <- rep(".", length(GenomicRanges::rank(result)))
    if(!is.null(score.field)) scores <- elementMetadata(result)[[score.field]]
    strands <- stringr::str_replace_all(strand(result), "\\*", ".")
    snames <- c(rep(".", length(result)))


    bed_result <- data.frame(seqnames=seqnames(result),
                     starts=start(result)-1,
                     ends=end(result),
                     names=snames,
                     scores=scores,
                     strands=strands)

    withr::with_options(c(scipen = 10),
            write.table(bed_result,
                file = file.path(target.directory,
                                 paste(file.name,
                                       ".bed",
                                       sep="")),
                quote = FALSE,
                sep="\t",
                row.names = FALSE,
                col.names = FALSE))
}

#' Convert XML structured meta data to table format
#'
#' @param ids an id or a list of ids
#' @param user_key a DeepBlue user key (optional for public data)
#' @import foreach
#' @importFrom dplyr bind_rows
#'
#' @return a data frame with meta data
#' @export
#'
#' @examples
#' #works for sample ids
#' deepblue_meta_data_to_table(list("s2694", "s2695"))
#'
#' #or experiment ids
#' deepblue_meta_data_to_table(list("e30035", "e30036"))
deepblue_meta_data_to_table <- function(ids, user_key = deepblue_options("user_key"))
{
    all_meta_data <- deepblue_info(ids, user_key = user_key)

    #for some ids a data table is already returned by deepblue_info, e.g.
    #samples
    if(is.data.frame(all_meta_data)) return(all_meta_data)

    #if we have only one id, make a list
    if(length(ids) == 1) all_meta_data <- list(all_meta_data)

    #just to shut up R CMD check
    i <- NULL

    foreach(i = 1:length(all_meta_data), .combine = bind_rows) %do%{

        file_info <- all_meta_data[[i]]
        extra_metadata <- unlist(file_info$extra_metadata)

        file_info$extra_metadata <- NULL
        file_info$upload_info <- NULL
        file_info$columns <- NULL

        sample_info <- unlist(file_info$sample_info)
        file_info$sample_info <- NULL

        meta_data <- unlist(file_info)
        meta_data <- c(meta_data, sample_info, extra_metadata)
        meta_data <- as.data.frame(t(meta_data), stringsAsFactors = FALSE)

        return(meta_data)
    }
}


#' Export meta data as tab delimited file
#'
#' @param ids an id or a list of DeepBlue ids
#' @param target.directory  where the meta data should be stored
#' @param file.name name of the file
#' @param user_key DeepBlue user key
#' @importFrom withr with_options
#' @return return value of write.table
#'
#' @export
#'
#' @examples
#' deepblue_export_meta_data(list("e30035", "e30036"),
#' file.name = "test_export",
#' target.directory = tempdir())
deepblue_export_meta_data <- function(ids,
                                 target.directory = "./",
                                 file.name,
                                 user_key = deepblue_options("user_key"))
{
    meta_data <- deepblue_meta_data_to_table(ids)

    withr::with_options(c(scipen = 10),
                        write.table(meta_data,
                                    file = file.path(target.directory,
                                                     paste(file.name,
                                                           ".meta.txt",
                                                           sep="")),
                                    quote = FALSE,
                                    sep="\t",
                                    row.names = FALSE,
                                    col.names = TRUE))
}

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
#'@param bed.format whether to store the results as BED files or tab delimited.
#'@return A list containing the requests IDs data
#' @examples
#' data_id = deepblue_select_experiments(
#' experiment_name="E002-H3K9ac.narrowPeak.bed", chromosome="chr1")
#' request_id = deepblue_get_regions(query_id =data_id,
#'   output_format = "CHROMOSOME,START,END")
#' request_data = deepblue_batch_export_results(list(request_id))
deepblue_batch_export_results <- function(requests,
                                          target.directory=NULL,
                                          suffix="result",
                                          prefix="DeepBlue",
                                          sleep.time = 1,
                                          bed.format = TRUE,
                                          user_key = deepblue_options("user_key")){
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
            request_info <- deepblue_info(request_id, user_key = user_key)

            if(request_info$state == "done" && need.saving[request])
            {
                #check if it is a get region command and download meta data
                if(request_info$command == "get_regions"){
                    message(paste("Downloading meta data for id", request_id))
                    meta_data_request <- deepblue_get_experiments_by_query(
                        request_info$query_id)
                    meta_data_experiments <- deepblue_download_request_data(
                        meta_data_request)
                }
                else{
                    meta_data_experiments <- NULL
                }
                #download data
                message(paste("Downloading results for id", request_id))
                result <- deepblue_download_request_data(request_id,
                                                         user_key = user_key)
                all.results[[request_id]] <- result

                if(!is.null(target.directory)){
                    #save to disk
                    if(!dir.exists(target.directory)){
                    dir.create(target.directory,
                               showWarnings=FALSE,
                               recursive = TRUE)
                    }
                    file.name <- paste(
                        prefix,
                        request_id,
                        suffix, sep="_")
                    if(!bed.format){
                        deepblue_export_tab(result,
                                            target.directory = target.directory,
                                            file.name = file.name)
                    }
                    else{
                        deepblue_export_bed(result,
                                            target.directory = target.directory,
                                            file.name = file.name)
                    }
                    if(!is.null(meta_data_experiments)){
                        deepblue_export_meta_data(names(meta_data_experiments),
                                target.directory = target.directory,
                                file.name = file.name)
                    }
                }
                anything.done <- TRUE
                need.saving[request] <- FALSE
            }
            else if(request_info$state %in% c("failed", "error")
                    && need.saving[request]){
                need.saving[request] <- FALSE
                error_commands[[request_id]] <- request_id
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

