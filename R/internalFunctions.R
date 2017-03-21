#'@title deepblue_wait_request
#'@description Process the user request. Takes in three parameters;
#'requested regions, sleep time, and user key.
#'@return request_id info
#'@param request_id A string with the request_id
#'@param sleep_time An integer with default value 1s
#'@param user_key A string
setGeneric("deepblue_wait_request", function(request_id, sleep_time = 1,
                                             user_key=deepblue_options("user_key"))
{
    info = deepblue_info(request_id, user_key)
    state = info$state
    if(is.null(state)) stop("no state information found")
    while (!state %in% c('done','error','failed') &&
      !is.null(sleep_time) )
    {
        Sys.sleep(sleep_time)
        info = deepblue_info(request_id, user_key)
        state = info$state
    }
    return (info)
})

#'@title deepblue_download_request_data
#'@description Returns the requested data as the expected type object.
#'Expects two input parameters; Request information and
#'user key. It depends on outputs from several functions, namely;
#'deepblue_get_request_data, convert_to_df, and convert_to_grange.
#'
#'@param request_id - Id of the request that will be downloaded
#'@param user_key A string
#'@param force_download forces DeepBlueR to download the request overwriting
#' any results that might already be in the cache
#'@param do_not_cache whether to use local caching of requests
#'@return grange_regions Final output in GRanges format or as data frame
#'@examples
#' data_id = deepblue_select_experiments(
#' experiment_name="E002-H3K9ac.narrowPeak.bed", chromosome="chr1")
#' request_id = deepblue_get_regions(query_id =data_id,
#'   output_format = "CHROMOSOME,START,END")
#' request_data = deepblue_download_request_data(request_id)
setGeneric("deepblue_download_request_data",
           function(request_id,
                    user_key=deepblue_options("user_key"),
                    force_download = deepblue_options("force_download"),
                    do_not_cache = deepblue_options("do_not_cache"))
           {
    deepblue_switch_get_request_data(request_id = request_id,
                                     user_key = user_key,
                                     force_download = force_download,
                                     do_not_cache = do_not_cache)
})

#' @importFrom R.utils bunzip2
#' @importFrom filehash dbInsert
#' @importFrom filehash dbFetch
#' @importFrom filehash dbExists
#' @title switch_get_request_data
#' @param request_id The request command generated
#' with deepblue_get_request_data
#' @param user_key the user_key used to submit the request.
#' @param force_download forces DeepBlueR to download the request overwriting
#' any results that might already be in the cache
#' @param do_not_cache whether to use local caching of requests
#' @description Download the requested data from DeepBlue.
#' @return request data
#' Can be either region sets,
#' a region count, a list of experiments, or a score matrix.
#' @aliases get_request_data
#' @keywords internal
deepblue_switch_get_request_data = function(request_id,
                                            user_key=deepblue_options("user_key"),
                                            force_download=deepblue_options("force_download"),
                                            do_not_cache=deepblue_options("do_not_cache"))
{
    #check if results is in cache
    if(!do_not_cache){
        db <- deepblue_get_db()
        if(!force_download){
            if(dbExists(db, request_id)) return(dbFetch(db, request_id))
        }
    }

    #check if result is already done
    deepblue_wait_request(request_id, user_key=user_key)

    #check if something went wrong
    request_info = deepblue_info(request_id, user_key)
    if (request_info$state != "done") {
        stop("Processing was not finished.
             Please, check it status with deepblue_info(request_id)");
    }
    command = request_info$command

    if(command %in% c("get_regions", "score_matrix")){
        url =
            paste(
                "http://deepblue.mpi-inf.mpg.de/xmlrpc/download/?r=",
                request_id, "&key=", user_key, sep="")
        temp_download <- tempfile()
        download.file(url, temp_download, mode="wb")
        unzipped_request <- paste(temp_download, "_uncompress", sep="")
        message(paste("Decompressing downloaded file to",unzipped_request))
        R.utils::bunzip2(temp_download, unzipped_request, remove = FALSE, skip = TRUE)
        file.remove(temp_download)
        on.exit(file.remove(unzipped_request))
    }
    else{
        # DEFAULT
        # count_regions
        # get_experiments_by_query
        # coverage
        request_data <- deepblue_get_request_data(request_id, user_key)

        if(command == "count_regions"){
            request_data <- as.numeric(request_data)
        }

        if(!do_not_cache) dbInsert(db, request_id, request_data)
        return(request_data)
    }

    # Only the get_regions and score_matrix commands can have
    # the data converted to tables.
    message(paste("Reading file from", unzipped_request))

    regions_df = deepblue_convert_to_df(
        file_to_parse=unzipped_request, request_info=request_info)

    if (request_info$command %in%
        c("score_matrix", "get_experiments_by_query") ||
        request_info$format == "") {
        if(!do_not_cache) dbInsert(db, request_id, regions_df)
        return (regions_df)
    }

    else if(command == "get_regions"){
        if(nrow(regions_df) > 0){

            if("START" %in% colnames(regions_df) &&
               "END" %in% colnames(regions_df)){
                regions_df$START <- as.integer(regions_df$START)
                regions_df$END <- as.integer(regions_df$END)
                regions_df <- deepblue_convert_to_grange(df=regions_df)
            }

            if(!do_not_cache) dbInsert(db, request_id, regions_df)
            return(regions_df)
        }
        else
            stop("No regions were returned in this request.")
    }
}

#'@title convert_to_df
#'@importFrom stringr str_split
#'@importFrom data.table fread
#'@description save output in a data frame for further processing.
#'Expects two parameters; the output string from method
#'deepblue_get_request_data and request information from process_request
#'@param string_to_parse A string
#'@param request_info The request information returned by DeepBlue
#'@param dict The data structure that contains the DeepBlue columns types
#'@return regions A data frame
#'@keywords internal
deepblue_convert_to_df = function(file_to_parse, request_info,
    dict=col_dict){

    if("format" %in% names(request_info)) {
        if (request_info$format == "") {
            request_info$format = "CHROMOSOME,START,END"
        }
        #get column names from
        col_names <- str_split(request_info$format, pattern = ",")[[1]]
        #get column types from dictionary
        col_types <- sapply(col_names, function(x){
            col_type <- dict[[x]]

            # Meta columns that have arguments.
            if (grepl("@COUNT.NON-OVERLAP" , x)) return ("integer")
            else if (grepl("@COUNT.OVERLAP" , x)) return ("integer")

            #return column type, defaults to character
            if(is.null(col_type)) return("character")
            #need to change string to character
            else if(col_type == "string") return("character")
            else if (col_type == 'category') return ('factor')
            else return(col_type)
        }, USE.NAMES = FALSE)

        fread(input = file_to_parse,
              sep="\t",
              colClasses = col_types,
              header = FALSE,
              col.names = col_names,
              strip.white = FALSE,
              stringsAsFactors = FALSE,
              data.table = TRUE)
    } else{
        fread(input = file_to_parse,
              sep="\t",
              header = TRUE,
              strip.white = FALSE,
              stringsAsFactors = FALSE,
              data.table = TRUE)
    }
}

#'@title convert_to_grange
#'@description Converts the requested data into GRanges object.
#'Expects one input; A dataframe with requested data.
#'@importFrom GenomicRanges makeGRangesFromDataFrame
#'@importFrom GenomicRanges as.data.frame
#'@param df A data frame
#'@return region_gr A GRanges object
#'
#'@seealso \code{\link[GenomicRanges]{makeGRangesFromDataFrame}}
#'@keywords internal
deepblue_convert_to_grange = function (df = NULL)
{
    if("GTF_ATTRIBUTES" %in% colnames(df)){
        gtf_attributes <- deepblue_parse_gtf(df$GTF_ATTRIBUTES)
        df$GTF_ATTRIBUTES <- NULL
        df <- dplyr::bind_cols(df, gtf_attributes)
    }
    region_gr = makeGRangesFromDataFrame(df, keep.extra.columns = TRUE,
                                         seqnames.field = 'CHROMOSOME',
                                         start.field = 'START',
                                         end.field = 'END',
                                         strand.field = c('STRAND','Strand'))
    return (region_gr)
}

#' @description Load the column types from DeepBlue
#' @title get columns
#' @return Dictionary will all column names and types
#' @keywords internal
deepblue_column_types = function()
{
    cols = deepblue_list_column_types()
    col_info = deepblue_info(cols$id)
    dict=new.env()
    for (i in 1:length(col_info))
    {
        dict[[col_info$name[i]]] = col_info$column_type[i]
    }

    #add extra column types not reported by DeepBlue
    dict[["@LENGTH"]] = "integer"
    dict[["@NAME"]] = "string"
    dict[["@SEQUENCE"]] = "string"
    dict[["@EPIGENETIC_MARK"]] = "string"
    dict[["@PROJECT"]] = "string"
    dict[["@BIOSOURCE"]] = "string"
    dict[["@SAMPLE_ID"]] = "string"
    dict[["@AGG.MIN"]] = "double"
    dict[["@AGG.MAX"]] = "double"
    dict[["@AGG.MEDIAN"]] = "double"
    dict[["@AGG.MEAN"]] = "double"
    dict[["@AGG.VAR"]] = "double"
    dict[["@AGG.SD"]] = "double"
    dict[["@AGG.COUNT"]] = "integer"
    dict[["@COUNT.OVERLAP"]] = "integer"
    dict[["@COUNT.NON-OVERLAP"]] = "integer"
    dict[["@CALCULATED"]] = "string"
    dict[["@GENE_ATTRIBUTE"]] = "string"

    return(dict)
}

col_dict = deepblue_column_types()

#' @description Parse the GTF semicolon separated attributes into a data frame
#' @title deepblue_parse_gtf
#' @param all_gtf a character vector of GTF attributes for a single region.
#' @return the parsed GTF data
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @keywords internal
deepblue_parse_gtf <- function(all_gtf){
    parsed_results <- lapply(as.list(all_gtf), function(gtf){
        gtf_no_quotes <- str_replace_all(gtf, "\"", "")
        split_gtf <- lapply(str_split(gtf_no_quotes, "; "), function(x){
            str_split(x, " ")
        })
        attr.names <- unlist(lapply(split_gtf[[1]], function(x) { x[[1]] }))
        attr.values <- lapply(split_gtf[[1]], function(x) { x[[2]] })
        names(attr.values) <- attr.names
        return(attr.values)
    })

    return(dplyr::bind_rows(parsed_results))
}


#' Format byte size as human readable units
#' @source utils:::format.object_size
#' @param x size in bytes
#' @param units target unit or 'auto'
#'
#' @return formatted size
#'
deepblue_format_object_size <- function (x, units = "b"){
    units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb",
                                "Tb", "Pb", "B", "KB", "MB", "GB", "TB", "PB", "KiB",
                                "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))
    if (units == "auto")
        units <- if (x >= 1024^4)
            "Tb"
    else if (x >= 1024^3)
        "Gb"
    else if (x >= 1024^2)
        "Mb"
    else if (x >= 1024)
        "Kb"
    else "b"
    switch(units, b = , B = paste(x, "bytes"), Kb = , KB = paste(round(x/1024,
               1L), "Kb"), Mb = , MB = paste(round(x/1024^2, 1L), "Mb"),
           Gb = , GB = paste(round(x/1024^3, 1L), "Gb"), Tb = ,
           TB = paste(round(x/1024^4, 1L), "Tb"), Pb = , PB = paste(round(x/1024^5,
              1L), "Pb"), KiB = paste(round(x/1024, 1L), "KiB"),
           MiB = paste(round(x/1024^2, 1L), "MiB"), GiB = paste(round(x/1024^3,
              1L), "GiB"), TiB = paste(round(x/1024^4, 1L), "TiB"),
           PiB = paste(round(x/1024^5, 1L), "PiB"), EiB = paste(round(x/1024^6,
              1L), "EiB"), ZiB = paste(round(x/1024^7, 1L), "ZiB"),
           YiB = paste(round(x/1024^8, 1L), "YiB"))
}
