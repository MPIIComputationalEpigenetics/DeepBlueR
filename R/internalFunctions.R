#'@title deepblue.wait_request
#'@description Process the user request. Takes in three parameters;
#'requested regions, sleep time, and user key.
#'@return request_id info
#'@param request_id A string with the request_id
#'@param sleep_time An integer with default value 1s
#'@param user_key A string
setGeneric("deepblue.wait_request", function(request_id, sleep_time = 1,
                                             user_key=deepblue.USER_KEY)
{
    info = deepblue.info(request_id, user_key)[[1]]
    state = info$state
    if(is.null(state)) stop("no state information found")
    while (!state %in% c('done','error','failed') &&
      !is.null(sleep_time) )
    {
        Sys.sleep(sleep_time)
        info = deepblue.info(request_id, user_key)[[1]]
        state = info$state
    }
    return (info)
})

#'@title deepblue.download_request_data
#'@description Returns the requested data as the expected type object.
#'Expects two input parameters; Request information and
#'user key. It depends on outputs from several functions, namely;
#'deepblue.get_request_data, convert_to_df, and convert_to_grange.
#'
#'@param request_id - Id of the request that will be downloaded
#'@param user_key A string
#'@param type To which type the downloaded data will be converted:
#''string', 'df', or 'grange'.
#'@return grange_regions Final output in GRanges format
#'@examples
#' data_id = deepblue.select_experiments(
#' experiment_name="E002-H3K9ac.narrowPeak.bed", chromosome="chr1")
#' request_id = deepblue.get_regions(query_id =data_id,
#'   output_format = "CHROMOSOME,START,END")
#' request_data = deepblue.download_request_data(request_id)
setGeneric("deepblue.download_request_data",
           function(request_id,
                    user_key=deepblue.USER_KEY,
                    type="grange")
           {
    request_info = deepblue.wait_request(request_id, user_key=user_key)

    if (request_info$state == "done") {
        message("The request was processed successfully.")
    } else {
        stop(request_info$message)
    }

    regions_string = deepblue.switch_get_request_data(
      request_id = request_id,
      user_key = user_key
    )

    if (type == "string") {
        return (regions_string)
    }

    regions_df = deepblue.convert_to_df(string_to_parse=regions_string,
                                        request_info=request_info)

    if (type == "df") {
        return (regions_df)
    }

    return(deepblue.convert_to_grange(df=regions_df))
})

#' @import XML RCurl
#' @title switch_get_request_data
#' @param request_id The request command generated
#' with deepblue.get_request_data
#' @param user_key the user_key used to submit the request.
#' @description Download the requested data from DeepBlue.
#' @return request data
#' Can be either region sets,
#' a region count, a list of experiments, or a score matrix.
#' @aliases get_request_data
#' @keywords internal
deepblue.switch_get_request_data = function(request_id,
                                            user_key=deepblue.USER_KEY)
{
    request_info = deepblue.info(request_id, user_key)[[1]]
    if (request_info$state != "done") {
        stop("Processing was not finished.
             Please, check it status with deepblue.info(request_id)");
    }

    command = request_info$command
    switch(command,
           "count_regions" = deepblue.get_request_data(
              request_id, user_key
            ),
           "get_experiments_by_query" = deepblue.get_request_data(
              request_id,
              user_key
            ),
           "get_regions" = {
              url =
                paste(
                  "http://deepblue.mpi-inf.mpg.de/xmlrpc/download/?r=",
                           request_id, "&key=", user_key, sep="")
               temp_download <- tempfile()
               download.file(url, temp_download, mode="wb")
               handle <-  bzfile(temp_download)
               result <- paste(readLines(handle), collapse="\n")
               close(handle)
               return(result)
           },
           "score_matrix" = {
               url = paste(
                "http://deepblue.mpi-inf.mpg.de/xmlrpc/download/?r=",
                           request_id, "&key=", user_key, sep="")
               temp_download <- tempfile()
               download.file(url, temp_download, mode="wb")
               handle <-  bzfile(temp_download)
               result <- paste(readLines(handle), collapse="\n")
               close(handle)
               return(result)
           },
           stop(paste("Unknow command", command)))
}

#'@title convert_to_df
#'@importFrom stringr str_split
#'@importFrom data.table fread
#'@description save output in a data frame for further processing.
#'Expects two parameters; the output string from method
#'deepblue.get_request_data and request information from process_request
#'@param string_to_parse A string
#'@param request_info The request information returned by DeepBlue
#'@param dict The data structure that contains the DeepBlue columns types
#'@return regions A data frame
#'@keywords internal
deepblue.convert_to_df = function(string_to_parse, request_info,
    dict=col_dict){
    browser()
    if("format" %in% names(request_info)) {
        if (request_info$format == "") {
            request_info$format = "CHROMOSOME,START,END"
        }
        #get column names from
        col_names <- str_split(request_info$format, pattern = ",")[[1]]
        #get column types from dictionary
        col_types <- sapply(col_names, function(x){
            col_type <- dict[[x]]
            #return column type, defaults to character
            if(is.null(col_type)) return("character")
            #need to change string to character
            else if(col_type == "string") return("character")
            else if (col_type == 'category') return ('factor')
            else return(col_type)
        })

        input <- paste(paste(col_names,
                             collapse="\t"), "\n", string_to_parse, sep="")
    } else{
        col_types <- NULL
        input <- string_to_parse
    }
    #read data frame efficiently from string
    #paste a header in front so that fread accepts colClasses
    fread(input = input,
          sep="\t",
          colClasses = col_types,
          header=TRUE,
          strip.white = FALSE,
          stringsAsFactors = FALSE,
          data.table = FALSE)
}

#'@title convert_to_grange
#'@description Converts the requested data into GRanges object.
#'Expects one input; A dataframe with requested data.
#'@importFrom GenomicRanges makeGRangesFromDataFrame
#'@param df A data frame
#'@return region_gr A GRanges object
#'
#'@seealso \code{\link[GenomicRanges]{makeGRangesFromDataFrame}}
#'@keywords internal
deepblue.convert_to_grange = function (df = NULL)
{
    if("GTF_ATTRIBUTES" %in% colnames(df)){
        gtf_attributes <- deepblue.parse_gtf(df$GTF_ATTRIBUTES)
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
deepblue.column_types = function()
{
    cols = deepblue.list_column_types()
    col_ids = c()
    for (i in 1:length(cols))
    {
        co=cols[[i]][[1]]
        col_ids = c(col_ids,co)
    }

    col_names=c()
    col_types = c()
    col_info = deepblue.info(col_ids)
    dict=new.env()
    for (i in 1:length(col_info))
    {
        dict[[col_info[[i]]$name]] = col_info[[i]]$column_type
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

col_dict = deepblue.column_types()

#' @description Parse the GTF semicolon separated attributes into a data frame
#' @title deepblue.parse_gtf
#' @param all_gtf a character vector of GTF attributes for a single region.
#' @return the parsed GTF data
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @keywords internal
deepblue.parse_gtf <- function(all_gtf){
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