#'@export
#'@title deepblue.wait_request
#'@description Process the user request. Takes in three parameters; requested regions, sleep time, and user key.
#'@param request_id A string with the request_id
#'@param sleep.time An integer with default value 1s
#'@param user_key A string
setGeneric("deepblue.wait_request", function(request_id, sleep_time = 1, user_key=deepblue.USER_KEY)
{
    info = deepblue.info(request_id, user_key)[[1]]
    
    state = info$state
    if(is.null(state)) stop("no state information found")
    while (state != 'done' & state != 'error' & !is.null(sleep_time) )
    {
        Sys.sleep(sleep_time)
        info = deepblue.info(request_id, user_key)[[1]]
        state = info$state
    }
    return (info)
})

#'@export
#'@title deepblue.download_request_data
#'@description Returns the requested data as the expected type object. Expects two input parameters; Request information and
#'user key. It depends on outputs from several functions, namely;
#'deepblue.get_request_data, convert_to_df, and convert_to_grange.
#'
#'@param request_info A list
#'@param user A string
#'
#'@return grange_regions Final output in GRanges format
setGeneric("deepblue.download_request_data", function (request_id, user_key=deepblue.USER_KEY, type="grange")
{
    request_info = deepblue.wait_request(request_id, user_key=user_key)
    
    if (request_info$state == "done") {
        message("The request was processed successfully.")
    } else {
        stop(request_info$message)
    }
    
    request_id = request_info$`_id`
    regions_string = deepblue.switch_get_request_data(request_id = request_id, user_key = user_key)
    
    if (type == "string") return (regions_string)
    
    regions_df = deepblue.convert_to_df(output=regions_string, inf=request_info)
    
    if (type == "df") return (regions_df)
    else return(deepblue.convert_to_grange(df=regions_df))
})

#' @import XML RCurl
#' @title switch_get_request_data
deepblue.switch_get_request_data = function(request_id, user_key=deepblue.USER_KEY)
{
    request_info = deepblue.info(request_id, user_key)[[1]]
    if (request_info$state != "done") {
        stop("Processing was not finished. Please, check it status with deepblue.info(request_id)");
    }
    
    command = request_info$command
    switch(command,
           "count_regions" = deepblue.get_request_data(request_id, user_key),
           "get_experiments_by_query" = deepblue.get_request_data(request_id, user_key),
           "get_regions" = {
               url = paste("http://deepblue.mpi-inf.mpg.de/xmlrpc/download/?r=", request_id, "&key=", user_key, sep="")
               temp_download <- tempfile()
               download.file(url, temp_download, mode="wb")
               handle <-  bzfile(temp_download)
               paste(readLines(handle), collapse="\n")
           },              
           "score_matrix" = {
               url = paste("http://deepblue.mpi-inf.mpg.de/xmlrpc/download/?r=", request_id, "&key=", user_key, sep="")
               temp_download <- tempfile()
               download.file(url, temp_download, mode="wb")
               handle <-  bzfile(temp_download)
               paste(readLines(handle), collapse="\n")
           }, 
           stop(paste("Unknow command", command)))
}

#'@title convert_to_df
#'@import stringr data.table
#'@description save output in a data frame for further processing.Expects two parameters; the output string from method deepblue.get_request_data and request information from method process_request.
#'@param output A string
#'@param inf A list with request information
#'@return regions A data frame
deepblue.convert_to_df = function(output, inf, dict=col_dict){
    
    #get column names from
    col_names <- str_split(inf$format, pattern = ",")[[1]]
    
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
    
    #read data frame efficiently from string
    #paste a header in front so that fread accepts colClasses
    fread(input = paste(paste(col_names, collapse="\t"), "\n", output, sep=""),
          sep="\t",
          colClasses = col_types,
          header=TRUE,
          strip.white = FALSE,
          stringsAsFactors = FALSE,
          data.table = FALSE)
}

#'@title convert_to_grange
#'@description Converts the requested data into GRanges object. Expects one input; A dataframe with requested data.
#'@import GenomicRanges
#'@param df A data frame
#'@return region_gr A GRanges object
#'
#'@seealso \code{\link[GenomicRanges]{makeGRangesFromDataFrame}}
deepblue.convert_to_grange = function (df = NULL)
{
    region_gr = makeGRangesFromDataFrame(df, keep.extra.columns = TRUE,
                                         seqnames.field = 'CHROMOSOME', start.field = 'START',
                                         end.field = 'END',strand.field = c('STRAND','Strand'))
    return (region_gr)
}

#' @description Load the column types from DeepBlue
#' @title get columns
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
    
    return(dict)
}

col_dict = deepblue.column_types()