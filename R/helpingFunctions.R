#'@export
#'@title process_request
#'@description Process the user request. Takes in three parameters; requested regions, sleep time, and user key.
#'@param request_id A string with the request_id
#'@param sleep.time An integer with default value 1s
#'@param user_key A string

deepblue.process_request = function (request_id,sleep.time = 1, user_key=deepblue.USER_KEY)
{
  info = deepblue.info(request_id, user_key)[[1]]

  state = info$state
  while (state != 'done' & state != 'error')
  {
    Sys.sleep(sleep.time)
    info = deepblue.info(request_id, user_key)[[1]]
    state = info$state
  }
  return (info)
}

#'@title convert_to_df
#'@description save output in a data frame for further processing.Expects two parameters; the output string from method deepblue.get_request_data and request information from method process_request.
#' @param output A string
#' @param inf A list with request information
#' @return regions A data frame

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
#'
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

#' @export
#'@title get_request_data
#'@description Returns the requested data as GRanges object. Expects two input parameters; Request information and
#'user key. It depends on outputs from several functions, namely;
#'deepblue.get_request_data, convert_to_df, and convert_to_grange.
#'
#'@param request_info A list
#'@param user A string
#'
#'@return grange_regions Final output in GRanges format
#'@seealso \code{\link{deepblue.get_request_data}}, \code{\link{convert_to_df}}, and
#'\code{\link{convert_to_grange}}.


deepblue.get_requested_data = function (request_info, user=deepblue.USER_KEY, type="grange")
{
    request_id = request_info$`_id`
    regions_string = deepblue.get_request_data_r(request_id = request_id, user_key = user)

    if (type == "string") return (regions_string)

    regions_df = deepblue.convert_to_df(output=regions_string, inf=request_info)
    if (type == "df") return (regions_df)

    return(deepblue.convert_to_grange(df=regions_df))
}

#' @description Load the column types from DeepBlue
#' @title get columns
get_columns = function()
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
#' @title coulmns dictionary
col_dict = get_columns()