suppressMessages(library(GenomicRanges))

#'process_request
#'
#'Process the user request. Takes in three parameters; requested regions, sleep time, and user key.
#'@param requested_regions A string 
#'@param sleep.time An integer with default value 1s
#'@param user_key A string 

process_request = function (requested_regions,sleep.time = 1, user_key=deepblue.USER_KEY)
{
  info = deepblue.info(as.character(requested_regions[2]), user_key)
  
  state = info[[2]]$value$state
  while (state != 'done' & state != 'error')
  {
    Sys.sleep(sleep.time)
    info = deepblue.info(as.character(requested_regions[2]), user_key)
    state = info[[2]]$value$state
  }
  info
}

#'convert_to_df
#'save output in a data frame for further processing.Expects two parameters; the output string from 
#'method deepblue.get_request_data and request information from method process_request.
#'
#' @param output A string 
#' @param inf A list with request information
#' @return regions A data frame  

convert_to_df = function(output=NULL,inf=NULL)
{
  final = unlist(strsplit(as.character(output),'\n'))
  final = as.data.frame(final, stringsAsFactors=FALSE)
  regions = data.frame(do.call('rbind', strsplit(as.character(final$final),'\t',fixed=TRUE)),stringsAsFactors=FALSE)
  colnames(regions) = unlist(strsplit(inf$value$value$format,','))
  regions = convert_type(regions)
  return (regions)
}

#'convert_type
#'Sets data types of each column in the data frame. Expects one input; A dataframe.
#'
#'@param df A dataframe
#'@return df A dataframe

convert_type = function(df=NULL)
{
  stopifnot(is.list(df))
  df[] = rapply(df,utils::type.convert,classes = 'character', how = 'replace', as.is = TRUE)
  return(df)
}

#extract value from list

get_value = function (input = NULL)
{
  value = as.character(input[2])
  return (value)
}

#'convert_to_grange
#'Converts the requested data into GRanges object. Expects one input; A dataframe with requested data.
#'
#'@param df A data frame
#'@return region_gr A GRanges object
#'
#'@seealso \code{\link[GenomicRanges]{makeGRangesFromDataFrame}}

convert_to_grange = function (df = NULL)
{
  region_gr = makeGRangesFromDataFrame(df, keep.extra.columns = TRUE,
                                       seqnames.field = 'CHROMOSOME', start.field = 'START',
                                       end.field = 'END',strand.field = c('STRAND','Strand'))
  return (region_gr)
}

#'get_request_data
#'Returns the requested data as GRanges object. Expects two input parameters; Request information and
#'user key. It depends on outputs from several functions, namely;
#'deepblue.get_request_data, convert_to_df, and convert_to_grange.
#'
#'@param request_info A list
#'@param user A string
#'
#'@return grange_regions Final output in GRanges format
#'@seealso \code{\link{deepblue.get_request_data}}, \code{\link{convert_to_df}}, and
#'\code{\link{convert_to_grange}}.

get_request_data = function (request_info=NULL, user=deepblue.USER_KEY)
  
{
  request_id = request_info[[2]]$value$`_id`
  final_regions = deepblue.get_request_data(request_id = request_id, user_key = user)
  regions = convert_to_df(output=final_regions[2], inf=request_info[2])
  grange_regions = convert_to_grange(df=regions)
  return (grange_regions)
}
