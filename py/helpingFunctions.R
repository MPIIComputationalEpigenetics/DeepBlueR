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

convert_to_df_fast = function(output, inf, dict=col_dict){

    #dependencies used here
    library(data.table)
    library(stringr)
    
    #get column names from 
    col_names <- str_split(inf$format, pattern = ",")[[1]]
    
    #get column types from dictionary
    col_types <- sapply(col_names, function(x){
        col_type <- dict[[x]]
        #return column type, defaults to character
        if(is.null(col_type)) return("character")
        #need to change string to character
        else if(col_type == "string") return("character")
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

convert_to_df = function(output=NULL,inf=NULL)
{
  final = unlist(strsplit(as.character(output),'\n'))
  final = as.data.frame(final, stringsAsFactors=FALSE)
  regions = data.frame(do.call('rbind', strsplit(as.character(final$final),'\t',fixed=TRUE)),stringsAsFactors=FALSE)
  colnames(regions) = unlist(strsplit(inf$value$value$format,','))
  regions = convert_type(regions,col_dict)
  return (regions)
}

#'convert_type
#'Sets data types of each column in the data frame. Expects two input; A dataframe and a dictionary.
#'
#'@param df A dataframe
#'@param dict A dictionary with column names as keys and column types as values.  
#'@return df A dataframe

convert_type = function(df=NULL,dict)
{
  stopifnot(is.list(df))
  cols = colnames(df)
  types = c()
  for (i in cols)
  {
    types=c(types,dict[[i]])
  }
  for (i in 1:length(types))
  {
    if (types[i] == "integer") 
    { 
      df[i] <- as.integer(df[[i]])
    }
    else if (types[i] == "character")
    {
      df[i] <- as.character(df[[i]])
    }
    else if (types[i] == "double")
    {
      df[i] <- as.double(df[[i]])
    }
    else 
    {
      df[i] <- as.factor(df[[i]])
    }
  }
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

#//TODO: REMOVE??
get_request_data = function (request_info=NULL, user=deepblue.USER_KEY, type="grange")
{
    request_id = request_info[[2]]$value$`_id`
    final_regions = deepblue.get_request_data(request_id = request_id, user_key = user)
    regions = convert_to_df_fast(output=final_regions[[2]][[1]], inf=request_info[[2]][[1]])
    
    if(type == "grange") return(convert_to_grange(df=regions))
    else return (regions)
}
#TODO: REMOVE??
get_columns = function()
{
  cols = deepblue.list_column_types()
  col_ids = c()
  for (i in 1:length(cols[[2]]))
  {
    co=cols[[2]][i]$value[[1]]
    col_ids = c(col_ids,co)
  }
  
  col_names=c()
  col_types = c()
  
  for (i in col_ids)
  {
    inf = deepblue.info(i)
    col_names=c(col_names,inf[[2]]$value$name)
    col_types=c(col_types,inf[[2]]$value$column_type)
  }
  
  col_info=data.frame(col_names,col_types,stringsAsFactors = FALSE)
  col_info[col_info=='category']='factor'
  dict=new.env()
  for(i in seq(nrow(col_info)))
  {
    dict[[col_info[i,1]]] = col_info[i,2]
  }
  return(dict)
}
col_dict = get_columns()