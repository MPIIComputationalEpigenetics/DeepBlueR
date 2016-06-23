#'@title show DeepBlue command object
#'@keywords internal
#'@import methods
#'@description Provides a summary of a DeepBlue command object
#'@return query ID of the object
#'@param object DeepBlueCommand object
setMethod("show",
          signature = "DeepBlueCommand",
          function(object) {
              cat("An object of class ", class(object), "\n", sep = "")
              cat("Query status:", object@status, "\n")
              cat("Query ID:", object@query_id, "\n")
              return(object@query_id)
          })


#'@title deepblue_download_request_data
#'@export
#'@import methods
#'@description Returns the requested data as the expected type object.
#'Expects two input parameters; Request information and
#'user key. It depends on outputs from several functions, namely;
#'deepblue_get_request_data, convert_to_df, and convert_to_grange.
#'
#'@param request_id DeepBlueCommand object
#'
#'@return grange_regions Final output in GRanges format
setMethod("deepblue_download_request_data",
          signature = c("DeepBlueCommand"),
          function(request_id){
              user_key = request_id@user_key
              request_id = request_id@query_id

              request_info = deepblue_wait_request(request_id,
                                                   user_key=user_key)

              if (request_info$state == "done") {
                  message("The request was processed successfully.")
              } else {
                  stop(request_info$message)
              }

              request_data = deepblue_switch_get_request_data(
                  request_id = request_id, user_key = user_key)

              if(request_info$command == "count_regions")
                  return(as.numeric(request_data))

              # Only the get_regions and score_matrix commands can have
              # the data converted to tables.
              if (!request_info$command %in% c('get_regions','score_matrix')) {
                return(request_data)
              }

              regions_df = deepblue_convert_to_df(
                  string_to_parse=request_data, request_info=request_info)

              if (request_info$command %in%
                  c("score_matrix", "get_experiments_by_query") ||
                  request_info$format == "")
                  return (regions_df)

              else if(request_info$command == "get_regions"){
                  if(nrow(regions_df) > 0)
                      return(deepblue_convert_to_grange(df=regions_df))
                  else
                      stop("No regions were returned in this request.")
              }
          })