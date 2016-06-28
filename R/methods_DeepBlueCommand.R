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

                deepblue_switch_get_request_data(request_id = request_id,
                                                      user_key = user_key)
          })