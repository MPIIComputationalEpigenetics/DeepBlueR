setMethod("show", 
          signature = "DeepBlueCommand",
          function(object) {
              cat("An object of class ", class(object), "\n", sep = "")
              cat("Query status:", object@status, "\n")
              cat("Query ID:", object@query_id, "\n")
              return(object@query_id)  
          })


#'@title deepblue.download_request_data
#'@export 
#'@description Returns the requested data as the expected type object. Expects two input parameters; Request information and
#'user key. It depends on outputs from several functions, namely;
#'deepblue.get_request_data, convert_to_df, and convert_to_grange.
#'
#'@param request_id DeepBlueCommand object 
#'
#'@return grange_regions Final output in GRanges format
setMethod("deepblue.download_request_data",
          signature = c("DeepBlueCommand"),
          function(request_id){
              user_key = request_id@user_key
              request_id = request_id@query_id
              
              request_info = deepblue.wait_request(request_id, user_key=user_key)
              
              if (request_info$state == "done") {
                  message("The request was processed successfully.")
              } else {
                  stop(request_info$message)
              }
              
              regions_string = deepblue.switch_get_request_data(request_id = request_id, user_key = user_key)
              
              if(request_info$command == "count_regions") return(as.numeric(regions_string))
              
              regions_df = deepblue.convert_to_df(string_to_parse=regions_string, request_info=request_info)
              
              if (request_info$command %in% c("score_matrix", "get_experiments_by_query") || request_info$format == "") return (regions_df)
              
              else if(request_info$command == "get_regions") return(deepblue.convert_to_grange(df=regions_df))
          })