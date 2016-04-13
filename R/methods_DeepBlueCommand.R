setMethod("show", 
          signature = "DeepBlueCommand",
          function(object) {
              cat("An object of class ", class(object), "\n", sep = "")
              cat("Query status:", object@status, "\n")
              cat("Query ID:", object@query_id, "\n")
              return(object@query_id)  
          })

setMethod("deepblue.download_request_data",
          signature = c("DeepBlueCommand"),
          function(request_id){
              request_id@result <- deepblue.download_request_data(request_id = request_id@query_id, user_key = request_id@user_key)
              return(request_id)
          })