#'@title DeepBlueCommand class
#'@export 
#'@description An S4 class returned when calling a DeepBlue-R function. It holds information about
#'the original call, the query / request status, previous commands, the user_key, and results in GRanges format once
#'a request is downloaded.#'
#'@param call language
#'@param status character
#'@param query_id character
#'@param previous_commands list
#'@param user_key character
#'@param result GRanges 
DeepBlueCommand <- setClass("DeepBlueCommand",
                            slots = c(call = "language",
                                      status = "character",
                                      query_id = "character",
                                      previous_commands = "list",
                                      user_key = "character",
                                      result = "GRanges"),
                            validity=function(object) {
                                if (object@status == "error")
                                    return(FALSE)
                                else return(TRUE)
                            }, 
                            prototype = list(result = GRanges(), 
                                             user_key = "anonymous_r_user",
                                             previous_commands = list())
)
