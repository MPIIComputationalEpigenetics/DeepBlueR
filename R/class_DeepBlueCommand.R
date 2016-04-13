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
