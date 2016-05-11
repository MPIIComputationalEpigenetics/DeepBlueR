# Accessing Deepblue through R
# For DeepBlue version 1.7.7

# We include a modified version of the XML-RPC library (http://bioconductor.org/packages/release/extra/html/XMLRPC.html) for R in this file.
#' @title deepblue URL
#' @description Location of the DeepBlue XML-RPC server
#' @keywords internal
deepblue.URL = "http://deepblue.mpi-inf.mpg.de/xmlrpc"
#' @title default User Key
#' @description Default anonymous user key
#' @keywords internal
deepblue.USER_KEY = "anonymous_key"
#' @title Verbose
#' @description Show or hide debugging messages
#' @keywords internal
deepblue.debug.VERBOSE = FALSE




#' @export 
#' 
#' @title aggregate 
#' @description Summarize the data regions content in range regions. Use the fields @AGG.MIN, @AGG.MAX, @AGG.MEDIAN, @AGG.MEAN, @AGG.VAR, @AGG.SD, @AGG.COUNT in the get_regions command format parameter for retrieving the computed values.
#' @family Operating on the data regions
#' 
#' @param data_id - A string (id of the query with the data)
#' @param ranges_id - A string (id of the query with the regions range)
#' @param column - A string (name of the column that will be used in the aggregation)
#' @param user_key - A string (users token key)
#'
#' @return regions - A string (query id of this aggregation operation)
#'
#' @examples
#' annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
#' 	genome="hg19", chromosome="chr1")
#' data_id = deepblue.select_experiments(
#' 	experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue.aggregate(data_id = data_id, ranges_id=annotation_id,
#' 	column = "SCORE")
#'
deepblue.aggregate <- function(data_id= NULL, ranges_id= NULL, column= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'aggregate', data_id, ranges_id, column, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title cancel_request 
#' @description Stop, cancel, and remove request data. Its data will be remove if the request did finish.
#' @family Commands for all types of data
#' 
#' @param id - A string (Request ID to be canceled, stopped or removed.)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (ID of the canceled request)
#'
#' @examples
#' deepblue.cancel_request(id = "r12345")
#'
deepblue.cancel_request <- function(id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'cancel_request', id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title chromosomes 
#' @description List all chromosomes of a given genome.
#' @family Inserting and listing genomes
#' 
#' @param genome - A string (the target genome)
#' @param user_key - A string (users token key)
#'
#' @return chromosomes - A array (A list containing all chromosomes, with theirs names and sizes)
#'
#' @examples
#' deepblue.chromosomes(genome = "g1")
#'
deepblue.chromosomes <- function(genome= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'chromosomes', genome, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title collection_experiments_count 
#' @description Count the number of elements of the given collection that contains experiments.
#' @family Inserting and listing experiments
#' 
#' @param controlled_vocabulary - A string (controlled vocabulary name)
#' @param genome - A string or a vector of string (the target genome)
#' @param type - A string or a vector of string (type of the experiment: peaks or signal)
#' @param epigenetic_mark - A string or a vector of string (name(s) of selected epigenetic mark(s))
#' @param biosource - A string or a vector of string (name(s) of selected biosource(s))
#' @param sample - A string or a vector of string (id(s) of selected sample(s))
#' @param technique - A string or a vector of string (name(s) of selected technique(s))
#' @param project - A string or a vector of string (name(s) of selected projects)
#' @param user_key - A string (users token key)
#'
#' @return terms - A array (controlled_vocabulary terms with count)
#'
#' @examples
#' deepblue.collection_experiments_count(controlled_vocabulary="epigenetic_marks",
#'   genome = "hg19", type = "peaks", biosource = "blood")
#'
deepblue.collection_experiments_count <- function(controlled_vocabulary= NULL, genome= NULL, type= NULL, epigenetic_mark= NULL, biosource= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'collection_experiments_count', controlled_vocabulary, genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title commands 
#' @description Lists all existing commands.
#' @family Checking DeepBlue status
#' 
#'
#' @return commands - A struct (command descriptions)
#'
#' @examples
#' deepblue.commands()
#'
deepblue.commands <- function() {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'commands')
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title count_regions 
#' @description Send a request to count the number of regions in the result of the given query.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param user_key - A string (users token key)
#'
#' @return request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data())
#'
#' @examples
#' data_id = deepblue.select_experiments(
#' 	experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue.count_regions(query_id = data_id)
#'
deepblue.count_regions <- function(query_id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'count_regions', query_id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title echo 
#' @description Echos the server's version.
#' @family Checking DeepBlue status
#' 
#' @param user_key - A string (users token key)
#'
#' @return message - A string (echo message including version)
#'
#' @examples
#' deepblue.echo(user_key = "anonymous_key")
#'
deepblue.echo <- function(user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'echo', user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title extend 
#' @description Extend regions.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (id of the query that contains the regions)
#' @param length - A int (The new region length)
#' @param direction - A string (The direction that the region will be extended: 'BACKWARD', 'FORWARD', 'BOTH'. (Empty value will be used for both direction.)
#' @param use_strand - A boolean (Use the region column STRAND to define the region direction)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new query)
#'
#' @examples
#' annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
#' 	genome="hg19", chromosome="chr1")
#' deepblue.extend(query_id = annotation_id, length = 2000, direction = "BOTH",
#'   use_strand = TRUE)
#'
deepblue.extend <- function(query_id= NULL, length= NULL, direction= NULL, use_strand= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'extend', query_id, if (is.null(length)) NULL else as.integer(length), direction, use_strand, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title extract_ids 
#' @description Extract the names from a list of ID and Names.
#' @family Utilities for connecting operations
#' 
#' @param list - A array (list of lists of IDs and Names)
#'
#' @return ids - A array (list containing the extracted IDs)
#'
#' @examples
#' deepblue.extract_ids(list = list(list("a124", "Annotation 1"),
#'     list("a1235", "Annotation 2")))
#'
deepblue.extract_ids <- function(list= NULL) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'extract_ids', list)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title extract_names 
#' @description Extract the names from a list of ID and Names.
#' @family Utilities for connecting operations
#' 
#' @param list - A array (list of lists of IDs and Names)
#'
#' @return names - A array (list containing the extracted names)
#'
#' @examples
#' deepblue.extract_names(
#'     list = list(list("a124", "Annotation 1"),
#'         list("a1235", "Annotation 2")))
#'
deepblue.extract_names <- function(list= NULL) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'extract_names', list)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title faceting_experiments 
#' @description Experiments faceting.
#' @family Inserting and listing experiments
#' 
#' @param genome - A string or a vector of string (the target genome)
#' @param type - A string or a vector of string (type of the experiment: peaks or signal)
#' @param epigenetic_mark - A string or a vector of string (name(s) of selected epigenetic mark(s))
#' @param biosource - A string or a vector of string (name(s) of selected biosource(s))
#' @param sample - A string or a vector of string (id(s) of selected sample(s))
#' @param technique - A string or a vector of string (name(s) of selected technique(s))
#' @param project - A string or a vector of string (name(s) of selected projects)
#' @param user_key - A string (users token key)
#'
#' @return faceting - A struct (Map with the mandatory fields of the experiments metadata, where each contains a list of terms that appears.)
#'
#' @examples
#' deepblue.faceting_experiments(genome = "hg19", type = "peaks",
#'   biosource = "blood")
#'
deepblue.faceting_experiments <- function(genome= NULL, type= NULL, epigenetic_mark= NULL, biosource= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'faceting_experiments', genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title filter_regions 
#' @description Filters the result of the given query by the given restrictions.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (id of the query to be filtered)
#' @param field - A string (field that is filtered by)
#' @param operation - A string (operation used for filtering. For 'string' must be '==' or '!=' and for 'number' must be one of these: ==,!=,>,>=,<,<=)
#' @param value - A string (value the operator is applied to)
#' @param type - A string (type of the value: 'number' or 'string' )
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of filtered query)
#'
#' @examples
#' deepblue.filter_regions(query_id = "q12345", field = "VALUE", operation = ">",
#'   value = "100", type = "number", user_key = "anonymous_key")
#'
deepblue.filter_regions <- function(query_id= NULL, field= NULL, operation= NULL, value= NULL, type= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'filter_regions', query_id, field, operation, value, type, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title flank 
#' @description Generate flanking regions from the given regions.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (id of the query that contains the regions)
#' @param start - A int (Number of base pairs after the end of the region. Use a negative number to denote the number of base pairs before the start of the region.)
#' @param length - A int (The new region length)
#' @param use_strand - A boolean (Use the region column STRAND to define the region direction)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new query)
#'
#' @examples
#' annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
#' 	genome="hg19", chromosome="chr1")
#' deepblue.flank(query_id = annotation_id, start = 0, length = 2000,
#'   use_strand = TRUE)
#'
deepblue.flank <- function(query_id= NULL, start= NULL, length= NULL, use_strand= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'flank', query_id, if (is.null(start)) NULL else as.integer(start), if (is.null(length)) NULL else as.integer(length), use_strand, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_children 
#' @description Gets the scope for the biosource.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (name of the biosource)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (related biosources)
#'
#' @examples
#' deepblue.get_biosource_children(biosource = "Blood")
#'
deepblue.get_biosource_children <- function(biosource= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'get_biosource_children', biosource, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_parents 
#' @description Gets the biosources that are parents of the given biosource.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (name of the biosource)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (parents biosources)
#'
#' @examples
#' deepblue.get_biosource_parents(biosource = "Blood")
#'
deepblue.get_biosource_parents <- function(biosource= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'get_biosource_parents', biosource, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_related 
#' @description Gets biosources related to the given one. e.g. the children terms and theirs synonyms.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (name of the biosource)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (related biosources)
#'
#' @examples
#' deepblue.get_biosource_related(biosource = "Blood")
#'
deepblue.get_biosource_related <- function(biosource= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'get_biosource_related', biosource, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_synonyms 
#' @description Gets the synonyms for the biosource.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (name of the biosource)
#' @param user_key - A string (users token key)
#'
#' @return synonyms - A array (synonyms of the biosource)
#'
#' @examples
#' deepblue.get_biosource_synonyms(biosource = "prostate gland")
#'
deepblue.get_biosource_synonyms <- function(biosource= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'get_biosource_synonyms', biosource, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title get_experiments_by_query 
#' @description Return a list of experiments and annotations that have at least one region in the data set represented by the query.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (id of the query)
#' @param user_key - A string (users token key)
#'
#' @return experiments - A array (List containing experiments names and ids)
#'
#' @examples
#' deepblue.get_experiments_by_query(query_id = "q12345")
#'
deepblue.get_experiments_by_query <- function(query_id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'get_experiments_by_query', query_id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title get_regions 
#' @description Send a request  to retrieve the regions for the given query in the requested BED format.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param output_format - A string (Output format)
#' @param user_key - A string (users token key)
#'
#' @return request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data())
#'
#' @examples
#' data_id = deepblue.select_experiments(
#' 	experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue.get_regions(query_id =data_id, output_format = "CHROMOSOME,START,END")
#'
deepblue.get_regions <- function(query_id= NULL, output_format= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'get_regions', query_id, output_format, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title get_request_data 
#' @description Get the request data.
#' @family Requests status information and results
#' 
#' @param request_id - A string (ID of the request)
#' @param user_key - A string (users token key)
#'
#' @return data - A string or a vector of string (The output can be (i) a string (get_regions, score_matrix, and count_regions), or (ii) a list of ID and names (get_experiments_by_query).)
#'
#' @examples
#' data_id = deepblue.select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed", chromosome="chr1")
#' request_id = deepblue.get_regions(
#'     query_id =data_id, output_format = "CHROMOSOME,START,END")
#' deepblue.get_request_data(request_id = request_id)
#'
deepblue.get_request_data <- function(request_id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'get_request_data', request_id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title info 
#' @description Return information for the given ID (or IDs).
#' @family Commands for all types of data
#' 
#' @param id - A string or a vector of string (ID or an array of IDs)
#' @param user_key - A string (users token key)
#'
#' @return information - A array or a vector of array (List of Maps, where each map contains the info of an object.)
#'
#' @examples
#' deepblue.info(id = "e30035")
#'
deepblue.info <- function(id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'info', id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title input_regions 
#' @description Include a region set that will be used by the follow ups operations.
#' @family Operating on the data regions
#' 
#' @param genome - A string (the target genome)
#' @param region_set - A string (Regions in CHROMOSOME	START	END format)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' regions_set = "chr1	28735	29810
#' chr1	135124	135563
#' chr1	327790	328229
#' chr1	437151	438164
#' chr1	449273	450544
#' chr1	533219	534114
#' chr1	544738	546649
#' chr1	713984	714547
#' chr1	762416	763445
#' chr1	788863	789211"
#' deepblue.input_regions(genome = "hg19", region_set = regions_set)
#'
deepblue.input_regions <- function(genome= NULL, region_set= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'input_regions', genome, region_set, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title intersection 
#' @description Select regions from the first query that does intersect with at least one second query region.
#' @family Operating on the data regions
#' 
#' @param query_a_id - A string (id of the first query)
#' @param query_b_id - A string (id of the second query)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new query)
#'
#' @examples
#' annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
#' 	genome="hg19", chromosome="chr1")
#' data_id = deepblue.select_experiments(
#' 	experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue.intersection(query_a_id = annotation_id, query_b_id = data_id)
#'
deepblue.intersection <- function(query_a_id= NULL, query_b_id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'intersection', query_a_id, query_b_id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title is_biosource 
#' @description Return information for the given biosource name.
#' @family Commands for all types of data
#' 
#' @param biosource_name - A string (Name of the biosource)
#' @param user_key - A string (users token key)
#'
#' @return information - A string or a vector of string (A string containing the biosource name)
#'
#' @examples
#' deepblue.is_biosource(biosource_name = "blood")
#'
deepblue.is_biosource <- function(biosource_name= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'is_biosource', biosource_name, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_annotations 
#' @description Lists all existing annotations.
#' @family Inserting and listing annotations
#' 
#' @param genome - A string or a vector of string (the target genome)
#' @param user_key - A string (users token key)
#'
#' @return annotations - A array (annotation ids)
#'
#' @examples
#' deepblue.list_annotations(genome = "hg19")
#'
deepblue.list_annotations <- function(genome= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_annotations', genome, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_biosources 
#' @description Lists all existing biosources.
#' @family Inserting and listing biosources
#' 
#' @param extra_metadata - A struct (Key-value that must match the biosource extra_metadata.)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (biosources)
#'
#' @examples
#' deepblue.list_biosources(extra_metadata = list(namespace = "uberon"))
#'
deepblue.list_biosources <- function(extra_metadata=NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_biosources', extra_metadata, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_column_types 
#' @description Lists all available column types.
#' @family Inserting and listing different column types
#' 
#' @param user_key - A string (users token key)
#'
#' @return column_types - A array (column types)
#'
#' @examples
#' deepblue.list_column_types()
#'
deepblue.list_column_types <- function(user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_column_types', user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_epigenetic_marks 
#' @description Lists all existing epigenetic marks.
#' @family Inserting and listing epigenetic marks
#' 
#' @param extra_metadata - A struct (Key-value that must match the biosource extra_metadata.)
#' @param user_key - A string (users token key)
#'
#' @return epigenetic_marks - A array (epigenetic mark names)
#'
#' @examples
#' deepblue.list_epigenetic_marks()
#'
deepblue.list_epigenetic_marks <- function(extra_metadata=NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_epigenetic_marks', extra_metadata, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_experiments 
#' @description Lists all existing experiments.
#' @family Inserting and listing experiments
#' 
#' @param genome - A string or a vector of string (the target genome)
#' @param type - A string or a vector of string (type of the experiment: peaks or signal)
#' @param epigenetic_mark - A string or a vector of string (name(s) of selected epigenetic mark(s))
#' @param biosource - A string or a vector of string (name(s) of selected biosource(s))
#' @param sample - A string or a vector of string (id(s) of selected sample(s))
#' @param technique - A string or a vector of string (name(s) of selected technique(s))
#' @param project - A string or a vector of string (name(s) of selected projects)
#' @param user_key - A string (users token key)
#'
#' @return experiments - A array (experiment names)
#'
#' @examples
#' deepblue.list_experiments(genome = "hg19", type = "peaks",
#'   epigenetic_mark = "H3K27ac", biosource = "blood")
#'
deepblue.list_experiments <- function(genome= NULL, type= NULL, epigenetic_mark= NULL, biosource= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_experiments', genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_genomes 
#' @description Lists all existing genomes.
#' @family Inserting and listing genomes
#' 
#' @param user_key - A string (users token key)
#'
#' @return genomes - A array (genome names)
#'
#' @examples
#' deepblue.list_genomes()
#'
deepblue.list_genomes <- function(user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_genomes', user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_in_use 
#' @description Lists all terms from the given controlled vocabulary that are used.
#' @family Commands for all types of data
#' 
#' @param controlled_vocabulary - A string (controlled vocabulary name)
#' @param user_key - A string (users token key)
#'
#' @return terms - A array (controlled_vocabulary terms with count)
#'
#' @examples
#' deepblue.list_in_use(controlled_vocabulary = "biosources")
#'
deepblue.list_in_use <- function(controlled_vocabulary= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_in_use', controlled_vocabulary, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_projects 
#' @description Lists all existing projects.
#' @family Inserting and listing projects
#' 
#' @param user_key - A string (users token key)
#'
#' @return projects - A array (project names)
#'
#' @examples
#' deepblue.list_projects()
#'
deepblue.list_projects <- function(user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_projects', user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_recent_experiments 
#' @description Lists all recent experiments.
#' @family Inserting and listing experiments
#' 
#' @param days - A double (maximum days ago the experiments were added)
#' @param genome - A string or a vector of string (the target genome)
#' @param epigenetic_mark - A string or a vector of string (name(s) of selected epigenetic mark(s))
#' @param sample - A string or a vector of string (id(s) of selected sample(s))
#' @param technique - A string or a vector of string (name(s) of selected technique(es))
#' @param project - A string or a vector of string (name(s) of selected projects)
#' @param user_key - A string (users token key)
#'
#' @return experiments - A array (names of recent experiments)
#'
#' @examples
#' deepblue.list_recent_experiments(days = 2, genome = "hg19")
#'
deepblue.list_recent_experiments <- function(days= NULL, genome= NULL, epigenetic_mark= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_recent_experiments', days, genome, epigenetic_mark, sample, technique, project, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_requests 
#' @description Lists all requests in given state.
#' @family Requests status information and results
#' 
#' @param request_state - A string (Name of the state to get requests for. The valid states are: new, running, done, and failed.)
#' @param user_key - A string (users token key)
#'
#' @return data_state - A array (Request-IDs and their state)
#'
#' @examples
#' deepblue.list_requests(request_state = 'running')
#'
deepblue.list_requests <- function(request_state= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_requests', request_state, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_samples 
#' @description Lists all existing samples that matches the given biosource and metadata.
#' @family Inserting and listing samples
#' 
#' @param biosource - A string or a vector of string (biosource name)
#' @param extra_metadata - A struct (Key-value that must match the sample extra_metadata.)
#' @param user_key - A string (users token key)
#'
#' @return samples - A array (samples id with their content)
#'
#' @examples
#' deepblue.list_samples(biosource = "Blood")
#'
deepblue.list_samples <- function(biosource= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_samples', biosource, extra_metadata, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_biosources 
#' @description Lists all biosources similar to the one provided.
#' @family Inserting and listing biosources
#' 
#' @param name - A string (biosource name)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (similar biosources)
#'
#' @examples
#' deepblue.list_similar_biosources(name = "blood")
#'
deepblue.list_similar_biosources <- function(name= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_similar_biosources', name, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_epigenetic_marks 
#' @description Lists all epigenetic marks similar to the one provided.
#' @family Inserting and listing epigenetic marks
#' 
#' @param name - A string (epigenetic mark name)
#' @param user_key - A string (users token key)
#'
#' @return epigenetic_marks - A array (similar epigenetic mark names)
#'
#' @examples
#' deepblue.list_similar_epigenetic_marks(name = "H3k27ac")
#'
deepblue.list_similar_epigenetic_marks <- function(name= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_similar_epigenetic_marks', name, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_experiments 
#' @description Lists all experiments similar to the one provided.
#' @family Inserting and listing experiments
#' 
#' @param name - A string (experiment name)
#' @param genome - A string or a vector of string (the target genome)
#' @param user_key - A string (users token key)
#'
#' @return experiments - A array (similar experiment names)
#'
#' @examples
#' deepblue.list_similar_experiments(name = "blood", genome = "hg19")
#'
deepblue.list_similar_experiments <- function(name= NULL, genome= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_similar_experiments', name, genome, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_genomes 
#' @description Lists all genomes similar to the one provided.
#' @family Inserting and listing genomes
#' 
#' @param name - A string (genome name)
#' @param user_key - A string (users token key)
#'
#' @return genomes - A array (similar genome names)
#'
#' @examples
#' deepblue.list_similar_genomes(name = "grc")
#'
deepblue.list_similar_genomes <- function(name= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_similar_genomes', name, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_projects 
#' @description Lists all projects similar to the one provided.
#' @family Inserting and listing projects
#' 
#' @param name - A string (project name)
#' @param user_key - A string (users token key)
#'
#' @return projects - A array (similar project names)
#'
#' @examples
#' deepblue.list_similar_projects(name = "BLUEPRINT")
#'
deepblue.list_similar_projects <- function(name= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_similar_projects', name, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_techniques 
#' @description Lists all techniques similar to the one provided.
#' @family Inserting and listing techniques
#' 
#' @param name - A string (technique name)
#' @param user_key - A string (users token key)
#'
#' @return techniques - A array (similar techniques)
#'
#' @examples
#' deepblue.list_similar_techniques(name = "chip seq")
#'
deepblue.list_similar_techniques <- function(name= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_similar_techniques', name, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title list_techniques 
#' @description Lists all existing techniques.
#' @family Inserting and listing techniques
#' 
#' @param user_key - A string (users token key)
#'
#' @return techniques - A array (techniques)
#'
#' @examples
#' deepblue.list_techniques()
#'
deepblue.list_techniques <- function(user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'list_techniques', user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title merge_queries 
#' @description Merges the regions of the given queries.
#' @family Operating on the data regions
#' 
#' @param query_a_id - A string (id of the first query)
#' @param query_b_id - A string (id of the second query)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (new query id)
#'
#' @examples
#' annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
#' 	genome="hg19", chromosome="chr1")
#' data_id = deepblue.select_experiments(
#' 	experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue.merge_queries(query_a_id = annotation_id, query_b_id = data_id)
#'
deepblue.merge_queries <- function(query_a_id= NULL, query_b_id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'merge_queries', query_a_id, query_b_id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title query_cache 
#' @description Return information for the given ID (or IDs).
#' @family Operating on the data regions
#' 
#' @param query_id - A string (query ID)
#' @param cache - A boolean (set or unset this query caching)
#' @param user_key - A string (users token key)
#'
#' @return information - A string (New query ID.)
#'
#' @examples
#' annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
#' 	genome="hg19", chromosome="chr1")
#' data_id = deepblue.select_experiments(
#' 	experiment_name="E002-H3K9ac.narrowPeak.bed")
#' merged_regions = deepblue.merge_queries(query_a_id = annotation_id,
#' 	query_b_id = data_id)
#' deepblue.query_cache(query_id = merged_regions, cache = TRUE)
#'
deepblue.query_cache <- function(query_id= NULL, cache= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'query_cache', query_id, cache, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title query_experiment_type 
#' @description Return information for the given ID (or IDs).
#' @family Operating on the data regions
#' 
#' @param query_id - A string (query ID)
#' @param type - A string (experiment type (peaks or signal))
#' @param user_key - A string (users token key)
#'
#' @return information - A string (New query ID.)
#'
#' @examples
#' h3k27ac_regions = deepblue.select_regions(genome ='GRCh38',
#'                                       epigenetic_mark ='H3k27ac',
#'                                       project ='BLUEPRINT Epigenome',
#'                                       chromosome ='chr1')
#' deepblue.query_experiment_type(query_id = h3k27ac_regions, type = "peaks")
#'
deepblue.query_experiment_type <- function(query_id= NULL, type= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'query_experiment_type', query_id, type, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title score_matrix 
#' @description Build a matrix containing the aggregation result of the the experiments data by aggregation regions.
#' @family Operating on the data regions
#' 
#' @param experiments_columns - A struct (map with experiments names and columns to be processed. Example : {'wgEncodeBroadHistoneDnd41H3k27acSig.wig':'VALUE', 'wgEncodeBroadHistoneCd20ro01794H3k27acSig.wig':'VALUE'})
#' @param aggregation_function - A string (aggregation function name: min, max, mean, var, sd, median, count, boolean)
#' @param aggregation_regions_id - A string (query ID of the regions that will be used as the aggregation boundaries)
#' @param user_key - A string (users token key)
#'
#' @return regions - A string (BED formated regions)
#'
#' @examples
#' tiling_regions = deepblue.tiling_regions(
#'     size=100000, genome="hg19", chromosome="chr1")
#' deepblue.score_matrix(
#'     experiments_columns =
#'         list(ENCFF721EKA="VALUE", ENCFF781VVH="VALUE"),
#'   aggregation_function = "mean",
#'   aggregation_regions_id = tiling_regions)

#'
deepblue.score_matrix <- function(experiments_columns= NULL, aggregation_function= NULL, aggregation_regions_id= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'score_matrix', experiments_columns, aggregation_function, aggregation_regions_id, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title search 
#' @description Search all data of all types for the given keyword. A minus (-) character in front of a keyword searches for data without the given keyword. The search can be restricted to the following data types are: annotations,biosources,column_types,epigenetic_marks,experiments,genomes,gene_sets,genes,projects,samples,techniques,tilings
#' @family Commands for all types of data
#' 
#' @param keyword - A string (keyword to search by)
#' @param type - A string or a vector of string (type of data to search for)
#' @param user_key - A string (users token key)
#'
#' @return results - A array (search results as [id, name, type])
#'
#' @examples
#' deepblue.search(keyword = "DNA Methylation BLUEPRINT", type = "experiments")
#'
deepblue.search <- function(keyword= NULL, type= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'search', keyword, type, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title select_annotations 
#' @description Selects annotation regions matching the given parameters.
#' @family Operating on the data regions
#' 
#' @param annotation_name - A string or a vector of string (name(s) of selected annotation(s))
#' @param genome - A string or a vector of string (the target genome)
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' deepblue.select_annotations(annotation_name = "Cpg Islands", genome = "hg19",
#'   chromosome = "chr1", start = 0, end = 2000000)
#'
deepblue.select_annotations <- function(annotation_name= NULL, genome= NULL, chromosome= NULL, start= NULL, end= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'select_annotations', annotation_name, genome, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title select_experiments 
#' @description Selects experiments data. It is a simpler version of the select_regions command.
#' @family Operating on the data regions
#' 
#' @param experiment_name - A string or a vector of string (name(s) of selected experiment(s))
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' deepblue.select_experiments(
#' 	experiment_name = c("E002-H3K9ac.narrowPeak.bed",
#' 						"E001-H3K4me3.gappedPeak.bed")
#' )
#'
deepblue.select_experiments <- function(experiment_name= NULL, chromosome= NULL, start= NULL, end= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'select_experiments', experiment_name, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title select_genes 
#' @description Selects genes as regions.
#' @family Operations on gene sets and genes identifiers
#' 
#' @param genes_name - A string or a vector of string (genes(s) - ENSB ID or ENSB name. Use the regular expression '.*' for selecting all.)
#' @param gene_set - A string (gene set name)
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' genes_names = c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')
#' deepblue.select_genes(genes_name = genes_names, gene_set = "gencode v23")
#'
deepblue.select_genes <- function(genes_name= NULL, gene_set= NULL, chromosome= NULL, start= NULL, end= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'select_genes', genes_name, gene_set, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title select_regions 
#' @description Selects experiment regions matching the given parameters.
#' @family Operating on the data regions
#' 
#' @param experiment_name - A string or a vector of string (name(s) of selected experiment(s))
#' @param genome - A string or a vector of string (the target genome)
#' @param epigenetic_mark - A string or a vector of string (name(s) of selected epigenetic mark(s))
#' @param sample_id - A string or a vector of string (id(s) of selected sample(s))
#' @param technique - A string or a vector of string (name(s) of selected technique(es))
#' @param project - A string or a vector of string (name(s) of selected projects)
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' deepblue.select_regions(genome="hg19", epigenetic_mark = "H3K27ac",
#'   project = " BLUEPRINT Epigenome")

#'
deepblue.select_regions <- function(experiment_name= NULL, genome= NULL, epigenetic_mark= NULL, sample_id= NULL, technique= NULL, project= NULL, chromosome= NULL, start= NULL, end= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'select_regions', experiment_name, genome, epigenetic_mark, sample_id, technique, project, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}



#' @export 
#' 
#' @title tiling_regions 
#' @description Creates regions with the tiling size over the chromosomes.
#' @family Operating on the data regions
#' 
#' @param size - A int (tiling size)
#' @param genome - A string (the target genome)
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' deepblue.tiling_regions(size = 10000, genome = "hg19",
#'   chromosome = "chr1")
#'
deepblue.tiling_regions <- function(size= NULL, genome= NULL, chromosome= NULL, user_key=deepblue.USER_KEY) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(deepblue.URL, 'tiling_regions', if (is.null(size)) NULL else as.integer(size), genome, chromosome, user_key)
    status = value[[1]]
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    } else return(value[[2]])
}


#' @title xml.rpc
#' @keywords internal
#' @description perform an XML-RPC call
xml.rpc =
    function(url, method, ..., .args = list(...),
             .opts = list(),
             .defaultOpts = list(httpheader = c('Content-Type' = "text/xml"), 
                                 followlocation = TRUE, 
                                 useragent = useragent),
             .convert = TRUE, 
             .curl = getCurlHandle(), 
             useragent = "DeepBlue-R-XMLRPC", 
             verbose=deepblue.debug.VERBOSE)
    {
        # Turn the method and arguments to an RPC body.
        body = createBody(method,  .args)
        if(verbose)
            print(body)
        
        # merge the .defaultOpts and the .opts into one list.
        .defaultOpts[["postfields"]] = saveXML(body)
        if(length(.opts))
            .defaultOpts[names(.opts)] = .opts
        
        rdr = dynCurlReader(.curl, baseURL = url)
        .defaultOpts[["headerfunction"]] = rdr$update
        postForm(url, .opts = .defaultOpts, style = "POST", curl = .curl)
        
        hdr = parseHTTPHeader(rdr$header())
        if(as.integer(hdr[["status"]]) %/% 100 !=  2) {
            print(hdr["status"])
            # call an RCurl error generator function.
            stop("Problems")
        }
        ans = rdr$value()
        if (verbose)
            print(ans)
        
        # Now either convert using the default converter fnction (convertToR)
        # or return as is or allow the caller to specify a 
        # function to use for conversion.
        if(is.logical(.convert)) {
            if(.convert){
                convertToR(ans)
            }
            
            else
                ans
        } else if(is.function(.convert))
            .convert(ans)
        else
            ans
    }

createBody =
    function(method, args)
    {
        top = newXMLNode("methodCall", newXMLNode("methodName", method))
        params = newXMLNode("params", parent = top)
        sapply(args, function(x) newXMLNode("param", 
                                            rpc.serialize(x), 
                                            parent = params))
        top
    }


setGeneric("rpc.serialize", function(x, ...) standardGeneric("rpc.serialize"))

setMethod("rpc.serialize", "ANY",
          function(x, ...) {
              if(isS4(x))
                  return(rpc.serialize.S4Object(x, ...))
              
              stop("Not sure how to convert this type of object to XMLRPC format")
          })

rpc.serialize.S4Object =
    function(x, ...)
    {
        els = slotNames(x)
        rpc.serialize(structure(lapply(els, function(id) slot(x, id)), 
                                names = els), ...)
    }

basicTypeMap =
    c("integer" = "i4",
      "double" = "double",
      "character" = "string",
      "logical" = "boolean",
      "POSIXt" = "dateTime.iso8601",
      "POSIXct" = "dateTime.iso8601",
      "Date" = "dateTime.iso8601",
      "list" = "array",
      "raw" = "base64")


cast <- function(x) {
    if (is.logical(x))
        as.integer(x)
    else
        x
}

setOldClass("AsIs")

setMethod("rpc.serialize", "AsIs",
          function(x) {
              type = basicTypeMap[typeof(x)]
              vectorArray(x, type)
          })

setMethod("rpc.serialize", "NULL",
          function(x, ...) {
              newXMLNode("value", newXMLNode("nil"))
          })

setMethod("rpc.serialize", "raw",
          function(x, ...) {
              val = base64Encode(x)
              newXMLNode("value", newXMLNode("base64", val))
          })


setMethod("rpc.serialize", "Date",
          function(x, ...) {
              val = format(x, "%Y%m%dT%H:%H:%S")
              if(length(x) == 1)
                  newXMLNode("value", newXMLNode("dateTime.iso8601", val))
              else
                  vectorArray(val, basicTypeMap["Date"])
          })

setMethod("rpc.serialize", "POSIXt",
          function(x, ...) {
              val = format(as.POSIXct(x), "%Y%m%dT%H:%H:%S")
              if(length(x) == 1)
                  newXMLNode("value", newXMLNode("dateTime.iso8601", val))
              else
                  vectorArray(val, basicTypeMap["POSIXt"])
          })

setMethod("rpc.serialize", "vector",
          function(x, ...) {
              type = basicTypeMap[typeof(x)]
              x = cast(x)
              
              if(length(names(x))) {
                  warning("Skipping names on vector!")
                  names(x) = NULL
              }
              
              if(length(x) == 1)
                  newXMLNode("value", newXMLNode(type, if(type == "string") {
                      newXMLCDataNode(x)
                  } 
                  else x))
              else {
                  vectorArray(x, type)
              }
          }
)


FormatStrings = c(numeric = "%f", integer = "%d", logical = "%s",
                  i4 = "%d", double = "%f",
                  string = "%s", Date = "%s",  POSIXt = "%s", POSIXct = "%s")


vectorArray =
    function(x, type)
    {
        top = newXMLNode("value")
        a = newXMLNode("array", parent = top)
        data = newXMLNode("data", parent = a)
        
        tmpl = if(type == "string")  # is.character(x))
            sprintf("<value><%s><![CDATA[%%s]]></%s></value>", type, type)
        else if(type == "dateTime.iso8601") {
            if(is(x, "Date"))
                x = format(x, "%Y%m%dT00:00:00")
            else
                x = format(as.POSIXct(x), "%Y%m%dT%H:%H:%S")
            sprintf("<value><%s>%%s</%s></value>", type, type)
        } else {
            if(type == "double") {
                x = as.character(x)
                pct = "%s"
            } else
                pct = FormatStrings[type]
            
            if(is.na(pct)) pct = "%s"
            sprintf("<value><%s>%s</%s></value>", type, pct, type)
        }
        
        txt = sprintf(tmpl, x)
        parseXMLAndAdd(txt, data)
        
        top
    }

setMethod("rpc.serialize", "list",
          function(x, ...) {
              if(length(names(x))) {
                  a = newXMLNode("struct")
                  sapply(names(x), function(id) {
                      newXMLNode("member",
                                 newXMLNode("name", id),
                                 rpc.serialize(x[[id]]), parent = a)
                  })
                  newXMLNode("value", a)
              } else {
                  a = newXMLNode("array")
                  data = newXMLNode("data", parent = a)
                  v <- sapply(x, function(x) {
                      rpc.serialize(x)
                  })
                  addChildren(data, v)
                  newXMLNode("value", a)
              }
          })


setGeneric('convertToR', function(node) standardGeneric('convertToR'))

setMethod('convertToR', 'XMLInternalDocument', function(node)
{
    fault = getNodeSet(node,path="//methodResponse/fault/value/struct")
    if (length(fault) > 0) {
        fault = xmlRPCToR(fault[[1]])
        e = simpleError(paste("faultCode: ",  fault$faultCode, 
                              " faultString: ", fault$faultString))
        class(e) = c("XMLRPCError", class(e))
        stop(e)
    }
    a = xpathApply(node, "//param/value", xmlRPCToR)
    if(length(a) == 1)
        a[[1]]
    else
        a
})

setMethod('convertToR', 'XMLInternalNode',
          function(node)
          {
              if(length(getNodeSet(node, "./param/value"))) {
                  ans = xpathApply(node, "./param/value", xmlRPCToR, 
                                   simplify = FALSE)
              } else
                  xmlToList(node)
          })

setMethod('convertToR', 'character',
          function(node)
          {
              xml = xmlParse(node, asText = TRUE)
              convertToR(xml)
          })


xmlRPCToR =
    function(node, ...)
    {
        if(is.null(node))
            return(NULL)
        
        if(xmlName(node) == "value") {
            node = node[[1]]
        }
        
        if(is(node, "XMLInternalTextNode")) {
            return(xmlValue(node))
        }
        
        type = xmlName(node)
        switch(type,
               'array' = xmlRPCToR.array(node, ...),
               'struct' = xmlRPCToR.struct(node, ...),
               'i4' = as.integer(xmlValue(node)),
               'int' = as.integer(xmlValue(node)),
               'boolean' = if(xmlValue(node) == "1") TRUE else FALSE,
               'double' = as.numeric(xmlValue(node)),
               'string' = xmlValue(node),
               'dateTime.iso8601' = as.POSIXct(strptime(xmlValue(node), 
                                                        "%Y%m%dT%H:%M:%S")),
               'base64' = base64(xmlValue(node), encode = FALSE),
               xmlValue(node)
        )
        
    }

xmlRPCToR.struct =
    function(node, ...)
    {
        ans = xmlApply(node, function(x) xmlRPCToR(x[["value"]][[1]], ...))
        names(ans) = xmlSApply(node, function(x) xmlValue(x[["name"]]))
        ans
    }

xmlRPCToR.array =
    function(node, ...)
    {
        i <- 1
        result <- list()
        while(!is.null(node[["data"]][[i]])){
            result <- append(result, list(xmlRPCToR(node[["data"]][[i]])))
            i <- i + 1
        }
        return(result)
    }

check_value =
    function(input)
    {
        status = input[[1]]
        print(status)
        if (status == "error") {
            stop(input[[2]])
        }
        value = input[[2]]
        return (value)
    }
