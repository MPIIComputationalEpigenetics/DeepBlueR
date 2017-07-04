# Accessing Deepblue through R
# For DeepBlue version 1.12.13

# We include a modified version of the XML-RPC library:
# http://bioconductor.org/packages/release/extra/html/XMLRPC.html
# for R in this file.




#' @export 
#' 
#' @title aggregate 
#' @description Summarize the data_id content using the regions specified in ranges_id as boundaries. Use the fields @AGG.MIN, @AGG.MAX, @AGG.SUM, @AGG.MEDIAN, @AGG.MEAN, @AGG.VAR, @AGG.SD, @AGG.COUNT in 'get_regions' command 'format' parameter to retrieve the computed values minimum, maximum, median, mean, variance, standard deviation and number of regions, respectively.
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
#' annotation_id = deepblue_select_annotations(
#'     annotation_name="CpG Islands",
#'     genome="hg19", chromosome="chr1")
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue_aggregate(
#'     data_id = data_id,
#'     ranges_id=annotation_id,
#'     column = "SCORE")
#'
deepblue_aggregate <- function(data_id= NULL, ranges_id= NULL, column= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'aggregate', data_id, ranges_id, column, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title binning 
#' @description Bin results according to counts.
#' @family Operating on the data regions
#' 
#' @param query_data_id - A string (query data that will made by the binning.)
#' @param column - A string (name of the column that will be used in the aggregation)
#' @param bins - A int (number of of bins)
#' @param user_key - A string (users token key)
#'
#' @return request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data())
#'
#' @examples
#' experiment_id = deepblue_select_experiments(
#'     experiment_name="S00XDKH1.ERX712765.H3K27ac.bwa.GRCh38.20150527.bed")
#' deepblue_binning (query_data_id=experiment_id, 
#'     column="SIGNAL_VALUE",
#'     bins=40)

#'
deepblue_binning <- function(query_data_id= NULL, column= NULL, bins= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'binning', query_data_id, column, if (is.null(bins)) NULL else as.integer(bins), user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title calculate_enrichment 
#' @description  Enrich the regions based on Gene Ontology terms.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param gene_model - A string (the gene model)
#' @param user_key - A string (users token key)
#'
#' @return request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data())
#'
#' @examples
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' 
#' filtered_id = deepblue_filter_regions(query_id = data_id,
#'     field = "VALUE",
#'     operation = ">",
#'     value = "100",
#'     type = "number",
#'     user_key = "anonymous_key")
#' 
#' deepblue_calculate_enrichment(query_id = filtered_id,
#'    gene_model = "gencode v23")
#'
deepblue_calculate_enrichment <- function(query_id= NULL, gene_model= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'calculate_enrichment', query_id, gene_model, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title cancel_request 
#' @description Stop, cancel, and remove request data. The request processed data is remove if its processing was finished.
#' @family Commands for all types of data
#' 
#' @param id - A string (Request ID to be canceled, stopped or removed.)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (ID of the canceled request)
#'
#' @examples
#' deepblue_cancel_request(id = "r12345")
#'
deepblue_cancel_request <- function(id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'cancel_request', id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title chromosomes 
#' @description List the chromosomes of a given Genome.
#' @family Inserting and listing genomes
#' 
#' @param genome - A string (the target genome)
#' @param user_key - A string (users token key)
#'
#' @return chromosomes - A array (A list containing all chromosomes, with theirs names and sizes)
#'
#' @examples
#' deepblue_chromosomes(genome = "g1")
#'
deepblue_chromosomes <- function(genome= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'chromosomes', genome, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title collection_experiments_count 
#' @description Count the number of experiments that match the selection criteria in each term of the selected controlled_vocabulary. The selection can be achieved through specifying a list of BioSources, experimental Techniques, Epigenetic Marks, Samples or Projects.
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
#' deepblue_collection_experiments_count(
#' 	controlled_vocabulary="epigenetic_marks",
#'     genome = "hg19", type = "peaks",
#'     biosource = "blood")
#'
deepblue_collection_experiments_count <- function(controlled_vocabulary= NULL, genome= NULL, type= NULL, epigenetic_mark= NULL, biosource= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'collection_experiments_count', controlled_vocabulary, genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title commands 
#' @description List all available DeepBlue commands.
#' @family Checking DeepBlue status
#' 
#'
#' @return commands - A struct (command descriptions)
#'
#' @examples
#' deepblue_commands()
#'
deepblue_commands <- function() {

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
    value <- xml.rpc(deepblue_options('url'), 'commands')
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title count_gene_ontology_terms 
#' @description Summarize the controlled_vocabulary fields, from experiments that match the selection criteria. It is similar to the 'collection_experiments_count' command, but this command return the summarization for all controlled_vocabulary terms.
#' @family Gene models and genes identifiers
#' 
#' @param genes - A string or a vector of string (Name(s) or ENSEMBL ID (ENSGXXXXXXXXXXX.X ) of the gene(s).)
#' @param go_terms - A string or a vector of string (gene ontology terms - ID or label)
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param gene_model - A string (the gene model)
#' @param user_key - A string (users token key)
#'
#' @return faceting - A struct (Map with the mandatory fields of the experiments metadata, where each contains a list of terms that appears.)
#'
#' @examples
#' gene_names = c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')
#' deepblue_count_gene_ontology_terms (genes = gene_names, gene_model = "gencode v23")
#' 

#'
deepblue_count_gene_ontology_terms <- function(genes= NULL, go_terms= NULL, chromosome= NULL, start= NULL, end= NULL, gene_model= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'count_gene_ontology_terms', genes, go_terms, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), gene_model, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title count_regions 
#' @description  Return the number of genomic regions present in the query.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param user_key - A string (users token key)
#'
#' @return request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data())
#'
#' @examples
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue_count_regions(query_id = data_id)
#'
deepblue_count_regions <- function(query_id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'count_regions', query_id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title coverage 
#' @description Send a request to count the number of regions in the result of the given query.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param genome - A string (Genome where the coverage will be calculated to)
#' @param user_key - A string (users token key)
#'
#' @return request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data())
#'
#' @examples
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue_coverage(query_id = data_id, genome="hg19")
#'
deepblue_coverage <- function(query_id= NULL, genome= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'coverage', query_id, genome, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title echo 
#' @description Greet the user with the DeepBlue version.
#' @family Checking DeepBlue status
#' 
#' @param user_key - A string (users token key)
#'
#' @return message - A string (echo message including version)
#'
#' @examples
#' deepblue_echo(user_key = "anonymous_key")
#'
deepblue_echo <- function(user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'echo', user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title extend 
#' @description Extend the genomic regions included in the query. It is possible to extend downstream, upstream or in both directions.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param length - A int (The new region length)
#' @param direction - A string (The direction that the region will be extended: 'BACKWARD', 'FORWARD', 'BOTH'. (Empty value will be used for both direction.)
#' @param use_strand - A boolean (Use the region column STRAND to define the region direction)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new query)
#'
#' @examples
#' annotation_id = deepblue_select_annotations(
#'     annotation_name="CpG Islands",
#'     genome="hg19", chromosome="chr1")
#' deepblue_extend(query_id = annotation_id,
#'     length = 2000, direction = "BOTH",
#'     use_strand = TRUE)
#'
deepblue_extend <- function(query_id= NULL, length= NULL, direction= NULL, use_strand= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'extend', query_id, if (is.null(length)) NULL else as.integer(length), direction, use_strand, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title faceting_experiments 
#' @description Summarize the controlled_vocabulary fields, from experiments that match the selection criteria. It is similar to the 'collection_experiments_count' command, but this command return the summarization for all controlled_vocabulary terms.
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
#' deepblue_faceting_experiments(genome = "hg19",
#'     type = "peaks",
#'     biosource = "blood")
#'
deepblue_faceting_experiments <- function(genome= NULL, type= NULL, epigenetic_mark= NULL, biosource= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'faceting_experiments', genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title filter_regions 
#' @description Filter the genomic regions by their content.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param field - A string (field that is filtered by)
#' @param operation - A string (operation used for filtering. For 'string' must be '==' or '!=' and for 'number' must be one of these: ==,!=,>,>=,<,<=)
#' @param value - A string (value the operator is applied to)
#' @param type - A string (type of the value: 'number' or 'string' )
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of filtered query)
#'
#' @examples
#' deepblue_filter_regions(query_id = "q12345",
#'     field = "VALUE",
#'     operation = ">",
#'     value = "100",
#'     type = "number",
#'     user_key = "anonymous_key")
#'
deepblue_filter_regions <- function(query_id= NULL, field= NULL, operation= NULL, value= NULL, type= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'filter_regions', query_id, field, operation, value, type, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title find_motif 
#' @description Find genomic regions based on a given motif that appears in the genomic sequence.
#' @family Inserting and listing annotations
#' 
#' @param motif - A string (motif (PERL regular expression))
#' @param genome - A string (the target genome)
#' @param chromosomes - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param overlap - A boolean (if the matching should do overlap search)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the annotation that contains the positions of the given motif)
#'
#' @examples
#' deepblue_find_motif(motif = "C[GT]+C", chromosomes=c("chr11", "chr12"),
#'     genome = "hg19", overlap = FALSE)

#'
deepblue_find_motif <- function(motif= NULL, genome= NULL, chromosomes= NULL, start= NULL, end= NULL, overlap= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'find_motif', motif, genome, chromosomes, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), overlap, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title flank 
#' @description Create a set of genomic regions that flank the query regions. The original regions are removed from the query. Use the merge command to combine flanking regions with the original query.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param start - A int (Number of base pairs after the end of the region. Use a negative number to denote the number of base pairs before the start of the region.)
#' @param length - A int (The new region length)
#' @param use_strand - A boolean (Use the region column STRAND to define the region direction)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new query)
#'
#' @examples
#' annotation_id = deepblue_select_annotations(
#'     annotation_name="CpG Islands",
#'     genome="hg19", chromosome="chr1")
#' deepblue_flank(query_id = annotation_id,
#'     start = 0, length = 2000,
#'     use_strand = TRUE)
#'
deepblue_flank <- function(query_id= NULL, start= NULL, length= NULL, use_strand= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'flank', query_id, if (is.null(start)) NULL else as.integer(start), if (is.null(length)) NULL else as.integer(length), use_strand, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_children 
#' @description A BioSource refers to a term describing the origin of a given sample, such as a tissue or cell line. These form a hierarchy in which children of a BioSource term can be fetched with this command. Children terms are more specific terms that are defined in the imported ontologies.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (biosource name)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (related biosources)
#'
#' @examples
#' deepblue_get_biosource_children(biosource = "Blood")
#'
deepblue_get_biosource_children <- function(biosource= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'get_biosource_children', biosource, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_parents 
#' @description A BioSource refers to a term describing the origin of a given sample, such as a tissue or cell line. These form a hierarchy in which the parent of a BioSource term can be fetched with this command. Parent terms are more generic terms that are defined in the imported ontologies.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (biosource name)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (parents biosources)
#'
#' @examples
#' deepblue_get_biosource_parents(biosource = "Blood")
#'
deepblue_get_biosource_parents <- function(biosource= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'get_biosource_parents', biosource, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_related 
#' @description A BioSource refers to a term describing the origin of a given sample, such as a tissue or cell line. These form a hierarchy in which the children of a BioSource term and its synonyms can be fetched with this command. Children terms are more specific terms that are defined in the imported ontologies. Synonyms are different aliases for the same biosource.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (biosource name)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (related biosources)
#'
#' @examples
#' deepblue_get_biosource_related(biosource = "Blood")
#'
deepblue_get_biosource_related <- function(biosource= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'get_biosource_related', biosource, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title get_biosource_synonyms 
#' @description Obtain the synonyms of the specified biosource. Synonyms are different aliases for the same biosource. A BioSource refers to a term describing the origin of a given sample, such as a tissue or cell line.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (name of the biosource)
#' @param user_key - A string (users token key)
#'
#' @return synonyms - A array (synonyms of the biosource)
#'
#' @examples
#' deepblue_get_biosource_synonyms(biosource = "prostate gland")
#'
deepblue_get_biosource_synonyms <- function(biosource= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'get_biosource_synonyms', biosource, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title get_experiments_by_query 
#' @description List the experiments and annotations that have at least one genomic region in the final query result.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param user_key - A string (users token key)
#'
#' @return experiments - A array (List containing experiments names and ids)
#'
#' @examples
#' deepblue_get_experiments_by_query(query_id = "q12345")
#'
deepblue_get_experiments_by_query <- function(query_id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'get_experiments_by_query', query_id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title get_regions 
#' @description Trigger the processing of the query's genomic regions. The output is a column based format with columns as defined in the 'output_format' parameter. Use the command 'info' for verifying the processing status. The 'get_request_data' command is used to download the regions using the programmatic interface. Alternatively, results can be download using the URL: http://deepblue.mpi-inf.mpg.de/download?r_id=<request_id>&key=<user_key>.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param output_format - A string (Output format)
#' @param user_key - A string (users token key)
#'
#' @return request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data())
#'
#' @examples
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue_get_regions(query_id =data_id,
#'     output_format = "CHROMOSOME,START,END")
#'
deepblue_get_regions <- function(query_id= NULL, output_format= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'get_regions', query_id, output_format, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title get_request_data 
#' @description Download the requested data. The output can be (i) a string (get_regions, score_matrix, and count_regions), or (ii) a list of ID and names (get_experiments_by_query), or (iii) a struct (coverage).
#' @family Requests status information and results
#' 
#' @param request_id - A string (ID of the request)
#' @param user_key - A string (users token key)
#'
#' @return data - A string or a vector of string (the request data)
#'
#' @examples
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed",
#'     chromosome="chr1")
#' request_id = deepblue_get_regions(
#'     query_id =data_id,
#'     output_format = "CHROMOSOME,START,END")
#' deepblue_get_request_data(request_id = request_id)
#'
deepblue_get_request_data <- function(request_id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'get_request_data', request_id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title info 
#' @description Information about a DeepBlue data identifier (ID). Any DeepBlue data ID can be queried with this command. For example, it is possible to obtain all available information about an Experiment using its ID, to obtain the actual Request processing status or the information about a Sample. A user can obtain information about him- or herself using the value 'me' in the parameter 'id'. Multiple IDs can be queried in the same operation.
#' @family Commands for all types of data
#' 
#' @param id - A string or a vector of string (ID or an array of IDs)
#' @param user_key - A string (users token key)
#'
#' @return information - A array or a vector of array (List of Maps, where each map contains the info of an object.)
#'
#' @examples
#' deepblue_info(id = "e30035")
#'
deepblue_info <- function(id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'info', id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title input_regions 
#' @description Upload a set of genomic regions that can be accessed through a query ID. An interesting use case for this command is to upload a set of custom regions for intersecting with genomic regions in DeepBlue to specifically select regions of interest.
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
#' deepblue_input_regions(genome = "hg19",
#'     region_set = regions_set)
#'
deepblue_input_regions <- function(genome= NULL, region_set= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'input_regions', genome, region_set, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title intersection 
#' @description Select genomic regions that intersect with at least one region of the second query. This command is a simplified version of the 'overlap' command.
#' @family Operating on the data regions
#' 
#' @param query_data_id - A string (query data that will be filtered.)
#' @param query_filter_id - A string (query containing the regions that the regions of the query_data_id must overlap.)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new query)
#'
#' @examples
#' annotation_id = deepblue_select_annotations(
#'     annotation_name="CpG Islands",
#'     genome="hg19", chromosome="chr1")
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue_intersection(query_data_id = annotation_id,
#'     query_filter_id = data_id)
#'
deepblue_intersection <- function(query_data_id= NULL, query_filter_id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'intersection', query_data_id, query_filter_id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title is_biosource 
#' @description Verify if the name is an existing and valid DeepBlue BioSource name. A BioSource refers to a term describing the origin of a given sample, such as a tissue or cell line.
#' @family Commands for all types of data
#' 
#' @param biosource - A string (biosource name)
#' @param user_key - A string (users token key)
#'
#' @return information - A string or a vector of string (A string containing the biosource name)
#'
#' @examples
#' deepblue_is_biosource(biosource = "blood")
#'
deepblue_is_biosource <- function(biosource= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'is_biosource', biosource, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_annotations 
#' @description List all annotations of genomic regions currently available in DeepBlue.
#' @family Inserting and listing annotations
#' 
#' @param genome - A string or a vector of string (the target genome)
#' @param user_key - A string (users token key)
#'
#' @return annotations - A array (annotations names and IDs)
#'
#' @examples
#' deepblue_list_annotations(genome = "hg19")
#'
deepblue_list_annotations <- function(genome= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_annotations', genome, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_biosources 
#' @description List BioSources included in DeepBlue. A BioSource refers to a term describing the origin of a given sample, such as a tissue or cell line. It is possible to filter the BioSources by their extra_metadata fields content. These fields vary depending on the original data source.
#' @family Inserting and listing biosources
#' 
#' @param extra_metadata - A struct (Metadata that must be matched)
#' @param user_key - A string (users token key)
#'
#' @return biosources - A array (biosources names and IDS)
#'
#' @examples
#' deepblue_list_biosources(extra_metadata = list(ontology_id = "UBERON:0002485"))
#'
deepblue_list_biosources <- function(extra_metadata=NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_biosources', extra_metadata, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_column_types 
#' @description Lists the ColumnTypes included in DeepBlue.
#' @family Inserting and listing different column types
#' 
#' @param user_key - A string (users token key)
#'
#' @return column_types - A array (column types names and IDS)
#'
#' @examples
#' deepblue_list_column_types()
#'
deepblue_list_column_types <- function(user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_column_types', user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_epigenetic_marks 
#' @description List Epigenetic Marks included in DeepBlue. This includes histone marks, DNA methylation, DNA sensitivity, etc. It is possible to filter the Epigenetic Marks by their extra_metadata field content.
#' @family Inserting and listing epigenetic marks
#' 
#' @param extra_metadata - A struct (Metadata that must be matched)
#' @param user_key - A string (users token key)
#'
#' @return epigenetic_marks - A array (epigenetic mark names and IDS)
#'
#' @examples
#' deepblue_list_epigenetic_marks()
#'
deepblue_list_epigenetic_marks <- function(extra_metadata=NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_epigenetic_marks', extra_metadata, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_experiments 
#' @description  List the DeepBlue Experiments that matches the search criteria defined by this command parameters.
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
#' @return experiments - A array (experiment names and IDS)
#'
#' @examples
#' deepblue_list_experiments(genome = "hg19", type = "peaks",
#'   epigenetic_mark = "H3K27ac", biosource = "blood")
#'
deepblue_list_experiments <- function(genome= NULL, type= NULL, epigenetic_mark= NULL, biosource= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_experiments', genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_expressions 
#' @description List the Expression currently available in DeepBlue. A expression is a set of data with an identifier and an expression value.
#' @family Expression data
#' 
#' @param expression_type - A string (expression type (supported: 'gene'))
#' @param sample_id - A string or a vector of string (sample ID(s))
#' @param replica - A int or a vector of int (replica(s))
#' @param project - A string or a vector of string (project(s) name)
#' @param user_key - A string (users token key)
#'
#' @return expressions - A array (expressions names and IDS)
#'
#' @examples
#' deepblue_list_expressions(expression_type='gene')

#'
deepblue_list_expressions <- function(expression_type= NULL, sample_id= NULL, replica= NULL, project= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_expressions', expression_type, sample_id, if (is.null(replica)) NULL else as.integer(replica), project, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_gene_models 
#' @description List all the Gene Models currently available in DeepBlue. A gene model is a set of genes usually imported from GENCODE. For example Gencode v22.
#' @family Gene models and genes identifiers
#' 
#' @param user_key - A string (users token key)
#'
#' @return gene_models - A array (gene models names and IDS)
#'
#' @examples
#' deepblue_list_gene_models()
#'
deepblue_list_gene_models <- function(user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_gene_models', user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_genes 
#' @description List the Genes currently available in DeepBlue.
#' @family Gene models and genes identifiers
#' 
#' @param genes - A string or a vector of string (Name(s) or ENSEMBL ID (ENSGXXXXXXXXXXX.X ) of the gene(s).)
#' @param go_terms - A string or a vector of string (gene ontology terms - ID or label)
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param gene_model - A string (the gene model)
#' @param user_key - A string (users token key)
#'
#' @return genes - A array (genes names and its content)
#'
#' @examples
#' deepblue_list_genes(
#'   chromosome="chr20",
#'   start=10000000,
#'   end=21696620,
#' gene_model='Gencode v22')

#'
deepblue_list_genes <- function(genes= NULL, go_terms= NULL, chromosome= NULL, start= NULL, end= NULL, gene_model= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_genes', genes, go_terms, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), gene_model, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_genomes 
#' @description List Genomes assemblies that are registered in DeepBlue.
#' @family Inserting and listing genomes
#' 
#' @param user_key - A string (users token key)
#'
#' @return genomes - A array (genome names)
#'
#' @examples
#' deepblue_list_genomes()
#'
deepblue_list_genomes <- function(user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_genomes', user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_in_use 
#' @description List all terms used by the Experiments mandatory metadata that have at least one Experiment or Annotation using them.
#' @family Commands for all types of data
#' 
#' @param controlled_vocabulary - A string (controlled vocabulary name)
#' @param user_key - A string (users token key)
#'
#' @return terms - A array (controlled_vocabulary terms with count)
#'
#' @examples
#' deepblue_list_in_use(controlled_vocabulary = "biosources")
#'
deepblue_list_in_use <- function(controlled_vocabulary= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_in_use', controlled_vocabulary, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_projects 
#' @description List Projects included in DeepBlue.
#' @family Inserting and listing projects
#' 
#' @param user_key - A string (users token key)
#'
#' @return projects - A array (project names)
#'
#' @examples
#' deepblue_list_projects()
#'
deepblue_list_projects <- function(user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_projects', user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_recent_experiments 
#' @description List the latest Experiments included in DeepBlue that match criteria defined in the parameters. The returned experiments are sorted by insertion date.
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
#' deepblue_list_recent_experiments(days = 2, genome = "hg19")
#'
deepblue_list_recent_experiments <- function(days= NULL, genome= NULL, epigenetic_mark= NULL, sample= NULL, technique= NULL, project= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_recent_experiments', days, genome, epigenetic_mark, sample, technique, project, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_requests 
#' @description List the Requests made by the user. It is possible to obtain only the requests of a given state.
#' @family Requests status information and results
#' 
#' @param request_state - A string (Name of the state to get requests for. The valid states are: new, running, done, and failed.)
#' @param user_key - A string (users token key)
#'
#' @return data_state - A array (Request-IDs and their state)
#'
#' @examples
#' deepblue_list_requests(request_state = 'running')
#'
deepblue_list_requests <- function(request_state= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_requests', request_state, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_samples 
#' @description List Samples included in DeepBlue. It is possible to filter by the BioSource and by extra_metadata fields content.
#' @family Inserting and listing samples
#' 
#' @param biosource - A string or a vector of string (name(s) of selected biosource(s))
#' @param extra_metadata - A struct (Metadata that must be matched)
#' @param user_key - A string (users token key)
#'
#' @return samples - A array (samples id with their content)
#'
#' @examples
#' deepblue_list_samples(biosource = "Blood")
#'
deepblue_list_samples <- function(biosource= NULL, extra_metadata=NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_samples', biosource, extra_metadata, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_biosources 
#' @description List all BioSources that have a similar name compared to the provided name. A BioSource refers to a term describing the origin of a given sample, such as a tissue or cell line. The similarity is calculated using the Levenshtein method.
#' @family Inserting and listing biosources
#' 
#' @param name - A string (biosource name)
#' @param user_key - A string (users token key)
#'
#' @return biosource - A string (biosource name)
#'
#' @examples
#' deepblue_list_similar_biosources(name = "blood")
#'
deepblue_list_similar_biosources <- function(name= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_similar_biosources', name, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_epigenetic_marks 
#' @description List all Epigenetic Marks that have a similar name compared to the provided name. The similarity is calculated using the Levenshtein method.
#' @family Inserting and listing epigenetic marks
#' 
#' @param name - A string (epigenetic mark name)
#' @param user_key - A string (users token key)
#'
#' @return epigenetic_marks - A array (similar epigenetic mark names)
#'
#' @examples
#' deepblue_list_similar_epigenetic_marks(name = "H3k27ac")
#'
deepblue_list_similar_epigenetic_marks <- function(name= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_similar_epigenetic_marks', name, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_experiments 
#' @description List all Experiments that have a similar name compared to the provided name. The similarity is calculated using the Levenshtein method.
#' @family Inserting and listing experiments
#' 
#' @param name - A string (experiment name)
#' @param genome - A string or a vector of string (the target genome)
#' @param user_key - A string (users token key)
#'
#' @return experiments - A array (similar experiment names)
#'
#' @examples
#' deepblue_list_similar_experiments(name = "blood", genome = "hg19")
#'
deepblue_list_similar_experiments <- function(name= NULL, genome= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_similar_experiments', name, genome, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_genomes 
#' @description Lists all Genomes that have a similar name compared to the provided name. The similarity is calculated using the Levenshtein method.
#' @family Inserting and listing genomes
#' 
#' @param name - A string (genome name)
#' @param user_key - A string (users token key)
#'
#' @return genomes - A array (similar genome names)
#'
#' @examples
#' deepblue_list_similar_genomes(name = "grc")
#'
deepblue_list_similar_genomes <- function(name= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_similar_genomes', name, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_projects 
#' @description List Projects that have a similar name compared to the provided name. The similarity is calculated using the Levenshtein method.
#' @family Inserting and listing projects
#' 
#' @param name - A string (project name)
#' @param user_key - A string (users token key)
#'
#' @return projects - A array (similar project names)
#'
#' @examples
#' deepblue_list_similar_projects(name = "BLUEPRINT")
#'
deepblue_list_similar_projects <- function(name= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_similar_projects', name, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_similar_techniques 
#' @description List Techniques that have a similar name compared to the provided name. The similarity is calculated using the Levenshtein method.
#' @family Inserting and listing techniques
#' 
#' @param name - A string (technique name)
#' @param user_key - A string (users token key)
#'
#' @return techniques - A array (similar techniques)
#'
#' @examples
#' deepblue_list_similar_techniques(name = "chip seq")
#'
deepblue_list_similar_techniques <- function(name= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_similar_techniques', name, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title list_techniques 
#' @description List the Techniques included in DeepBlue.
#' @family Inserting and listing techniques
#' 
#' @param user_key - A string (users token key)
#'
#' @return techniques - A array (techniques)
#'
#' @examples
#' deepblue_list_techniques()
#'
deepblue_list_techniques <- function(user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'list_techniques', user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title merge_queries 
#' @description Merge regions from two queries in a new query.
#' @family Operating on the data regions
#' 
#' @param query_a_id - A string (id of the first query)
#' @param query_b_id - A string (id of the second query)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (new query id)
#'
#' @examples
#' annotation_id = deepblue_select_annotations(
#'     annotation_name="CpG Islands",
#'     genome="hg19", chromosome="chr1")
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' deepblue_merge_queries(
#'     query_a_id = annotation_id,
#'     query_b_id = data_id)
#'
deepblue_merge_queries <- function(query_a_id= NULL, query_b_id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'merge_queries', query_a_id, query_b_id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title name_to_id 
#' @description Obtain the data ID(s) from the informed data name(s).
#' @family Commands for all types of data
#' 
#' @param name - A string or a vector of string (ID or an array of IDs)
#' @param collection - A string (Collection where the data name is in )
#' @param user_key - A string (users token key)
#'
#' @return information - A array or a vector of array (List of IDs.)
#'
#' @examples
#' deepblue_name_to_id("E002-H3K9ac.narrowPeak.bed", "experiments")
#' deepblue_name_to_id("prostate duct", "biosources")
#' deepblue_name_to_id("DNA Methylation", "Epigenetic_marks")
#'
deepblue_name_to_id <- function(name= NULL, collection= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'name_to_id', name, collection, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title overlap 
#' @description Select genomic regions that overlap or not overlap with with the specified number of regions of the second query. Important: This command is still experimental and changes may occour.
#' @family Operating on the data regions
#' 
#' @param query_data_id - A string (query data that will be filtered.)
#' @param query_filter_id - A string (query containing the regions that the regions of the query_data_id must overlap.)
#' @param overlap - A boolean (True if must overlap, or false if must not overlap.)
#' @param amount - A int (Amount of regions that must overlap. Use the parameter 'amount_type' ('bp' or '\%') to specify the unit.  For example, use the value '10' with the amount_type '\%' to specify that 10\% of the bases in both regions must overlap, or use '10' with the amount_type 'bp' to specify that at least 10 bases must or must not overlap.)
#' @param amount_type - A string (Type of the amount: 'bp' for base pairs and '\%' for percentage. )
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new query)
#'
#' @examples
#' annotation_id = deepblue_select_annotations(
#'     annotation_name="CpG Islands",
#'     genome="hg19", chromosome="chr1")
#' experiment_id = deepblue_select_experiments(
#'     experiment_name="S00XDKH1.ERX712765.H3K27ac.bwa.GRCh38.20150527.bed")
#' deepblue_overlap(query_data_id = experiment_id, query_filter_id = annotation_id, 
#'     overlap = TRUE, amount=10, amount_type="%")
#' 

#'
deepblue_overlap <- function(query_data_id= NULL, query_filter_id= NULL, overlap= NULL, amount= NULL, amount_type= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'overlap', query_data_id, query_filter_id, overlap, if (is.null(amount)) NULL else as.integer(amount), amount_type, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title preview_experiment 
#' @description  List the DeepBlue Experiments that matches the search criteria defined by this command parameters.
#' @family Inserting and listing experiments
#' 
#' @param experiment_name - A string (name(s) of selected experiment(s))
#' @param user_key - A string (users token key)
#'
#' @return experiment - A string (experiment's regions)
#'
#' @examples
#' deepblue_preview_experiment('S00JJRH1.ERX683143.H3K4me3.bwa.GRCh38.20150527.bed')
#'
deepblue_preview_experiment <- function(experiment_name= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'preview_experiment', experiment_name, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title query_cache 
#' @description Cache a query result in DeepBlue memory. This command is useful when the same query ID is used multiple times in different requests. The command is an advice for DeepBlue to cache the query result and there is no guarantee that this query data access will be faster.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param cache - A boolean (set or unset this query caching)
#' @param user_key - A string (users token key)
#'
#' @return information - A string (New query ID.)
#'
#' @examples
#' annotation_id = deepblue_select_annotations(
#'     annotation_name="CpG Islands",
#'     genome="hg19", chromosome="chr1")
#' data_id = deepblue_select_experiments(
#'     experiment_name="E002-H3K9ac.narrowPeak.bed")
#' merged_regions = deepblue_merge_queries(
#'     query_a_id = annotation_id,
#'     query_b_id = data_id)
#' deepblue_query_cache(
#'     query_id = merged_regions, cache = TRUE)
#'
deepblue_query_cache <- function(query_id= NULL, cache= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'query_cache', query_id, cache, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title query_experiment_type 
#' @description Filter the query ID for regions associated with experiments of a given type. For example, it is possible to select only peaks using this command with the 'peaks' parameter.
#' @family Operating on the data regions
#' 
#' @param query_id - A string (Query ID)
#' @param type - A string (experiment type (peaks or signal))
#' @param user_key - A string (users token key)
#'
#' @return information - A string (New query ID.)
#'
#' @examples
#' h3k27ac_regions = deepblue_select_regions(
#'     genome ='GRCh38',
#'     epigenetic_mark ='H3k27ac',
#'     project ='BLUEPRINT Epigenome',
#'     chromosome ='chr1')
#' deepblue_query_experiment_type(
#'     query_id = h3k27ac_regions,
#'     type = "peaks")
#'
deepblue_query_experiment_type <- function(query_id= NULL, type= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'query_experiment_type', query_id, type, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title score_matrix 
#' @description Build a matrix containing the aggregation result of the the experiments data by the aggregation boundaries.
#' @family Operating on the data regions
#' 
#' @param experiments_columns - A struct (map with experiments names and columns to be processed. Example : {'wgEncodeBroadHistoneDnd41H3k27acSig.wig':'VALUE', 'wgEncodeBroadHistoneCd20ro01794H3k27acSig.wig':'VALUE'})
#' @param aggregation_function - A string (aggregation function name: min, max, sum, mean, var, sd, median, count, boolean)
#' @param aggregation_regions_id - A string (query ID of the regions that will be used as the aggregation boundaries)
#' @param user_key - A string (users token key)
#'
#' @return score_matrix - A string (the score matrix containing the summarized data)
#'
#' @examples
#' tiling_regions = deepblue_tiling_regions(
#'     size=100000, genome="mm10", chromosome="chr1")
#' deepblue_score_matrix(
#'     experiments_columns =
#'         list(ENCFF721EKA="VALUE", ENCFF781VVH="VALUE"),
#'   aggregation_function = "mean",
#'   aggregation_regions_id = tiling_regions)

#'
deepblue_score_matrix <- function(experiments_columns= NULL, aggregation_function= NULL, aggregation_regions_id= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'score_matrix', experiments_columns, aggregation_function, aggregation_regions_id, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title search 
#' @description Search all data of all types for the given keyword. A minus (-) character in front of a keyword searches for data without the given keyword. The search can be restricted to the following data types are: Annotations, Biosources, Column_types, Epigenetic_marks, Experiments, Genomes, Gene_models, Gene_expressions, Genes, Gene_ontology, Projects, Samples, Techniques, Tilings.
#' @family Commands for all types of data
#' 
#' @param keyword - A string (keyword to search by)
#' @param type - A string or a vector of string (type of data to search for - Annotations, Biosources, Column_types, Epigenetic_marks, Experiments, Genomes, Gene_models, Gene_expressions, Genes, Gene_ontology, Projects, Samples, Techniques, Tilings)
#' @param user_key - A string (users token key)
#'
#' @return results - A array (search results as [id, name, type])
#'
#' @examples
#' deepblue_search(keyword = "DNA Methylation BLUEPRINT",
#'     type = "experiments")

#'
deepblue_search <- function(keyword= NULL, type= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'search', keyword, type, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title select_annotations 
#' @description Select regions from the Annotations that match the selection criteria. 
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
#' deepblue_select_annotations(
#'     annotation_name = "Cpg Islands",
#'     genome = "hg19",
#'     chromosome = "chr1",
#'     start = 0,
#'     end = 2000000)
#'
deepblue_select_annotations <- function(annotation_name= NULL, genome= NULL, chromosome= NULL, start= NULL, end= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'select_annotations', annotation_name, genome, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title select_experiments 
#' @description Selects regions from Experiments by the experiments names.
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
#' deepblue_select_experiments(
#' 	experiment_name = c("E002-H3K9ac.narrowPeak.bed",
#' 						"E001-H3K4me3.gappedPeak.bed")
#' )
#'
deepblue_select_experiments <- function(experiment_name= NULL, chromosome= NULL, start= NULL, end= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'select_experiments', experiment_name, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title select_expressions 
#' @description Select expressions (by their name or ID) as genomic regions from the specified model.
#' @family Expression data
#' 
#' @param expression_type - A string (expression type (supported: 'gene'))
#' @param sample_ids - A string or a vector of string (id(s) of selected sample(s))
#' @param replicas - A int or a vector of int (replica(s))
#' @param identifiers - A string or a vector of string (identifier(s) (for genes: ensembl ID or ENSB name).)
#' @param projects - A string or a vector of string (projects(s))
#' @param gene_model - A string (gene model name)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' genes_names =
#'     c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')
#' deepblue_select_expressions(
#'     expression_type="gene",
#'     sample_ids="s10205",
#'     identifiers = genes_names,
#'     gene_model = "gencode v23")

#'
deepblue_select_expressions <- function(expression_type= NULL, sample_ids= NULL, replicas= NULL, identifiers= NULL, projects= NULL, gene_model= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'select_expressions', expression_type, sample_ids, if (is.null(replicas)) NULL else as.integer(replicas), identifiers, projects, gene_model, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title select_genes 
#' @description Select genes (by their name or ID) as genomic regions from the specified gene model.
#' @family Gene models and genes identifiers
#' 
#' @param genes - A string or a vector of string (Name(s) or ENSEMBL ID (ENSGXXXXXXXXXXX.X ) of the gene(s).)
#' @param go_terms - A string or a vector of string (gene ontology terms - ID or label)
#' @param gene_model - A string (the gene model)
#' @param chromosome - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' genes_names =
#'     c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')
#' deepblue_select_genes(
#'     genes = genes_names,
#'     gene_model = "gencode v23")

#'
deepblue_select_genes <- function(genes= NULL, go_terms= NULL, gene_model= NULL, chromosome= NULL, start= NULL, end= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'select_genes', genes, go_terms, gene_model, chromosome, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title select_regions 
#' @description Selects Experiment regions that matches the criteria informed by the operation parameters.
#' @family Operating on the data regions
#' 
#' @param experiment_name - A string or a vector of string (name(s) of selected experiment(s))
#' @param genome - A string or a vector of string (the target genome)
#' @param epigenetic_mark - A string or a vector of string (name(s) of selected epigenetic mark(s))
#' @param sample_id - A string or a vector of string (id(s) of selected sample(s))
#' @param technique - A string or a vector of string (name(s) of selected technique(es))
#' @param project - A string or a vector of string (name(s) of selected projects)
#' @param chromosomes - A string or a vector of string (chromosome name(s))
#' @param start - A int (minimum start region)
#' @param end - A int (maximum end region)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (query id)
#'
#' @examples
#' deepblue_select_regions(
#'     genome="hg19",
#'     epigenetic_mark = "H3K27ac",
#'     project = " BLUEPRINT Epigenome")
#'
deepblue_select_regions <- function(experiment_name= NULL, genome= NULL, epigenetic_mark= NULL, sample_id= NULL, technique= NULL, project= NULL, chromosomes= NULL, start= NULL, end= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'select_regions', experiment_name, genome, epigenetic_mark, sample_id, technique, project, chromosomes, if (is.null(start)) NULL else as.integer(start), if (is.null(end)) NULL else as.integer(end), user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}



#' @export 
#' 
#' @title tiling_regions 
#' @description Generate tiling regions across the genome chromosomes. The idea is to "bin" genomic regions systematically in order to obtain discrete regions over which one can aggregate. Using the 'score_matrix' command, these bins (tiles) can be compared directly across experiments.
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
#' deepblue_tiling_regions(
#'     size = 10000,
#'     genome = "hg19",
#'     chromosome = "chr1")
#'
deepblue_tiling_regions <- function(size= NULL, genome= NULL, chromosome= NULL, user_key=deepblue_options('user_key')) {

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
    value <- xml.rpc(deepblue_options('url'), 'tiling_regions', if (is.null(size)) NULL else as.integer(size), genome, chromosome, user_key)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
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
    }

    if(is.data.frame(value[[2]]) && "count" %in% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}


#' @title xml.rpc
#' @keywords internal
#' @import XML
#' @importFrom RCurl getCurlHandle
#' @importFrom RCurl postForm
#' @importFrom RCurl parseHTTPHeader
#' @importFrom RCurl dynCurlReader
#' @importFrom RCurl base64
#' @importFrom RCurl base64Encode
#'
#' @return XML RPC request data converted to R objects
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
             verbose=deepblue_options("debug"))
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

              stop("Not sure how to convert this type")
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
              xml = xmlParse(node, asText = TRUE, encoding = "UTF-8")
              convertToR(xml)
          })


xmlRPCToR =
    function(node, ...)
    {
        type = xmlName(node)

        # if the node is a 'value' node, get its child element
        if (type == "value") {
          node = node[[1]]
          type = xmlName(node)
        }

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
        #check if our structure is nested
        descendant_struct <- getNodeSet(node, ".//struct")

        #case where we have tabular data
        if(length(descendant_struct) == 0){
            strings <- xpathSApply(node, "./member", getChildrenStrings)
            values <- as.list(strings[2,])
            names(values) <- strings[1,]
            return(values)
        }

        #further structs means recursive processing
        else{
            ans = xmlApply(node, function(x) xmlRPCToR(x[[2]][[1]], ...))
            names(ans) = xmlSApply(node, function(x) xmlValue(x[[1]]))
            return(ans)
        }
    }

xmlRPCToR.array =
    function(node, status = NULL, ...)
    {
        nodeSize <- xmlSize(node[[1]])
        elements <- xmlChildren(node[[1]])

        if(is.null(status)){
            status <- xmlRPCToR(elements[[1]])
            result <- xmlRPCToR(elements[[2]], status)
        }
        else{
            result <- vector("list", nodeSize)
            for(element in 1:nodeSize) {
                result[[element]] <- xmlRPCToR(elements[[element]])
            }

            for(r in 1:length(result)){
                test_result <- result[[r]]
                if(is.null(names(test_result))){
                    if(length(test_result) == 2){
                        names(result[[r]]) = c("id", "name")

                        if(length(result[[r]]$name) > 1)
                            result[[r]] <- c(id = result[[r]]$id, result[[r]]$name)
                    }
                    else if(length(test_result) == 3)
                    {
                        names(result[[r]]) = c("id", "name", "count")
                    }
                }
            }
            if(is.list(result) && length(result) == 1) return(result[[1]])

            framed_result <- tryCatch(data.table::rbindlist(result, fill = TRUE),
                                      error = function(e){ return(result)})

            return(framed_result)

        }
        if(is.null(status)) return(result)
        else return(list(status, result))
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

