# Accessing Deepblue through R
# For DeepBlue version 1.7.3

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
#' @title add_annotation 
#' @description Inserts a new annotation with the given parameters.
#' @family Inserting and listing annotations
#' 
#' @param name - A string (annotation name)
#' @param genome - A string (the target genome)
#' @param description - A string (description of the annotation)
#' @param data - A string (the BED formatted data)
#' @param format - A string (format of the provided data)
#' @param extra_metadata - A struct (additional metadata)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted annotation)
deepblue.add_annotation <- function(name= NULL, genome= NULL, description= NULL, data= NULL, format= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_annotation', name, genome, description, data, format, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_biosource 
#' @description Inserts a new biosource with the given parameters.
#' @family Inserting and listing biosources
#' 
#' @param name - A string (biosource name)
#' @param description - A string (description of the biosource)
#' @param extra_metadata - A struct (additional metadata)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted biosource)
deepblue.add_biosource <- function(name= NULL, description= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_biosource', name, description, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_epigenetic_mark 
#' @description Inserts a new epigenetic mark with the given parameters.
#' @family Inserting and listing epigenetic marks
#' 
#' @param name - A string (name of the epigenetic mark)
#' @param description - A string (description of the epigenetic mark)
#' @param extra_metadata - A struct (additional metadata)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted epigenetic mark)
deepblue.add_epigenetic_mark <- function(name= NULL, description= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_epigenetic_mark', name, description, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_experiment 
#' @description Inserts a new experiment with the given parameters.
#' @family Inserting and listing experiments
#' 
#' @param name - A string (experiment name)
#' @param genome - A string (the target genome)
#' @param epigenetic_mark - A string (epigenetic mark of the experiment)
#' @param sample - A string (id of the used sample)
#' @param technique - A string (technique used by this experiment)
#' @param project - A string (the project name)
#' @param description - A string (description of the experiment)
#' @param data - A string (the BED formated data)
#' @param format - A string (format of the provided data)
#' @param extra_metadata - A struct (additional metadata)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted experiment)
deepblue.add_experiment <- function(name= NULL, genome= NULL, epigenetic_mark= NULL, sample= NULL, technique= NULL, project= NULL, description= NULL, data= NULL, format= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_experiment', name, genome, epigenetic_mark, sample, technique, project, description, data, format, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_gene_set 
#' @description Inserts a new set of genes in the GTF format. Important: It will only include the rows that have 'gene' as feature.
#' @family Operations on gene sets and genes identifiers
#' 
#' @param name - A string (gene set name)
#' @param description - A string (description of the annotation)
#' @param data - A string (the BED formatted data)
#' @param format - A string (Currently, it is only supported GTF.)
#' @param extra_metadata - A struct (additional metadata)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted annotation)
deepblue.add_gene_set <- function(name= NULL, description= NULL, data= NULL, format= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_gene_set', name, description, data, format, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_genome 
#' @description Inserts a new genome with the given parameters.
#' @family Inserting and listing genomes
#' 
#' @param name - A string (genome name)
#' @param description - A string (description of the genome)
#' @param data - A string (genome data)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted genome)
deepblue.add_genome <- function(name= NULL, description= NULL, data= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_genome', name, description, data, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_project 
#' @description Inserts a new project with the given parameters.
#' @family Inserting and listing projects
#' 
#' @param name - A string (projectname)
#' @param description - A string (description of the project)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted project)
deepblue.add_project <- function(name= NULL, description= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_project', name, description, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_sample 
#' @description Inserts a new sample of a given biosourcea.
#' @family Inserting and listing samples
#' 
#' @param biosource_name - A string (biosource name)
#' @param extra_metadata - A struct (sample extra metadata. You can include any key-value collection here.)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted sample)
deepblue.add_sample <- function(biosource_name= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_sample', biosource_name, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_sample_from_gsm 
#' @description Import sample from an existing GSM identifier.
#' @family Inserting and listing samples
#' 
#' @param name - A string (biosource name)
#' @param gsm_id - A string (GSM ID)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted sample)
deepblue.add_sample_from_gsm <- function(name= NULL, gsm_id= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_sample_from_gsm', name, gsm_id, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_technique 
#' @description Inserts a technique with the given parameters.
#' @family Inserting and listing techniques
#' 
#' @param name - A string (technique name)
#' @param description - A string (description of technique)
#' @param extra_metadata - A struct (additional metadata)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted technique)
deepblue.add_technique <- function(name= NULL, description= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_technique', name, description, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title add_user_to_project 
#' @description Include or exclude an user from a project
#' @family Inserting and listing projects
#' 
#' @param user - A string (User name or ID)
#' @param project - A string (Project name or ID)
#' @param set - A boolean (True to include the user or false to remove)
#' @param user_key - A string (users token key)
#'
#' @return user_id - A string (id of the user)
deepblue.add_user_to_project <- function(user= NULL, project= NULL, set= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_user_to_project', user, project, set, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title change_extra_metadata 
#' @description Change the extra metadata content for experiments, annotations, biosources, and samples.
#' @family Operations that modify the data content
#' 
#' @param id - A string (id of the data)
#' @param extra_metadata_key - A string (extra_metadata key)
#' @param extra_metadata_value - A string (extra_metadata key (empty for delete this key))
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the modified data)
deepblue.change_extra_metadata <- function(id= NULL, extra_metadata_key= NULL, extra_metadata_value= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'change_extra_metadata', id, extra_metadata_key, extra_metadata_value, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title clone_dataset 
#' @description Clone the dataset, allowing to change the description, column format (restrictively), and extra_metadata.
#' @family Operations that modify the data content
#' 
#' @param dataset_id - A string (ID of the dataset (experiment or annotation ID))
#' @param new_name - A string (New dataset name)
#' @param new_epigenetic_mark - A string (New epigenetic mark)
#' @param new_sample - A string (New sample ID)
#' @param new_technique - A string (New technique)
#' @param new_project - A string (New project)
#' @param description - A string (description of the experiment - empty to copy from the cloned dataset)
#' @param format - A string (format of the provided data - empty to copy from the cloned dataset)
#' @param extra_metadata - A struct (additional metadata - empty to copy from the cloned dataset)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the new dataset)
deepblue.clone_dataset <- function(dataset_id= NULL, new_name= NULL, new_epigenetic_mark= NULL, new_sample= NULL, new_technique= NULL, new_project= NULL, description= NULL, format= NULL, extra_metadata=NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'clone_dataset', dataset_id, new_name, new_epigenetic_mark, new_sample, new_technique, new_project, description, format, extra_metadata, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title create_column_type_calculated 
#' @description Create a calculated column
#' @family Inserting and listing different column types
#' 
#' @param name - A string (column type name)
#' @param description - A string (description of the column type)
#' @param code - A string (Lua code that will be executed)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly created column type)
deepblue.create_column_type_calculated <- function(name= NULL, description= NULL, code= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'create_column_type_calculated', name, description, code, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title create_column_type_category 
#' @description Create a column type from a category set.
#' @family Inserting and listing different column types
#' 
#' @param name - A string (column type name)
#' @param description - A string (description of the column type)
#' @param items - A string or a vector of string (items that are accepted for this category set)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly created column type)
deepblue.create_column_type_category <- function(name= NULL, description= NULL, items= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'create_column_type_category', name, description, items, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title create_column_type_range 
#' @description Create a column type from a category set.
#' @family Inserting and listing different column types
#' 
#' @param name - A string (column type name)
#' @param description - A string (description of the column type)
#' @param minimum - A double (minimum value for this range (inclusive))
#' @param maximum - A double (maximum value for this range (inclusive))
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly created column type)
deepblue.create_column_type_range <- function(name= NULL, description= NULL, minimum= NULL, maximum= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'create_column_type_range', name, description, minimum, maximum, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title create_column_type_simple 
#' @description Create a column type from a category set.
#' @family Inserting and listing different column types
#' 
#' @param name - A string (column type name)
#' @param description - A string (description of the column type)
#' @param type - A string (type of the column type)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly created column type)
deepblue.create_column_type_simple <- function(name= NULL, description= NULL, type= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'create_column_type_simple', name, description, type, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title find_pattern 
#' @description Process an annotation that will contain all genomic positions from the given pattern.
#' @family Inserting and listing annotations
#' 
#' @param pattern - A string (pattern (regular expression))
#' @param genome - A string (the target genome)
#' @param overlap - A boolean (if the matching should do overlap search)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the annotation that contains the positions of the given pattern)
deepblue.find_pattern <- function(pattern= NULL, genome= NULL, overlap= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'find_pattern', pattern, genome, overlap, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title get_state 
#' @description Returns the current state of specific data.
#' @family Commands for all types of data
#' 
#' @param data_name - A string (Name of the data to lookup the state for)
#' @param user_key - A string (users token key)
#'
#' @return data_state - A int (State of the data)
deepblue.get_state <- function(data_name= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'get_state', data_name, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @param controlled_vocabulary - A string (id of the data)
#' @param user_key - A string (users token key)
#'
#' @return terms - A array (controlled_vocabulary terms with count)
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title remove 
#' @description Remove data from DeepBlue.
#' @family Commands for all types of data
#' 
#' @param id - A string (Data ID to be removed.)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the removed data)
deepblue.remove <- function(id= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'remove', id, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title set_biosource_parent 
#' @description Sets a biosource parent.
#' @family Set the relationship between different biosources
#' 
#' @param parent - A string (parent)
#' @param child - A string (child)
#' @param user_key - A string (users token key)
#'
#' @return nothing :-(
deepblue.set_biosource_parent <- function(parent= NULL, child= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'set_biosource_parent', parent, child, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title set_biosource_synonym 
#' @description Sets a biosource synonym.
#' @family Set the relationship between different biosources
#' 
#' @param biosource - A string (name of the biosource)
#' @param synonym_name - A string (name of the synonym)
#' @param user_key - A string (users token key)
#'
#' @return synonym_name - A string (inserted synonym_name)
deepblue.set_biosource_synonym <- function(biosource= NULL, synonym_name= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'set_biosource_synonym', biosource, synonym_name, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title set_project_public 
#' @description Set a project as public. You must be the project owner to perform this operation.
#' @family Inserting and listing projects
#' 
#' @param project - A string (Project name or ID)
#' @param set - A boolean (True to set the project as public of false for unset)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the project)
deepblue.set_project_public <- function(project= NULL, set= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'set_project_public', project, set, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
#' @title upload_chromosome 
#' @description Uploads the sequence data of the chromosome.
#' @family Inserting and listing genomes
#' 
#' @param genome - A string (the target genome)
#' @param chromosome - A string (chromosome name)
#' @param data - A string (chromosome sequence data)
#' @param user_key - A string (users token key)
#'
#' @return nothing :-(
deepblue.upload_chromosome <- function(genome= NULL, chromosome= NULL, data= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'upload_chromosome', genome, chromosome, data, user_key)
    status = value[[1]]
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(!is.list(value[[2]])){
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
