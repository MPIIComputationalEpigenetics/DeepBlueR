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
#'
#' @examples
#' \dontrun{
#'   data = "chr1	28735	29810
#' chr1	135124	135563
#' chr1	327790	328229
#' chr1	437151	438164
#' chr1	449273	450544
#' chr1	533219	534114
#' chr1	544738	546649
#' chr1	713984	714547
#' chr1	762416	763445
#' chr1	788863	789211"
#' deepblue.add_annotation(name="Interesting annotation", genome="hg19",
#' 	description="It is an annotation with some interesting regions",
#' 	data=data, format="CHROMOSOME,START,END",
#' 	extra_metadata=list(source="My own experiments"),
#' 	user_key="my_private_user_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.add_biosource(name="New BioSource",
#' 	description="This Biosource is used for..",
#' 	extra_metadata=list(source="Ontology Blabla",
#' 		reference="http://ontology.blabla/id"),
#' 	user_key="my_private_user_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.add_epigenetic_mark(name="H3k123ac Name",
#' 	description="A brand new histone mark",
#' 	extra_metadata=list(type="Histone Mark"),
#' 	user_key="my_secret_key" )
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   data = "chr1	28735	29810
#' chr1	135124	135563
#' chr1	327790	328229
#' chr1	437151	438164
#' chr1	449273	450544
#' chr1	533219	534114
#' chr1	544738	546649
#' chr1	713984	714547
#' chr1	762416	763445
#' chr1	788863	789211"
#' deepblue.add_experiment(name="Interesting experiment", genome="hg19",
#' 	epigenetic_mark="H3k27ac", sample="s123456", technique="Chip-seq",
#' 	project="My Own project",
#' 	description="It is an experiment with some interesting regions", data=data,
#' 	format="CHROMOSOME,START,END",
#' 	extra_metadata=list(source="My own experiments"),
#' 	user_key="my_private_user_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   gs = 'chr1HAVANA\tgene\t11869\t14409\t.\t+\t.\tgene_id "ENSG00000223972.5";",
#'      "gene_type "transcribed_unprocessed_pseudogene"; gene_status "KNOWN"; ",
#'      "gene_name "DDX11L1";  level 2; havana_gene "OTTHUMG00000000961.2";\n",
#'      "chr1\tHAVANA\tgene\t14404\t29570\t.\t-\t.\tgene_id "ENSG00000227232.5";",
#'      " gene_type "unprocessed_pseudogene"; gene_status "KNOWN"; ",
#'      "gene_name "WASH7P";  level 2; havana_gene "OTTHUMG00000000958.1";\n",
#'      "chr1\tENSEMBL\tgene\t17369\t17436\t.\t-\t.\tgene_id "ENSG00000278267.1";",
#'      " gene_type "miRNA"; gene_status "KNOWN"; gene_name "MIR6859-3";  level 3;'
#' 
#' deepblue.add_gene_set(name = "My new gene set", description = "A gene set",
#' 	data = gs, format = "gtf", extra_metadata = NULL,
#' 	user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   GRCh38_info = "chr1	248956422
#' chr2	242193529
#' chr3	198295559
#' chr4	190214555
#' chr5	181538259
#' chr6	170805979
#' chr7	159345973
#' chr8	145138636
#' chr9	138394717
#' chr10	133797422
#' chr11	135086622
#' chr12	133275309
#' chr13	114364328
#' chr14	107043718
#' chr15	101991189
#' chr16	90338345
#' chr17	83257441
#' chr18	80373285
#' chr19	58617616
#' chr20	64444167
#' chr21	46709983
#' chr22	50818468
#' chrX	156040895
#' chrY	57227415
#' chrM	16569"
#' 
#' deepblue.add_genome(name = "GRCh38",
#' 	description = "GRCh38 with the main chromosomes",
#' 	data = GRCh38_info, user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.add_project(name = "My project",
#' 	description = "Data that I will store for myself",
#' 	user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.add_sample(biosource_name = "BLOOD",
#' 	extra_metadata = list(age="20 days"), user_key = "my_secret_key")
#' }
#'
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
#' @param biosource_name - A string (biosource name)
#' @param gsm_id - A string (GSM ID)
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly inserted sample)
#'
#' @examples
#' \dontrun{
#'   deepblue.add_sample_from_gsm(biosource_name = "BLOOD",
#' 	gsm_id = "GSM1234567890", user_key = "my_secret_key")
#' }
#'
deepblue.add_sample_from_gsm <- function(biosource_name= NULL, gsm_id= NULL, user_key=deepblue.USER_KEY) {

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
    value <- xml.rpc(deepblue.URL, 'add_sample_from_gsm', biosource_name, gsm_id, user_key)
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
#'
#' @examples
#' \dontrun{
#'   deepblue.add_technique(name = "New technique",
#' 	description = "This technique is faster and cheaper",
#' 	extra_metadata = list(information = "some useful information"),
#' 	user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.add_user_to_project(user = "u1234", project = "My secret project",
#'  set = TRUE, user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.aggregate(data_id = "q123456", ranges_id = "q654321",
#'   column = "SCORE", user_key = deepblue.USER_KEY)
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.cancel_request(id = "r12345", user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.change_extra_metadata(id = "s1234", extra_metadata_key = "age",
#' 	extra_metadata_value = "20 days", user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.chromosomes(genome = "g1", user_key = deepblue.USER_KEY)
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.clone_dataset(dataset_id = "e12345",
#' 	new_name = "Dataset with better metadata",
#'   	new_sample = "s1234", new_technique = "CHiP-Seq",
#'   	new_project = "My private data", Auser_key = "my_secret_key")
#' }
#'
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
#' \dontrun{
#'   deepblue.collection_experiments_count(controlled_vocabulary="epigenetic_marks",
#'   genome = "hg19", type = "peaks", biosource = "blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.commands()
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.count_regions(query_id = "q12345", user_key = deepblue.USER_KEY)
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.create_column_type_calculated(name = "CALC_SQUARE_ROOT_VALUE",
#'   description ="The square root of the value column",
#'   code = "return sqrt(value_of('VALUE'))", user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.create_column_type_category(name = FRUITS,
#' 	description = "Fruits that we accept",
#'   	items = c("apple", "banana", "grape"),
#'   	user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.create_column_type_range(name = ACCEPTED_VALUE,
#' 	description = "Values that we accept",
#'   	minimum = 1, maximum = 99, user_key = "my_secret_key")
#' }
#'
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
#' @param type - A string (type of the column type (string, integer, double))
#' @param user_key - A string (users token key)
#'
#' @return id - A string (id of the newly created column type)
#'
#' @examples
#' \dontrun{
#'   deepblue.create_column_type_simple(name = "SOME_VALUE",
#' 	description = "This column will include some value",
#'   	type = "integer", user_key = "my_secret_key")

#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.echo(user_key = "anonymous_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.extend(query_id = "q1234", length = 2000, direction = "BOTH",
#'   use_strand = TRUE, user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.extract_ids(list = list(list("a124", "Annotation 1"), list("a1235", "Annotation 2")))
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.extract_names(list = list(list("a124", "Annotation 1"), list("a1235", "Annotation 2")))
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.faceting_experiments(genome = "hg19", type = "peaks",
#'   biosource = "blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.filter_regions(query_id = "q12345", field = "VALUE", operation = ">",
#'   value = "100", type = "number", user_key = "anonymous_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.find_pattern(pattern = "CAC[TAG]+CAC", genome = "hg19", overlap = FALSE,
#'   user_key = "my_secret_key")

#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.flank(query_id = "q1234", start = 0, length = 2000,
#'   use_strand = TRUE, user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.get_biosource_children(biosource = "Blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.get_biosource_parents(biosource = "Blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.get_biosource_related(biosource = "Blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.get_biosource_synonyms(biosource = "prostate gland")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.get_experiments_by_query(query_id = "q12345")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.get_regions(query_id = "q123456", output_format = "CHROMOSOME,START,END",
#'   user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.get_request_data(request_id = "r12345", user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.info(id = "e30035")
#' }
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
#'
#' @examples
#' \dontrun{
#'   regions_set = "chr1  28735 29810
#' chr1  135124  135563
#' chr1  327790  328229
#' chr1  437151  438164
#' chr1  449273  450544
#' chr1  533219  534114
#' chr1  544738  546649
#' chr1  713984  714547
#' chr1  762416  763445
#' chr1  788863  789211"
#' deepblue.input_regions(genome = "hg19", region_set = regions_set,
#'   user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.intersection(query_a_id = query_a, query_b_id = query_b,
#'   user_key = "my_secret_user_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.is_biosource(biosource_name = "blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_annotations(genome = "hg19")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_biosources(extra_metadata = list(namespace = "uberon"))
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_column_types()
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_epigenetic_marks()
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_experiments(genome = "hg19", type = "peaks",
#'   epigenetic_mark = "H3K27ac", biosource = "blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_genomes()
#' }
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
#' @param controlled_vocabulary - A string (controlled vocabulary name)
#' @param user_key - A string (users token key)
#'
#' @return terms - A array (controlled_vocabulary terms with count)
#'
#' @examples
#' \dontrun{
#'   deepblue.list_in_use(controlled_vocabulary = "biosources")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_projects()
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_recent_experiments(days = 2, genome = "hg19")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_requests(request_state = 'done', user_key = 'my_secret_key')
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_samples(biosource = "Blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_similar_biosources(name = "blood")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_similar_epigenetic_marks(name = "H3k27ac")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_similar_experiments(name = "blood", genome = "hg19")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_similar_genomes(name = "grc")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_similar_projects(name = "BLUEPRINT")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_similar_techniques(name = "chip seq")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.list_techniques()
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.merge_queries(query_a_id = query_a, query_b_id = query_a,
#'   user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.query_cache(query_id = "q12345", cache = TRUE,
#'   user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.query_experiment_type(query_id = "q1234", type = "signal",
#'   user_key = "my_secret_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.remove(id = data_id, user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.score_matrix(experiments_columns = list(list("Experiment One", "VALUE)),
#'   aggregation_function = "mean", aggregation_regions_id = "q123",
#'   user_key = "my_private_key")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.search(keyword = "DNA Methylation BLUEPRINT", type = "experiments")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.select_annotations(annotation_name = "Cpg Islands", genome = "hg19",
#'   chromosome = "chr1", start = 0, end = 2000000)
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.select_experiments(
#' 	experiment_name = c("E002-H3K9ac.narrowPeak.bed",
#' 						"E001-H3K4me3.gappedPeak.bed")
#' )
#' }
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
#'
#' @examples
#' \dontrun{
#'   genes_names = c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')
#' deepblue.select_genes(genes_name = genes_names, gene_set = "gencode v23")
#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.select_regions(genome="hg19", epigenetic_mark = "H3K27ac",
#'   project = " BLUEPRINT Epigenome")

#' }
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
#'
#' @examples
#' \dontrun{
#'   deepblue.set_biosource_parent(parent = "biosource-parent",
#'   child = "biosource-child", user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.set_biosource_synonym(biosource = "biosource name",
#'   synonym_name = "synonym name", user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.set_project_public(project = "My secret project", set = TRUE,
#'   user_key = "my_secret_key")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#'   deepblue.tiling_regions(size = 10000, genome = "hg19",
#'   chromosome = "chr1")
#' }
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
#'
#' @examples
#' \dontrun{
#'   data = "ACTGAGCTCAGC"
#' deepblue.upload_chromosome(genome = "genome_name",
#' 	chromosome = "chromosome_name", data = NULL,
#'   	user_key = "my_secret_key")
#' }
#'
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
