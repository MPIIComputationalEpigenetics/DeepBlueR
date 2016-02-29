# Accessing Deepblue trough R
# For DeepBlue version 1.6.5

# We include a modified version of the XML-RPC library (http://bioconductor.org/packages/release/extra/html/XMLRPC.html) for R in this file.

deepblue.URL = "http://deepblue.mpi-inf.mpg.de/xmlrpc"
deepblue.USER_KEY = "anonymous_key"

deepblue.debug.VERBOSE = FALSE


# add_annotation
# Inserts a new annotation with the given parameters.
deepblue.add_annotation <- function(name, genome, description, data, format, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_annotation', name, genome, description, data, format, extra_metadata, user_key)
}

# add_biosource
# Inserts a new biosource with the given parameters.
deepblue.add_biosource <- function(name, description, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_biosource', name, description, extra_metadata, user_key)
}

# add_epigenetic_mark
# Inserts a new epigenetic mark with the given parameters.
deepblue.add_epigenetic_mark <- function(name, description, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_epigenetic_mark', name, description, extra_metadata, user_key)
}

# add_experiment
# Inserts a new experiment with the given parameters.
deepblue.add_experiment <- function(name, genome, epigenetic_mark, sample, technique, project, description, data, format, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_experiment', name, genome, epigenetic_mark, sample, technique, project, description, data, format, extra_metadata, user_key)
}

# add_gene_set
# Inserts a new set of genes in the GTF format. Important: It will only include the rows that have 'gene' as feature.
deepblue.add_gene_set <- function(name, description, data, format, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_gene_set', name, description, data, format, extra_metadata, user_key)
}

# add_genome
# Inserts a new genome with the given parameters.
deepblue.add_genome <- function(name, description, data, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_genome', name, description, data, user_key)
}

# add_project
# Inserts a new project with the given parameters.
deepblue.add_project <- function(name, description, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_project', name, description, user_key)
}

# add_sample
# Inserts a new sample of a given biosourcea.
deepblue.add_sample <- function(biosource_name, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_sample', biosource_name, extra_metadata, user_key)
}

# add_sample_from_gsm
# Import sample from an existing GSM identifier.
deepblue.add_sample_from_gsm <- function(name, gsm_id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_sample_from_gsm', name, gsm_id, user_key)
}

# add_technique
# Inserts a technique with the given parameters.
deepblue.add_technique <- function(name, description, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_technique', name, description, extra_metadata, user_key)
}

# add_user_to_project
# Include or exclude an user from a project
deepblue.add_user_to_project <- function(user, project, set, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'add_user_to_project', user, project, set, user_key)
}

# aggregate
# Summarize the data regions content in range regions. Use the fields @AGG.MIN, @AGG.MAX, @AGG.MEDIAN, @AGG.MEAN, @AGG.VAR, @AGG.SD, @AGG.COUNT in the get_regions command format parameter for retrieving the computed values.
deepblue.aggregate <- function(data_id, ranges_id, column, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'aggregate', data_id, ranges_id, column, user_key)
}

# cancel_request
# Stop, cancel, and remove request data. Its data will be remove if the request did finish.
deepblue.cancel_request <- function(id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'cancel_request', id, user_key)
}

# change_extra_metadata
# Change the extra metadata content for experiments, annotations, biosources, and samples.
deepblue.change_extra_metadata <- function(id, extra_metadata_key, extra_metadata_value, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'change_extra_metadata', id, extra_metadata_key, extra_metadata_value, user_key)
}

# chromosomes
# List all chromosomes of a given genome.
deepblue.chromosomes <- function(genome, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'chromosomes', genome, user_key)
}

# clone_dataset
# Clone the dataset, allowing to change the description, column format (restrictively), and extra_metadata.
deepblue.clone_dataset <- function(dataset_id, new_name, new_epigenetic_mark, new_sample, new_technique, new_project, description, format, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'clone_dataset', dataset_id, new_name, new_epigenetic_mark, new_sample, new_technique, new_project, description, format, extra_metadata, user_key)
}

# commands
# Lists all existing commands.
deepblue.commands <- function() {
    xml.rpc(deepblue.URL, 'commands')
}

# count_regions
# Send a request to count the number of regions in the result of the given query.
deepblue.count_regions <- function(query_id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'count_regions', query_id, user_key)
}

# create_column_type_calculated
# Create a calculated column
deepblue.create_column_type_calculated <- function(name, description, code, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'create_column_type_calculated', name, description, code, user_key)
}

# create_column_type_category
# Create a column type from a category set.
deepblue.create_column_type_category <- function(name, description, items, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'create_column_type_category', name, description, items, user_key)
}

# create_column_type_range
# Create a column type from a category set.
deepblue.create_column_type_range <- function(name, description, minimum, maximum, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'create_column_type_range', name, description, minimum, maximum, user_key)
}

# create_column_type_simple
# Create a column type from a category set.
deepblue.create_column_type_simple <- function(name, description, type, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'create_column_type_simple', name, description, type, user_key)
}

# echo
# Echos the server's version.
deepblue.echo <- function(user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'echo', user_key)
}

# extract_ids
# Extract the names from a list of ID and Names.
deepblue.extract_ids <- function(list) {
    xml.rpc(deepblue.URL, 'extract_ids', list)
}

# extract_names
# Extract the names from a list of ID and Names.
deepblue.extract_names <- function(list) {
    xml.rpc(deepblue.URL, 'extract_names', list)
}

# faceting_experiments
# Experiments faceting.
deepblue.faceting_experiments <- function(genome, type, epigenetic_mark, biosource, sample, technique, project, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'faceting_experiments', genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
}

# filter_regions
# Filters the result of the given query by the given restrictions.
deepblue.filter_regions <- function(query_id, field, operation, value, type, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'filter_regions', query_id, field, operation, value, type, user_key)
}

# find_pattern
# Process an annotation that will contain all genomic positions from the given pattern.
deepblue.find_pattern <- function(pattern, genome, overlap, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'find_pattern', pattern, genome, overlap, user_key)
}

# flank
# Generate flanking regions for the given regions.
deepblue.flank <- function(query_id, start, length, use_strand, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'flank', query_id, as.integer(start), as.integer(length), use_strand, user_key)
}

# get_biosource_children
# Gets the scope for the biosource.
deepblue.get_biosource_children <- function(biosource, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_biosource_children', biosource, user_key)
}

# get_biosource_parents
# Gets the biosources that are parents of the given biosource.
deepblue.get_biosource_parents <- function(biosource, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_biosource_parents', biosource, user_key)
}

# get_biosource_related
# Gets biosources related to the given one. e.g. the children terms and theirs synonyms.
deepblue.get_biosource_related <- function(biosource, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_biosource_related', biosource, user_key)
}

# get_biosource_synonyms
# Gets the synonyms for the biosource.
deepblue.get_biosource_synonyms <- function(biosource, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_biosource_synonyms', biosource, user_key)
}

# get_experiments_by_query
# Return a list of experiments and annotations that have at least one region in the data set represented by the query.
deepblue.get_experiments_by_query <- function(query_id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_experiments_by_query', query_id, user_key)
}

# get_regions
# Send a request  to retrieve the regions for the given query in the requested BED format.
deepblue.get_regions <- function(query_id, output_format, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_regions', query_id, output_format, user_key)
}

# get_request_data
# Get the request data.
deepblue.get_request_data <- function(request_id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_request_data', request_id, user_key)
}

# get_state
# Returns the current state of specific data.
deepblue.get_state <- function(data_name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'get_state', data_name, user_key)
}

# info
# Return information for the given ID (or IDs).
deepblue.info <- function(id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'info', id, user_key)
}

# input_regions
# Include a region set that will be used by the follow ups operations.
deepblue.input_regions <- function(genome, region_set, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'input_regions', genome, region_set, user_key)
}

# intersection
# Select regions from the first query that does intersect with at least one second query region.
deepblue.intersection <- function(query_a_id, query_b_id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'intersection', query_a_id, query_b_id, user_key)
}

# is_biosource
# Return information for the given biosource name.
deepblue.is_biosource <- function(biosource_name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'is_biosource', biosource_name, user_key)
}

# list_annotations
# Lists all existing annotations.
deepblue.list_annotations <- function(genome, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_annotations', genome, user_key)
}

# list_biosources
# Lists all existing biosources.
deepblue.list_biosources <- function(extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_biosources', extra_metadata, user_key)
}

# list_column_types
# Lists all available column types.
deepblue.list_column_types <- function(user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_column_types', user_key)
}

# list_epigenetic_marks
# Lists all existing epigenetic marks.
deepblue.list_epigenetic_marks <- function(extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_epigenetic_marks', extra_metadata, user_key)
}

# list_experiments
# Lists all existing experiments.
deepblue.list_experiments <- function(genome, type, epigenetic_mark, biosource, sample, technique, project, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_experiments', genome, type, epigenetic_mark, biosource, sample, technique, project, user_key)
}

# list_genomes
# Lists all existing genomes.
deepblue.list_genomes <- function(user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_genomes', user_key)
}

# list_in_use
# Lists all terms from the given controlled vocabulary that are used.
deepblue.list_in_use <- function(controlled_vocabulary, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_in_use', controlled_vocabulary, user_key)
}

# list_projects
# Lists all existing projects.
deepblue.list_projects <- function(user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_projects', user_key)
}

# list_recent_experiments
# Lists all recent experiments.
deepblue.list_recent_experiments <- function(days, genome, epigenetic_mark, sample, technique, project, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_recent_experiments', days, genome, epigenetic_mark, sample, technique, project, user_key)
}

# list_requests
# Lists all requests in given state.
deepblue.list_requests <- function(request_state, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_requests', request_state, user_key)
}

# list_samples
# Lists all existing samples that matches the given biosource and metadata.
deepblue.list_samples <- function(biosource, extra_metadata=NULL, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_samples', biosource, extra_metadata, user_key)
}

# list_similar_biosources
# Lists all biosources similar to the one provided.
deepblue.list_similar_biosources <- function(name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_similar_biosources', name, user_key)
}

# list_similar_epigenetic_marks
# Lists all epigenetic marks similar to the one provided.
deepblue.list_similar_epigenetic_marks <- function(name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_similar_epigenetic_marks', name, user_key)
}

# list_similar_experiments
# Lists all experiments similar to the one provided.
deepblue.list_similar_experiments <- function(name, genome, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_similar_experiments', name, genome, user_key)
}

# list_similar_genomes
# Lists all genomes similar to the one provided.
deepblue.list_similar_genomes <- function(name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_similar_genomes', name, user_key)
}

# list_similar_projects
# Lists all projects similar to the one provided.
deepblue.list_similar_projects <- function(name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_similar_projects', name, user_key)
}

# list_similar_techniques
# Lists all techniques similar to the one provided.
deepblue.list_similar_techniques <- function(name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_similar_techniques', name, user_key)
}

# list_techniques
# Lists all existing techniques.
deepblue.list_techniques <- function(user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'list_techniques', user_key)
}

# merge_queries
# Merges the regions of the given queries.
deepblue.merge_queries <- function(query_a_id, query_b_id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'merge_queries', query_a_id, query_b_id, user_key)
}

# query_cache
# Return information for the given ID (or IDs).
deepblue.query_cache <- function(query_id, cache, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'query_cache', query_id, cache, user_key)
}

# query_experiment_type
# Return information for the given ID (or IDs).
deepblue.query_experiment_type <- function(query_id, type, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'query_experiment_type', query_id, type, user_key)
}

# remove
# Remove data from DeepBlue.
deepblue.remove <- function(id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'remove', id, user_key)
}

# score_matrix
# Gets the regions for the given query in the requested BED format.
deepblue.score_matrix <- function(experiments_format, aggregation_function, query_id, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'score_matrix', experiments_format, aggregation_function, query_id, user_key)
}

# search
# Search all data of all types for the given keyword. A minus (-) character in front of a keyword searches for data without the given keyword. The search can be restricted to the following data types are: annotations,biosources,column_types,epigenetic_marks,experiments,genomes,gene_sets,genes,projects,samples,techniques,tilings
deepblue.search <- function(keyword, type, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'search', keyword, type, user_key)
}

# select_annotations
# Selects annotation regions matching the given parameters.
deepblue.select_annotations <- function(annotation_name, genome, chromosome, start, end, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'select_annotations', annotation_name, genome, chromosome, as.integer(start), as.integer(end), user_key)
}

# select_experiments
# Selects experiments data. It is a simpler version of the select_regions command.
deepblue.select_experiments <- function(experiment_name, chromosome, start, end, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'select_experiments', experiment_name, chromosome, as.integer(start), as.integer(end), user_key)
}

# select_genes
# Selects genes as regions.
deepblue.select_genes <- function(genes_name, gene_set, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'select_genes', genes_name, gene_set, user_key)
}

# select_regions
# Selects experiment regions matching the given parameters.
deepblue.select_regions <- function(experiment_name, genome, epigenetic_mark, sample_id, technique, project, chromosome, start, end, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'select_regions', experiment_name, genome, epigenetic_mark, sample_id, technique, project, chromosome, as.integer(start), as.integer(end), user_key)
}

# set_biosource_parent
# Sets a biosource parent.
deepblue.set_biosource_parent <- function(parent, child, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'set_biosource_parent', parent, child, user_key)
}

# set_biosource_synonym
# Sets a biosource synonym.
deepblue.set_biosource_synonym <- function(biosource, synonym_name, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'set_biosource_synonym', biosource, synonym_name, user_key)
}

# set_project_public
# Set a project as public. You must be the project owner to perform this operation.
deepblue.set_project_public <- function(project, set, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'set_project_public', project, set, user_key)
}

# tiling_regions
# Creates regions with the tiling size over the chromosomes.
deepblue.tiling_regions <- function(size, genome, chromosome, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'tiling_regions', as.integer(size), genome, chromosome, user_key)
}

# upload_chromosome
# Uploads the sequence data of the chromosome.
deepblue.upload_chromosome <- function(genome, chromosome, data, user_key=deepblue.USER_KEY) {
    xml.rpc(deepblue.URL, 'upload_chromosome', genome, chromosome, data, user_key)
}


library(XML)
library(RCurl)
library(readr)
library(dplyr)

deepblue.get_request_data_r <-function(request_id, user_key=deepblue.USER_KEY, forceDownload=FALSE,
        .defaultOpts = list(httpheader = c('Content-Type' = "text/xml"), followlocation = TRUE, useragent = useragent),
        .curl = getCurlHandle())
{
  request_info = deepblue.info(request_id, user_key)[2]
  if (request_info$value$value$state != "done") {
    stop("Processing was not finished. Please, check it status with deepblue.info(request_id)");
  }

  command = request_info$value$value$command
  if (command %in% c("count_regions", "get_experiments_by_query"))  {
    deepblue.get_request_data(request_id, user_key)
  } else if (command %in% c("get_regions", "score_matrix")) {
    url = paste("http://deepblue.mpi-inf.mpg.de/xmlrpc/download/?r=", request_id, "&key=", user_key, sep="")
    
    #create name of temp file
    temp_download <- paste(tempdir(), request_id, sep="/")
    
    #download file only if it was not downloaded before or if forcedDownload is set
    if(!file.exists(temp_download) || forceDownload){
      message(paste("Downloading request", request_id))
      download.file(url, temp_download, mode="wb")
    } else{
      message(paste("Using locally cached copy of", request_id))
    }
    
    #read bz compressed file... 
    handle <-  bzfile(temp_download)
    
    #read tab separated file efficiently using the readr package (~x10 speedup compared to base read function)
    message(paste("Reading in request", request_id))
    read_tsv(handle, col_names = FALSE)  
  } else {
    stop(paste("Unknow command", command));
  }
}

xml.rpc =
function(url, method, ..., .args = list(...),
          .opts = list(),
          .defaultOpts = list(httpheader = c('Content-Type' = "text/xml"), followlocation = TRUE, useragent = useragent),
          .convert = TRUE, .curl = getCurlHandle(), useragent = "DeepBlue-R-XMLRPC", verbose=deepblue.debug.VERBOSE)
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
   # or return as is or allow the caller to specify a function to use for conversion.
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
  sapply(args, function(x) newXMLNode("param", rpc.serialize(x), parent = params))
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
  rpc.serialize(structure(lapply(els, function(id) slot(x, id)), names = els), ...)
}


basicTypeMap =
  c("integer" = "i4",
    "double" = "double",
    "character" = "string",
    "logical" = "boolean",
    "POSIXt" = "dateTime.iso8601",
    "POSIXct" = "dateTime.iso8601",
    "Date" = "dateTime.iso8601",
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
#              x = gsub("\\n", "", x)
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

#              else
              {
                if(length(x) == 1)
                  newXMLNode("value", newXMLNode(type, if(type == "string") newXMLCDataNode(x) else x))
                else {
                  vectorArray(x, type)
                }
              }
           })


FormatStrings = c(numeric = "%f", integer = "%d", logical = "%s",
                   i4 = "%d", double = "%f",
                  string = "%s", Date = "%s",  POSIXt = "%s", POSIXct = "%s")

vectorArray =
function(x, type)
{
  top = newXMLNode("value")
  a = newXMLNode("array", parent = top)
  data = newXMLNode("data", parent = a)
#  sapply(x, function(x) newXMLNode("value", newXMLNode(type, if(type == "string") newXMLCDataNode(x) else x), parent = data))

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

#  sapply(x, function(x)  newXMLNode(type, if(type == "string") newXMLCDataNode(x) else x, parent = data))
  top
}

setMethod("rpc.serialize", "list",
           function(x, ...) {
              if(length(names(x))) {
                  a = newXMLNode("struct")
                  sapply(names(x), function(id) {
                                     type = basicTypeMap[typeof(x[[id]])]
                                     newXMLNode("member",
                                        newXMLNode("name", id), rpc.serialize(x[[id]]),
                                        parent = a)
                                   })
                  newXMLNode("value", a)
              } else {
                a = newXMLNode("array")
                data = newXMLNode("data", parent = a)
                sapply(x, function(x) {
                        elName = basicTypeMap[typeof(x)]
                        newXMLNode("value", newXMLNode(elName, if(elName == "string") newXMLCDataNode(x) else x, parent = data))
                })
                newXMLNode("value", a)
              }
           })


setGeneric('convertToR', function(node) standardGeneric('convertToR'))

setMethod('convertToR', 'XMLInternalDocument', function(node)
{
    fault = getNodeSet(node,path="//methodResponse/fault/value/struct")
    if (length(fault) > 0) {
          fault = xmlRPCToR(fault[[1]])
          e = simpleError(paste("faultCode: ",  fault$faultCode, " faultString: ", fault$faultString))
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
     ans = xpathApply(node, "./param/value", xmlRPCToR, simplify = FALSE)
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
         'dateTime.iso8601' = as.POSIXct(strptime(xmlValue(node), "%Y%m%dT%H:%M:%S")),
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
  ans = xmlApply(node[["data"]], function(x) xmlRPCToR(x[[1]]))
}

