#' Lift over region results between Genome Assemblies used in DeepBlue
#'
#' @importFrom rtracklayer liftOver
#' @importFrom rtracklayer import.chain
#' @importFrom R.utils gunzip
#' @importFrom GenomeInfoDb genome
#' @importFrom GenomeInfoDb genome<-
#'
#' @param regions The GRanges object to lift over to another assembly
#' @param source The source assembly version, e.g. hg38. If NULL, we try to
#' read the genome version from the GRanges object.
#' @param target The target assembly version, e.g. hg19. Required.
#' @param collapse Whether to return a single GRanges object or a list of
#' GRanges (one per region in the input). The latter is the default behavior of
#' liftOver since multiple assignments are possible.
#'
#' @description This is a wrapper function for the liftOver function found in
#' the rtracklayer package. For common genome assemblies available in DeepBlue,
#' this function automatically downloads the necessary chain file and calls
#' liftOver.
#'
#' @return A GRanges object using the target chromosome positions
#' @export
#'
#' @examples data_id = deepblue_select_experiments(
#' experiment_name="E002-H3K9ac.narrowPeak.bed", chromosome="chr1")
#' request_id = deepblue_get_regions(query_id =data_id,
#'                                  output_format = "CHROMOSOME,START,END")
#'request_data = deepblue_download_request_data(request_id)
#'deepblue_liftover(request_data, source = "hg38", target = "hg19")

deepblue_liftover <- function(regions,
                              source = "hg19",
                              target = "hg38",
                              collapse = TRUE){
    supported_assemblies <- c("hg19", "hg38", "mm9", "mm10")

    if(is.null(source)) source <- GenomeInfoDb::genome(regions)
    if(is.na(source)) stop("You need to specify the source genome assembly")
    if(source == "hs37d5") source <- "hg19"
    if(source == "GRChm38") source <- "mm10"

    if(class(regions) != "GRanges")
        stop("can only liftover GenomicRanges objects. These can be obtained via deepblue_get_regions")

    if(!(source %in% supported_assemblies))
        stop("source genome assembly version not supported")
    if(!(target %in% supported_assemblies))
        stop("target genome assembly version not supported")
    if(source == target) stop("source and target genome assembly version are identical")


    if(source == "hg19" && target == "hg38"){
        url <- "ftp://hgdownload.cse.ucsc.edu/goldenPath/hg38/liftOver/hg38ToHg19.over.chain.gz"
        file_name <- "hg38ToHg19.over.chain"
    }
    else if(source == "hg38" && target == "hg19"){
        url <- "ftp://hgdownload.cse.ucsc.edu/goldenPath/hg19/liftOver/hg19ToHg38.over.chain.gz"
        file_name <- "hg19ToHg38.over.chain"
    }
    else if(source == "mm9" && target == "mm10"){
        url <- "http://hgdownload.cse.ucsc.edu/goldenpath/mm10/liftOver/mm10ToMm9.over.chain.gz"
        file_name <- "mm10ToMm9.over.chain"
    }
    else if(source == "mm10" && target == "mm9"){
        url <- "http://hgdownload.cse.ucsc.edu/goldenpath/mm9/liftOver/mm9ToMm10.over.chain.gz"
        file_name <- "mm10ToMm9.over.chain"
    }

    zip_download <- paste(tempdir(), "/", file_name, ".gz", sep = "")
    chain_download <- paste(tempdir(), file_name, sep = "/")

    if(!file.exists(zip_download)){
        message("Downloading chain file")
        download.file(url, zip_download, mode="wb")
    }
    if(!file.exists(chain_download)){
        message(paste("Decompressing chain file to", chain_download))
        R.utils::gunzip(zip_download, chain_download, remove = FALSE, skip = TRUE)
    }

    message(paste("Loading chain file", chain_download))

    chain <- import.chain(chain_download)

    message(paste("Performing coordinate liftover from", source, "to", target))
    liftover_result <- liftOver(regions, chain)
    GenomeInfoDb::genome(liftover_result) <- target

    if(collapse) return(unlist(liftover_result))
    else return(liftover_result)
}
