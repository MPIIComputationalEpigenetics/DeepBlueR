#' @export
#'
#' @title extract_ids
#' @description A utility command that returns a list of IDs extracted
#' from a data frame of ID and names.
#' @family Utilities for connecting operations
#'
#' @param df - A array of IDs and names
#'
#' @return ids - A vector containing the extracted IDs)
#'
#' @examples
#' deepblue_extract_ids(
#'     df = data.frame(id = c("a124", "a1235"),
#'     name = c("Annotation 1", "Annotation 2")))
#'
deepblue_extract_ids <- function(df = NULL) {

    if(is.null(df)){
        warning("you need to supply a data frame")
        return(NULL)
    }
    else if(!is.data.frame(df)){
        warning("argument not a data frame")
        return(NULL)
    }

    return(as.character(df$id))
}



#' @export
#'
#' @title extract_names
#' @description A utility command that returns a list of names extracted from a list of ID and names.
#' @family Utilities for connecting operations
#'
#' @param df - A array of IDs and Names
#'
#' @return names - A vector containing the extracted names
#' @examples
#' deepblue_extract_ids(
#'     df = data.frame(id = c("a124", "a1235"),
#'     name = c("Annotation 1", "Annotation 2")))
#'
deepblue_extract_names <- function(df = NULL) {

    if(is.null(df)){
        warning("you need to supply a data frame")
        return(NULL)
    }
    else if(!is.data.frame(df)){
        warning("argument not a data frame")
        return(NULL)
    }

    return(as.character(df$name))
}

#' @export
#'
#' @importFrom diffr diffr
#' @importFrom stringr str_replace_all
#' @importFrom rjson toJSON
#' @title diff
#' @description A utility command that creates a diff view of info
#' for two DeepBlue ids
#' @family Utilities for information processing
#'
#' @param id1 - A DeepBlue id
#' @param id2 - Another DeepBlue id
#' @param user_key - A string (users token key)
#'
#' @return None
#' @examples
#' deepblue_diff(
#'     id1 = "e16918",
#'     id2 = "e16919")
#'
deepblue_diff <- function(id1, id2, user_key = deepblue_options("user_key")){
    file1 = tempfile()
    file1_info <- deepblue_info(id1, user_key = user_key)
    writeLines(str_replace_all(rjson::toJSON(file1_info), ",", ",\n"), con = file1)
    file2 = tempfile()
    file2_info <- deepblue_info(id2, user_key = user_key)
    writeLines(str_replace_all(rjson::toJSON(file2_info), ",", ",\n"), con = file2)
    diffr(file1, file2, before = id1, after = id2)
}

#' @export
#'
#' @title select column
#' @description A utility command that creates a list of experiments
#' in which a specific column is selected. Such a list is needed as input
#' for deepblue_score_matrix.
#' @family Utilities for information processing
#' @seealso \code{\link{deepblue_score_matrix}}
#' @seealso \code{\link{deepblue_list_experiments}}
#'
#' @param experiments - A data frame with experiments obtained from
#' deepblue_list_experiments
#' @param column - The name of the column that is extracted from each experiment
#' file
#' @param user_key - A string (users token key)
#'
#' @return A list of experiments with the selected column
#' @examples
#' blueprint_DNA_meth <- deepblue_list_experiments(
#' genome = "GRCh38",
#' epigenetic_mark = "DNA Methylation",
#' technique = "Bisulfite-Seq",
#' project = "BLUEPRINT EPIGENOME")
#'
#' blueprint_DNA_meth <- blueprint_DNA_meth[grep("bs_call",
#'  deepblue_extract_names(blueprint_DNA_meth)),]
#'
#' exp_columns <- deepblue_select_column(blueprint_DNA_meth, "VALUE")
deepblue_select_column <- function(experiments, column, user_key = deepblue_options("user_key")){
    experiments_columns <- list()

    for(experiment in deepblue_extract_names(experiments)){
        experiments_columns[[experiment]] <- column
    }

    return(experiments_columns)
}
