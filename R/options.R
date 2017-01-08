#' @title options
#' @description options manager from the settings package
#' @param ... list of new options
#' @param .__defaults disallowed option
#' @param .__reset disallowed option
#' @return default options
#' @importFrom settings options_manager
#' @export
#'
deepblue_options <- settings::options_manager(
    url="http://deepblue.mpi-inf.mpg.de/xmlrpc",
    user_key = "anonymous_key",
    do_not_cache = FALSE,
    force_download = FALSE,
    debug = FALSE)

#' Reset DeepBlueR options
#'
#' @param new_options list of new options that should be used.
#' default options if NULL
#'
#' @return new (default) options
#' @importFrom settings reset
#' @export
#'
#' @examples deepblue_reset_options()
deepblue_reset_options <- function(new_options = NULL){
    if(!is.null(new_options)) settings::reset(deepblue_options)
    else settings::reset(deepblue_options)
    deepblue_options(new_options)
}
