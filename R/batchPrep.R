#' Import and prepare a batch for analysis
#'
#' @param folder A path to a folder containing multiple exports from ABI7500
#' @param progress logical, if TRUE a progressbar is shown works only on windows
#' @param equipment Specify from what equipment the export originates
#' @param ... Argumnts passed to read_[EQUIPMENT]
#'
#' @import "dplyr"
#' @export
batchPrep <- function(folder, ..., progress=TRUE, equipment="ABI"){

  message("This function is no longer maintained, use prepare_batch() instead.")


}


