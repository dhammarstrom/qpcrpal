#' Extract raw data (normalized reporter), sample name and target name from Quantstudio 5 exports.
#'
#' @param filename A path or filename of an excel file containing 'Results' and 'Raw Data' exported from QuantStudio
#' @param sample_separator A character string specifying separator of 'Sample Name' string.
#' @param sample_ID A integer specifying sample sub-info position in 'Sample Name' from export. Default=1
#' @param sample_condition Default=2
#' @param sample_timepoint Default=3
#' @param sample_replicate Default=4
#' @param start_cycle First cycle to extract from export, default=1
#' @param end_cycle Last cycle to extract from export, default=40
#' @param skip How many rows needs to be skipped before column names in the export?
#' @return A data frame containing sample and target information and raw data over 40 cycles.
#' @import "dplyr"
#' @import "readxl"
#'
#'
#'
#' @export
read_quant5<-function(filename,
                   sample_separator=" ",
                   sample_ID=1,
                   sample_condition=2,
                   sample_timepoint=3,
                   sample_replicate=4,
                   start_cycle=1,
                   end_cycle=40,
                   skip=19){

  # reads 'Results' tab from excel export
  capture.output(
    results <- data.frame(read_excel(filename, sheet = "Results", skip=skip)))
  # limit data to rows containing well info
  wellID <- paste(rep(toupper(letters[1:16]), 24), rep(seq(1:24), each=16), sep="")
  results <- results[results[,2] %in% wellID, ]


  # creates a new clean data.frame
  clean.data <- data.frame(Well=rep(NA, length=length(results[,1])),
                           ID=rep(NA, length=length(results[,1])),
                           condition=rep(NA, length=length(results[,1])),
                           timepoint=rep(NA, length=length(results[,1])),
                           replicate=rep(NA, length=length(results[,1])),
                           target=rep(NA, length=length(results[,1])),
                           filename=rep(filename, length=length(results[,1])))

  # save specific data in clean data frame
  clean.data$Well <- results[,2]
  clean.data$ID <- sapply(strsplit(results[,4], sample_separator), "[", sample_ID)
  clean.data$condition <- sapply(strsplit(results[,4], sample_separator), "[", sample_condition)
  clean.data$timepoint <- sapply(strsplit(results[,4], sample_separator), "[", sample_timepoint)
  clean.data$replicate <- sapply(strsplit(results[,4], sample_separator), "[", sample_replicate)
  clean.data$target <- results[,5]

  clean.data$filename <- as.character(clean.data$filename)

  # raw data read
  raw <- data.frame(readxl::read_excel(filename, sheet = "Raw Data", skip=skip))

  # extract only amplification cycles
  cycles <- start_cycle:end_cycle
  raw <- raw[raw[,3] %in% cycles,]

  # calculate normalized reporter
  raw$Rn <- raw[,4]/raw[,7]

  # Make well ID to match results data frame
  raw$Well<-raw[,2]

  # join data frames
  joined <- dplyr::inner_join(clean.data, raw[,c(1,3,9)], by="Well")

  # return data frame
  joined
}
