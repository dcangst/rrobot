#' Read Storex File
#'
#' Reads storex pos list to data.frame
#' @param storex_file path to storex pos list
#' @family general
#' @export
read_storex <- function(storex_file){
  storex_raw <- read.table(file = storex_file, stringsAsFactors = FALSE)
  storex <- as_tibble(
    str_split(storex_raw[, 1], ",|=", simplify = TRUE)) %>%
    mutate(
      cartridge = suppressWarnings(as.integer(V1)),
      position = as.integer(V2),
      barcode = as.character(V3),
      carrier = rep(
        x = V1[which(is.na(cartridge))],
        times = c(
          which(is.na(cartridge))[2] - 1,
          length(cartridge) - which(is.na(cartridge))[2] + 1))) %>%
    filter(!is.na(cartridge)) %>%
    select(carrier, cartridge, position, barcode)
  return(storex)
}
