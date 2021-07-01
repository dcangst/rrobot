#' read XML from Tecan Infinity Reader
#'
#' reads iControl XML file and returns a list
#'
#' @param file path to XML file
#' @section Output:
#'    \code{list} with data in longform format and parameters
#' @export
readInfiniteXML <- function(file){
  data_xml <- withCallingHandlers(xml2::read_xml(file),
    warning = function(w){
      if (any(grepl("is not absolute", w))){
        invokeRestart( "muffleWarning" )
      }
    })

  # plate name
  plate_name <- xml_text(xml_find_first(data_xml, "//Plate"), trim = TRUE)

  # how many Measurements were made on the plate? What are their labels
  measurementNames <- xml_attr(xml_find_all(data_xml, "//Section"), "Name")
  nMeasurement <- length(measurementNames)

  #parameter
  pars_node <- xml_find_all(data_xml, "//Section/Parameters")
  parameter <- lapply(pars_node, function(x){
    .listToDf(xml_attrs(xml_children(x)))
  })
  names(parameter) <- measurementNames

  #data
  data <- xml_find_all(data_xml, "//Section/Data")
  data_nodes <- lapply(data, xml_children)
  data_attrs1 <- lapply(lapply(data_nodes, xml_attrs), .listToDf)
  data_values <- lapply(
    lapply(
      lapply(data_nodes, xml_children), xml_text), .listToDf)
  names(data_values) <- c(measurementNames)
  data_values_df <- data.frame(data_values)
  data_values_df <- sapply(data_values_df, as.numeric)
  n_values <- dim(data_values_df)[1]

  #times
  time <- data.frame(
    t_start = .convertXMLtime(xml_attr(xml_find_all(data_xml, "//Section"),
      "Time_Start")),
    t_end = .convertXMLtime(xml_attr(xml_find_all(data_xml, "//Section"),
      "Time_End")),
    name = str_c("time_", measurementNames))
  times <- data.frame(plyr::dlply(time, ("name"),
    function(x)(seq(x[, 1], x[, 2], length.out = n_values))))

  #data.frame
  rows <- str_sub(data_attrs1[[1]]$Pos, 1, 1)
  rows_n <- as.integer(sapply(rows, function(x){
    which(x == LETTERS)
  }))
  cols <- as.integer(str_sub(data_attrs1[[1]]$Pos, 2))
  wells <- (rows_n - 1) * max(cols) + cols

  data <- data.frame(
    plate = plate_name,
    pos = data_attrs1[[1]]$Pos,
    row = rows,
    col = cols,
    well = wells,
    data_values_df,
    times,
    stringsAsFactors = FALSE)
  data <- data[order(data$well), ]

  return(list(data = as_tibble(data), parameter = parameter))
}
