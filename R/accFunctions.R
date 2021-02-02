#' Convert time from Reader XML
#'
#' converts the time from the reader xml to a R format
#' @param string string of time in format provided by tecan infinity M200
#' or list_lengths thereof
#' @section Output:
#'   \code{POSIX.ct} timestamp or a vector thereof
#' @family internal
.convertXMLtime <- function(string){
  options(digits.secs = 6)
  raw_datetime <- str_replace(string, "Z", "")
  raw_datetime <- str_split(string, "T")
  timestamp <- strptime(lapply(
    raw_datetime, str_c, collapse = " "),
    format = "%Y-%m-%d %H:%M:%OS")
  return(timestamp)
}

#' binary to dec
#'
#' takes a vector representing a binary number and gives the decimal value
#' @param x a vector consisting of 0 and 1 representing a binary number
#' @family internal
.binToDec <- function(x){
  if (length(x[x > 1]) > 0){
    stop("not a binary number")
  }
  power <- 2 ^ (rev(seq_along(x)) - 1)
  return(sum(x * power))
}

#' dec to binary
#'
#' takes a decimal number and returns a vector representing a binary number.
#' used for adv. well selection, hence length=7
#'
#' @param x a decimal number
#' @param l length of the vector
#' @family internal
.decToBin <- function(x, l = 7){
  as.numeric(intToBits(x)[1:l])
}

#' ANSII from Vector
#'
#' generates ANSII letter from a vector with numbers 1 to 7 indicating which well in this group of 7 is used.
#' @param x vector containing a selection of the numbers 1-7
#' @param length length of binary number, should always be 7.
#' @family accessory functions
#' @export
ANSIIfromVector  <- function(x, length = 7){
  bin <- rep(0, length)
  for (i in 1:length(x)){
    bin[x[i]] <- 1
  }
  dec <- .binToDec(rev(bin))
  ansi_dec <- dec + 48
  ansi_char <- ansi_table[ansi_table$ansi_dec == ansi_dec, 2]
  return(ansi_char)
}


#' ANSII String from Wells
#'
#' calculates string for well selection in advanced worklist commands
#' @param wellSelection Well position(s) in the labware. The position starts with 1 and
#'      increases from rear to front and left to right.
#' @param ncol Number of columns in labware, defaults to 24 (a 384 well plate)
#' @param nrow Number of rows in labware, defaults to 16 (a 384 well plate)
#' @family accessory functions
#' @export
ANSIIFromWells <- function(wellSelection, ncol = 24, nrow = 16){
  wellLayout <- str_c(
    format(as.hexmode(c(ncol, nrow)),
      width = 2, upper.case = TRUE),
      collapse = "")
  wellPlate <- ncol * nrow
  nGroup7 <- ceiling(wellPlate / 7)

  plate_wells  <- matrix(1:wellPlate, ncol = ncol, byrow = TRUE)
  plate_group7 <- matrix(rep(1:nGroup7, each = 7)[1:wellPlate],
    ncol = ncol, nrow = nrow)

  df <- tibble(well = as.vector(plate_wells),
    group7 = as.vector(plate_group7),
    group7pos = rep(1:7, nGroup7)[1:wellPlate])

  well_df <- df[match(wellSelection, df$well), ]
  well_df[is.na(well_df)] <- 0

  bitmap <- rep("0", nGroup7)
  used <- well_df %>%
    group_by(group7) %>%
    summarize(ansi = ANSIIfromVector(group7pos))
  for (i in 1:nGroup7){
    bitmap[used$group7[i]] <- used$ansi[i]
  }
  bitmap_string <- str_c(c("\"", wellLayout, bitmap, "\""),
    collapse = "")
  return(bitmap_string)
}

#' Wells from ANSII string
#'
#' calculates the selected wells from the ANSII string (The position starts with
#'   1 and increases from rear to front and left to right.)
#' @param wellSelection well selection string
#' @family accessory functions
#' @export
wellsFromANSII <- function(wellSelection){
  ncol <- as.integer(as.hexmode(str_sub(wellSelection, 2, 3)))
  nrow <- as.integer(as.hexmode(str_sub(wellSelection, 4, 5)))
  groups <- unlist(str_split(str_sub(wellSelection,
    start = 6, end = -2), ""))
  exChars <- c(".", "\\", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?")
  whichSpecial <- groups %in% exChars
  groups[whichSpecial] <- str_c("\\", groups[whichSpecial])
  groups_int <- ansi_table[sapply(groups, grep, ansi_table$hex), 1] - 48
  wells_matrix <- matrix(
    unlist(
      lapply(groups_int, .decToBin)
    )[1:(nrow * ncol)],
    nrow, ncol)
  if (sum(wells_matrix) == 0){
    wells <- 0
  } else {
    wells <- matrix(1:(nrow * ncol), nrow, byrow = TRUE)[wells_matrix == 1]
  }
  return(wells)
}

#' Volume string generation
#'
#' generates the string of volumes needed for the advanced worklist
#' @param volumes \code{vector("numeric",12L)}
#' @family internal
.advVolumeString <- function(volumes, tipMask){
  vol_warning <- ""
  if (length(volumes) == 1){
    volumes12 <- c(tipMask * volumes, rep(0, 4))
    vol_warning <- "Only one Volume, guessed from tipMask & Volume. Check!"
  } else if (length(volumes) == 8){
    # check for mismatch between selected tips and supplied volumes
    if (sum(tipMask * volumes - volumes) < 0){
      stop("tipMask / volumes mismatch!")
    }
    volumes12 <- c(volumes, rep(0, 4))
  } else {
    stop("volumes vector not of length 8 (or 1)")
  }

  volumes_str <- vector("character", 12L)
  volumes_str[which(volumes12 == 0)] <- "0"
  volumes_str[which(!volumes12 == 0)] <- paste0("\"",
    volumes12[which(!volumes12 == 0)], "\"")
  volumes_str <- str_c(volumes_str, collapse = ",")
  return(list(volumes_str, vol_warning))
}

#' get grid/site from racklabel
#'
#' internal function to get grid/site from racklabel.
#' Used in advanced worklist functions
#' @param RackLabel name of the Rack
#' @family internal
getGridSite <- function(RackLabel){
  RackLabel_wt <- gwl$worktable[gwl$worktable$RackLabel == RackLabel, ]
  if (dim(RackLabel_wt)[1] == 1) {
    grid <- RackLabel_wt$grid
    site <- RackLabel_wt$site
  } else if (dim(RackLabel_wt)[1] < 1){
    print(gwl$worktable$worktable)
    stop(paste0("Racklabel: "), RackLabel,
      " not defined. Define Racklabels using addToWorktable() or manually @ gwl$worktable")
  } else if (dim(RackLabel_wt)[1] > 1){
    print(gwl$worktable$worktable)
    stop("Duplicate RackLabels? check worktable")
  }
  return(list(site = site, grid = grid))
}

#' add/subtract Volumes on worktable
#'
#' internal function to add/subtract volumes from labware
#' @param volumes Volumes
#' @param RackLabel name of the Racklabel
#' @param direction 1 to add volume, -1 to subtract volume
#' @family internal
.updateVolume <- function(volumes, RackLabel, direction = 1){
  gwl$worktable[gwl$worktable$RackLabel == RackLabel, ]$Volume <<-
    gwl$worktable[gwl$worktable$RackLabel == RackLabel, ]$Volume +
    direction * sum(volumes)
}

#' List to data.frame
#'
#' takes a list of vectors (possibly named) and makes a data.frame for easier
#' display. Trys to make guesses for column names. Used in \code{\link{read_Infinite_XML}}
#' @param list a list of vectors
#' @family internal
.listToDf <- function(list){
  list_lengths <- sapply(list, length)
  df <- data.frame(matrix(nrow = length(list), ncol = max(list_lengths)))
  names(df) <- names(list[[which(list_lengths == max(list_lengths))[1]]])
  for (i in seq_along(list)){
    length(list[[i]]) <- max(list_lengths)
    df[i, ] <- list[[i]]
  }
  return(df)
}

#' rc_to_well
#'
#' well number from row and column
#' @param row a vector of integers or characters
#' @param col a vector of integers
#' @param n_row number of rows in labware
#' @param n_col number of columns in labware
#' @family general
#' @export
rc_to_well <- function(row, col, n_row, n_col){
  if (is.character(row[1])){
     row <- match(row, LETTERS[1:n_row])
  }
  well <- col + (row - 1) * n_col
  return(well)
}

#' generateMasks
#'
#' tip & volumen masks from wells and volumes
#' @param tip a vector of tips
#' @param vol a vector of volumes for each of the tips
#' @family general
#' @export
generateMasks <- function(tip, vol) {
    tip_mask <- integer(8)
    vol_mask <- integer(8)

    tip_mask[tip] <- 1
    vol_mask[tip] <- vol

    masks <- lst(tip_mask, vol_mask)
    return(masks)
}

#' Shortest path for pin deactivation
#'
#' using the TSP package
#' @param pins vector of the pins to be ordered
#' @param method method to use, see \code{\link{TSP::solve_TSP}}
#' @family general
#' @export
shortestPath <- function(pins, method = "two_opt") {
  well_row <- (pins - 1) %/% 24
  well_col <- pins - well_row * 24 - 1

  data <- dist(t(matrix(c(well_row, well_col), nrow = 2, byrow = T)),
    diag = T, upper = T)

  order <- TSP::solve_TSP(TSP::ATSP(data), method = method)
  return(pins[order])
}

#' Parse Arguments for log
#'
#' takes arguments to R script, parse them and pretty prints their values for log
#'
#' @param arg_names names of arguments
#' @param arg_types One of NULL or a cols specification. See vignette("column-types", package = "readr") for more details.
#' @family accessory
#' @export
parse_args <- function(arg_names, arg_types = NULL){
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    args <- as.character(rep(1, str_count(arg_types)))
    warning("no cml arguments! inventing some!")
  }
  length(args) <- length(arg_names)
  names(args) <- arg_names
  if (is.character(arg_types)){
    arg_types <- readr:::as.col_spec(arg_types)
  }
  args_df <- data.frame(t(args), stringsAsFactors = FALSE)
  args_df <- tibble::as_tibble(args_df)
  args_df <- readr::type_convert(args_df, col_types = arg_types)

  cat("arguments:\n")
  print.data.frame(args_df)
  cat("\n")
  return(args_df)
}

#' binary string to integer
#'
#' no description
#'
#' @param x character representation of a binary number, eg. "1101" (=13)
#' @section Output:
#'    an integer
#' @family hospital_experiment
#' @export
binToInt <- function(x) {
  out <- NULL
  for (i in x) {
    out <- c(
      out,
      sum(2 ^ (which(rev(unlist(strsplit(as.character(i), "")) == 1)) - 1)))
  }
  out
}

#' integer to binary string
#'
#' no description
#'
#' @param x integer
#' @param l length of output (will be padded with 0)
#' @section Output:
#'    a string
#' @family hospital_experiment
#' @export
intToBin <- function(x, l = 4) {
  out <- NULL
  for (i in x) {
    out <- c(out, str_c(as.numeric(rev(intToBits(i)[1:l])), collapse = ""))
  }
  out
}
