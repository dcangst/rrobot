#' Analyze colonies from Pickolo .csv
#' 
#' Function to map colony growth to wells on 96well/384well plate
#' Defaults should work for 384 Well plate
#'
#' @param csv_path path to csv file from Pickolo
#' @param n_row number of rows spotted
#' @param row_start_coord at what coordinate on the picture do the rows start
#' @param row_d distance of rows
#' @param n_col number of cols spotted
#' @param col_start_coord at what coordinate on the picture do the cols start
#' @param col_d distance of cols
#' @section Output:
#'    \code{list} with \code{data.frames} data and pars
#' @family pickolo methods
#' @export
readColonies <- function(csv_path,
  n_row = 16,
  row_start_coord = 185,
  row_d = 56.5,
  n_col = 24,
  col_start_coord = 155,
  col_d = 56.5){

  pickoloData <- read_csv(csv_path,
    col_types = cols(
      x = col_integer(),
      y = col_integer(),
      size = col_integer(),
      circularity = col_double(),
      `min-distance` = col_integer(),
      red = col_integer(),
      green = col_integer(),
      blue = col_integer(),
      hue = col_integer(),
      saturation = col_integer(),
      value = col_integer(),
      selected = col_integer()
    ))
  names(pickoloData)[5] <- "min_distance"
  pickoloData$selected <- pickoloData$selected != 0

  row_wells <- (row_start_coord + 0:n_row * row_d) - row_d / 2
  row_breaks <- c(0, row_wells, 1200)
  row_lab <- c(
    paste0("-", LETTERS[1]),
    LETTERS[1:n_row],
    paste0(LETTERS[n_row], "+"))
  col_wells <- (col_start_coord + 0:n_col * col_d) - col_d / 2
  col_breaks <- c(0, col_wells, 1600)
  col_lab <- as.character(c(0:(n_col + 1)))

  pickoloData$row <- as.character(
    cut(pickoloData$y, breaks = row_breaks, labels = row_lab))
  pickoloData$col <- as.integer(as.character(
    cut(pickoloData$x, breaks = col_breaks, labels = col_lab)))

  well_par <- lst(
    row_wells = row_wells,
    row_breaks = row_breaks,
    row_lab = row_lab,
    col_wells = col_wells, col_breaks = col_breaks,
    col_lab = col_lab)

  plate <- tibble(
    well = 1:384,
    row = rep(LETTERS[1:n_row], each = n_col),
    col = rep(1:n_col, n_row))

  growth <- merge(plate,
    pickoloData[pickoloData$selected, 12:14], all.x = TRUE)
  growth <- growth[order(growth$well), ]
  growth$auto <- !is.na(growth$selected)
  growth$manual <- NA

  growth <- unique(growth)
  if (dim(growth)[1] != n_row * n_col) {
    stop("Data set not complete")
  }

  output <- list(
    growth = growth[, c(1:3, 5:6)],
    data = pickoloData,
    pars = well_par)

  return(output)
}
#' Plot Colonies from Pickolo
#' 
#' Plot identified colonies over image from pickolo
#'
#' @param pickoloData List, output of \code{\link{readColonies}}
#' @param img_path path to the image of the plate
#' @section Output:
#'    a \code{ggplot} plot
#' @family pickolo methods
#' @export
plotColonies <- function(
  pickoloData,
  img_path,
  plotLabels = FALSE,
  colChange = NULL,
  line_size = 0.1,
  title_size = 4, label_size = 0.8,
  axis_text_size = 5,
  pars = pickoloData$pars){

  growth <- pickoloData$growth

  img <- readJPEG(img_path)
  filename <- tail(str_split(img_path, "/")[[1]], 1)

  growth$row_int <-
    match(as.character(growth$row), LETTERS)

  select <- growth$auto
  select[!is.na(growth$manual)] <- na.omit(growth$manual)

  platePlot <- ggplot(pickoloData$data,
    aes(x, y, color = as.character(selected))) +
    annotation_custom(
      rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
      0, dim(img)[2], 0, -dim(img)[1]) +
    scale_y_reverse(name = element_blank(),
      limits = c(dim(img)[1], 0),
      breaks = pars$row_breaks,
      labels = c(pars$row_lab, "")) +
    scale_x_continuous(name = element_blank(),
      limits = c(0, dim(img)[2]),
      breaks = pars$col_breaks,
      labels = c(pars$col_lab, "")) +
    geom_hline(yintercept = pars$row_wells,
      alpha = 0.5, size = line_size) +
    geom_vline(xintercept = pars$col_wells,
      alpha = 0.5, size = line_size) +
    theme_bw() +
    theme(plot.background = element_blank(),
      legend.position = "none",
      axis.text.y = element_text(vjust = 1, size = axis_text_size),
      axis.text.x = element_text(hjust = 0, size = axis_text_size)) +
    geom_text(data = NULL, x = 800, y = 0, label = filename,
      color = "black", size = title_size) +
    # geom_text(data = NULL, x = 800, y = 50, label = str_c(dim(img), collapse = "|"),
    #   color = "black", size = title_size) +
    scale_colour_manual(values = c(
      "FALSE" = rgb(1, 0, 0, 1),
      "TRUE" = rgb(0, 0, 0, 1),
      "none" = rgb(0, 0, 0, 0),
      "-" = rgb(1, 0, 0, 1),
      "+" = rgb(0, 1, 0, 1)))

  if (sum(select) != 0){
    df <- data.frame(
      x = pars$col_wells[as.numeric(growth[select, ]$col)],
      y = pars$row_wells[as.numeric(growth[select, ]$row_int)],
      xWell = pars$col_wells[2] - pars$col_wells[1],
      yWell = pars$row_wells[2] - pars$row_wells[1])

    platePlot <- platePlot +
      geom_rect(data = df,
        mapping = aes(
          xmin = x + xWell / 10,
          xmax = x + xWell - xWell / 10,
          ymin = y + yWell / 10,
          ymax = y + yWell - xWell / 10,
          color = selected),
        size = line_size * 3, color = "black", alpha = 0)
  }

  if (plotLabels){
    platePlot <- platePlot +
      geom_text(
        label = paste0(pickoloData$data$row, pickoloData$data$col),
        size = label_size)
  }

  if (!is.null(colChange)){
    if (dim(colChange)[1] > 0){
    colChange$row_int <- match(as.character(colChange$row), LETTERS)
    colChange_df <- data.frame(
      x = pars$col_wells[as.numeric(colChange$col)],
      y = pars$row_wells[as.numeric(colChange$row_int)],
      xWell = pars$col_wells[2] - pars$col_wells[1],
      yWell = pars$row_wells[2] - pars$row_wells[1],
      change = colChange$change)

    platePlot <- platePlot +
    geom_rect(data = colChange_df,
      mapping = aes(
        xmin = x + xWell / 10,
        xmax = x + xWell - xWell / 10,
        ymin = y + yWell / 10, ymax = y + yWell - xWell / 10, color = change),
      size = line_size * 3, alpha = 0)
    }
  }
  return(platePlot)
}
