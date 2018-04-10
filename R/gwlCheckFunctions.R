#' Read in gwl file
#' 
#' read in gwl file
#'
#' @param gwl_path path to gwl file, defaults to \code{file.choose()} to open dialog
#' @param .progress should a progress bar be shown?
#' @section Output:
#'    list of named character vectors
#' @family gwl_check
#' @export
read_gwl <- function(gwl_path=file.choose(), .progress=TRUE){
  cat("reading .gwl...\n");flush.console()
  gwl_file <- scan(gwl_path, what = character(), sep = "\n", flush = TRUE)
  cat("parsing .gwl... \n");flush.console()
  n_commands <- length(gwl_file)
  if (.progress & n_commands > 1){
    pb <- txtProgressBar(style = 3, min = 1, max = n_commands)
  }
  worklist <- list()
  for (i in seq_along(gwl_file)){
    gwl_command <- unlist(str_split(gwl_file[i], ";", n = 2))
    command <- str_extract(gwl_command[2], "\\w+")
    if (gwl_command[1] == "C"){
      worklist[[i]] <- c(
        gwl_command[1], "gwl_comment", gwl_command[2]
      )
      names(worklist[[i]]) <- c("type", "command", "attributes")
    } else if (gwl_command[1] == "B"){
      if (command %in% c("Aspirate", "Dispense")){
        pars <- str_extract(gwl_command[2], "(?<=\\().+?(?=\\))")
        pars_split <- unlist(str_split(pars, ","))

        worklist[[i]] <- c(gwl_command[1], command,
          intToBin(pars_split[1], l = 8),
          str_replace_all(pars_split[2], "\\\"", ""),
          str_c(str_replace_all(pars_split[3:14], "\\\"", "")[1:8],
            collapse = ","),
          pars_split[15:17],
          as.integer(as.hexmode(str_sub(pars_split[18], 2, 3))),
          as.integer(as.hexmode(str_sub(pars_split[18], 4, 5))),
          str_c(wellsFromANSII(pars_split[18]), collapse = ","),
          pars_split[19:20])
        names(worklist[[i]]) <- c("type", "command", "tipMask",
          "LC",
          "volumes",
          "grid", "site", "spacing",
          "ncol",
          "nrow",
          "wellSelection",
          "loop", "arm")
      } else if (command == "MoveLiha"){
        pars <- str_extract(gwl_command[2], "(?<=\\().+?(?=\\))")
        pars_split <- unlist(str_split(pars, ","))

        worklist[[i]] <- c(gwl_command[1], command,
          intToBin(pars_split[1], l = 8),
          pars_split[2:4],
          as.integer(as.hexmode(str_sub(pars_split[5], 2, 3))),
          as.integer(as.hexmode(str_sub(pars_split[5], 4, 5))),
          str_c(wellsFromANSII(pars_split[5]), collapse = ","),
          pars_split[6:11])
        names(worklist[[i]]) <- c("type", "command", "tipMask",
          "grid", "site", "spacing",
          "ncol",
          "nrow",
          "wellSelection",
          "zMove", "zTarget", "offset", "speed", "loop", "arm")
      } else if (command == "Comment"){
        pars <- str_extract(gwl_command[2], "(?<=\\().+?(?=\\))")
        worklist[[i]] <- c(gwl_command[1], "evo_comment",
          str_replace_all(pars, "\\\"", ""))
        names(worklist[[i]]) <- c("type", "command", "attributes")
      } else {
        worklist[[i]] <- c(gwl_command[1], command,
          str_extract(gwl_command[2], "(?<=\\().+?(?=\\))"))
        names(worklist[[i]]) <- c("type", "command", "attributes")
      }
    }
    if (.progress & n_commands > 1){
      setTxtProgressBar(pb, i)
    }
  }
  if (.progress & n_commands > 1){
    close(pb);flush.console()
  }
  return(worklist)
}

#' gwl data_frame part
#' 
#' fhelper function to create a dataframe of a gwl line
#'
#' @family gwl_check
#' @export
gwl_df_part <- function(
  type = as.character(NA),
  command = as.character(NA),
  attributes = as.character(NA),
  tipMask = as.character(NA),
  LC = as.character(NA),
  volumes = as.character(NA),
  grid = NA,
  site = NA,
  spacing = NA,
  ncol = NA,
  nrow = NA,
  wellSelection = as.character(NA),
  zMove = NA,
  zTarget = NA,
  offset = NA,
  speed = NA,
  loop = as.character(NA),
  arm = as.character(NA)){
  
  out <- data_frame(
    type = type,
    command = command,
    attributes = attributes,
    tipMask = tipMask,
    LC = LC,
    volumes = volumes,
    grid = grid,
    site = site,
    spacing = spacing,
    ncol = ncol,
    nrow = nrow,
    wellSelection = wellSelection,
    zMove = zMove,
    zTarget = zTarget,
    offset = offset,
    speed = speed,
    loop = loop,
    arm = arm)

  return(out)
}

#' Read in gwl file to data_frame
#' 
#' read in gwl file, and covert to data_frame
#'
#' @param gwl_path path to gwl file, defaults to \code{file.choose()} to open dialog
#' @param .progress should a progress bar be shown?
#' @section Output:
#'    list of named character vectors
#' @family gwl_check
#' @export
read_gwl_df <- function(gwl_path=file.choose(), .progress=TRUE){
  cat("reading .gwl...\n");flush.console()
  gwl_file <- scan(gwl_path, what = character(), sep = "\n", flush = TRUE)
  cat("parsing .gwl... \n");flush.console()
  n_commands <- length(gwl_file)
  if (.progress & n_commands > 1){
    pb <- txtProgressBar(style = 3, min = 1, max = n_commands)
  }
  worklist <- list()
  for (i in seq_along(gwl_file)){
    gwl_command <- unlist(str_split(gwl_file[i], ";", n = 2))
    command <- str_extract(gwl_command[2], "\\w+")
    if (gwl_command[1] == "C"){
      worklist[[i]] <- gwl_df_part(
        type = gwl_command[1],
        command = "gwl_comment",
        attributes = gwl_command[2]
      )
    } else if (gwl_command[1] == "B"){
      if (command %in% c("Aspirate", "Dispense")){
        pars <- str_extract(gwl_command[2], "(?<=\\().+?(?=\\))")
        pars_split <- unlist(str_split(pars, ","))
        worklist[[i]] <- gwl_df_part(
          type = gwl_command[1],
          command = command,
          tipMask = intToBin(pars_split[1], l = 8),
          LC = str_replace_all(pars_split[2], "\\\"", ""),
          volumes = str_c(str_replace_all(pars_split[3:14], "\\\"", "")[1:8],
            collapse = ","),
          grid = pars_split[15],
          site = pars_split[16],
          spacing = pars_split[17],
          ncol = as.integer(as.hexmode(str_sub(pars_split[18], 2, 3))),
          nrow = as.integer(as.hexmode(str_sub(pars_split[18], 4, 5))),
          wellSelection = str_c(wellsFromANSII(pars_split[18]), collapse = ","),
          loop = pars_split[19],
          arm = pars_split[20])
      } else if (command == "MoveLiha"){
        pars <- str_extract(gwl_command[2], "(?<=\\().+?(?=\\))")
        pars_split <- unlist(str_split(pars, ","))
        worklist[[i]] <- gwl_df_part(
          type = gwl_command[1],
          command = command,
          tipMask = intToBin(pars_split[1], l = 8),
          grid = pars_split[2],
          site = pars_split[3],
          spacing = pars_split[4],
          ncol = as.integer(as.hexmode(str_sub(pars_split[5], 2, 3))),
          nrow = as.integer(as.hexmode(str_sub(pars_split[5], 4, 5))),
          wellSelection = str_c(wellsFromANSII(pars_split[5]), collapse = ","),
          zMove = pars_split[6],
          zTarget = pars_split[7],
          offset = pars_split[8],
          speed = pars_split[9],
          loop = pars_split[10],
          arm = pars_split[11])
      } else if (command == "Comment"){
        pars <- str_extract(gwl_command[2], "(?<=\\().+?(?=\\))")
        worklist[[i]] <- gwl_df_part(
          type = gwl_command[1],
          command = "evo_comment",
          attributes = str_replace_all(pars, "\\\"", ""))
      } else {
        worklist[[i]] <- gwl_df_part(
          type = gwl_command[1], 
          command = command,
          attributes = str_extract(gwl_command[2], "(?<=\\().+?(?=\\))"))
      }
    }
    if (.progress & n_commands > 1){
      setTxtProgressBar(pb, i)
    }
  }
  if (.progress & n_commands > 1){
    close(pb);flush.console()
  }
  out <- bind_rows(worklist)
  return(out)
}

#' crate a knitr::kable tables with images from worklist
#' 
#' generate pretty tables
#'
#' @param worklist worklist, read in with read_gwl
#' @param path where should the tables be saved
#' @param .progress should a progress bar be shown?
#' @section Output:
#'  a lsit of pretty knitr::kable tables with images
#'  images are safed in a subfolder worklist_img_temp
#' @family gwl check
#' @export
gwlToTable <- function(worklist, path=".", .progress=TRUE){
  cat("creating temporary directory... \n");flush.console()

  dir.create(paste0(path, "/worklist_temp"),
    recursive = TRUE, showWarnings = FALSE)
  tables <- list()
  n_commands <- length(worklist)
  colors <- list(
    Dispense = c("#f2f2f2", "red"),
    Aspirate = c("#f2f2f2", "blue"),
    MoveLiha = c("#f2f2f2", "black"))
  img_height <- 16 * 6
  img_width <- 24 * 6
  k <- 1

  cat("making pretty tables...\n");flush.console()

  if (.progress & n_commands > 1){
    pb <- txtProgressBar(style = 3, min = 1, max = n_commands)
  }

  for (i in seq_along(worklist)){
    type <- worklist[[i]]["type"]
    command <- worklist[[i]]["command"]
    if (type == "C"){
      table <- knitr::kable(as.data.frame(t(c("line" = i, worklist[[i]]))))
      if ( k == 1){
        tables[[k]] <- table
        tables[[k + 1]] <- ""
        k <- k + 2
      } else {
        same_table <- identical(names(worklist[[i]]), names(worklist[[i - 1]]))
        if (same_table){
          tables[[k - 2]][length(tables[[k - 2]]) + 1]  <- table[3]
        } else {
          tables[[k]] <- table
          tables[[k + 1]] <- ""
          k <- k + 2
        }
      }
    } else if (type == "B"){
      if (command %in% c("Aspirate", "Dispense", "MoveLiha")){
        ncol <- as.numeric(worklist[[i]]["ncol"])
        nrow <- as.numeric(worklist[[i]]["nrow"])
        wells <- as.numeric(unlist(
          str_split(worklist[[i]]["wellSelection"], ",")))

        plate <- data.frame(
          row = rep(1:nrow, ncol),
          col = rep(1:ncol, each = nrow),
          well = as.numeric(matrix(1:(nrow * ncol), nrow, byrow = TRUE)),
          active = as.numeric(
            matrix(1:(nrow * ncol), nrow, byrow = TRUE) %in% wells))

        wellsText <- plate[plate$active == 1, 1:2]
        wellsText$rowAl <- LETTERS[wellsText$row]
        wellsString <- str_c(wellsText$rowAl, wellsText$col, collapse = ",")

        if (all(plate$active == 1)){
          plot_matrix <- t(matrix(plate$active, nrow, ncol, byrow = FALSE))
          plot_matrix <- rbind(rep(0, nrow), plot_matrix, rep(0, nrow))
          ncol <- ncol + 2
        } else {
          plot_matrix <- t(matrix(plate$active, nrow, ncol, byrow = FALSE))
        }

        img_path <- paste0(path, "/worklist_temp/plot", str_pad(i,
          floor(log10(n_commands)) + 1, side = "left", pad = "0"), ".png")
        png(filename = img_path, width = img_width, height = img_height)
        par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
        image(x = 1:ncol, y = 1:nrow, z = plot_matrix,
          ylim = c( (nrow + 0.5), 0.5), xlim = c(0.5, ncol + 0.5),
          col = colors[[command]], axes = FALSE)
        dev.off()

        table <- select(
          as.data.frame(t(c("line" = i, worklist[[i]], "wells" = wellsString))),
          -wellSelection, -ncol, -nrow, -loop, -arm, -spacing)
        img_code <- paste0("<img src=\"", img_path,
          "\" height=\"", img_height, "px\" width=\"", img_width, "px\" />")
        img_code_l <- str_count(img_code)

        if (k == 1){
          tables[[k]] <- knitr::kable(table)
          tables[[k]][1] <- paste0(tables[[k]][1], "image",
            str_dup(" ", img_code_l - 5), "|")
          tables[[k]][2] <- paste0(tables[[k]][2], ":",
            str_dup("-", img_code_l - 1), "|")
          tables[[k]][3] <- paste0(tables[[k]][3], img_code, "|")
          tables[[k + 1]] <- ""
          k <- k + 2
        } else {
          same_table <- identical(names(worklist[[i]]),
            names(worklist[[i - 1]]))
          if (same_table){
            tables[[k - 2]][length(tables[[k - 2]]) + 1] <- paste0(
              knitr::kable(table)[3], img_code, "|")
          } else {
            tables[[k]] <- knitr::kable(table)
            tables[[k]][1] <- paste0(tables[[k]][1], "image",
              str_dup(" ", img_code_l - 5), "|")
            tables[[k]][2] <- paste0(tables[[k]][2], ":",
              str_dup("-", img_code_l - 1), "|")
            tables[[k]][3] <- paste0(tables[[k]][3], img_code, "|")
            tables[[k + 1]] <- ""
            k <- k + 2
          }
        }
      } else if (command %in% c("evo_comment")){
        table <- knitr::kable(as.data.frame(t(c("line" = i, worklist[[i]]))))
        if (k == 1){
          tables[[k]] <- paste0("###", worklist[[i]]["attributes"])
          tables[[k + 1]] <- table
          tables[[k + 2]] <- ""
          k <- k + 3
        } else {
          same_table <- identical(
            names(worklist[[i]]), names(worklist[[i - 1]]))
          if (same_table & worklist[[i - 1]]["type"] != "C"){
            tables[[k - 2]][length(tables[[k - 2]]) + 1]  <- table[3]
          } else {
            tables[[k]] <- paste0("###", worklist[[i]]["attributes"])
            tables[[k + 1]] <- table
            tables[[k + 2]] <- ""
            k <- k + 3
          }
        }
      } else {
        table <- knitr::kable(as.data.frame(t(c("line" = i, worklist[[i]]))))
        if (k == 1){
          tables[[k]] <- table
          tables[[k + 1]] <- ""
          k <- k + 2
        } else {
          same_table <- identical(names(worklist[[i]]),
            names(worklist[[i - 1]]))
          if (same_table){
            tables[[k - 2]][length(tables[[k - 2]]) + 1]  <- table[3]
          } else {
            tables[[k]] <- table
            tables[[k + 1]] <- ""
            k <- k + 2
          }
        }
      }
    }
    if (.progress & n_commands > 1){
      setTxtProgressBar(pb, i)
    }
  }
  if (.progress & n_commands > 1){
    close(pb);flush.console()
  }
  return(tables)
}

#' crate a html report from a worklist file
#' 
#' generate pretty html report
#'
#' @param worklist path to worklist, defaults to \code{file.choose()}
#' @param output where should the output be saved? If \code{NULL} then the html
#'  is saved back to where the worklist lives.
#' @section Output:
#'  generates html with pretty knitr::kable tables with images
#' @family gwl check
#' @export
gwlToHTML <- function(gwl=file.choose(), output = NULL, .progress = TRUE,
  cleanup = TRUE){

  gwl <- normalizePath(gwl)

  if (is.null(output)){
    path <- dirname(gwl)
    base <- basename(gwl)
    output <- paste0(str_sub(gwl, end = -4), "html")
  }
  rmd_path <- paste0(path, "/worklist_temp/", base, ".Rmd")
  worklist <- read_gwl(gwl, .progress)
  tables <- gwlToTable(worklist, path, .progress)

  css <- "<style>
          #TOC {
            position: fixed;
            left: 0;
            top: 65px;
            width: 300px;
            height: 100%;
            overflow:auto;
            }
          body {
            max-width: 1000px;
            margin: auto;
            margin-left:300px;
            line-height: 20px;
          }
          </style>"

  write(str_c(c(paste0(css, "\n#", base, "\n\n"),
    unlist(tables)), collapse = "\n"), rmd_path)

  rmarkdown::render(input = rmd_path, output_file = output,
    output_format = rmarkdown::html_document(
      toc = TRUE, self_contained = TRUE, theme = "united",
      hightlight = "tango"))

  if (cleanup){
    unlink(paste0(path, "/worklist_temp"), recursive = TRUE)
  }
}

#' crate data.frames from worklist
#' 
#' generates data.frame combining commands if they are the same
#'
#' @param worklist worklist, read in with read_gwl
#' @param path where should the tables be saved
#' @param .progress should a progress bar be shown?
#' @section Output:
#'  a lsit of pretty knitr::kable tables with images
#'  images are safed in a subfolder worklist_img_temp
#' @family gwl check
#' @export
gwlToDataFrame <- function(worklist, path= ".", .progress = TRUE){
  tables <- list()
  n_commands <- length(worklist)
  k <- 1
  cat("making data.frames...\n");flush.console()

  if (.progress & n_commands > 1){
    pb <- txtProgressBar(style = 3, min = 1, max = n_commands)
  }

  for (i in seq_along(worklist)){
    type <- worklist[[i]]["type"]
    command <- worklist[[i]]["command"]
    if (type == "C"){
      table <- as.data.frame(t(c("line" = i, worklist[[i]])))
      if (k == 1){
        tables[[k]] <- table
        tables[[k + 1]] <- ""
        k <- k + 2
      } else {
        same_table <- identical(names(worklist[[i]]), names(worklist[[i - 1]]))
        if (same_table){
          tables[[k - 2]][length(tables[[k - 2]]) + 1]  <- table[3]
        } else {
          tables[[k]] <- table
          tables[[k + 1]] <- ""
          k <- k + 2
        }
      }
    } else if (type == "B"){
      if (command %in% c("Aspirate", "Dispense", "MoveLiha")){
        ncol <- as.numeric(worklist[[i]]["ncol"])
        nrow <- as.numeric(worklist[[i]]["nrow"])
        wells <- as.numeric(unlist(
          str_split(worklist[[i]]["wellSelection"], ",")))

        plate <- data.frame(
             row = rep(1:nrow, ncol),
             col = rep(1:ncol, each = nrow),
            well = as.numeric(
              matrix(1:(nrow * ncol), nrow, byrow = TRUE)),
          active = as.numeric(
              matrix(1:(nrow * ncol), nrow, byrow = TRUE) %in% wells))

        wellsText <- plate[plate$active == 1, 1:2]
        wellsText$rowAl <- LETTERS[wellsText$row]
        wellsString <- str_c(wellsText$rowAl, wellsText$col, collapse = ",")

        if (all(plate$active == 1)){
          plot_matrix <- t(matrix(plate$active, nrow, ncol, byrow = FALSE))
          plot_matrix <- rbind(rep(0, nrow), plot_matrix, rep(0, nrow))
          ncol <- ncol + 2
        } else {
          plot_matrix <- t(matrix(plate$active, nrow, ncol, byrow = FALSE))
        }

        table <- select(as.data.frame(t(c(
          "line" = i, worklist[[i]],
          "wells" = wellsString))),
          -wellSelection, -ncol, -nrow, -loop, -arm, -spacing)

        if (k == 1){
          tables[[k]] <- table
          tables[[k + 1]] <- ""
          k <- k + 2
        } else {
          same_table <- identical(names(worklist[[i]]),
            names(worklist[[i - 1]]))
          if (same_table){
            tables[[k - 2]] <- rbind(tables[[k - 2]], table)
          } else {
            tables[[k]] <- table
            tables[[k + 1]] <- ""
            k <- k + 2
          }
        }
      } else if (command %in% c("evo_comment")){
        table <- as.data.frame(t(c("line" = i, worklist[[i]])))
        if (k == 1){
          tables[[k]] <- paste0("###", worklist[[i]]["attributes"])
          tables[[k + 1]] <- table
          tables[[k + 2]] <- ""
          k <- k + 3
        } else {
          same_table <- identical(names(worklist[[i]]),
            names(worklist[[i - 1]]))
          if (same_table & worklist[[i - 1]]["type"] != "C"){
            tables[[k - 2]] <- rbind(tables[[k - 2]], table)
          } else {
            tables[[k]] <- paste0("###", worklist[[i]]["attributes"])
            tables[[k + 1]] <- table
            tables[[k + 2]] <- ""
            k <- k + 3
          }
        }
      } else {
        table <- as.data.frame(t(c("line" = i, worklist[[i]])))
        if (k == 1){
          tables[[k]] <- table
          tables[[k + 1]] <- ""
          k <- k + 2
        } else {
          same_table <- identical(names(worklist[[i]]),
            names(worklist[[i - 1]]))
          if (same_table){
            tables[[k - 2]] <- rbind(tables[[k - 1]], table)
          } else {
            tables[[k]] <- table
            tables[[k + 1]] <- ""
            k <- k + 2
          }
        }
      }
    }
    if (.progress & n_commands > 1){
      setTxtProgressBar(pb, i)
    }
  }
  if (.progress & n_commands > 1){
    close(pb);flush.console()
  }
  return(tables)
}
