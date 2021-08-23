#'Initialize Worklist & set defaults
#'
#'Initialization. Generates object \code{gwl} in \code{.GlobalEnv}
#'
#'@param filename string, the name of the generated worklist and worktable
#'  (+ "_worktable.txt")
#'@param RackLabel Max. 32 characters User-defined label (name) which is
#'  assigned to the labware.
#'@param RackID Max. 32 characters, Labware barcode.
#'@param RackType Max. 32 characters, Labware type (configuration name),
#'  e.g. “384 Well, landscape”.
#'@param Position 1 .. number of wells, Well position in the labware. The
#'  position starts with 1 and increases from rear to front and left to right.
#'@param TubeID Max. 32 characters, Tube barcode.
#'@param Volume 0 .. +7158278, Pipetting volume in µl.
#'@param LiquidClass Max. 32 characters, This optional parameter overwrites the
#'  liquid class specified in the Worklist command.
#'@param TipMask 1 .. 128, This optional parameter specifies the tip you want
#'  to use. The tip number is bit-coded, i.e. Tip 1 = 1, Tip 2 = 2,
#'  Tip 3 = 4; Tip 4 = 8 and so on. Please note that you can only use a tip
#'  which has been enabled with Tip Selection in the Worklist command.
#'  If you do not specify TipMask, Freedom EVOware uses one of the tips
#'  specified with Tip Selection in the Worklist command.
#'@param ForcedRackType Max. 32 characters, This optional parameter is the
#'  configuration name of the labware. If specified, it is used instead
#'  of the labware type which is found on the Freedom EVOware worktable.
#'@param MinDetectedVolume 0 .. +7158278 Liquid volume in µl. If this optional
#'  parameter is specified and Liquid Level Detection is enabled in the
#'  selected liquid class, it is used by the LiHa to determine the minimum
#'  liquid height which must be available in the well. Otherwise a liquid
#'  detection error (not enough liquid) will be triggered. MinDetectedVolume
#'  specifies the usable volume of liquid. If the aspiration position in the
#'  liquid class specifies tip immersion (tip submerging), the actual liquid
#'  height in the well must be higher to allow immersion (i.e. the volume of
#'  liquid in the well must be more than MinDetectedVolume).
#'@param Wash Default wash step to use
#'@param WTtemplate character, data.frame or NULL. character: worktable
#'  template to load ("basic" only atm), data.frame: a worktable layout as a
#'  data.frame, NULL: an empty worktable.
#'@section Output:
#'   invisible(gwl), also saves object \code{gwl} to \code{.GlobalEnv}
#'@family general methods
#'@export
init <- function(
  filename = paste0(format(Sys.time(), "%y%m%d"), "_gwl"),
  RackLabel = "trough200_1_frt",
  RackID = "",
  RackType = "",
  Position = 0,
  TubeID = "",
  Volume = 0,
  LiquidClass = "",
  TipMask = 0,
  ForcedRackType = "",
  MinDetectedVolume = "",
  Wash = "W",
  WTtemplate = "basic") {

  gwl_defaults <- data.frame(
    filename           = filename,
    RackLabel          = RackLabel,
    RackID             = RackID,
    RackType           = RackType,
    Position           = Position,
    TubeID             = TubeID,
    Volume             = Volume,
    LiquidClass        = LiquidClass,
    TipType            = "",
    TipMask            = TipMask,
    ForcedRackType     = TipMask,
    MinDetectedVolume  = MinDetectedVolume,
    Wash               = Wash,
    stringsAsFactors = FALSE)

    gwl_worklist  <- data.frame(
        line = 1,
        command = paste0("C;INIT - gwl created with rrobot v.",
          packageVersion("rrobot")),
        mode = "basic",
        warning = "",
        stringsAsFactors = FALSE
        )
    gwl_worktable <- data.frame(
        RackLabel = "INIT",
        RackType = "",
        grid = 99,
        site = 99,
        Volume = 0,
        LiquidClass = "none",
        stringsAsFactors = FALSE
        )
    if (is.data.frame(WTtemplate)){
      gwl_worktable  <- rbind(gwl_worktable, WTtemplate)
    } else if (is.character(WTtemplate)){
      eval(parse(text = paste0("gwl_worktable <- ", WTtemplate)))
    }
    gwl <- list(
        worklist = gwl_worklist,
        worktable = gwl_worktable,
        defaults = gwl_defaults
        )
    gwl <<- gwl
    invisible(gwl)
}

#' Write worklist to file
#'
#' writes worklist to file, filename defined in \code{init}.
#' @param gwl object of class gwl
#' @param lineNumbers logical, should linenumbers be prepended to existing comments? Defaults to \code{FALSE}
#' @param print logical, should the worklist be printed in R? Defaults to \code{FALSE}
#' @param saveWT logical, should the worktable be saved as a \code{txt} file?
#' @family general methods
#' @export
write.gwl <- function(gwl,
  print = FALSE,
  lineNumbers = TRUE,
  saveWT = FALSE,
  quietly = FALSE){
  #add linenumbers
  if (lineNumbers){
      selectComments <- !is.na(str_match(gwl$worklist$command, "Comment"))
      lineN <- gwl$worklist$line[selectComments]
      maxLineN <- max(gwl$worklist$line)
      gwl$worklist$command[selectComments] <- str_replace(
        gwl$worklist$command[selectComments],
        fixed("B;Comment(\""),
        str_c("B;Comment(\"", lineN, "/", maxLineN, " | "))
  }
  if (saveWT){
      write_tsv(gwl$worktable,
        str_c(gwl$defaults$filename, "_worktable.txt"))
      cat(paste0("worktable saved as ",
        paste0(gwl$defaults$filename,
        "_worktable.txt")), "\n")
  }

  write(gwl$worklist$command,
    str_c(gwl$defaults$filename, ".gwl"),
    sep = "")

  if (print){
    print(gwl)
  }
  if (!quietly){
    cat("worklist saved as", str_c(gwl$defaults$filename, ".gwl"), "\n")
    cat("Check worktable layout!", "\n")
  }
}

#' add to worktable
#'
#' adds the labware to a worktable file to keep track of used labware
#' @param RackLabel Max. 32 characters User-defined label (name) which is
#'      assigned to the labware.
#' @param grid integer 1 - 67 labware location - carrier grid position
#' @param site integer 0 - 127 labware location - (site on carrier - 1)
#' @param RackType Max. 32 characters, Labware type (configuration name),
#'      e.g. “384 Well, landscape”. Only used for basic worklists.
#' @param Volume needed in Rack: this will be filled up by the functions
#' @param LiquidClass class in the Rack.
#' @return a string with warnings generated by the function
#' @family general methods
#' @export
addToWorktable <- function(RackLabel = "",
  grid = 0,
  site = 0,
  RackType = "",
  Volume = 0,
  LiquidClass = ""){

  worktable <- gwl$worktable
  labware_add <- data.frame(
    RackLabel = RackLabel,
    grid = grid,
    site = site,
    RackType = RackType,
    Volume = Volume,
    LiquidClass = LiquidClass)

  for (i in 1:dim(labware_add)[1]){
    RackLabel_defined <- dim(
      subset(worktable, RackLabel == labware_add$RackLabel[i]))[1]

    if (RackLabel_defined == 1){
      rlang::abort("RackLabel already defined")
    } else {
      gridSiteCheck <-
        worktable[worktable$grid == grid & worktable$site == site, ]
      if (dim(gridSiteCheck)[1] == 1){
        rlang::abort(paste0("Grid/Site allready occupied by ", gridSiteCheck$RackLabel))
      }
      worktable <- rbind(worktable, labware_add[i, ])
    }
  }

  gwl$worktable <<- worktable
}

#' add to worklist
#'
#' adds a command to the worklist
#' @param command character, or vector of characters. ” must be escaped (\")
#' @param mode one of "basic", "advanced", specifying whether it is a basic or an
#'      advanced worklist command.
#' @param CMadd_warning passed to the worklist, Info only.
#' @family general methods
#' @export
addToWorklist <- function(command, mode, CMadd_warning = ""){
  all_warning <- str_c(CMadd_warning, sep = "|")
  last_line <- tail(gwl$worklist, 1)$line
  command_add <- data.frame(
    line = seq_along(command) + last_line,
    command = command,
    mode = mode,
    warning = all_warning)
  gwl$worklist <<- rbind(gwl$worklist, command_add)
}
