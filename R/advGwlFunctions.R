#' Advanced Aspirate Command
#'
#' @param tipMask \code{vector("integer",8L)} containing 0, 1 indicating
#'      selected tips (e.g. \code{c(1,0,1,0,0,0,0,0)} = pins 1 + 3)
#' @param liquidClass string, liquid class name
#' @param volumes \code{vector("integer",8L)} 8 integers which specify the
#'       volume in µl for each tip. Specify 0 for tips which are not used or
#'       not fitted.
#' @param Racklabel Max. 32 characters User-defined label (name) which is
#'      assigned to the labware.
#' @param spacing integer 1 -  .. tip spacing, must be 2 for 384 well plates
#' @param wellSelection vector containing the wells from which the LiHa should
#'      aspirate (should be in one column!)
#' @param ncol integer, number of colums (left to right on worktable) of labware
#' @param nrow integer, number of rows (back to front on worktable) of labware
#' @param noOfLoopOptions integer 0 - 7 number of loop options required. If
#'      non-zero, parameters \code{loopname, action, difference} are included
#'      n times, where n = \code{noOfLoopOptions}
#' @param loopName loop name, \code{vector("character",noOfLoopOptions)}
#' @param action integer 0 - 3 loop action: 0 = vary the column,
#'      1=vary the row 2=vary the well 3=vary the rack,
#'       \code{vector("character",noOfLoopOptions)}
#' @param difference integer number (“vary by”) which is used for the loop
#'       action, \code{vector("character",noOfLoopOptions)}
#' @param arm integer 0 - 1 number of the LiHa performing the action:
#'      0 = LiHa 1=LiHa 2, should always be 0
#' @family advanced worklist commands
#' @export
adv_aspirate <- function(
    tipMask = c(1, 0, 0, 0, 0, 0, 0, 0),
    liquidClass = gwl$defaults$LiquidClass,
    volumes = gwl$defaults$Volume,
    RackLabel,
    spacing=2,
    wellSelection, ncol, nrow,
    noOfLoopOptions = 0, loopName = "", action = 0, difference = 0,
    arm = 0
    ){
    tipMask_bin <- .binToDec(rev(tipMask))
    volumes_str <- .advVolumeString(volumes, tipMask)
    CMadd_warning <- volumes_str[[2]]
    GridSite <- getGridSite(RackLabel)
    wellSelection_string <- ANSIIFromWells(wellSelection, ncol, nrow)

    if (noOfLoopOptions != 0){
        loopString <- paste0(noOfLoopOptions)
        for (i in 1:noOfLoopOptions){
            loopString <- paste0(
              loopString, ",",
              paste0("\"", loopName[i], "\""), ",",
              action[i], ",",
              difference[i])
        }
    } else {
        loopString <- "0"
    }

    asp_command <- str_c(
        paste0("B;Aspirate(", tipMask_bin),
        paste0("\"", liquidClass, "\""),
        volumes_str[[1]],
        GridSite$grid,
        GridSite$site,
        spacing,
        wellSelection_string,
        loopString,
        paste0(arm, ");"),
        sep = ",")
    .updateVolume(volumes, RackLabel, direction = -1)
    addToWorklist(asp_command, "advanced", CMadd_warning = CMadd_warning)
    invisible(asp_command)
}

#' Advanced Dispense Command
#'
#' @param tipMask \code{vector("integer",8L)}  containing 0, 1 indicating
#'      selected tips (e.g. \code{c(1,0,1,0,0,0,0,0)} = pins 1 + 3)
#' @param liquidClass string1 liquid class name
#' @param volumes \code{vector("integer",12L)} 12 expressions which specify the
#'       volume in µl for each tip. Specify 0 for tips which are not used or
#'       not fitted.
#' @param RackLabel Max. 32 characters User-defined label (name) which is
#'      assigned to the labware.
#' @param spacing integer 1 -  .. tip spacing, must be 2 for 384 well plates
#' @param wellSelection vector containing the wells into which the LiHa should
#'      dispense (should be in one column!)
#' @param ncol integer, number of colums (left to right on worktable) of labware
#' @param nrow integer, number of rows (back to front on worktable) of labware
#' @param noOfLoopOptions integer 0 - 7 number of loop options required. If
#'      non-zero, parameters \code{loopname, action, difference} are included
#'      n times, where n = \code{noOfLoopOptions}
#' @param loopName loop name, \code{vector("character",noOfLoopOptions)}
#' @param action integer 0 - 3 loop action: 0 = vary the column,
#'      1=vary the row 2=vary the well 3=vary the rack,
#'       \code{vector("character",noOfLoopOptions)}
#' @param difference integer number (“vary by”) which is used for the loop
#'       action, \code{vector("character",noOfLoopOptions)}
#' @param arm integer 0 - 1 number of the LiHa performing the action:
#'      0 = LiHa 1=LiHa 2, should always be 0
#' @family advanced worklist commands
#' @export
adv_dispense <- function(
    tipMask = c(1, 0, 0, 0, 0, 0, 0, 0),
    liquidClass = gwl$defaults$LiquidClass,
    volumes = gwl$defaults$Volume,
    RackLabel,
    spacing = 2,
    wellSelection, ncol, nrow,
    noOfLoopOptions = 0, loopName = "", action = 0, difference = 0,
    arm = 0
    ){

    tipMask_bin <- .binToDec(rev(tipMask))
    volumes_str <- .advVolumeString(volumes, tipMask)
    CMadd_warning <- volumes_str[[2]]
    GridSite <- getGridSite(RackLabel)
    wellSelection_string <- ANSIIFromWells(wellSelection, ncol, nrow)

    if (noOfLoopOptions != 0){
        loopString <- paste0(noOfLoopOptions)
        for (i in 1:noOfLoopOptions){
            loopString <- paste0(
              loopString, ",",
              paste0("\"", loopName[i], "\""), ",",
              action[i], ",", difference[i])
        }
    } else {
        loopString <- "0"
    }

    disp_command <- str_c(
        paste0("B;Dispense(", tipMask_bin),
        paste0("\"", liquidClass, "\""),
        volumes_str[[1]],
        GridSite$grid,
        GridSite$site,
        spacing,
        wellSelection_string,
        loopString,
        paste0(arm, ");"),
        sep = ",")

    .updateVolume(volumes, RackLabel, direction = 1)
    addToWorklist(disp_command, "advanced", CMadd_warning = CMadd_warning)
    invisible(disp_command)
}

#' Adv Comment Command
#'
#' insert a comment into the advanced worklist shows up in evoware running log
#' (as opposed to \code{gwl_comment} which only shows up in the .gwl file).
#' @family advanced worklist commands
#' @export
adv_gwl_comment <- function(comment){
    comment <- str_c(d = "B;Comment(\"", comment, "\");", sep = "")
    addToWorklist(comment, "advanced")
}


#' Adv Wash Command
#'
#' @param tipMask \code{vector("integer",8L)}  containing 0, 1 indicating
#'      selected tips (e.g. \code{c(1,0,1,0,0,0,0,0)} = pins 1 + 3)
#' @param RackLabel_waste Name of the waste rack
#' @param RackLabel_cleaner Name of the cleaner rack
#' @param wasteVol integer 0 - 100 volume in waste in ml
#' @param wasteDelay integer 0 - 1000 delay before closing valves in waste in ms
#' @param cleanerVol expression 0 - 100 volume in cleaner in ml
#' @param cleanerDelay integer 0 - 1000 delay before closing valves in cleaner
#'      in ms
#' @param Airgap floating point 0 - 100 volume of airgap in µl which is
#'      aspirated after washing the tips (system trailing airgap)
#' @param airgapSpeed integer 1 - 1000 speed of airgap aspiration in µl/s
#' @param retractSpeed integer 1 - 100 retract speed in mm/s
#' @param FastWash integer 0 - 1 1 = use fast-wash-module
#' @param lowVolume integer 0 - 1 1 = use pinch valves
#' @param arm integer 0 - 1 number of the LiHa performing the action:
#'      0 = LiHa 1=LiHa 2, should always be 0
#' @family advanced worklist commands
#' @export
adv_wash <- function(
    tipMask = rep(1, 8),
    RackLabel_waste = "wash1_waste",
    RackLabel_cleaner = "wash1_shallow",
    wasteVol = 3,
    wasteDelay = 500,
    cleanerVol = 4,
    cleanerDelay = 500,
    Airgap = 10,
    airgapSpeed = 70,
    retractSpeed = 30,
    FastWash = 1,
    lowVolume = 0,
    arm = 0
    ){
    CMadd_warning <- ""
    tipMask_bin <- .binToDec(rev(tipMask))
    GridSite_waste <- getGridSite(RackLabel_waste)
    GridSite_cleaner <- getGridSite(RackLabel_cleaner)

    wash_command <- str_c(
        paste0("B;Wash(", tipMask_bin),
        GridSite_waste$grid,
        GridSite_waste$site,
        GridSite_cleaner$grid,
        GridSite_cleaner$site,
        paste0("\"", wasteVol, "\""),
        wasteDelay,
        paste0("\"", cleanerVol, "\""),
        cleanerDelay,
        Airgap,
        airgapSpeed,
        retractSpeed,
        FastWash,
        lowVolume,
        0,
        paste0(arm, ");"),
        sep = ",")

    .updateVolume(wasteVol * 1000, RackLabel_waste, direction = -1)
    .updateVolume(cleanerVol * 1000, RackLabel_cleaner, direction = -1)
    addToWorklist(wash_command, "advanced", CMadd_warning = CMadd_warning)
    invisible(wash_command)
}

#' Adv Mix Command
#'
#' @param tipMask \code{vector("integer",8L)}  containing 0, 1 indicating
#'      selected tips (e.g. \code{c(1,0,1,0,0,0,0,0)} = pins 1 + 3)
#' @param liquidClass string1 liquid class name
#' @param volumes \code{vector("integer",12L)} 12 expressions which specify the
#'       volume for each tip. Specify 0 for tips which are not used or
#'       not fitted.
#' @param p_RackLabel Max. 32 characters User-defined label (name) which is
#'      assigned to the labware.
#' @param grid integer 1 - 67 labware location - carrier grid position
#' @param site integer 0 - 127 labware location - (site on carrier - 1)
#' @param spacing integer 1 -  .. tip spacing, must be 2 for 384 well plates
#' @param wellSelection vector containing the wells from which the LiHa should
#'      aspirate (should be in one column!)
#' @param ncol integer, number of colums (left to right on worktable) of labware
#' @param nrow integer, number of rows (back to front on worktable) of labware
#' @param cycles integer 0 .. 100 number of mixing cycles
#' @param noOfLoopOptions integer 0 - 7 number of loop options required. If
#'      non-zero, parameters \code{loopname, action, difference} are included
#'      n times, where n = \code{noOfLoopOptions}
#' @param loopName loop name, \code{vector("character",noOfLoopOptions)}
#' @param action integer 0 - 3 loop action: 0 = vary the column,
#'      1=vary the row 2=vary the well 3=vary the rack,
#'       \code{vector("character",noOfLoopOptions)}
#' @param difference integer number (“vary by”) which is used for the loop
#'       action, \code{vector("character",noOfLoopOptions)}
#' @param arm integer 0 - 1 number of the LiHa performing the action:
#'      0 = LiHa 1=LiHa 2, should always be 0
#' @family advanced worklist commands
#' @export
adv_mix <- function(
  tipMask = c(1, 0, 0, 0, 0, 0, 0, 0),
  liquidClass = gwl$defaults$LiquidClass,
  volumes = gwl$defaults$Volume,
  RackLabel,
  spacing=2,
  wellSelection,
  ncol,
  nrow,
  cycles,
  noOfLoopOptions = 0,
  loopName = "",
  action = 0,
  difference = 0,
  arm = 0
  ){

  tipMask_bin <- .binToDec(rev(tipMask))
  volumes_str <- .advVolumeString(volumes, tipMask)
  CMadd_warning <- volumes_str[[2]]
  GridSite <- getGridSite(RackLabel)

  wellSelection_string <- ANSIIFromWells(wellSelection, ncol, nrow)

  if (noOfLoopOptions != 0){
    loopString <- paste0(noOfLoopOptions)
    for (i in 1:noOfLoopOptions){
      loopString <- paste0(loopString, ",",
        paste0("\"", loopName[i], "\""), ",", action[i], ",",
        difference[i])
    }
  } else {
    loopString <- "0"
  }

  mix_command <- str_c(
      paste0("B;Mix(", tipMask_bin),
      paste0("\"", liquidClass, "\""),
      volumes_str[[1]],
      GridSite$grid,
      GridSite$site,
      spacing,
      wellSelection_string,
      cycles,
      loopString,
      paste0(arm, ");"),
      sep = ",")

  addToWorklist(mix_command, "advanced", CMadd_warning = CMadd_warning)
  invisible(mix_command)

}

#' Move LiHa
#'
#' @param tipMask \code{vector("integer",8L)}  containing 0, 1 indicating
#'   selected tips (e.g. \code{c(1,0,1,0,0,0,0,0)} = pins 1 + 3)
#' @param RackLabel Max. 32 characters User-defined label (name) which is
#'   assigned to the labware.
#' @param spacing integer 1 -  .. tip spacing, must be 2 for 384 well plates
#' @param wellSelection vector containing the wells from which the LiHa should
#'   aspirate (should be in one column!)
#' @param ncol integer, number of colums (left to right on worktable) of labware
#' @param nrow integer, number of rows (back to front on worktable) of labware
#' @param zMove integer 0 - 4 type of movement: 0 = positioning with global
#'   z-travel 1 = positioning with local z-travel 2 = x-move 3 = y-move
#'   4 = z-move
#' @param zTarget integer 0 - 4 z-position after move: 0 = z-travel
#'   1 = z-dispense 2 = z-start 3 = z-max 4 = global z-travel
#' @param offset floating point [-1000,1000] offset in mm added to z-position
#'   (zTarget)
#' @param speed floating point [0.1,400] move speed in mm/s if zMove is 2,3 or 4
#' @param noOfLoopOptions integer 0 - 7 number of loop options required. If
#'   non-zero, parameters \code{loopname, action, difference} are included
#'   n times, where n = \code{noOfLoopOptions}
#' @param loopName loop name, \code{vector("character",noOfLoopOptions)}
#' @param action integer 0 - 3 loop action: 0 = vary the column,
#'    1=vary the row, 2=vary the well 3=vary the rack,
#'    \code{vector("character",noOfLoopOptions)}
#' @param difference integer number (“vary by”) which is used for the loop
#'    action, \code{vector("character",noOfLoopOptions)}
#' @param arm integer 0 - 1 number of the LiHa performing the action:
#'   0 = LiHa 1=LiHa 2, should always be 0
#' @family advanced worklist commands
#' @export
moveLiHa <- function(
  tipMask = c(1, 0, 0, 0, 0, 0, 0, 0),
  RackLabel,
  spacing = 1,
  wellSelection,
  ncol,
  nrow,
  zMove,
  zTarget,
  offset,
  speed,
  noOfLoopOptions = 0,
  loopName = "",
  action = 0,
  difference = 0,
  arm = 0
  ){
  CMadd_warning <- ""
  tipMask_bin <- .binToDec(rev(tipMask))
  GridSite <- getGridSite(RackLabel)
  wellSelection_string <- ANSIIFromWells(wellSelection, ncol, nrow)

  if (noOfLoopOptions != 0){
      loopString <- paste0(noOfLoopOptions)
      for (i in 1:noOfLoopOptions){
          loopString <- paste0(loopString, ",",
            paste0("\"", loopName[i], "\""), ",",
            action[i], ",", difference[i])
      }
  } else {
      loopString <- "0"
  }

  moveLiHa_command <- str_c(
    paste0("B;MoveLiha(", tipMask_bin),
    GridSite$grid,
    GridSite$site,
    spacing,
    wellSelection_string,
    zMove,
    zTarget,
    offset,
    speed,
    loopString,
    paste0(arm, ");"),
    sep = ",")

  addToWorklist(moveLiHa_command, "advanced", CMadd_warning = CMadd_warning)
  invisible(moveLiHa_command)
}


#' Start Timer
#'
#' @param timer Number of Timer to (re-)start
#' @family advanced worklist commands
#' @export
startTimer <- function(timer){
    startTimer_command <- str_c(d = "B;StartTimer(", timer, ");", sep = "")
    addToWorklist(startTimer_command, "advanced")
}

#' Wait for Timer
#'
#' @param timer Number of Timer to (re-)start
#' @param timeSpan duration [0.02 - 86400]
#' @family advanced worklist commands
#' @export
waitTimer <- function(timer, timeSpan){
    waitTimer_command <- str_c(d = "B;WaitTimer(", timer, ",",
      timeSpan, ");", sep = "")
    addToWorklist(waitTimer_command, "advanced")
}

#' MCA Move
#'
#' Move MCA to labware
#'
#' @param RackLabel Max. 32 characters User-defined label (name) which is
#'   assigned to the labware.
#' @param quadrant integer 1-4, which quadrant?
#' @param ncol integer, number of colums (left to right on worktable) of labware
#' @param nrow integer, number of rows (back to front on worktable) of labware
#' @param zMove integer 0 - 5 type of movement: 0 = positioning with global
#'   z-travel, 1 = positioning with local z-travel, 2 = x-move, 3 = y-move,
#'   4 = z-move, 5 = Positioning with advanced z-travel (undocumented & untested..)
#' @param zTarget integer 0 - 4 z-position after move: 0 = z-travel
#'   1 = z-dispense 2 = z-start 3 = z-max 4 = global z-travel
#' @param offset integer [-1000,1000] offset in mm added to z-position
#'   (zTarget)
#' @param speed floating point [0.1,400] move speed in mm/s if zMove is 2,3 or 4
#' @param noOfLoopOptions integer 0 - 7 number of loop options required. If
#'   non-zero, parameters \code{loopname, action, difference} are included
#'   n times, where n = \code{noOfLoopOptions}
#' @param loopName loop name, \code{vector("character",noOfLoopOptions)}
#' @param action integer 0 - 3 loop action: 0 = vary the column,
#'    1=vary the row, 2=vary the well 3=vary the rack,
#'    \code{vector("character",noOfLoopOptions)}
#' @param difference integer number (“vary by”) which is used for the loop
#'    action, \code{vector("character",noOfLoopOptions)}
#' @param arm integer 0 - 1, number of the MCA96 performing the action:
#'   0 = MCA 1=MCA 2, should always be 0
#' @family advanced worklist commands
#' @export
moveMCA <- function(
  RackLabel,
  quadrant = 1,
  ncol = 24,
  nrow = 16,
  zMove,
  zTarget,
  offset = 0,
  speed,
  noOfLoopOptions = 0,
  loopName = "",
  action = 0,
  difference = 0,
  arm = 0){
  CMadd_warning <- ""

  if (ncol == 12 & nrow == 8){
    wellSelection <- 1:96
  } else if (ncol == 24 & nrow == 16) {
    wellSelection <- switch(quadrant,
      as.vector(matrix(1:384, 24, 16)[c(T, F), c(T, F)]),
      as.vector(matrix(1:384, 24, 16)[c(F, T), c(T, F)]),
      as.vector(matrix(1:384, 24, 16)[c(T, F), c(F, T)]),
      as.vector(matrix(1:384, 24, 16)[c(F, T), c(F, T)]))
  } else {
    stop("must target a 96 or a 384well plate!")
  }
  wellSelection_string <- ANSIIFromWells(wellSelection, ncol, nrow)
  GridSite <- getGridSite(RackLabel)

  if (noOfLoopOptions != 0){
      loopString <- paste0(noOfLoopOptions)
      for (i in 1:noOfLoopOptions){
          loopString <- paste0(loopString, ",",
            paste0("\"", loopName[i], "\""), ",",
            action[i], ",", difference[i])
      }
  } else {
      loopString <- "0"
  }

  moveMCA_command <- str_c(
    "B;MCAMove(0",
    GridSite$grid,
    GridSite$site,
    "1",
    wellSelection_string,
    zMove,
    zTarget,
    offset,
    speed,
    loopString,
    paste0(arm, ");"),
    sep = ",")

  addToWorklist(moveMCA_command, "advanced", CMadd_warning = CMadd_warning)
  invisible(moveMCA_command)
}

#' MCA96 Vector command
#'
#' execute MCA vector
#'
#' @param move_vector string, the name of the vector
#' @param grid integer, labware location grid
#' @param site integer, labware locatation site STARTING WITH 1 at the back NOT 0!
#' @param direction integer 0 or 1, 0 = from safe to end, 1 from end to safe
#' @param back integer 0 or 1, 0 = don't move back to safe, 1 = move back to safe
#' @param beginAction Gripper action at safe, O = open, 1 = close, 2 = do not move
#' @param endAction Gripper action at end, O = open, 1 = close, 2 = do not move
#' @param speed integer 0 or 1, 0 = max. speed, 1 = speed as defined in vector teaching
#' @param arm integer 0 - 1, number of the MCA96 performing the action:
#'   0 = MCA 1=MCA 2, should always be 0
#' @family advanced worklist commands
#' @export
MCAvector <- function(
  move_vector,
  grid,
  site,
  direction = 0,
  back = 1,
  beginAction = 2,
  endAction = 2,
  speed = 1,
  arm = 0){
  CMadd_warning <- ""

  if (site == 0){
    stop("MCAVector Sites start with 1 (from the back)!!!!")
  }

  MCAVector_command <- str_c(
    str_c("B;MCAMoveVector(\"", move_vector, "\""),
    grid,
    site,
    direction,
    back,
    beginAction,
    endAction,
    speed,
    str_c(arm, ");"),
    sep = ",")

  addToWorklist(MCAVector_command, "advanced", CMadd_warning = CMadd_warning)
  invisible(MCAVector_command)
}

#' MCA Relative movement
#'
#' Sets an offset to the MCA
#'
#' @param col_offset integer, [-23,23], positive numbers move the MCA to the
#'   right, negative to the left. Defaults to 0 = reset offset.
#' @param row_offset integer, [-15,15], positive numbers move the MCA to the
#'   front, negative to the back. Defaults to 0 = reset offset.
#' @param arm integer 0 - 1, number of the MCA96 performing the action:
#'   0 = MCA 1=MCA 2, should always be 0
#' @param verbose logical, print comments?
#' @family advanced worklist commands
#' @export
MCArelative <- function(
  col_offset = 0,
  row_offset = 0,
  arm = 0,
  verbose = FALSE){
  CMadd_warning <- ""

  if (col_offset == 0){
    offsetMap_col <- "00"
  } else {
    offsetMap_col <- switch(as.integer(col_offset > 0) + 1,
      "01", # col_offset < 0
      "10") # col_offset > 0
  }
  if (row_offset == 0){
    offsetMap_row <- "00"
  } else {
    offsetMap_row <- switch(as.integer(row_offset > 0) + 1,
      "10", # row_offset < 0
      "01") # row_offset > 0
  }

  offsetMap  <- str_c(offsetMap_row, offsetMap_col)

  if (verbose) {
    gwl_comment(
      str_c("offsetting MCA: col (X) = ", col_offset, "
        / row (Y) = ", row_offset)
    )}

  MCArelative_command <- str_c(
    str_c("B;MCARelativeMovement(", offsetMap),
    col_offset,
    row_offset,
    str_c(arm, ");"),
    sep = ",")

  addToWorklist(MCArelative_command, "advanced", CMadd_warning = CMadd_warning)
  invisible(MCArelative_command)
}

#' Send Firmware command
#'
#' sends a firmware command
#'
#' @param command string, the command
#' @param options int 0-7, bit-coded options: 1 = wait for termination of
#'   command, 2 = store error code in variable, 4 = store answer in variable
#' @param parameter int 1-16 number of the parameter in the response which is
#'   stored in the variable “response”
#' @param response string, variable to store response, empty for no storage
#' @param error string, variable to store error, empty for no storage
#' @param errorScope int 0-2, scope of error variable: 0 = Run, 1 = Instance,
#'   2 = Script
#' @param responseScope int 0-2, scope of response variable: 0 = Run,
#'   1 = Instance, 2 = Script
#' @param responseType int 0-1, type of response variable. 0 = numeric,
#'   1 = string
#' @family advanced worklist commands
#' @export
fwCommand <- function(
  command,
  options = 1,
  parameter = 1,
  response = "",
  error = "",
  errorScope = 2,
  responseScope = 2,
  responseType = 0){
  CMadd_warning <- ""

  fwCommand_command <- str_c(
    str_c("B;Command(\"", command, "\""),
    options,
    parameter,
    response,
    error,
    errorScope,
    responseScope,
    str_c(responseType, ");"),
    sep = ",")

  addToWorklist(fwCommand_command, "advanced", CMadd_warning = CMadd_warning)
  invisible(fwCommand_command)
}
