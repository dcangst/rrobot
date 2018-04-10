#' Aspirate Command
#'
#' @param RackLabel Max. 32 characters User-defined label (name) which is assigned to the labware.
#' @param RackID Max. 32 characters, Labware barcode.
#' @param RackType Max. 32 characters, Labware type (configuration name), e.g. “384 Well, landscape”.
#' @param Position 1 .. number of wells, Well position in the labware. The position starts with 1 and increases from rear to front and left to right.
#' @param TubeID Max. 32 characters, Tube barcode.
#' @param Volume 0 .. +7158278, Pipetting volume in µl.
#' @param LiquidClass Max. 32 characters, This optional parameter overwrites the liquid class specified in the Worklist command.
#' @param TipType Reserved, must be omitted.
#' @param TipMask 1 .. 128, This optional parameter specifies the tip you want to use. The tip number is bit-coded, i.e. Tip 1 = 1, Tip 2 = 2, Tip 3 = 4; Tip 4 = 8 and so on. Please note that you can only use a tip which has been enabled with Tip Selection in the Worklist command. If you do not specify TipMask, Freedom EVOware uses one of the tips specified with Tip Selection in the Worklist command.
#' @param ForcedRackType Max. 32 characters, This optional parameter is the configuration name of the labware. If specified, it is used instead of the labware type which is found on the Freedom EVOware worktable.
#' @param MinDetectedVolume 0 .. +7158278 Liquid volume in µl. If this optional parameter is specified and Liquid Level Detection is enabled in the selected liquid class, it is used by the LiHa to determine the minimum liquid height which must be available in the well. Otherwise a liquid detection error (not enough liquid) will be triggered. MinDetectedVolume specifies the usable volume of liquid. If the aspiration position in the liquid class specifies tip immersion (tip submerging), the actual liquid height in the well must be higher to allow immersion (i.e. the volume of liquid in the well must be more than MinDetectedVolume).
#' @family basic worklist commands
#' @export
aspirate <- function(RackLabel = gwl$defaults$RackLabel,
    RackID = gwl$defaults$RackID,
    RackType = gwl$defaults$RackType,
    Position = gwl$defaults$Position,
    TubeID = gwl$defaults$TubeID,
    Volume = gwl$defaults$Volume,
    LiquidClass= gwl$defaults$LiquidClass,
    TipType = "",
    TipMask = gwl$defaults$TipMask,
    ForcedRackType = gwl$defaults$ForcedRackType,
    MinDetectedVolume = gwl$defaults$MinDetectedVolume){

    getGridSite(RackLabel)
    asp_command <- str_c("A",
        RackLabel,
        RackID,
        RackType,
        Position,
        TubeID,
        Volume,
        LiquidClass,
        TipType,
        TipMask,
        ForcedRackType,
        MinDetectedVolume,
        sep = ";")

    .updateVolume(Volume, RackLabel, direction = -1)
    addToWorklist(asp_command, "basic")
}

#' Dispense Command
#'
#' @param RackLabel Max. 32 characters User-defined label (name) which is assigned to the labware.
#' @param RackID Max. 32 characters, Labware barcode.
#' @param RackType Max. 32 characters, Labware type (configuration name), e.g. “384 Well, landscape”.
#' @param Position 1 .. number of wells, Well position in the labware. The position starts with 1 and increases from rear to front and left to right.
#' @param TubeID Max. 32 characters, Tube barcode.
#' @param Volume 0 .. +7158278, Pipetting volume in µl.
#' @param LiquidClass Max. 32 characters, This optional parameter overwrites the liquid class specified in the Worklist command.
#' @param TipType Reserved, must be omitted.
#' @param TipMask 1 .. 128, This optional parameter specifies the tip you want to use. The tip number is bit-coded, i.e. Tip 1 = 1, Tip 2 = 2, Tip 3 = 4; Tip 4 = 8 and so on. Please note that you can only use a tip which has been enabled with Tip Selection in the Worklist command. If you do not specify TipMask, Freedom EVOware uses one of the tips specified with Tip Selection in the Worklist command.
#' @param ForcedRackType Max. 32 characters, This optional parameter is the configuration name of the labware. If specified, it is used instead of the labware type which is found on the Freedom EVOware worktable.
#' @param MinDetectedVolume 0 .. +7158278 Liquid volume in µl. If this optional parameter is specified and Liquid Level Detection is enabled in the selected liquid class, it is used by the LiHa to determine the minimum liquid height which must be available in the well. Otherwise a liquid detection error (not enough liquid) will be triggered. MinDetectedVolume specifies the usable volume of liquid. If the aspiration position in the liquid class specifies tip immersion (tip submerging), the actual liquid height in the well must be higher to allow immersion (i.e. the volume of liquid in the well must be more than MinDetectedVolume).
#' @family basic worklist commands
#' @export
dispense <- function(RackLabel = gwl$defaults$RackLabel,
    RackID = gwl$defaults$RackID,
    RackType = gwl$defaults$RackType,
    Position = gwl$defaults$Position,
    TubeID = gwl$defaults$TubeID,
    Volume = gwl$defaults$Volume,
    LiquidClass= gwl$defaults$LiquidClass,
    TipType = "",
    TipMask = gwl$defaults$TipMask,
    ForcedRackType = gwl$defaults$ForcedRackType,
    MinDetectedVolume = gwl$defaults$MinDetectedVolume){

    getGridSite(RackLabel)

    disp_command <- str_c("D",
        RackLabel,
        RackID,
        RackType,
        Position,
        TubeID,
        Volume,
        LiquidClass,
        TipType,
        TipMask,
        ForcedRackType,
        MinDetectedVolume,
        sep = ";")

    .updateVolume(Volume, RackLabel, direction = 1)
    addToWorklist(disp_command, "basic")
}

#' Tecan Wash Command
#'
#' The Wash Tips record washes the tip which was used by the preceeding Aspirate record. There are no parameters (the wash position, wash parameters are specified in the Worklist command).
#'
#' @param W "W" (or "W1"), "W2","W3","W4" or "WD" to select which wash scheme to use.
#' @family basic worklist commands
#' @export
wash <- function(W = gwl$defaults$Wash){
    wash_command <- str_c(W, "", sep = ";")
    addToWorklist(wash_command, "basic")
}

#' Flush Command
#'
#' It discards the contents of the tips without washing them.
#' @family basic worklist commands
#' @export
gwl_flush <- function(){
    flush_command <- str_c("F", "", sep = ";")
    addToWorklist(flush_command, "basic")
}

#' Break Command
#'
#' The Break record forces the execution of previously specified aspirate, dispense or wash actions which have not yet been executed. If you don’t specify a Break record, Freedom EVOware normally executes pipetting commands in groups to optimize the efficiency. For example, if you have specified four tips in the Worlist command, Freedom EVOware will queue Aspirate records until four of them are ready for execution. This allows pipetting to take place using all four tips at the same time. Specify the Break record if you want to execute all of the currently queued commands without waiting. You can use the Break record e.g. to create a worklist which pipettes using only one tip at a time (even if you chose more than one tip in the tip selection).
#' @family basic worklist commands
#' @export
gwl_break <- function(){
    break_command <- str_c("B", "", sep = ";")
    addToWorklist(break_command, "basic")
}

#' Comment Command
#'
#' insert a comment into the worklist (ignored by EVOware).
#' @family basic worklist commands
#' @export
gwl_comment <- function(comment){
    comment <- str_c(d = "C", comment, sep = ";")
    addToWorklist(comment, "basic")
}
#' Reagent Distribution Command
#'
#' The Reagent Distribution record specifies the aspiration and dispensing parameters for all of the tips you have chosen (and dispenses the same volume in all of the chosen wells in the destination labware).
#'
#' @param SrcRackLabel Max. 32 characters User-defined label (name) which is assigned to the source labware.
#' @param SrcRackID Max. 32 characters Source labware barcode.
#' @param SrcRackType Max. 32 characters Source labware type (configuration name), e.g. “384 Well, landscape”.
#' @param SrcPosStart 1 .. number of wells First well to be used in the source labware.
#' @param SrcPosEnd 1 .. number of wells Last well to be used in the source labware.
#' @param DestRackLabel Max. 32 characters User-defined label (name) which is assigned to the destination labware.
#' @param DestRackID Max. 32 characters Destination labware barcode.
#' @param DestRackType Max. 32 characters Destination labware type (configuration name), e.g. “384 Well, landscape”.
#' @param DestPosStart 1 .. number of wells First well to be used in the destination labware.
#' @param DestPosEnd 1 .. number of wells Last well to be used in the destination labware.
#' @param Volume 0 .. +7158278 Dispense volume in the destination labware in µl.
#' @param LiquidClass Max. 32 characters This optional parameter overwrites the liquid class specified in the Worklist command. #' @param NoOfDitiReuses Optional maximum number of DiTi reuses allowed (default 1 = no DiTi reuse).
#' @param NoOfMultiDisp Optional maximum number of dispenses in a multi-dispense sequence (default 1 = no multi-dispense).
#' @param Direction Optional pipetting direction (0 = left to right, 1 = right to left; default = 0).
#' @param ExcludeDestWell   Optional vector of wells in destination labware to be excluded from pipetting (e.g. c(23,44,36))
#' @family basic worklist commands
#' @export
distribute <- function(
  SrcRackLabel = "",
  SrcRackID = "",
  SrcRackType = "",
  SrcPosStart = "",
  SrcPosEnd = "",
  DestRackLabel = "",
  DestRackID = "",
  DestRackType = "",
  DestPosStart = "",
  DestPosEnd = "",
  Volume = gwl$defaults$Volume,
  LiquidClass = gwl$defaults$LiquidClass,
  NoOfDitiReuses = 1,
  NoOfMultiDisp = 1,
  Direction = 0,
  ExcludeDestWell = NA){

  getGridSite(SrcRackLabel)
  getGridSite(DestRackLabel)

  distribute_command <- str_c("R",
    SrcRackLabel,
    SrcRackID,
    SrcRackType,
    SrcPosStart,
    SrcPosEnd,
    DestRackLabel,
    DestRackID,
    DestRackType,
    DestPosStart,
    DestPosEnd,
    Volume,
    LiquidClass,
    NoOfDitiReuses,
    NoOfMultiDisp,
    Direction,
    sep = ";")

  if (!is.na(ExcludeDestWell)){
    distribute_command <- str_c(distribute_command,
      str_c(ExcludeDestWell, collapse = ";"), sep = ";")
  }
    .updateVolume(Volume, SrcRackLabel, direction = -1)
    .updateVolume(Volume, DestRackLabel, direction = 1)
    addToWorklist(distribute_command, "basic")
}
