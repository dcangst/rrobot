#' wait for t seconds
#'
#' simpler timer
#'
#' @param timer Number of Timer to (re-)start
#' @param timeSpan duration [0.02 - 86400]
#' @family advanced worklist procedures
#' @export
evowait <- function(timer, timeSpan){
  startTimer(timer)
  waitTimer(timer, timeSpan)
}

#' Custom Sterile Wash procedure
#'
#' Washes & sterilizes all tips, hopefully.
#'
#' @param wash_rack Wash station used (default: wash1_shallow)
#' @param wash_waste Waste used (default: wash1_waste)
#' @param wash_solution1 RackLabel containing first wash solution
#' @param wash_solution1_liquidClass LiquidClass used to pipet first wash
#'      solution
#' @param wash_solution2 RackLabel containing second wash solution
#' @param wash_solution2_liquidClass LiquidClass used to pipet
#'      second wash solution
#' @param wash_solution3 RackLabel containing second wash solution
#' @param wash_solution3_liquidClass LiquidClass used to pipet
#'      second wash solution
#' @family advanced worklist procedures
#' @export
sterile_wash <- function(
  wash_solution1 = "trough_bleach",
  wash_solution1_liquidClass = "sterileWash_N_bleach",
  wash_solution2 = "trough_EtOH",
  wash_solution2_liquidClass = "sterileWash_N_EtOH",
  wash_solution3 = "trough_H2O",
  wash_solution3_liquidClass = "sterileWash_N_H2O"
  ){
  adv_gwl_comment("sterile wash: NORMAL")
  gwl_comment("START: sterile wash: NORMAL")

  adv_wash(tipMask = rep(1, 8), RackLabel_waste = "wash2_waste",
    RackLabel_cleaner = "wash2_shallow", wasteVol = 1, wasteDelay = 500,
    cleanerVol = 0.1, cleanerDelay = 0, Airgap = 10, airgapSpeed = 70,
    retractSpeed = 30, FastWash = 1, lowVolume = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution1_liquidClass,
    volumes = rep(350, 8), RackLabel = wash_solution1, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution1_liquidClass,
      volumes = rep(350, 8), RackLabel = wash_solution1,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  adv_wash(tipMask = rep(1, 8), RackLabel_waste = "wash1_waste",
    RackLabel_cleaner = "wash1_shallow", wasteVol = 1, wasteDelay = 500,
    cleanerVol = 2, cleanerDelay = 500, Airgap = 10, airgapSpeed = 70,
    retractSpeed = 30, FastWash = 1, lowVolume = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution2_liquidClass,
    volumes = rep(400, 8), RackLabel = wash_solution2, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution2_liquidClass,
      volumes = rep(400, 8), RackLabel = wash_solution2,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution3_liquidClass,
    volumes = rep(400, 8), RackLabel = wash_solution3, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution3_liquidClass,
      volumes = rep(400, 8), RackLabel = wash_solution3,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  moveLiHa(tipMask = rep(1, 8), RackLabel = wash_solution3,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8,
      zMove = 0, zTarget = 4, offset = 0, speed = 10,
      noOfLoopOptions = 0, loopName = "", action = 0, difference = 0, arm = 0)
  gwl_comment("END: sterile wash: NORMAL")
}

#' Short Sterile Wash procedure for Tip Touch Transfers
#'
#' Washes & sterilizes all tips, hopefully.
#'
#' @param wash_rack Wash station used (default: wash1_shallow)
#' @param wash_waste Waste used (default: wash1_waste)
#' @param wash_solution1 RackLabel containing first wash solution
#' @param wash_solution1_liquidClass LiquidClass used to pipet first wash
#'      solution
#' @param wash_solution2 RackLabel containing second wash solution
#' @param wash_solution2_liquidClass LiquidClass used to pipet
#'      second wash solution
#' @param wash_solution3 RackLabel containing second wash solution
#' @param wash_solution3_liquidClass LiquidClass used to pipet
#'      second wash solution
#' @family advanced worklist procedures
#' @export
sterile_wash_TT <- function(
  wash_solution1="trough_bleach",
  wash_solution1_liquidClass = "sterileWash_TT_bleach",
  wash_solution2="trough_EtOH",
  wash_solution2_liquidClass = "sterileWash_TT_EtOH",
  wash_solution3="trough_H2O",
  wash_solution3_liquidClass = "sterileWash_TT_H2O"
  ){

  gwl_comment("START: sterile wash: TipTouch")
  adv_gwl_comment("sterile wash: TipTouch")

  adv_wash(tipMask = rep(1, 8), RackLabel_waste = "wash2_waste",
    RackLabel_cleaner = "wash2_shallow", wasteVol = 1, wasteDelay = 500,
    cleanerVol = 0.1, cleanerDelay = 0, Airgap = 10, airgapSpeed = 70,
    retractSpeed = 30, FastWash = 1, lowVolume = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution1_liquidClass,
    volumes = rep(350, 8), RackLabel = wash_solution1, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution1_liquidClass,
      volumes = rep(350, 8), RackLabel = wash_solution1,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  adv_wash(tipMask = rep(1, 8), RackLabel_waste = "wash1_waste",
    RackLabel_cleaner = "wash1_shallow", wasteVol = 3, wasteDelay = 500,
    cleanerVol = 5, cleanerDelay = 500, Airgap = 10, airgapSpeed = 70,
    retractSpeed = 30, FastWash = 1, lowVolume = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution2_liquidClass,
    volumes = rep(400, 8), RackLabel = wash_solution2, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution2_liquidClass,
      volumes = rep(400, 8), RackLabel = wash_solution2,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution3_liquidClass,
    volumes = rep(400, 8), RackLabel = wash_solution3, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution3_liquidClass,
      volumes = rep(400, 8), RackLabel = wash_solution3,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  moveLiHa(tipMask = rep(1, 8), RackLabel = wash_solution3,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8,
      zMove = 0, zTarget = 4, offset = 0, speed = 10,
      noOfLoopOptions = 0, loopName = "", action = 0, difference = 0, arm = 0)
  gwl_comment("END: sterile wash: TipTouch")
}

#' Custom Sterile Wash procedure for Phage work
#'
#' Washes & sterilizes all tips, hopefully. Kills phages, hopefully.
#'
#' @param wash_rack Wash station used (default: wash1_shallow)
#' @param wash_waste Waste used (default: wash1_waste)
#' @param wash_solution1 RackLabel containing first wash solution
#' @param wash_solution1_liquidClass LiquidClass used to pipet first wash
#'      solution
#' @param wash_solution2 RackLabel containing second wash solution
#' @param wash_solution2_liquidClass LiquidClass used to pipet second wash
#'      solution
#' @param wash_solution3 RackLabel containing third wash solution
#' @param wash_solution3_liquidClass LiquidClass used to pipet
#'      third wash solution
#' @param wash_solution4 RackLabel containing fourth wash solution
#' @param wash_solution4_liquidClass LiquidClass used to pipet
#'      fourth wash solution
#' @family advanced worklist procedures
#' @export
sterile_wash_phage <- function(
  wash_solution1 = "trough100_bck",
  wash_solution1_liquidClass = "sterileWash_N_NaOH",
  wash_solution2 = "trough_bleach",
  wash_solution2_liquidClass = "sterileWash_N_bleach",
  wash_solution3 = "trough_EtOH",
  wash_solution3_liquidClass = "sterileWash_N_EtOH",
  wash_solution4 = "trough_H2O",
  wash_solution4_liquidClass = "sterileWash_N_H2O"
  ){
  adv_gwl_comment("sterile wash: PHAGE")
  gwl_comment("START: sterile wash: PHAGE")

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution1_liquidClass,
    volumes = rep(350, 8), RackLabel = wash_solution1, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution1_liquidClass,
      volumes = rep(350, 8), RackLabel = wash_solution1,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  adv_wash(tipMask = rep(1, 8), RackLabel_waste = "wash2_waste",
    RackLabel_cleaner = "wash2_shallow", wasteVol = 1, wasteDelay = 500,
    cleanerVol = 0.1, cleanerDelay = 0, Airgap = 10, airgapSpeed = 70,
    retractSpeed = 30, FastWash = 1, lowVolume = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution2_liquidClass,
    volumes = rep(350, 8), RackLabel = wash_solution2, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution2_liquidClass,
      volumes = rep(350, 8), RackLabel = wash_solution2,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  adv_wash(tipMask = rep(1, 8), RackLabel_waste = "wash1_waste",
    RackLabel_cleaner = "wash1_shallow", wasteVol = 1, wasteDelay = 500,
    cleanerVol = 2, cleanerDelay = 500, Airgap = 10, airgapSpeed = 70,
    retractSpeed = 30, FastWash = 1, lowVolume = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution3_liquidClass,
    volumes = rep(400, 8), RackLabel = wash_solution3, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution3_liquidClass,
      volumes = rep(400, 8), RackLabel = wash_solution3,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  adv_aspirate(tipMask = rep(1, 8), liquidClass = wash_solution4_liquidClass,
    volumes = rep(400, 8), RackLabel = wash_solution4, spacing = 1,
    wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0, loopName = "",
    action = 0, difference = 0, arm = 0)
  adv_dispense(tipMask = rep(1, 8), liquidClass = wash_solution4_liquidClass,
      volumes = rep(400, 8), RackLabel = wash_solution4,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8, noOfLoopOptions = 0,
      loopName = "", action = 0, difference = 0, arm = 0)

  moveLiHa(tipMask = rep(1, 8), RackLabel = wash_solution4,
      spacing = 1, wellSelection = 1:8, ncol = 1, nrow = 8,
      zMove = 0, zTarget = 4, offset = 0, speed = 10,
      noOfLoopOptions = 0, loopName = "", action = 0, difference = 0, arm = 0)
  gwl_comment("END: sterile wash: PHAGE")
}

#' Tip Touch
#'
#' Transfering by tiptouching using the tips
#'
#' @param tip_mask tip mask for source
#' @param wells Wells source
#' @param rack RackLabel of source
#' @param ncol integer, number of colums (left to right on worktable) of labware
#' @param nrow integer, number of rows (back to front on worktable) of labware
#' @param spacing integer 1 -  .. tip spacing, must be 2 for 384 well plates
#' @param speed_move movement speed up to z-dispense
#' @param speed_in movement speed when dipping
#' @param speed_out movement speed when retracting
#' @family advanced worklist procedures
#' @export
tiptouch <- function(
  tip_mask,
  wells,
  rack,
  ncol = 24,
  nrow = 16,
  spacing = 2,
  speed_move,
  speed_in,
  speed_out){
  # get innoculum
    moveLiHa( # move to z-travel
      tipMask       = tip_mask,
      RackLabel     = rack,
      spacing       = spacing,
      wellSelection = wells,
      ncol = ncol, nrow = nrow,
      zMove = 0,
      zTarget = 0,
      offset = 0,
      speed = speed_move)
    moveLiHa( # move z to z-dispense
      tipMask       = tip_mask,
      RackLabel     = rack,
      spacing       = spacing,
      wellSelection = wells,
      ncol = ncol, nrow = nrow,
      zMove = 4,
      zTarget = 1,
      offset = 0,
      speed = speed_move)
    moveLiHa( # move z to z-max
      tipMask       = tip_mask,
      RackLabel     = rack,
      spacing       = spacing,
      wellSelection = wells,
      ncol = ncol, nrow = nrow,
      zMove = 4,
      zTarget = 3,
      offset = 0,
      speed = speed_in)
    moveLiHa( # move z to z-dispense
      tipMask       = tip_mask,
      RackLabel     = rack,
      spacing       = spacing,
      wellSelection = wells,
      ncol = ncol, nrow = nrow,
      zMove = 4,
      zTarget = 1,
      offset = 0,
      speed = speed_out)
    moveLiHa( # move to z-travel
      tipMask       = tip_mask,
      RackLabel     = rack,
      spacing       = spacing,
      wellSelection = wells,
      ncol = ncol, nrow = nrow,
      zMove = 0,
      zTarget = 0,
      offset = 0,
      speed = speed_move)
}

#' iPinTool deactivate pins
#'
#' generates worklist procedures to deactivate pins. All pins are always activated
#' when the pin tool is dropped onto the carrier.
#'
#' @param pins vector of integers 1:384, designating the pins to be deactivated.
#' @param RackLabel name of carrier as on gwl$worktable
#' @param readyVector name of the vector to move over the stub
#' @param pushVector1 name of the vector that pushes the pins 1-192
#' @param pushVector2 name of the vector that pushes the pins 193-384
#' @family advanced worklist procedures
#' @export
iPTdeactivate <- function(
  pins,
  RackLabel = "iPT tool",
  readyVector = "PT getReady",
  pushVector1 = "PT Act 1-192",
  pushVector2 = "PT Act 193-384"){

  adv_gwl_comment("preparing iPT384...")
  adv_gwl_comment(
    str_c("deactivating pins: ", str_c(pins, collapse = ", "))
    )

  grid <- getGridSite(RackLabel)$grid

  pins_sorted <- shortestPath(pins)

  MCAvector(readyVector, grid, 1, back = 0)

  for (i in pins_sorted){
    if (i <= 192){
      MCAvector(pushVector1, grid, i)
    } else {
      MCAvector(pushVector2, grid, i - 192)
    }
  }

  MCAvector(readyVector, grid, 1, back = 0, direction = 1)

}

#' iPT384 pin transfer
#'
#' pick up 4 adjacent wells on a 384 well plate with the 4 pins in the back left
#'  corner of the iPT384. Make sure all other pins are deactivated!
#'
#' @param rack string, Racklabel of target, as in gwl$worktable
#' @param well int 1:384, which well should be hit with pin 1
#' @param n_dips number of dips in plate
#' @param speed_move movement speed up to z-dispense
#' @param speed_in movement speed when dipping
#' @param speed_out movement speed when retracting
#' @family advanced worklist procedures
#' @export
iPTdip <- function(
  rack,
  well,
  n_dips = 3,
  speed_move = 35,
  speed_in = 5,
  speed_out = 5){

  well_row <- (well - 1) %/% 24
  well_col <- well - well_row * 24 - 1
  adv_gwl_comment(str_c(
    "dipping ", n_dips,"x in ",rack))
  MCArelative(well_col, well_row)
  moveMCA( # move over plate, z-travel
    RackLabel = rack,
    quadrant = 1,
    ncol = 24,
    nrow = 16,
    zMove = 0,
    zTarget = 0,
    offset = 0,
    speed = speed_move)
  moveMCA( # z-move to z-dispense
    RackLabel = rack,
    quadrant = 1,
    ncol = 24,
    nrow = 16,
    zMove = 4,
    zTarget = 1,
    offset = 0,
    speed = speed_move)
  for (i in 1:n_dips){
    moveMCA( # z-move to z-max
      RackLabel = rack,
      quadrant = 1,
      ncol = 24,
      nrow = 16,
      zMove = 4,
      zTarget = 3,
      offset = 0,
      speed = speed_in)
    moveMCA( # z-move to z-dispense
      RackLabel = rack,
      quadrant = 1,
      ncol = 24,
      nrow = 16,
      zMove = 4,
      zTarget = 1,
      offset = 0,
      speed = speed_out)
  }
  moveMCA( # z-move to global z-travel
    RackLabel = rack,
    quadrant = 1,
    ncol = 24,
    nrow = 16,
    zMove = 4,
    zTarget = 4,
    offset = 0,
    speed = speed_move)
  MCArelative() # reset offset
}

#' Wash pin tool
#'
#' Procedure to wash pintool
#'
#' @param t_wait integer, time (s) to wait on blotting paper
#' @param t_dry integer, time (s) over dryer
#' @param n_dips integer, number of dips in reagents
#' @param reservoirs vector of RackLabel of reservoirs to dip
#' @param blots vector of Racklaberl of blotting spots
#' @family advanced worklist procedures
#' @export
washPinTool <- function(
  t_wait = 2,
  t_dry = 90,
  n_dips = 3,
  reservoirs = c("Bleach", "ddH2O", "Isopropanol"),
  blots = c("Blot1", "BlotBleach", "BlotH2O")){
  adv_gwl_comment("washing tips...")
  # blot 1: dirty
  moveMCA(
    RackLabel = blots[1],
    zMove = 0,
    zTarget = 3,
    speed = 10)
  # wait t_wait seconds
  evowait(1, t_wait)
  #dip 3x in first reservoir
  moveMCA(
    RackLabel = reservoirs[1],
    ncol = 12,
    nrow = 8,
    zMove = 0,
    zTarget = 2,
    speed = 10)
  for (i in 1:n_dips){
    moveMCA(
      RackLabel = reservoirs[1],
      ncol = 12,
      nrow = 8,
      zMove = 4,
      zTarget = 3,
      speed = 60)
    moveMCA(
      RackLabel = reservoirs[1],
      ncol = 12,
      nrow = 8,
      zMove = 4,
      zTarget = 2,
      speed = 60)
  }
  # blot 2: bleach
  moveMCA(
    RackLabel = blots[2],
    zMove = 0,
    zTarget = 3,
    speed = 10)
  # wait t_wait seconds
  evowait(1, t_wait)
  #dip 3x in 2nd reservoir
  moveMCA(
    RackLabel = reservoirs[2],
    ncol = 12,
    nrow = 8,
    zMove = 0,
    zTarget = 2,
    speed = 10)
  for (i in 1:n_dips){
    moveMCA(
      RackLabel = reservoirs[2],
      ncol = 12,
      nrow = 8,
      zMove = 4,
      zTarget = 3,
      speed = 60)
    moveMCA(
      RackLabel = reservoirs[2],
      ncol = 12,
      nrow = 8,
      zMove = 4,
      zTarget = 2,
      speed = 60)
  }
  #dip 3x in 3nd reservoir
  moveMCA(
    RackLabel = reservoirs[3],
    ncol = 12,
    nrow = 8,
    zMove = 0,
    zTarget = 2,
    speed = 10)
  for (i in 1:n_dips){
    moveMCA(
      RackLabel = reservoirs[3],
      ncol = 12,
      nrow = 8,
      zMove = 4,
      zTarget = 3,
      speed = 60)
    moveMCA(
      RackLabel = reservoirs[3],
      ncol = 12,
      nrow = 8,
      zMove = 4,
      zTarget = 2,
      speed = 60)
  }
  # blot 3: EtOH
  moveMCA(
    RackLabel = blots[3],
    zMove = 0,
    zTarget = 3,
    speed = 10)
  # wait t_wait seconds
  evowait(1, t_wait)
  #dry
  moveMCA(
    RackLabel = "PT dryer",
    zMove = 0,
    zTarget = 3,
    speed = 10)
  fwCommand("O1SLO3,1")
  evowait(1, t_dry)
  fwCommand("O1SLO3,0")
  moveMCA(
    RackLabel = "PT dryer",
    zMove = 0,
    zTarget = 4,
    speed = 10)
}
