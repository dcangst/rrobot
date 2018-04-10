#' rrobot.
#' A package containing tools for worklist generation for the freedom evo.
#'
#'
#'@section base methods:
#'
#'      \code{\link{init}} - initialize a new worklist
#'
#'      \code{\link{write.gwl}} - write worklist
#'
#'      \code{\link{addToWorktable}} - add labware to worktable
#' 
#'      \code{\link{addToWorklist}} - add an arbitrary command
#'
#'@section Basic Worklist methods:
#'      
#'      \code{\link{aspirate}}
#'      
#'      \code{\link{dispense}}
#'      
#'      \code{\link{wash}}
#'          
#'      \code{\link{gwl_flush}}
#'      
#'      \code{\link{gwl_break}}
#'      
#'      \code{\link{distribute}}
#'
#'@section Advanced Worklist methods:
#'      
#'      \code{\link{adv_aspirate}}
#'      
#'      \code{\link{adv_dispense}}
#'      
#'      \code{\link{adv_gwl_comment}}
#'
#'      \code{\link{adv_wash}}
#'
#'      \code{\link{adv_mix}}
#' 
#'      \code{\link{moveLiHa}}
#' 
#'      \code{\link{moveMCA}}
#' 
#'      \code{\link{MCAvector}}
#' 
#'      \code{\link{MCArelative}}
#'
#'      \code{\link{startTimer}}
#'
#'      \code{\link{waitTimer}}
#'
#'      \code{\link{sterile_wash}}
#' 
#'      \code{\link{fwCommand}}
#'
#'@section Worklist check methods:
#' 
#'      \code{\link{read_gwl}} - read in a gwl file
#'
#'      \code{\link{gwlToTable}} - produces a list of markdown tables with images
#'
#'      \code{\link{gwlToHTML}} - produces a HTML report of the worklist
#'
#'@section Reader methods:
#' 
#'      \code{\link{readInfiniteXML}} - returns data.frame, multiple reads
#'
#'@section Pickolo methods:
#' 
#'      \code{\link{readColonies}} - in which wells was there growth?
#'
#'      \code{\link{plotColonies}} - returns a ggplot plot of the plate & colonies
#'      
#'@section Data:
#'
#'      \code{\link{basic}} - basic worktable layout at ETHZ as of 27.01.2015
#'
#'      \code{\link{ansi_table}} - look up table for ANSI (Windows-1252) characters (used in \code{\link{ANSIIFromWells}})
#'
#' @name rrobot
#' @docType package
NULL


