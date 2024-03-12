################################################################################
# Example Script for generating Worklists using R w/ rrobot                    #
#  - as an example we generate a worklists that tries to add media to each     #
#    well of a 96 well plate to achieve equal OD                               #
# usage:                                                                       #
#   Rscript.exe [params] pathToReaderXML                                       #
################################################################################
logString <- paste("Log | mkgwl_setup.R", format(Sys.time()), " ")
cat(stringr::str_pad(logString, 79, side = "right", "-"), "\n")

# install rrobot library if necessary
if (!("rrobot" %in% installed.packages())) {
  install.packages("remotes")
  remotes::install_github("dcangst/rrobot")
}

# load libraries
library(rrobot)
library(here)

# load parameter
parse_args("pathToReaderXML", arg_types = NULL)

volCulture <- 100
volMax <- 250


################################################################################
# Juggle data                                                                  #
################################################################################
pathToReaderXML <- here("ODexample.xml")
odValues <- readInfiniteXML(pathToReaderXML)$data

# fake ODs for the purposes of this script
odValues$OD595 <- sample(c(rnorm(94, 0.4, 0.05), rnorm(2, 0.04, 0.001)), 96)

pipettingData <- odValues %>%
  # discard wells with low or no growth (defined as OD595 < 0.1)
  filter(OD595 > 0.1) %>%
  # calculate volume to be added (rounded to microliters)
  mutate(
    volAdd = round((OD595 / min(OD595) - 1) * volCulture),
    ODnew = OD595 * volCulture / (volCulture + volAdd))

# discard wells that can't be diluted enough in the available volume
cat("discarding wells: ")
pipettingData %>%
  filter(volCulture + volAdd > volMax) %>%
  print(n = Inf)

pipettingData <- pipettingData %>%
  filter(volCulture + volAdd <= volMax) %>%
  # add tip number used for the well (trivial, but less so for 384 well plates, needed below)
  mutate(tip = as.integer(as.factor(row)))

# we want to flush tips after 6 dispense steps (i.e. do the whole plate in two
# sections). add this to pipetting data
nMultiDispense <- 6

pipettingData <- pipettingData %>%
  arrange(col, row) %>%
  group_by(row) %>%
  mutate(
    pipGroup = rep(1:ceiling(n() / nMultiDispense), each = nMultiDispense, length.out = n()),
    ) %>%
  group_by(pipGroup) %>%
  group_split()

################################################################################
# make worklist to add volumes                                                 #
################################################################################

# initialize worklist with reasonable defaults
init(filename = "exampleWorklist", LiquidClass = "Minimal FD trough")

# add a comment. These will be visible in EvoWare at runtime
adv_gwl_comment(paste0("Equalizing OD..."))

# write pipetting commands
pipGroups <- seq_along(pipettingData)

for (iGroup in pipGroups){
  tipVolumes <- pipettingData[[iGroup]] %>%
    group_by(tip) %>%
    summarise(
      nWells = length(tip),
      volume = sum(volAdd + 20),
      .groups = "drop"
      )

  if (!all(tipVolumes$volume < 800)) {
    rlang::abort("volume too high")
  }

  # generate the tip mask (boolean vector, 0 = tip not used, 1 = tip used) and the volume mask (volume in ul)
  masks <- generateMasks(tipVolumes$tip, tipVolumes$volume)

  # time to aspirate!

  # well aspirate from a 100ml trough at grid 29 site 1, predefined in gwl$worktable as 'trough100_mid'. Troughs have
  # 1 column and 8 rows, so the well numbers are trivial (== 1:8) for other labware see helper function rc_to_well

  wellsAspirate <- 1:8 * masks$tip_mask

  # aspirate media
  adv_aspirate(
    tipMask = masks$tip_mask,
    volumes = masks$vol_mask + 20, # aspirate additional 20ul to put right back, helps with droplets
    RackLabel = "trough100_mid",
    wellSelection = wellsAspirate,
    liquidClass = "Minimal FD trough", # could leave this out because we set a default
    ncol = 1, nrow = 8)
  # return 20ul
  adv_dispense(
    tipMask = masks$tip_mask,
    volumes = 20,
    RackLabel = "trough100_mid",
    wellSelection = wellsAspirate,
    liquidClass = "Minimal FD trough",
    ncol = 1, nrow = 8)

  # dispense in columns of plate at grid 40 site 1, predefined in gwl$worktable as 'MP3pos_mid'.
  for (icol in sort(unique(pipettingData[[iGroup]]$col))) {
    # figure out volumes
    tipVolumeCol <- pipettingData[[iGroup]] %>%
      filter(col == icol) %>%
      group_by(tip) %>%
      summarise(
        n_wells = length(tip),
        volume = volAdd,
        well = well,
        .groups = "drop")

    # make masks
    masksCol <- generateMasks(tipVolumeCol$tip, tipVolumeCol$volume)

    # write dispense command
    adv_dispense(
      tipMask       = masksCol$tip_mask,
      volumes       = c(masksCol$vol_mask, 4),
      RackLabel     = "MP3pos_mid",
      wellSelection = tipVolumeCol$well,
      ncol = 12, nrow = 8)
  }

  if (iGroup != last(pipGroups)) {
    # wash in between groups
    sterile_wash()
  }

}

write.gwl(gwl)
cat(stringr::str_pad(paste0(logString, "end "), 79, side = "right", "-"),
  "\n\n")


# checks:

gwlToHTML("exampleWorklist.gwl")
