
# Change Log
* **2018/04/10**
    - some restructuring, move to github.

* **2017/03/27**
    - some more tidyversification (http://www.tidyverse.org)

* **2017/03/06** **version 1.0**
    - restructuring, MCA Functions to handle new PinTool

* **2016/11/16**
    - move functions specific to the treatment strategies experiment to new package

* **2016/01/25** **version 0.6**
    - hospital exp function integrated
    - new gwl check functions to read back gwls and show in a readable format using rmarkdown. see `?rrobot`

* **2015/07/06**
    - two new methods to analyze dat from pickolo, see `?rrobot`

* **2015/03/12**
    - new reader import function 'readODfromXML3', compatible with multiple reads per plate (e.g. OD & fluorescence)

* **2015/01/27** **version 0.4**
    - adv. worklist commands only take a `Racklabel` argument. All labware has to be defined using `addToWorktable()` first.
    - `addToWorktable` now used to add Racks to Worktable. No longer called from inside functions. Instead the new function `.updateVolume` is used to add the volumes to the labware on the worktable (or subtract in case of aspirate)
    - Volumes supplied to adv. worklist commands: if 1 Volume is specified, it is used for all tips selected in `TipMask`. Supplying more than 1 but less than 8 Volumes returns an error. This is done in the function `.advVolumeString`

* **2014/12/23** **version 0.3** implemented some more adv. worklist commands (see `?rrobot` for more details); changed sterile_wash to use those; updated documentation; updated basic worktable layout to represent new layout (from 19.12.2014); also minor stuff & bugfixes.

* **2014/12/10** basic worktable layout preset: use `init(WTtemplate="basic")` to add all basic carriers as in worktable template basic on EvoWare. Alternatively you can specify a data.frame (needs to be the same format as `basic`). + bugfixes

* **2014/12/9** **version 0.2** implements advanced aspirate and advanced dispense methods. Lots of under the hood changes (mainly: moved away from writing the file on the go. Instead, a `data.frame` (named `gwl`)with the commands is created and writen to disk using `write.gwl`)

* **2014/12/6** fixed sterile wash step (all adv. worklist commands need to start with 'B;'). fixed issue with empty line as first line of created worklist. moved changelog to changelog.md

* **2014/12/5** initial implementation of sterile wash step. Uses Advanced Worklist Commands. IMPORTANT: refer to script "SUB_WashStepddH20" for worktable layout.

* **2014/12/5** added worktable generation ('accFunction.R - addtoWorklist()'): every command will add the rack & the volume to `gwl_defaults$worktable`. This is to have an overview of what kind of labware should be on deck. This could maybe be used to facilitate advanced worklists, where the labware has to be adressed using grid coordinates.
