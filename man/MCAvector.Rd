% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/advGwlFunctions.R
\name{MCAvector}
\alias{MCAvector}
\title{MCA96 Vector command}
\usage{
MCAvector(
  move_vector,
  grid,
  site,
  direction = 0,
  back = 1,
  beginAction = 2,
  endAction = 2,
  speed = 1,
  arm = 0
)
}
\arguments{
\item{move_vector}{string, the name of the vector}

\item{grid}{integer, labware location grid}

\item{site}{integer, labware locatation site STARTING WITH 1 at the back NOT 0!}

\item{direction}{integer 0 or 1, 0 = from safe to end, 1 from end to safe}

\item{back}{integer 0 or 1, 0 = don't move back to safe, 1 = move back to safe}

\item{beginAction}{Gripper action at safe, O = open, 1 = close, 2 = do not move}

\item{endAction}{Gripper action at end, O = open, 1 = close, 2 = do not move}

\item{speed}{integer 0 or 1, 0 = max. speed, 1 = speed as defined in vector teaching}

\item{arm}{integer 0 - 1, number of the MCA96 performing the action:
0 = MCA 1=MCA 2, should always be 0}
}
\description{
execute MCA vector
}
\seealso{
Other advanced worklist commands: 
\code{\link{MCArelative}()},
\code{\link{adv_aspirate}()},
\code{\link{adv_dispense}()},
\code{\link{adv_gwl_comment}()},
\code{\link{adv_mix}()},
\code{\link{adv_wash}()},
\code{\link{fwCommand}()},
\code{\link{moveLiHa}()},
\code{\link{moveMCA}()},
\code{\link{startTimer}()},
\code{\link{waitTimer}()}
}
\concept{advanced worklist commands}
