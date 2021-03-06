% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procedures.R
\name{tiptouch}
\alias{tiptouch}
\title{Tip Touch}
\usage{
tiptouch(
  tip_mask,
  wells,
  rack,
  ncol = 24,
  nrow = 16,
  spacing = 2,
  speed_move,
  speed_in,
  speed_out
)
}
\arguments{
\item{tip_mask}{tip mask for source}

\item{wells}{Wells source}

\item{rack}{RackLabel of source}

\item{ncol}{integer, number of colums (left to right on worktable) of labware}

\item{nrow}{integer, number of rows (back to front on worktable) of labware}

\item{spacing}{integer 1 -  .. tip spacing, must be 2 for 384 well plates}

\item{speed_move}{movement speed up to z-dispense}

\item{speed_in}{movement speed when dipping}

\item{speed_out}{movement speed when retracting}
}
\description{
Transfering by tiptouching using the tips
}
\seealso{
Other advanced worklist procedures: 
\code{\link{evowait}()},
\code{\link{iPTdeactivate}()},
\code{\link{iPTdip}()},
\code{\link{sterile_wash_TT}()},
\code{\link{sterile_wash_c}()},
\code{\link{sterile_wash_phage}()},
\code{\link{sterile_wash}()},
\code{\link{washPinTool}()}
}
\concept{advanced worklist procedures}
