# SunCable Wind and Solar farm modelling

This repository contains efforts to model the Wind and Solar outputs for the
SunCable project.


## Notes regarding the data

The raw data are in the Darwin timezone.

The times in the files refer to the **start** of a 1-hour time period.

The wind data look like they refer to instantaneous **times**, giving the power
output at the reported time.  However, we were instructed to treat the data as
**time periods**, with the reported time being the start time of each period.
This makes the times being reported the same for both data files.
