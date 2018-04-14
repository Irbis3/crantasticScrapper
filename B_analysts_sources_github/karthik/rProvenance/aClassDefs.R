#
# We want to allow having a combination of task classes, e.g.  AssignmentTask and LatticePlotTask
# Perhaps use S3 classes to express this for simplicity.
#
#
setOldClass(c("POSIXct", "POSIXt"))
setClass("Task",
          representation(expression = "ANY", # should be language
                         status = "logical",
                         time = "POSIXct",
                         visible = "logical"
                         ),
         prototype = list(status = TRUE))

setClass("PlotTask",
          contains = "Task")
setClass("GRZPlotTask",
          contains = "PlotTask")
setClass("LatticePlotTask",
           contains = "PlotTask")

setClass("AssignTask",
          representation( symName = "name"),
          contains = "Task")

setClass("SubAssignTask",
          representation(subExpr = "language"),
                         contains = "AssignTask")


setClass("ProvenanceTask",
          contains = "Task")

