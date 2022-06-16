## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "sdm",
  description = "This modules supplies a SpaDES simulation with pre-populated rasters for disturbances. Useful for constraining the stochasticity in a model from fires or other disturbance types.",
  keywords = c("distrubance", "static", "fire"),
  authors = structure(list(list(given = "Christopher", family = "Mallon", role = c("aut", "cre"), email = "chris.mallon@gov.ab.ca", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(sdm = "0.0.1.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "sdm.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.0.10)", "ggplot2"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("disturbance_directory", "character", "", NA, NA,
                    "Path to the directory where the disturbance rasters are stored."),
    defineParameter("base_file_name", "character", "", NA, NA,
                    "The file name of the disturbance rasters. Basically, a string that precedes the numeric year."),
    defineParameter("file_name_end", "character", "", NA, NA,
                    "The part of the file name that comes after the numeric year. Should include the file type, for example, '.tif'.")

  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.sdm = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, 1, "sdm", "burn", 2.5) # How to say "first year"?
    },

    burn = {
      sim <- burn(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "sdm", "burn", 2.5)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}


Init <- function(sim) {
  sim$fireSizes <- list()
  sim$rstCurrentBurn <- raster(sim$fireReturnInterval) ## creates no-value raster
  sim$rstCurrentBurn[] <- 0L
  return(invisible(sim))
}

burn <- function(sim) {
  sim$rstCurrentBurn <- load_disturbance_raster(P(sim)$disturbance_directory, P(sim)$base_file_name, time(sim))
  sim$fireSizes <- add_list_of_fire_sizes(sim$fireSizes, sim$rstCurrentBurn)
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  return(invisible(sim))
}

