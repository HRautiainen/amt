
#' Single Step Likelihood
#'
#' Calculating the likelihood of observing a single step.
#'
#' @param dk `[dispersal_kernel]` A `dispersal_kernel` returned by
#' `\link{dispersal_kernel}()`.
#' @param x2_ `[numeric]` The x-coordinate of the end point of the step.
#' @param y2_ `[numeric]` The y-coordinate of the end point of the step.
#' @param log `[logical = TRUE]` Whether the returned likelihoods should be
#' log-likelihoods.
#'
#' @details Returns the (log) likelihood of observing the step under the model
#' used to create `dk`, the (log) likelihood of observing the step under a
#' null model, and the maximum (log) likelihood under the model. The null model
#' assumes there is an equal probability of ending the step in any cell within
#' `dk$max.dist`.
#'
#' @return List of class `step_pR2` with elements `mod` for the (log) likelihood
#' of the step given the model, and `null` for the (log) likelihood of the step
#' under the null model.
#'
#' @export
step_like <- function(dk, x2_, y2_, log = TRUE){
  #Get likelihood of x2, y2
  mod <- raster::extract(x = dk$dispersal_kernel, y = cbind(x2_, y2_))
  #Get null value
  r_null_vals <- getValues(dk$dispersal_kernel)
  null <- 1/sum(!is.na(r_null_vals))
  #Get max value
  max <- max(getValues(dk$dispersal_kernel), na.rm = TRUE)
  #Compose named vector
  like <- c("mod" = mod, "null" = null, "max" = max)
  #Apply log() if necessary
  if (log){
    like <- log(like)
  }
  #Return
  return(like)
}

#' Calculate log-likelihood for an (i)SSF
#'
#' Calculates the log-likelihood of all observed steps from a fitted (i)SSF.
#'
#' @param object `[fit_clogit]` A fitted (i)SSF.
#' @param data `[data.frame]` The data used to fit `model`.
#' @param steps `[character or numeric]` The observed steps for which
#' log-likelihood should be calculated. Either a character string specifying how
#' the function should choose the steps ("all", "sample") or a vector of the row
#' numbers of `data` that should be used. See details.
#' @prop `[numeric = 0.1]` If `steps = "sample"`, the proportion of observed
#' steps to sample from `data`. Ignored if `steps != "sample"`.
logLik.fit_clogit <- function(object, data, steps = "sample", prop = 0.1){
  #Check inputs
  if (!inherits(object, "fit_clogit")){
    stop("'object' must be of class 'fit_clogit'")
  }
  if (!inherits(data, "data.frame")){
    stop(paste("'data' must inherit class 'data.frame' and should be",
               "the dataset used to fit 'object'."))
  } else {
    #If it passes, get the maximum number of steps (the # observed)
    max_steps <- sum(data$case_)
  }

  #Check and process 'steps'
  if (is.character(steps)){
    if (steps == "all"){
      steps <- which(data$case_)
    } else {
      if (steps == "sample"){
        if (prop <= 0 | prop > 1){
          stop("Argument 'prop' must satisfy 0 < prop <= 1.")
        }
        num_samp <- floor(prop * max_steps)
        steps <- sample(which(data$case_), size = num_samp, replace = FALSE)
      } else {
        stop("If 'steps' is a character string, it must be 'all' or 'sample'.")
      }
    }
  } else {
    if (is.numeric(steps)){
      if (max(steps) > max_steps){
        stop("The argument 'steps' is greater than the number of observed steps.")
      }
    } else {
      stop("The argument 'steps' must be a character string or a numeric vector.")
    }
  }

  #Subset the steps for which to calculate log-likelihood
  dat_sub <- data[steps, ]


}
