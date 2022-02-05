#' unscale predictor
#'
#' Reverse a scaling function that has been applied to a conditional effects object
#'
#' These are further details.
#'
#' @param cond_eff_obj The conditional effects object.
#' @param scaled_predictor A description of the parameter 'y'.
#' @param effect_num The number assigned to the effect in the conditional effects object.
#'
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3
#'
unscale_response <- function(cond_eff_obj,scaled_response){

  c <- attr(scaled_response,"scaled:center")
  s <- attr(scaled_response,"scaled:scale" )

  # unscale the central tendency
  cond_eff_obj[[1]]$`estimate__` <- c + s*cond_eff_obj[[1]]$`estimate__`
  cond_eff_obj[[1]]$`se__`       <- c + s*cond_eff_obj[[1]]$`se__`
  cond_eff_obj[[1]]$`lower__`    <- c + s*cond_eff_obj[[1]]$`lower__`
  cond_eff_obj[[1]]$`upper__`    <- c + s*cond_eff_obj[[1]]$`upper__`

  if(length(attr(cond_eff_obj[[1]],"points")$resp__) > 0){  # unscale the points
    attr(cond_eff_obj[[1]],"points")$resp__ <- c + s*attr(cond_eff_obj[[1]],"points")$resp__
  }
  if(length(attr(cond_eff_obj[[1]],"spaghetti")$estimate__) > 0){  # unscale the prediction lines
    attr(cond_eff_obj[[1]],"spaghetti")$estimate__ <- c + s*attr(cond_eff_obj[[1]],"spaghetti")$estimate__
  }
  return(cond_eff_obj)
}

