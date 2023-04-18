# This function plots a time series of GPP observed and simulated
# values. The simulations are done with `rsofun` and run for one (or more) sets
# of given parameters. The P-model usage vignette from `rsofun` was used as a
# starting point.

# obs: a p_model_validation object for a single site (could be NULL)
# drivers: a p_model_drivers object for a single site
# init_parameters: the initial parameter values that were used for
#                  calibration
# calib_outputs: a list of output objects from (several) calibration runs
#                (could be NULL)

plot_pmodel_gpp <- function(
    drivers,
    obs = NULL,
    init_parameters,
    calib_outputs = NULL
){
  library(ggplot2)

  # initialize ggplot object
  p <- ggplot() +
    labs(
      x = "Date",
      y = "GPP"
    )

  # get colours
  col_lines <- scales::hue_pal()(1 + length(calib_outputs))

    # observed data
  if(!is.null(obs)){
    obs_data <- obs |>
      tidyr::unnest(data)

    p <- p +
      geom_line(
        data = obs_data,
        aes(date,gpp),
        colour = "black"
      )
  }

  # format model data (before calibration)
  model_data <- rsofun::runread_pmodel_f(
    drivers = drivers,
    par = init_parameters
  ) |>
    dplyr::slice(1) |>
    tidyr::unnest(data)

  p <- p +
    geom_line(
      data = model_data,
      aes(date, gpp),
      colour = col_lines[1],
      alpha = 0.7
    )

  # simulated data
  if(!is.null(calib_outputs)){
    for(i in 1:length(calib_outputs)){
      params <- init_parameters
      for(j in names(calib_outputs[[i]]$par)){
        params[[j]] <- calib_outputs[[i]]$par[j]
      }

      df <- rsofun::runread_pmodel_f(
        drivers = drivers,
        par = params
      ) |>
        dplyr::slice(1) |>
        tidyr::unnest(data)

      p <- p +
        geom_line(
          data = df,
          aes(date, gpp),
          colour = col_lines[i+1],
          alpha = 0.7
        )
    }
  }
  p
}

# Example of use
# plot_pmodel_gpp(
#   obs = p_model_validation,
#   drivers = p_model_drivers,
#   init_parameters = params,
#   calib_outputs = list(
#     pars_calib,
#     pars_calib_likelihood,
#     pars_calib_join
#   )
# )
