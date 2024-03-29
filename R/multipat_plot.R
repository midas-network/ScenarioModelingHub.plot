
### Data Preparation
#'
#' @export
get_multipat_data <- function(model_data, other_data, r) {

  model_data <- model_data[model_name == unique(round_info[rnd_num == r,
                                                          ens_default]) &
                             quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95, NA)]
  model_data[,target_end_date := as.character(target_end_date)]
  date <- intersect(model_data$target_end_date, other_data$target_end_date)
  model_data <- model_data[target_end_date %in% date]
  model_data$pathogen <- getOption("pathogen")
  model_data <- model_data[, c("outcome", "scenario_id", "value", "pathogen",
                               "target_end_date", "location_name", "quantile")]
  other_data <- other_data[target_end_date %in% date &
                             quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95, NA)]
  setnames(other_data, "location", "location_name")
  tot_data <- list(model = model_data, other = other_data)
  return(tot_data)

}

# Plot function
#'
#' @export
create_multipat_plotly <- function(lst_df, location, scen_sel, scen_sel2,
                                   pi1, pi2, target, rd_num) {

  # First pathogen preparation
  if (pi1 == 0.5) {
    model_data <- lst_df$model[location_name == location & outcome == target &
                                 is.na(quantile)]
  } else {
    model_data <- lst_df$model[location_name == location & outcome == target &
                                 quantile == pi1]
  }
  # Second pathogen preparation
  if (pi2 == 0.5) {
    other_data <- lst_df$other[scenario_id == scen_sel2 &
                                 location_name == location &
                                 outcome == target & is.na(quantile)]
  } else {
    other_data <- lst_df$other[scenario_id == scen_sel2 &
                                 location_name == location & outcome == target &
                                 quantile == pi2]
  }

  # Observed Data

  obs_data <- gs_data[[target]]
  if (dim(obs_data)[1] != 0)
    obs_data <- obs_data[time_value %in% as.Date(model_data$target_end_date) &
                           geo_value_fullname == location]

  other_obs_data <- dir("../visualization/data-goldstandard/",
                        pattern = unique(other_data$pathogen),
                        ignore.case = TRUE, full.names = TRUE)
  if (length(other_obs_data) > 0 & target == "Incident Hospitalizations") {
    other_obs_data <- data.table::fread(other_obs_data)
    other_obs_data <- other_obs_data[as.Date(time_value) %in%
                                     as.Date(model_data$target_end_date)]
    other_obs_data[, pathogen := unique(other_data$pathogen)]
    cum_obs_data <- dplyr::left_join(
      obs_data, other_obs_data, by = c("time_value", "geo_value_fullname",
                                       "fips")) %>%
      dplyr::mutate(value = `value.x` + `value.y`) %>%
      dplyr::select(time_value, fips, geo_value_fullname, value) %>%
      dplyr::mutate(pathogen = paste0(stringr::str_to_title(unique(obs_data$pathogen)), " + ",
                                      stringr::str_to_title(unique(other_obs_data$pathogen))))
  } else {
    other_obs_data <- NULL
    cum_obs_data <- NULL
  }


  # Preparation Subplot
  lst_plot <- lapply(scen_sel, function(x) {

    model_data <- model_data[scenario_id == x]

    p <- plot_ly(height = 850) %>%
      layout(yaxis = list(title = target))
    if (dim(obs_data)[1] != 0) {
      obs_data[,time_value := as.Date(time_value)]
      p <- p %>% add_trace(
        data = obs_data, x = ~time_value, y = ~value, type = "scatter",
        mode = "lines+markers", legendgroup = "observed_data",
        name = paste0(stringr::str_to_title(unique(obs_data$pathogen)),
                      " Observed Data"),
        hovertemplate = paste0(stringr::str_to_title(unique(obs_data$pathogen)),
                               " Observed Data: ", '%{y:.2f}<extra></extra>'))
    }

    if (!is.null(cum_obs_data)) {
      cum_obs_data[,time_value := as.Date(time_value)]
      p <- p %>% add_trace(
        data = cum_obs_data, x = ~time_value, y = ~value, type = "scatter",
        mode = "lines+markers", legendgroup = "other observed_data",
        name = paste0(unique(cum_obs_data$pathogen), " Observed Data"),
        hovertemplate = paste0(unique(cum_obs_data$pathogen), " Observed Data: ",
                               '%{y:.2f}<extra></extra>'))
    }

    if (dim(model_data)[1] > 0) {
      model_data[,target_end_date := as.Date(target_end_date)]
      p <- p %>% add_trace(
        data = model_data, x = ~target_end_date, y = ~value, type = "bar",
        name = paste0(unique(model_data$pathogen), " (", x, ")"),
        hovertemplate = paste0(unique(model_data$pathogen), " (scen. ",
                               str_extract(x, "[[:alpha:]]"), "): ",
                               '%{y:.2f}<extra></extra>')
      )
      title <- paste0(str_to_title(unique(model_data$pathogen)), " Round ",
                      rd_num)
      annotation <- NULL
    } else {
      title <- NULL
      annotation <- list(
        list(text = paste0(
          str_to_title(getOption("pathogen")),
          " projections are not available for the target: ", target,
          ", the quantile: ", pi2, ", the scenario(s): ",
          paste(scen_sel, collapse = ", "),
          ", and the location: ", location, ". Please select other options."),
          font = list(size = 14, color = "grey"), y = -0.2,
          x =  0, xanchor = "left",
          xref = "paper", yref = 'paper', showarrow = FALSE))

    }

    if (dim(other_data)[1] > 0) {
      other_data[, target_end_date := as.Date(target_end_date)]
      p <- p %>%
        add_trace(data = other_data, x = ~target_end_date,
                  y = ~value, type = "bar",
                  legendgrouptitle = unique(other_data$pathogen),
                  legendgroup = unique(other_data$pathogen),
                  name = paste0(unique(other_data$pathogen), " (", scen_sel2 ,
                                ")"),
                  hovertemplate = paste0(
                    unique(other_data$pathogen), " (scen. ",
                    str_extract(scen_sel2, "[[:alpha:]]"), "): ",
                    '%{y:.2f}<extra></extra>')) %>%
        layout(barmode = "stack")
      if (is.null(title)) {
        title <- paste(title, paste0(str_to_title(unique(other_data$pathogen)),
                                     " Round ",
                                     str_extract(other_round[unique(
                                       other_data$scenario_id)], "[[:digit:]]+"
                                       )), sep = "")
      } else {
        title <- paste(title, paste0(str_to_title(unique(other_data$pathogen)),
                                     " Round ", str_extract(other_round[unique(
                                       other_data$scenario_id)], "[[:digit:]]+"
                                       )), sep = ", ")
      }
    } else {
      annotation <- c(
        annotation, list(
          list(text = paste0(
            str_to_title(unique(lst_df$other$pathogen)),
            " projections are not available for the target: ", target,
            ", the quantile: ", pi2, ", the scenario: ", scen_sel2,
            ", and the location: ", location, ". Please select other options."),
            font = list(size = 14, color = "grey"), y = -0.2,
            x =  0, xanchor = "left",
            xref = "paper", yref = 'paper', showarrow = FALSE)))
    }

    p <- p %>% layout(
      xaxis = list(tickangle = 45, mirror = TRUE,
                   linecolor = "black", linewidth = 0.5),
      yaxis = list(mirror = TRUE, linecolor = "black",
                   linewidth = 0.5),
      annotations = list(x = 0.5, y = 1, xref = "paper", yref = "paper",
                         xanchor = "center", yanchor = "bottom",
                         showarrow = FALSE, bgcolor = "#e0e0e0",
                         bordercolor = "#a0a0a0", font = list(size = 14),
                         text = paste("Scenario",
                                      gsub("[^[:alpha:]]", "", x), ";",
                                      gsub(" \\(.+\\)", "",
                                           grep(x, scenario_name, value = TRUE)
                                      ))))
    return(list(plot = p, title = title, annotation = annotation))

  })



  if (length(scen_sel) > 3) n_row <- 2 else n_row <- 1
  p <- subplot(purrr::map(lst_plot, "plot"), nrows = n_row,
               titleX = FALSE, titleY = FALSE)

  # change color
  col_name <- unique(unlist(purrr::map(p$x$data, "name")))
  pal <- hcl(h = seq(15, 375, length = length(col_name) + 1), l = 65, c = 100)
  for (i in 1:length(col_name)) {
    col_index <- grep(col_name[i], unlist(purrr::map(p$x$data, "name")),
                      fixed = TRUE)
    for (j in col_index) {
      p$x$data[[j]]$marker$color <- pal[i]
      p$x$data[[j]]$marker$line <- pal[i]
    }
  }

  obs_sel <- grep(paste0(stringr::str_to_title(unique(obs_data$pathogen)),
                         " Observed Data"), purrr::map(p$x$data, "name"))
  if (length(obs_sel) > 0 ) {
    for (i in obs_sel) {
      p$x$data[[i]]$marker$color <- "rgba(0,0,0,1)"
      p$x$data[[i]]$marker$line <- "rgba(0,0,0,1)"
      p$x$data[[i]]$line$color[1] <- "rgba(0,0,0,1)"
      p$x$data[[i]]$visible <- "legendonly"
      if (i != obs_sel[1]) {
        p$x$data[[i]]$showlegend <- FALSE
      }

    }
  }

  if (!is.null(cum_obs_data)) {
    cum_obs_sel <- grep(gsub("\\+", ".", paste0(unique(cum_obs_data$pathogen)),
                             " Observed Data"), purrr::map(p$x$data, "name"))
    if (length(cum_obs_sel) > 0 ) {
      for (i in cum_obs_sel) {
        p$x$data[[i]]$marker$color <- "rgba(90,90,90,1)"
        p$x$data[[i]]$marker$line <- "rgba(90,90,90,1)"
        p$x$data[[i]]$line$color[1] <- "rgba(90,90,90,1)"
        p$x$data[[i]]$visible <- "legendonly"
        if (i != cum_obs_sel[1]) {
          p$x$data[[i]]$showlegend <- FALSE
        }
      }
    }
  }

  if (dim(other_data)[1] > 0) {
    # Second pathogen legend unique (only 1 possible choice)
    sel <- grep(unique(other_data$pathogen), purrr::map(p$x$data,
                                                      "legendgroup"))
    for (i in sel[-length(sel)]) {
      p$x$data[[i]]$showlegend <- FALSE
    }
  }
  annotations <- c(unlist(unique(purrr::map(lst_plot, "annotation")), FALSE),
                   list(list(
                     text = target, font = list(size = 16), y = 0.5,
                     x = -0.05, textangle = 270, xanchor = "center",
                     xref = "paper", yref = 'paper', showarrow = FALSE)))

  x1_ticks <- x2_ticks <- x3_ticks <- x4_ticks <- TRUE
  if (length(scen_sel) > 3) {
    x1_ticks <- x2_ticks <- FALSE
  }

  y2_ticks <- y3_ticks <- y4_ticks <- FALSE
  y1_ticks <- TRUE
  if (length(scen_sel) > 3) {
    y3_ticks <- TRUE
  }

  if (length(scen_sel) > 1) {
    text_sel <- "multi-scenario selection"
  } else {
    text_sel <- paste0(scenario_dictionary[scen_sel], " (scenario: ", scen_sel,
                       ")")
  }

  title_text <- paste(
    target, " - ", location, " (",
    unique(unlist(purrr::map(lst_plot, "title"))), ")<br>", '<sub>',
    str_to_title(unique(lst_df$other$pathogen)), ': ', other_info[scen_sel2],
    " (scenario: ", scen_sel2, "), quantile:",  pi2,"", "<br>", "",
    str_to_title(unique(lst_df$model$pathogen)), ": ", text_sel,
    ", quantile:", pi1,"", "<br>", "<br></sub>", sep = "")

  # plot style
  p <- p %>%
    layout(
      title = list(font = list(size = 16), yanchor = "top",
                   xanchor = "center", x = 0.5,
                   text = title_text),
      annotations = annotations,
      margin = list(l = 100, r = 25, b =  150, t = 150),
      autosize = TRUE, showlegend = TRUE,
      legend = list(x = 0, xanchor = "left", y = -0.1,  orientation = "h",
                    font = list(size = 14), tracegroupgap = 15,
                    traceorder = "normal"),
      yaxis = list(matches = "y2", showticklabels = y1_ticks),
      yaxis2 = list(matches = "y", showticklabels = y2_ticks),
      yaxis3 = list(matches = "y", showticklabels = y3_ticks),
      yaxis4 = list(matches = "y", showticklabels = y4_ticks),
      xaxis = list(title = "", matches = "x2", showticklabels = x1_ticks),
      xaxis2 = list(matches = "x", showticklabels = x2_ticks),
      xaxis3 = list(matches = "x", showticklabels = x3_ticks),
      xaxis4 = list(matches = "x", showticklabels = x4_ticks),
      barmode = 'stack', hovermode = "x")  %>%
    config(displaylogo = TRUE, modeBarButtonsToRemove =
             c("lasso2d", "select2d", "toImage"))

  p

}
