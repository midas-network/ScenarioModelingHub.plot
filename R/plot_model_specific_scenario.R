#'
#'
#'
#'
#' @importFrom stringr str_extract
#' @import data.table
#' @importFrom lubridate period
get_model_specific_trend_data <- function(model_data, location, rtab,
                                          target_type, selected_model) {

  rndnum = as.integer(str_extract(rtab, "\\d+"))

  # Gold standard data
  targets <- grep(target_type, names(getOption("gs_targets")), value = TRUE)

  m_df <- model_data[
    location_name==location & outcome %in% targets &
      model_name == selected_model &
      quantile %in% c(NA, 0.025, 0.25, 0.75, 0.975)][
      order(scenario_id, outcome, target_wk)]

  # could be zero rows, if this model doesn't have this state, or this outcome, or points
  if(nrow(m_df)==0) return(NULL)

  # dcast wide
  m_df <- dcast(m_df, outcome+scenario_id+target_end_date~quantile, value.var = "value")
  setnames(m_df, old="NA", new="value")

  min_model_date <- m_df[,min(target_end_date)]
  max_model_date <- m_df[,max(target_end_date)]

  pred_date = as.Date(min_model_date)-7

  # filter the gold standard data, based on 26 wks prior to the pred_date
  if (!is.null(gs_data)) {
    g_df <- lapply(gs_data[targets], function(x) {
      if (dim(x)[1]>0 & !(any(grepl("quantile",names(x))))) {
        x[geo_value_fullname==location &
            time_value > (pred_date - lubridate::period(4, "month")) &
            time_value <= max_model_date ]
      } else if (any(grepl("quantile",names(x)))) {
        x <- x[-(1:dim(x)[1]), .(time_value, fips, geo_value_fullname, value)]
      } else {
        x
      }
    })
    g_df <- rbindlist(g_df, idcol="outcome")
  } else {
    g_df <- NULL
  }

  return(list("m_df" = m_df, "g_df" = g_df))

}

#'
#'
#' @importFrom ggplot2 ggplot geom_line aes geom_point facet_wrap
#' @importFrom ggplot2 ggplot scale_color_manual labs theme element_text
#' @importFrom ggplot2 scale_y_continuous ggtitle
#' @importFrom scales comma
#' @export
create_model_specific_ggplot <- function(model_data, location, rtab, target_type, model_name) {

  pl_input <- get_model_specific_trend_data(model_data, location, rtab, target_type,model_name)

  # This can be null (see above)
  if(is.null(pl_input)) return(NULL)

  # We want the faceting variable to be ordered correctly
  facet_levels <- grep(target_type, names(getOption("model_targets")), value = TRUE)

  r = as.numeric(str_extract(rtab,"\\d+"))


  ggplot() +
    geom_line(data=pl_input[["m_df"]], aes(x=target_end_date, y=value, color=scenario_id), size=1.5) +
    geom_point(data=pl_input[["g_df"]], aes(x=time_value, y=value), color="black", size=2.5) +
    #geom_line(data=pl_input[["g_df"]], aes(x=time_value, y=value), color="black", size=1.2) +
    facet_wrap(~factor(outcome, levels=facet_levels), scales="free_y") +
    scale_color_manual(labels = scen_info[rnd_num==r, scen_title], values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
    labs(x="", y="Projected Outcomes", color="") +
    theme(legend.position = "bottom",
          text=element_text(size = 16)
          ) +
    scale_y_continuous(label=scales::comma) +
    ggtitle(paste0(
      "Model Specific Projections, by Scenario - Round ", r , " - ", location))


}
#' @importFrom plotly subplot layout
#' @export
create_model_specific_plotly <- function(model_data, location, rtab,
                                         target_type, model_name, pi=0) {

  # Create the plot data
  pl_input <- get_model_specific_trend_data(model_data, location, rtab,
                                            target_type,model_name)

  # This can be null (see above)
  if(is.null(pl_input)) return(NULL)

  # We want the faceting variable to be ordered correctly
  facet_levels <- grep(target_type, names(getOption("model_targets")),
                       value = TRUE)

  # Get the round number
  r = as.numeric(str_extract(rtab,"\\d+"))

  # add the trace names
  pl_input[["m_df"]] <- pl_input[["m_df"]][
    scen_info[rnd_num==r, .(scenario_id, scen_title)],
    on=.(scenario_id)]


  # change the names of lower and upper if pi!0
  if(pi!=0) {
    if(pi==50) oldnames = c("0.25", "0.75")
    else oldnames = c("0.025", "0.975")
    setnames(pl_input[["m_df"]],old=oldnames, new=c("lower", "upper"))
  }

  avail_outcomes <- intersect(facet_levels, unique(pl_input[["m_df"]]$outcome))

  subplots <- lapply(seq_along(avail_outcomes), function(x) {

    if (is.null(pl_input[["g_df"]])) {
      g_df_plot <- NULL
    } else {
      g_df_plot <- pl_input[["g_df"]][outcome==avail_outcomes[x]]
    }

    outcome_subplot(pl_input[["m_df"]][outcome==avail_outcomes[x]],
                    g_df_plot,
                    outcome==avail_outcomes[x],
                    model_legend = x ==1,
                    pi=pi
    )
  })

  # Make some subplot annotations (titles)
  outcome_annotations <- lapply(seq_along(avail_outcomes), function(outc) {
      list(x =(outc-1)/length(avail_outcomes),
           y = 1.05,
           text = paste0("<b>", avail_outcomes[outc], "</b>"),
           font=list(size=12), showarrow = F, xref='paper', yref='paper'
           )
    })


  # join the subplots
  p <- subplot(subplots, shareX = TRUE, shareY=FALSE) %>%
    layout(
      annotations = outcome_annotations,
     # height=1050,
      margin=0.1,
      title = list(
        text = paste0(
          "Model Specific Projections, by Scenario - Round ", r , " - ", location
          ),
        ref = "container", yref="container", x=0, y=0.98, xanchor="left",
        pad =list(b=30)
        ),
      yaxis=list(title = "Projected Outcomes")
    )

  return(p)

}

#'
#'
#' @importFrom plotly plot_ly add_trace add_ribbons
outcome_subplot <- function(m_df, g_df, outcome_name, model_legend=F, trace_names=NULL, pi=0) {




  p <- plot_ly(height = 1050) %>%
    # add the model data, using the model_legend to turn legend on or off
    add_trace(data=m_df,
              x=~target_end_date,
              y=~value,
              color=~scen_title,
              colors = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
              type="scatter",
              mode="lines",
              line=list(width=4),
              showlegend = model_legend,
              legendgroup = ~scen_title
              )

  if (!is.null(g_df)) {
    p <- p %>%
      # add the gold standard black markers
      add_trace(data=g_df,
                x=~time_value,
                y=~value,
                type="scatter",
                mode="markers",
                marker=list(color='black'),
                showlegend=F)
  }

  # Now, if pi !=0, we need to add the uncertainty level trace
  if(pi !=0) {
    p <- p %>% add_ribbons(
      data=m_df,
      x=~target_end_date,
      ymin =~lower,
      ymax=~upper,
      color=~scen_title,
      colors = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
      showlegend=F,opacity=0.4, connectgaps=TRUE,
      legendgroup =~scen_title)
  }

  p <- p %>%
    # add the layout
    layout(
      xaxis = list(title=""),
      yaxis=list(rangemode = "nonnegative"),
      legend=list(
        orientation="h"
      )
    )

  return(p)
}



