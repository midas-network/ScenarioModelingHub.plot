
#' Need a function to fix colors to specific model_names
#' Note the we could have multiple ensembles, and if this is the case, we need to provide
#' specific colors for those multiple Ensembles
#'
#' @importFrom stringr str_detect
#' @export
get_model_colors <- function(m_df, ensemble_colors=NULL) {

  all_models = unique(m_df$model_name)
  models = all_models[str_detect(all_models, "Ensemble") == FALSE]
  ens = all_models[str_detect(all_models, "Ensemble")]

  if(length(ens)>1) {
    if(is.null(ensemble_colors)) {
      stop("More than one ensemble, but no ensemble_colors passed to get_model_colors", call. = F)
    }
    col_models = c(g_color_hue(length(models)), ensemble_colors)
    names(col_models) <- c(as.character(models), as.character(ens))

  }

  if(length(ens) == 1) {
    col_models = c(g_color_hue(length(models)),"#000000")
    names(col_models) <- c(as.character(models), as.character(ens))
  }

  if(length(ens) == 0) {
    col_models = g_color_hue(length(models))
    names(col_models) <- c(as.character(models))
  }

  return(col_models)

}



# Function to get default ggplot color scheme
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#'
#' @importFrom grDevices hcl
#' @export
g_color_hue <- function(n, alpha=1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]

}

# Adds title as annotation, helpful for subplot
#'
#' @importFrom plotly add_annotations
#' @export
add_annotation <- function(fig, text, x=0.01, y=1.06, yref="y domain", xref="x domain", xanchor="left", yanchor="top",
                           showarrow = FALSE, fontsize=10){

  return(fig %>% add_annotations(
    text = paste0("<b>",text,"</b>"), x=x, y=y, yref=yref,xref=xref, xanchor=xanchor,yanchor=yanchor,
    showarrow=showarrow, font=list(size=fontsize))
  )

}

# get vertical lines
#'
#' @export
vline <- function(x=0, style="solid", color="grey") {
  vline_details = list(
    type="line",
    y0=0, y1=1,yref="paper",
    x0=x, x1=x, line=list(color=color,dash=style)
  )

  return(vline_details)
}
