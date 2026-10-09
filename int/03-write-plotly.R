library(tidyverse)
library(here)
library(ihpdr)

library(plotly)
library(transx)
options(transx.display = FALSE)

diff4 <- function(x) (log(x) - dplyr::lag(log(x), n = 4L)) * 100

# reuse full_data from 01-download.R (sourced via 02-write-json.R in run_all.R)
# so plotly charts come from the same release, local or remote, as the stats
if (!exists("full_data")) {
  source(here("int", "01-download.R"))
}

main <- full_data %>%
  select(Date, country, hpi) %>%
  mutate(hpi = as.numeric(hpi)) %>%
  pivot_wider(names_from = country, values_from = hpi)

rhpi_int <- main %>%
  purrr::map_df(round, 2) %>%
  select(-`Aggregate - 2005 Fixed Weights`) %>%
  rename("Aggregate - 26 countries" = `Aggregate - Dynamic Weights`)

rhpi_int_yoy <- main %>%
  purrr::modify_if(is.numeric, diff4) %>%
  purrr::map_df(round, 2) %>%
  select(-`Aggregate - 2005 Fixed Weights`) %>%
  rename("Aggregate - 26 countries" = `Aggregate - Dynamic Weights`)


plot_index_int <- function(data, returns = FALSE, save = FALSE) {
  ret <- if (returns) ".2f%" else ".2f"
  ret_title <- if (returns) "Year on Year (%)" else "Index"
  ret_filename <- if (returns) "rhpi_yoy" else "rhpi"

  plt <- data %>%
    pivot_longer(
      `Aggregate - 26 countries`,
      names_to = "vars1",
      values_to = "vals1"
    ) %>%
    pivot_longer(
      -c(Date, vars1, vals1),
      names_to = "vars2",
      values_to = "vals2"
    ) %>%
    plot_ly(
      x = ~Date,
      y = ~vals1,
      color = ~vars1,
      type = "scatter",
      mode = "line",
      colors = "viridis"
    ) %>%
    add_trace(
      y = ~vals2,
      color = ~vars2,
      mode = "line",
      visible = "legendonly"
    ) %>%
    plotly::layout(
      title = "Nominal House Prices",
      hovermode = "x unified",
      yaxis = list(hoverformat = ret, title = ret_title),
      xaxis = list(
        type = "date",
        tickformat = "%Y-Q%q",
        title = "",
        showgrid = FALSE
      ),
      hoverlabel = list(namelength = -1),
      legend = list(orientation = "h", xanchor = "center", x = 0.5),
      annotations = list(
        x = 0.5,
        y = 1.08,
        xref = "paper",
        yref = "paper",
        text = "Click a country in the legend below to add it to the chart",
        showarrow = FALSE,
        font = list(size = 12, color = "grey40")
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("pan2d", "toggleSpikelines")
    )
  if (save) {
    htmlwidgets::saveWidget(
      widget = plotly::partial_bundle(plt),
      file = paste0("public/visualizations/int/", ret_filename, ".html"),
      selfcontained = FALSE,
      libdir = "libs"
    )
  }
  plt
}

rhpi_int %>%
  plot_index_int(returns = FALSE, save = TRUE)

rhpi_int_yoy %>%
  plot_index_int(returns = TRUE, save = TRUE)
