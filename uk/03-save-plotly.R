
snames <- series_names(radf_rhpi)
file_snames <- janitor::make_clean_names(snames)
for(i in 1:length(snames)) {
  aplt <- plotly::ggplotly(autoplot(radf_rhpi, mc_cv, select_series = snames[i]))
  htmlwidgets::saveWidget(
    widget = plotly::partial_bundle(aplt), 
    file = paste0("public/visualizations/", file_snames[i], ".html"),
    selfcontained = FALSE,
    libdir = "libs",
    title = snames[i]
  )
}

for(i in 1:length(snames)) {
  aplt <- plotly::ggplotly(autoplot(radf_pti, mc_cv, select_series = snames[i]))
  htmlwidgets::saveWidget(
    widget = plotly::partial_bundle(aplt), 
    file = paste0("public/visualizations/", file_snames[i], ".html"),
    selfcontained = FALSE,
    libdir = "libs",
    title = snames[i]
  )
}
