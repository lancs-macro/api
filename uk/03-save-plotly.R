
snames <- series_names(radf_rhpi)
file_snames <- janitor::make_clean_names(snames)
for(i in 1:length(snames)) {
  aplt <- plotly::ggplotly(autoplot(radf_rhpi, mc_cv, select_series = snames[i]))
  htmlwidgets::saveWidget(
    widget = plotly::partial_bundle(aplt), 
    file = paste0("public/visualizations/", file_snames[i], ".html"),
    selfcontained = TRUE,
    libdir = "libs",
    selfcontained = FALSE,
    title = snames[i]
  )
}


for(i in series_names(radf_rhpi)) {
  aplt_pti[[i]] <- autoplot(radf_rhpi, mc_cv, select_series = i)
}