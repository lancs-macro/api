
for(ctry in series_names(radf_rhpi)) {
  aplt <- plotly::ggplotly(autoplot(radf_rhpi, mc_cv, select_series = ctry))
  htmlwidgets::saveWidget(aplt, paste0("public/visualizations/", ctry, ".html"))
}


for(i in series_names(radf_rhpi)) {
  aplt_pti[[i]] <- autoplot(radf_rhpi, mc_cv, select_series = i)
}