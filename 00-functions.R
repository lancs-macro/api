
library(rlang)
library(ggplot2)

NULL_plot <- function(n = 1, .size  = 5) {
  text <- "The series does not exhibit exuberant behavior"
  np <- list(length = n)
  for (i in 1:n) {
    np[[i]] <- ggplot() + 
      annotate("text", x = 4, y = 25, size = .size, label = text) +
      theme_void()
  }
  if (n > 1) np else np[[1]] 
}

# Custom Labels  ----------------------------------------------------------


extract_yq <- function(object) {
  yq <- object %>% 
    select_if(lubridate::is.Date) %>% 
    setNames("Date") %>% 
    mutate(Quarter = lubridate::quarter(Date),
           Year = lubridate::year(Date)) %>% 
    tidyr::unite(labels, c("Year", "Quarter"), sep = " Q") %>% 
    rename(breaks = Date)
}

custom_date <- function(object, variable, div) {
  yq <- extract_yq(object)
  seq_slice <- seq(1, NROW(yq), length.out = div)
  yq %>% 
    slice(as.integer(seq_slice)) %>% 
    pull(!!parse_expr(variable))
}

scale_custom <- function(object, div = 7) {
  require(lubridate)
  scale_x_date(
    breaks = custom_date(object, variable = "breaks", div = div),
    labels = custom_date(object, variable = "labels", div = div)
  )
}


# Plot Normal Series ------------------------------------------------------

my_theme <- theme_light() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank() ,
    panel.grid.major = element_line(linetype = "dashed")
  )


plot_var <- function(.data, .var, custom_labels = TRUE, rect = FALSE, 
                     rect_data = NULL, div = 7) {
  g <- .data %>% 
    # mutate(last_obs = ifelse(row_number() > nrow(.) - 1, TRUE, FALSE)) %>% 
    ggplot(aes_string("Date", as.name(.var))) +
    geom_line(size = 0.8) + 
    my_theme +
    theme(axis.title.y = element_blank())
  
  if (rect) {
    g <- g +  geom_rect(
      mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
      data = rect_data, inherit.aes =FALSE,
      fill = "grey70", alpha = 0.55
    )
  }
  
  if(custom_labels){
    g <- g + scale_custom(object = .data, div = div)
  }
  g
}

growth_rate <- function(x, n  = 1) (log(x) - dplyr::lag(log(x), n = n))*100

plot_growth_var <- function(.data, .var, rect_data) {
  
  .data <- .data %>% 
    mutate_at(vars(-Date), growth_rate) %>% 
    tidyr::drop_na()
  
  q75 <- apply(.data[,-1], 1, quantile, 0.25)
  q25 <- apply(.data[,-1], 1, quantile, 0.75)
  suppressWarnings({
    .data %>% 
      ggplot(aes_string("Date", as.name(.var))) +
      geom_rect(
        mapping = aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
        data = rect_data, inherit.aes=FALSE, fill = "grey70", alpha = 0.55)+
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#174B97", alpha = 0.5) +
      geom_line(size = 0.8) + 
      ylab("% Quarter on Quarter") +
      my_theme +
      scale_custom(object = .data)
  })
}

# Autoplot radf objects ---------------------------------------------------


analysis_theme <- theme_light() +
  theme(
    title = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank() ,
    panel.grid.major = element_line(linetype = "dashed")
  )

autoplot_var <- function(radf_var, cv_var, input, custom_labels = TRUE) {
  g <- exuber::autoplot(radf_var, cv = cv_var, include = TRUE, select = input) + 
    analysis_theme
  
  g$layers[[1]]$aes_params$size <- 0.8
  
  if(custom_labels){
    g <- g + scale_custom(object = fortify(radf_var, cv = cv_var))
  }
  g
  
}

# Datestamp into yq
to_yq <- function(ds, radf_var, cv_var){
  idx <- tibble(Date = index(radf_var, trunc = FALSE))
  index_yq <- extract_yq(idx)
  
  ds_yq <- function(ds) {
    start <- ds[, 1]
    start_ind <- which(index_yq$breaks %in% start)
    start_label <- index_yq[start_ind ,2]
    
    end <- ds[, 2]
    end_ind <- which(index_yq$breaks %in% end)
    if (anyNA(end)) end_ind <- c(end_ind, NA)
    end_label <- index_yq[end_ind ,2]
    
    ds[, 1] <- start_label 
    ds[, 2] <- end_label
    ds
  }
  
  ds %>% 
    ds_yq()
}

