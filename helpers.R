#as this project was created as an assignment for a course, this helper file was provided to students for use in their assignments and projects.
make_explainer_obj <- function(fitted_workflow){
  fitted_model <-
    fitted_workflow %>% 
    extract_fit_parsnip() # <- parsnip model_fit object
  
  feature_data <- 
    fitted_workflow %>% 
    extract_mold() %>% 
    pluck("predictors") 
  
  outcome_data <- 
    fitted_workflow %>% 
    extract_mold() %>% 
    pluck("outcomes") %>% 
    pluck(1)    # <- is is a 1D df, make it a vector
  
  vip_features <- 
    explain_tidymodels(
      fitted_model, 
      data = feature_data, 
      y = as.numeric(as.character(outcome_data))
    )
  
  return(vip_features)
}


ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name, 
                      "after permutations\n(higher indicates more important)")
  
  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% 
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    group_by(variable) %>% 
    mutate(
      M = mean(dropout_loss),
      min_loss = perm_vals %>% pull(dropout_loss)
    ) %>% 
    ggplot(aes(dropout_loss, variable)) 
  if(length(obj) > 1) {
    p <- p + 
      facet_wrap(vars(label)) +
      geom_segment(aes(xend = min_loss, x = M, yend = variable ), linewidth = 8, color = "dodgerblue2") +
      geom_boxplot(aes(color = label, fill = label), alpha = 1, coef = 500, width = .33)
  } else {
    p <- p + 
      geom_segment(aes(xend = min_loss, x = M, yend = variable ), linewidth = 8, color = "dodgerblue2") +
      geom_boxplot(fill = "dodgerblue4", color = "dodgerblue4", alpha = 1, coef = 500, width = .33) +
      scale_x_continuous(limits = c(perm_vals %>% pull(dropout_loss), NA))
    
  }
  p +
    theme(legend.position = "none") +
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL)
}

ggplot_pdp <- function(obj, x) {
  
  p <- 
    as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>% 
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(color = "midnightblue", linewidth = 1.2, alpha = 0.8)
  
  p
}

ggplot_ice <- function(obj, x, center = TRUE) {
  agg_d <- as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_"))
  
  first_y <- agg_d %>% summarize(first_y = first(`_yhat_`)) %>% pull(first_y)
  lines_data <- as_tibble(obj$cp_profiles) %>% 
    group_by(`_ids_`) %>%
    mutate(firsts = first(`_yhat_`))
  if (center) {
    agg_d <- mutate(agg_d, `_yhat_` = `_yhat_` - first_y)
    lines_data <- lines_data %>% 
      mutate(`_yhat_` = `_yhat_` - firsts)
  }
  
  p <-
    agg_d %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = lines_data,
              aes(x = {{ x }}, group = `_ids_`),
              linewidth = 0.5, alpha = 0.1, color = "gray50")
  
  num_colors <- n_distinct(obj$agr_profiles$`_label_`)
  
  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), linewidth = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", linewidth = 1.2, alpha = 0.8)
  }
  
  return(p)
}

pdp_2way <- function(fitted_workflow, x1, x2, grid_steps = 64, N = 100, custom_range = NULL){
  var1 <- enquo(x1)
  x1_var_name <- quo_name(var1)
  
  var2 <- enquo(x2)
  x2_var_name <- quo_name(var2)
  
  d <- fitted_workflow %>% 
    extract_mold() %>% 
    pluck("predictors")
  
  if (is.null(custom_range)){
    grid <- crossing(
      !! x1_var_name := seq(min(pull(d, !!var1)), max(pull(d, !!var1)), length.out = grid_steps),
      !! x2_var_name := seq(min(pull(d, !!var2)), max(pull(d, !!var2)), length.out = grid_steps)
    )
  } else {
    grid <- crossing(
      !! x1_var_name := seq(custom_range[1,1], custom_range[2,1],  length.out = grid_steps),
      !! x2_var_name := seq(custom_range[1,2], custom_range[2,2], length.out = grid_steps)
    )
  }
  
  
  pred_df <-  d %>%
    sample_n(N) %>%
    mutate(.id = 1:n()) %>%
    select(-c(!!var1, !!var2)) %>%
    cross_join(grid)
  
  p <- extract_fit_parsnip(fitted_workflow) %>%
    augment(pred_df) %>%
    group_by(!!var1, !!var2) %>%
    summarize(m = mean(.pred_0)) %>%
    ggplot(aes(x = !!var1, y = !!var2, fill = m)) +
    geom_raster(interpolate = TRUE) +
    # geom_tile(linewidth=0.0) +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  return(p)
  
}
