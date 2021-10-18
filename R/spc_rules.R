# Code to add rule break columns to a dataframe x
# Assumes the following columns in x:
# y   : the measure
# cl  : the centre line
# lcl : the lower control limit
# ucl : the upper control limit

add_rule_breaks <- function(x, controlChart = TRUE, shift_threshold = 8,
                            trend_threshold = 6) {
  if(controlChart) {
    x$sigma_rule <- (x$y > x$ucl) | (x$y < x$lcl)
    
    x <- x %>%
      dplyr::mutate(ll1 = cl - (1/3)*(cl - lcl),
                    ll2 = cl - (2/3)*(cl - lcl),
                    ul1 = cl + (1/3)*(ucl - cl),
                    ul2 = cl + (2/3)*(ucl - cl),
                    z = (y - cl)/((ucl - cl)/3),
                    zone = sign(z)*ceiling(pmin(abs(z), 4))
      )
    
    x <- rule_four(x)
    x <- rule_five(x)
    
  } else {
    x$sigma_rule <- rep(FALSE, nrow(x))
  }
  
  x <- shift_rule(x, threshold = shift_threshold)
  x <- trend_rule(x, threshold = trend_threshold)
  
  x <- add_highlight(x, controlChart)
  x
}

shift_rule <- function(df, threshold = 8) {
  df <- df %>% mutate(y_sgn = sign(y - cl),
                      y_sgn_lag = dplyr::lag(y_sgn),
                      y_sgn_0 = dplyr::if_else(y_sgn == 0, y_sgn_lag, y_sgn))
  
  runs <- rle(df$y_sgn_0)
  df$run_number <- rep(c(1:length(runs$lengths)), runs$lengths)
  eff_run_lengths <- df %>% group_by(run_number) %>%
    summarise(effective_length = abs(sum(y_sgn)), .groups = "drop_last") %>%
    mutate(rule_breaking = effective_length >= threshold)
  runs$values <- eff_run_lengths %>% pull(rule_breaking)
  partofrun <- inverse.rle(runs)
  df$shift_rule <- partofrun
  df

}

trend_rule <- function(df, threshold = 6) {
  
  df <- df %>% mutate(y_lag = dplyr::lag(y),
                      delta_y = y - y_lag,
                      delta_sgn = sign(delta_y),
                      delta_sgn_lag = dplyr::lag(delta_sgn),
                      delta_sgn = dplyr::if_else(delta_sgn == 0, delta_sgn_lag, delta_sgn))
  
  trend_runs <- rle(df$delta_sgn)
  rulebreakingtrends <- trend_runs$lengths >= threshold #TODO: prevent rule breaks from >= thresh. NAs
  trend_runs$values <- rulebreakingtrends
  partoftrend <- inverse.rle(trend_runs)
  df$trend_rule <- partoftrend
  
  df
}

rule_four <- function(df) {
  
  df <- df %>%
    dplyr::mutate(z3p = zone == 3,
                  z3p_lead1 = dplyr::lead(z3p, n = 1L),
                  z3p_lead2 = dplyr::lead(z3p, n = 2L),
                  n_z3p = z3p + z3p_lead1 + z3p_lead2,
                  rule_four_p = case_when(n_z3p >= 2 ~ TRUE,
                                        dplyr::lag(n_z3p, n = 1L) >=2 ~ TRUE,
                                        dplyr::lag(n_z3p, n = 2L) >=2 ~ TRUE,
                                        TRUE ~ FALSE
                  ),
                  z3n = -zone == 3,
                  z3n_lead1 = dplyr::lead(z3n, n = 1L),
                  z3n_lead2 = dplyr::lead(z3n, n = 2L),
                  n_z3n = z3n + z3n_lead1 + z3n_lead2,
                  rule_four_n = case_when(n_z3n >= 2 ~ TRUE,
                                          dplyr::lag(n_z3n, n = 1L) >=2 ~ TRUE,
                                          dplyr::lag(n_z3n, n = 2L) >=2 ~ TRUE,
                                          TRUE ~ FALSE
                  ),
                  rule_four = rule_four_p | rule_four_n
    ) %>%
    dplyr::select(-z3p_lead1, -z3p_lead2,-n_z3p, -z3n_lead1, -z3n_lead2,-n_z3n,
                  -rule_four_p, -rule_four_n)
  
  df
  
}

rule_five <- function(df) {
  
  df <- df %>%
    dplyr::mutate(z1 = abs(zone) == 1)
  
  cl_runs <- rle(df$z1)
  rulebreakingclruns <- (cl_runs$lengths >= 15) & (cl_runs$values == TRUE)
  cl_runs$values <- rulebreakingclruns
  partofclrun <- inverse.rle(cl_runs)
  df$rule_five <- partofclrun
  
  df
  
}

add_highlight <- function(df, controlChart) {
  
  df$highlight <- rep("None", nrow(df))
  if(controlChart) {
    df$highlight <- ifelse(df$rule_five, "Rule Five", df$highlight)
    df$highlight <- ifelse(df$rule_four, "Rule Four", df$highlight)
  }
  df$highlight <- ifelse(df$trend_rule, "Trend", df$highlight)
  df$highlight <- ifelse(df$shift_rule, "Shift", df$highlight)
  if(controlChart) {
    df$highlight <- ifelse(df$sigma_rule, "Sigma", df$highlight)
  }
  df[is.na(df$highlight),'highlight'] <- "None"
  
  df <- df %>% dplyr::mutate(highlight = forcats::as_factor(highlight),
                             highlight = forcats::fct_expand(highlight,
                                                             c("Sigma",
                                                               "Shift",
                                                               "Trend",
                                                               "Rule Four",
                                                               "Rule Five",
                                                               "None")
                             ),
                             highlight = forcats::fct_relevel(highlight,
                                                              c("Sigma",
                                                                "Shift",
                                                                "Trend",
                                                                "Rule Four",
                                                                "Rule Five",
                                                                "None")
                             )
  )
  
  return(df)
}


format_spc_chart <- function(chart, brewer_palette_name = "Set1") {
  
  chart_data_cols <- colnames(chart$data)
  
  if("highlight" %in% chart_data_cols) {
    colpal <- setNames(c(brewer.pal(n = 5, name = brewer_palette_name),
                         "#000000"),
                       c("Sigma",
                         "Shift",
                         "Trend",
                         "Rule Four",
                         "Rule Five",
                         "None"))
  } else {
    colpal <- setNames(c(brewer.pal(n = 3, name = brewer_palette_name)[1],
                         "#000000"),
                       c(TRUE, FALSE))
  }
  
  chart_f <- chart +
    scale_linetype_manual(values = c(cl = "solid",
                                     y = "solid",
                                     lcl = "dashed", ucl = "dashed",
                                     ll1 = "dotted", ul1 = "dotted",
                                     ll2 = "dotted", ul2 = "dotted")) +
    scale_size_manual(values=c(cl = 0.5, y = 0.25,
                               ucl = 0.5, lcl = 0.5,
                               ll1 = 0.5, ul1 = 0.5,
                               ll2 = 0.5, ul2 = 0.5)) +
    scale_color_manual(values = colpal) +
    theme_classic() +
    theme(title=element_text(size=14, hjust=0.5),
          axis.title=element_text(size=12),
          axis.text = element_text(size=12),
          legend.text=element_text(size=10),
          legend.title = element_blank())
  
  return(chart_f)
}

