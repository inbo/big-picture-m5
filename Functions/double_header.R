# Author: Matt Roumaya
# Found on https://www.mattroumaya.com/posts/dealing-with-doubleheadr/
# 15/01/2026

double_header <- function(x) {
  
  require(janitor)
  
  df <- as_tibble(x)
  
  keydat <- df %>%
    slice(1) %>%
    select_if(negate(is.na)) %>%
    pivot_longer(everything()) %>%
    group_by(grp = cumsum(!startsWith(name, "..."))) %>%
    mutate(value = sprintf("%s (%s)", first(name), value)) %>%
    ungroup %>%
    select(-grp)
  
  df <- df %>%
    rename_at(vars(keydat$name), ~ keydat$value) %>%
    slice(-1) %>%
    janitor::clean_names()
}