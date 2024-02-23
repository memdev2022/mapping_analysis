# This function filters entries

PN_FilterEntry <- function(df,version){
  
  cat("Example \n+ Year > 2018 & `Task type 2` == 1 \n+ Entry_ID %in% c('Richmond_2013, Task_2', 'Schlichting_2017, Task_3')")
  if (version=='trend') {ans.default="`Year` > 2018 & `Task type 2` == 1"}
  else if (version=='compare') {ans.default="`Entry_ID` %in% c('Richmond_2011, Task_2', 'Schlichting_2017, Task_3')"}
  {
    # filter_string <- readline("Enter your inclusion criteria >>> ")
    filter_string <- dlg_input("Enter your inclusion criteria", default=ans.default)$res
    }
  
  df.filtered <- df %>%
    filter(!! rlang::parse_expr(filter_string))
  
  if (nrow(df.filtered)==0) {msg_box('Warning: No entry matches your criteria')}
  
  return(df.filtered)
  }