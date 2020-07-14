##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param col.names
##' @param align
##' @param format
##' @param scale_by
##' @param caption
##' @param header1
##' @param header2
##' @param label
make_sim_tbl <- function(data, 
                         col.names, 
                         align, 
                         format = 'latex',
                         scale_by = 1,
                         caption = NULL, 
                         header1 = NULL,
                         header2 = NULL,
                         label = NULL){
  
  if(scale_by != 1) 
    caption <- paste0(caption, '. All table values are scaled by ', 
                      scale_by, ' for convenience')
  
  tbl <- data %>%
    select(-ncov) %>%
    kable(
      booktabs  = TRUE,
      escape    = FALSE,
      format    = format,
      col.names = col.names,
      align     = align,
      caption   = caption,
      label     = label
    ) %>%
    kable_styling() %>% 
    pack_rows(
      index           = table(data$ncov), 
      hline_before    = FALSE, 
      hline_after     = TRUE,
      latex_gap_space = "0.75em"
    ) 
  
  if(!is.null(header1)) tbl %<>% add_header_above(header = header1)
  if(!is.null(header2)) tbl %<>% add_header_above(header = header2)
  
  tbl
  
}
