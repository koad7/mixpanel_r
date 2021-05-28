#' @export
plot.events_by_country <- function(x, y, ...){
  x %>% 
    echarts4r::e_country_names(country) %>% 
    echarts4r::e_charts(country) %>% 
    echarts4r::e_map(events) %>% 
    echarts4r::e_visual_map(events) %>% 
    echarts4r::e_animation(FALSE) %>% 
    echarts4r::e_tooltip()
}

#' @export
plot.events_by_time <- function(x, y, ...){
  x %>% 
    echarts4r::e_charts(time) %>% 
    echarts4r::e_line(events) %>% 
    echarts4r::e_animation(FALSE) %>% 
    echarts4r::e_tooltip()
}

#' @export
plot.events_by_referrer <- function(x, y, ...){
  x %>% 
    echarts4r::e_color_range(events, color) %>% 
    echarts4r::e_charts() %>% 
    echarts4r::e_cloud(referrer, events, color, shape = "circle") %>% 
    echarts4r::e_tooltip()
}
