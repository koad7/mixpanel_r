#' Transformation Map
#' 
#' Create Transformation Map as currently live on 
#' \href{https://intelligence.weforum.org/}{SIG}.
#' 
#' @param type The object to return, see value section below.
#' 
#' @return Depending on \code{type}, either a list containing 
#' \code{nodes} and \code{edges} or an object of class \code{igraph}.
#' 
#' @export
transformation_map <- function(type = c("list", "igraph")){

  type <- match.arg(type)
  edges <- Rwefsigapi::tpk_get_network(3L)

  # clean names so it's tidyeval friendly
  nms <- names(edges)
  nms <- gsub("\\.", "_", nms)
  names(edges) <- nms
  edges <- set_names(edges, nms)

  # compute clean edges
  edges <- edges %>% 
    mutate(
      clean_name_ins = clean_name(name_ins),
      clean_name_iss = clean_name(name_iss),
      clean_name_top = clean_name(name_top),
      ins_id = paste0(clean_name_ins, "_insight_area"),
      iss_id = paste0(clean_name_iss, "_key_issue"),
      top_id = paste0(clean_name_top, "_insight_area")
    )

  # distinct edges
  edges_top <- select(edges, source = ins_id, target = iss_id)
  edges_bottom <- select(edges, source = top_id, target = iss_id)
  new_edges <- bind_rows(edges_top, edges_bottom) %>% 
    count(source, target, name = "weight")

  insights_area <- select(edges, id = ins_id, name = name_ins)
  key_issues <- select(edges, id = iss_id, name = name_iss)
  nodes <- bind_rows(insights_area, key_issues) %>% 
    count(id, name, name = "occurences") %>% 
    mutate(
      level = case_when(
        grepl("insight_area$", id) ~ "insight",
        TRUE ~ "issue"
      )
    )

  if(type == "igraph")
    return(igraph::graph_from_data_frame(new_edges, vertices = nodes))

  list(nodes = nodes, edges = new_edges) %>% 
    .construct_tm()
}