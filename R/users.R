#' Users Country
#' 
#' Compute users most frequent country.
#' 
#' @inheritParams session_time
#' 
#' @return A \link[tibble]{tibble} of the user, the country,
#' and the number of country each user has visited SIG from,
#'  useful if there is a tie (\code{n}). 
#' 
#' @export
users_by_country <- function(data) UseMethod("users_by_country")

#' @export
users_by_country.default <- function(data){
  data %>% 
    filter(!is.na(mp_country_code)) %>% 
    count(distinct_id, mp_country_code) %>% 
    group_by(distinct_id) %>% 
    filter(n == max(n, na.rm = TRUE)) %>% 
    ungroup() %>% 
    add_count(distinct_id)
}

#' Users Client
#' 
#' Compute users most frequent client used.
#' 
#' @inheritParams session_time
#' 
#' @return A \link[tibble]{tibble} of the user, the client,
#' and the number of times each user has visited SIG from the 
#' client,useful if there is a tie (\code{n}). 
#' 
#' @export
users_by_client <- function(data) UseMethod("users_by_client")

#' @export
users_by_client.default <- function(data){
  data %>% 
    filter(!is.na(client)) %>% 
    count(distinct_id, client) %>% 
    group_by(distinct_id) %>% 
    filter(n == max(n, na.rm = TRUE)) %>% 
    ungroup() %>% 
    add_count(distinct_id)
}

#' Insights Area & Key Issues
#' 
#' Get Insights Areas/Key issues each user visited and the number of
#' events triggered on each Insights Area/Key Area.
#' 
#' @param tm Transformation map as returned by \code{transformation_map}.
#' 
#' @return A function to apply to mixpanel data.
#' 
#' @examples
#' \dontrun{
#' data(sample_data)
#' 
#' tm <- transformation_map()
#' users_by_insight_area(tm)(sample_data)
#' 
#' get_key_issues <- users_by_key_issue(tm)
#' get_key_issues(sample_data)
#' }
#' 
#' @name by_map
#' @export
users_by_insight_area <- function(tm) UseMethod("users_by_insight_area")

#' @export
#' @method users_by_insight_area mixpanel_tm
users_by_insight_area.mixpanel_tm <- function(tm){
  nodes <- tm$nodes
  function(data){
    data %>% 
      count(distinct_id, entityName, name = "events") %>% 
      inner_join(nodes, by = c("entityName" = "name"))
  }
}

#' @rdname by_map
#' @export
users_by_key_issue <- function(tm) UseMethod("users_by_key_issue")

#' @export
#' @method users_by_key_issue mixpanel_tm
users_by_key_issue.mixpanel_tm <- function(tm){
  nodes <- tm$nodes
  function(data){
    data %>% 
      count(distinct_id, secondaryEntityName, name = "events") %>% 
      inner_join(nodes, by = c("secondaryEntityName" = "name"))
  }
}