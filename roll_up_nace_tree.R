# Function: roll_up_hierarchy --------------------------------------------------------------------------

# Purpose: This function can be used to aggregate a NACE or SBI code economic activity tree, 
#          so the codes represent enough of something for example number of companies, 
#          number of customers or revenue.

# Arguments:
#   * tbl_hierarchy - a data frame that should contain the entire NACE or SBI hierarchy with a 
#       quantity or amount. It should contain the following columns:
#         - code        - a NACE or SBI code.
#         - code_parent - a NACE or SBI code that refers to the direct parent code.
#         - layer_no    - an integer indicating the hierarchie's level, 1 being the top level
#         - value       - the quantity that is used to evaluate whether the code should be
#                         kept in place or should be 'pushed up' the hierarchy.
#
#   * threshold - The minimum value that the tbl_hierarchy value column should have to leave a  
#       code in it's place.

# Return value: a data frame containing
#   * code      - The original code which was input.
#   * code_new  - The replacement code, which indicates the new position in the hierarchy
#   * value     - The value associated with the original code (input)
#
roll_up_hierarchy <- function(tbl_hierarchy, threshold) {
  
  # End-points of the roll-up 
  tbl_hierarchy_aggr <- data_frame(code = as.character(),
                                   code_new = as.character(),
                                   value = as.integer())
  
  # Codes that are pushed up the tree (code has too low value to be end-point)
  tbl_level_migrating <- data_frame(code = as.character(),
                                    code_parent = as.character(),
                                    value = as.integer())
  
  # Iterator for the levels in the hierarchy, from bottom to top
  levels <- sort(unique(tbl_hierarchy$layer_no), decreasing = TRUE)
  
  for (i in levels[-length(levels)]) {
    
    # Get the current level  
    tbl_level <- tbl_hierarchy %>%
      filter(layer_no == i)  %>%
      mutate(code_orig = code) %>% 
      select(code, code_parent, code_orig, value)
    
    # Make previous level codes have parent code of their parents
    tbl_level_prev <- tbl_level %>%
      select(-value, -code_orig) %>%
      inner_join(tbl_level_migrating, by = c("code" = "code_parent")) %>%
      rename(code_orig = code.y) 
    
    # Calculate this level's and previous level values per code
    tbl_up_or_down <- rbind(tbl_level, tbl_level_prev) %>%
      group_by(code, code_parent) %>%
      mutate(value_code = sum(value, na.rm = TRUE)) %>%
      ungroup() 
    
    # Identify the codes that have a sufficient quantity to stay at the current level
    tbl_stays <- tbl_up_or_down %>%
      filter(value_code >= threshold & value > 0) %>%
      mutate(code_new = code,
             code = code_orig) %>%
      select(code, code_new, value)
    
    # Add the codes that stay behind to the result table
    tbl_hierarchy_aggr <- rbind(tbl_hierarchy_aggr, tbl_stays)
    
    # Gather the codes that need to be moved up further
    tbl_level_migrating <- tbl_up_or_down %>%
      filter(value > 0 &
               value_code < threshold & value_code > 0) %>%
      mutate(code = code_orig) %>%
      select(code, code_parent, value) 
    
  }
  
  # Handling the top level
  # Get the current level  
  tbl_level <- tbl_hierarchy %>%
    filter(layer_no == i - 1 & value > 0)  %>%
    mutate(code_new = code) %>% 
    select(code, code_new, value)
  
  # Place all data that hasn't been placed yet on the top level
  tbl_remainder <- rbind(tbl_level,
                         tbl_level_migrating %>% 
                           rename(code_new = code_parent))
  
  tbl_hierarchy_aggr <- rbind(tbl_hierarchy_aggr, tbl_remainder)
  
}

# Function: hierarchy_code_level ------------------------------------------------------------

# Purpose: This function can be used enrich each of the codes from it's chosen level's 
#           code value

# Arguments:
#   * tbl_hierarchy - a data frame that should contain the entire NACE or SBI hierarchy, 
#         having at least the following columns:
#         - code        - a NACE or SBI code.
#         - code_parent - a NACE or SBI code that refers to the direct parent code.
#         - layer_no    - an integer indicating the hierarchie's level, 1 being the top level
#   * level_no - an integer indicating the level, 1 being the top most level (e.g. A, B etc.)

# Return value: a data frame containing the complete tbl_hierarchy data frame, with the new column:
#   * code_level_x  - The code at the chosen level of the hierarchy
#
hierarchy_code_level <- function(tbl_hierarchy, level_no){
  
  # Codes that are pushed up the tree 
  tbl_level_stored <- data_frame(code = as.character(),
                                 code_parent = as.character())
  
  # Iterator for the levels in the hierarchy, from top to specified level
  levels <- sort(unique(tbl_hierarchy$layer_no))
  
  for (i in levels[-level_no]) {
    
    # Get the current level  
    tbl_level <- tbl_hierarchy %>%
      filter(layer_no == i)  %>%
      select(code, code_parent)
    
    # Push parent_code to the next level
    tbl_level_next <- tbl_level %>%
      left_join(tbl_level_stored, by = c("code_parent" = "code")) %>%
      mutate(code_parent = ifelse(!is.na(code_parent.y), code_parent.y, code_parent)) %>% 
      select(code, code_parent) 
    
    # Store result
    tbl_level_stored <- rbind(tbl_level_stored, tbl_level_next)
  }
  
  # Rename column for selected level code
  names(tbl_level_stored) <- replace(
    names(tbl_level_stored),
    names(tbl_level_stored) == "code_parent",
    paste0("code_level_", as.character(level_no))
  )
  
  # Add code back to original data frame
  tbl_hierarchy %<>%
    left_join(tbl_level_stored, by = "code")
}

# Function: hierarchy_code_all ------------------------------------------------------------

# Purpose: This function can be used enrich each of the codes with all of the codes from the 
#          levels above the current code

# Arguments:
#   * tbl_hierarchy - a data frame that should contain the entire NACE or SBI hierarchy, 
#         having at least the following columns:
#         - code        - a NACE or SBI code.
#         - code_parent - a NACE or SBI code that refers to the direct parent code.
#         - layer_no    - an integer indicating the hierarchie's level, 1 being the top level

# Return value: a data frame containing the complete tbl_hierarchy data frame, adding the column:
#   * code_level_x  - The code at the chosen level of the hierarchy
#

hierarchy_code_all <- function(tbl_hierarchy){
  
  # Iterator for the levels in the hierarchy, from top to specified level
  levels <- sort(unique(tbl_hierarchy$layer_no))
  for (i in levels[-1]) {
    tbl_hierarchy <- hierarchy_code_level(tbl_hierarchy, level_no = i)
  }
  return(tbl_hierarchy)
}

# Function: clean_hierarchy ------------------------------------------------------------

# Purpose: This function cleans up all nace codes from the hierarchy that contain a NA value
#          and are non-connective

# Arguments:
#   * tbl_hierarchy - a data frame that should contain the entire NACE or SBI hierarchy, 
#         having at least the following columns:
#         - code        - a NACE or SBI code.
#         - code_parent - a NACE or SBI code that refers to the direct parent code.
#         - layer_no    - an integer indicating the hierarchie's level, 1 being the top level
#         - value       - a number containing the values associated with the code

# Return value: a data frame containing only the codes with other values than NA and are non-connecting

clean_hierarchy <- function(tbl_hierarchy) {
  
  # Iterator for the levels in the hierarchy, from bottom to top
  levels <- sort(unique(tbl_hierarchy$layer_no), decreasing = TRUE)
  
  # Codes that have quantities of connective codes
  tbl_level_codes <- data_frame(code = as.character(),
                                code_parent = as.character(),
                                layer_no = as.integer(),
                                value = as.numeric(),
                                has_child = as.logical())
  
  for (i in levels) {
    
    # Get the current level  
    tbl_level <- tbl_hierarchy %>%
      filter(layer_no == i)  %>%
      select(code, code_parent, layer_no, value)
    
    # Joing previous layer to see whether codes are connective
    tbl_level_prev <- tbl_level %>%
      left_join(tbl_level_codes, by = c("code" = "code_parent")) %>%
      mutate(layer_no = i,
             has_child = !is.na(code.y)) %>% 
      rename(value = value.x) %>% 
      group_by(code, code_parent, layer_no, value) %>% 
      summarise(has_child = max(has_child)) %>% 
      ungroup() 
    
    # Remove codes that have no value and are not connective
    tbl_level_prev %<>%
      filter(has_child == 1 | (!is.na(value) & value != 0))
    
    tbl_level_codes <- rbind(tbl_level_codes, tbl_level_prev)
  }
  return(tbl_level_codes)
}

# Function: plot_hierarchy ------------------------------------------------------------

# Purpose: Plotting hierarchies based on a single value, mostly for example puroposes

# Arguments:
#   * tbl_hierarchy - a data frame that should contain the entire NACE or SBI hierarchy, 
#         having at least the following columns:
#         - code        - a NACE or SBI code.
#         - code_parent - a NACE or SBI code that refers to the direct parent code.
#         - layer_no    - an integer indicating the hierarchie's level, 1 being the top level
#         - value       - a number containing the values associated with the code

# Return value: a data frame containing only the codes with other values than NA and are non-connecting

plot_hierarchy <- function(tbl_hierarchy, title = ""){
  
  library(ggraph)
  library(igraph)
  
  tbl_nodes <- data_frame(code = with(tbl_hierarchy, unique(c(code, code_parent)))) %>% 
    left_join(tbl_hierarchy, by = "code") %>% 
    mutate(value = ifelse(is.na(value), 0, value),
           layer_no = ifelse(is.na(layer_no), 0, layer_no),
           layer_no = factor(layer_no)) 
  
  tbl_links <- tbl_hierarchy %>% select(code_parent, code, everything())
  
  graph <- graph_from_data_frame(tbl_links, tbl_nodes, directed = TRUE)
  
  # Create Graph
  set.seed(42)
  p_hierarchy <- ggraph(graph, 'dendrogram', circular = TRUE) +
    geom_edge_diagonal(edge_width = 0.5, alpha = .4) +
    geom_node_point(aes(colour = layer_no, size = value), 
                    alpha = 0.4) +
    guides(col = FALSE, size = FALSE) +
    labs(title = title)
  
  rm(graph, tbl_nodes, tbl_links)
  return(p_hierarchy)
}
