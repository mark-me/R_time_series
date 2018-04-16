# Function: roll_up_nace_tree --------------------------------------------------------------------------

# Purpose: This function can be used to aggregate a NACE or SIC code economic activity tree, 
#          so the codes represent enough of something for example number of companies, 
#          number of customers or revenue.

# Arguments:
#   * tbl_nace_tree - a data frame that should contain the entire NACE or SIC hierarchy with a 
#       quantity or amount. It should contain the following columns:
#         - code        - a NACE or SIC code.
#         - code_parent - a NACE or SIC code that refers to the direct parent code.
#         - qty         - the quantity that is used to evaluate whether the code should be
#                         kept in place or should be 'pushed up' the hierarchy.
#
#   * threshold - The minimum value that the tbl_nace_tree qty should have to leave a code in 
#       it's place.

# Return value: a list containing two data frames:
#   * tbl_rolled_up  - New economic activity tree with quantities and 'linking' codes to create 
#                      a new economic activity tree with only the rolled up codes
#   * tbl_dictionary - New economic activity tree, which can be used to recode a market or 
#                      customer table.
#

roll_up_nace_tree <- function(tbl_nace_tree, threshold) {
  
  # Iterator for layers, from bottom to top
  econ_activity_layers <- sort(unique(tbl_nace_tree$layer_no), decreasing = TRUE)
  
  tbl_dictionary <- NULL # Container for semantics new economic activity tree
  tbl_rolled_up <- data_frame( code = as.character(), # Container for quantity roll-up in economic activity tree
                               code_parent = as.character(),
                               layer_no = as.integer(),
                               qty = as.integer(),
                               qty_promoted = as.integer(),
                               qty_new = as.integer(),
                               qty_up = as.integer(),
                               qty_sticky = as.integer(),
                               has_sticky = as.logical())
  
  for (i in econ_activity_layers) {
    
    # Current layer in iteration
    tbl_layer <- tbl_nace_tree %>% filter(layer_no == i)
    
    tbl_dictionary <-rbind(tbl_dictionary, tbl_layer) # Add current layer to dictionary
    
    # Create 2nd order ascendancy paths of all layers in the dictionary
    tbl_dictionary_2nd <- tbl_dictionary %>%
      filter(layer_no == i) %>%
      inner_join(tbl_nace_tree, by = c("code_parent" = "code")) %>%
      select(code,
             code_parent = code_parent.y,
             layer_no = layer_no.y,
             qty = qty.x)
    
    # Add 2nd order ascendancy paths dictionary
    tbl_dictionary <- rbind(tbl_dictionary, tbl_dictionary_2nd)
    
    # Calculate propagated quantities 
    tbl_gather_ups <- tbl_rolled_up %>%
      filter(layer_no > i) %>%
      group_by(code_parent) %>%
      summarise(qty_promoted = sum(qty_up, na.rm = TRUE),
                has_sticky = max(has_sticky)) # Indicates whether code or subcodes have sticky quantities
    
    tbl_rolled_up_lyr <- tbl_nace_tree %>%
      filter(layer_no == i) %>%
      left_join(tbl_gather_ups, by = c("code" = "code_parent")) %>%
      mutate(qty_promoted = ifelse(is.na(qty_promoted), 0, qty_promoted)) %>%
      mutate(qty_new = qty + qty_promoted) %>%
      mutate(qty_up = ifelse(qty_new < threshold, qty_new, 0)) %>%
      mutate(qty_sticky = ifelse(qty_new >= threshold, qty_new, 0)) %>% 
      mutate(has_sticky = has_sticky | qty_sticky > 0) # Indicates whether code or subcodes have sticky quantities
    
    tbl_rolled_up <- rbind(tbl_rolled_up, tbl_rolled_up_lyr)
    
  }
  rm(tbl_layer, tbl_dictionary_2nd, tbl_gather_ups, tbl_rolled_up_lyr)
  
  # Just keep those activity codes that have 'sticking' quantities
  tbl_rolled_up %<>%
    mutate(qty_sticky = ifelse(layer_no == 1 & qty_up > 0, 
                               qty_up, 
                               qty_sticky)) %>%
    filter(has_sticky) %>%
    select(code, layer_no, qty_sticky)
  
  # Remove codes without quantities from the dictionary
  tbl_dictionary %<>%
    filter(qty > 0 & layer_no != 1)
  
  # Find the layer at which the activities code quantity 'lands'
  tbl_dict_min <- rbind(
    tbl_dictionary %>%
      select(-layer_no) %>%
      left_join(tbl_rolled_up, by = "code") %>%
      filter(!is.na(qty_sticky)),
    tbl_dictionary %>%
      select(-layer_no) %>%
      left_join(tbl_rolled_up, by = c("code_parent" = "code")) %>%
      filter(!is.na(qty_sticky))
  ) %>%
    group_by(code) %>%
    summarise(layer_lands = max(layer_no)) %>%
    mutate(layer_lands = layer_lands + 1)
  
  tbl_dictionary %<>% left_join(tbl_dict_min, by = "code")
  
  # Cleaning up tbl_dictionary, keeping only nodes that specify where quantities end up 
  tbl_dictionary <- rbind(
    tbl_dictionary %>% # Starting nodes which are on or above threshold
      group_by(code) %>%
      summarise(
        code_max = max(layer_no),
        layer_lands = max(layer_lands)
      ) %>%
      filter(code_max < layer_lands) %>%
      select(code, layer_no = code_max) %>%
      inner_join(tbl_dictionary, by = c("code", "layer_no")) %>%
      mutate(code_parent = code),
    tbl_dictionary %>% # Nodes with the nodes where their quantities end up
      filter(layer_no == layer_lands)
  )
  
  tbl_dictionary %<>% select(code, code_new = code_parent, qty) # Select only relevant variables
  
  # Build list with data-sets for return value
  lst_tree_econ_activity <- list(tbl_rolled_up = tbl_rolled_up,
                                 tbl_dictionary = tbl_dictionary)
  
  return(lst_tree_econ_activity)
}
