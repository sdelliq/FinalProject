#Summary:
#Everything's ok except for explainable things:
  #There are duplicated cf piva (since they're masked)
  #The sum of the gbv doesn't add up to the gbv residual since penaltiess and expenses are NAs


#The ID must exist in Link_Counterparty_Entity
check_id_inLinkingTable <- function(linkingTable, entity_or_counterparty_df, id_column) {
  unique_ids <- unique(linkingTable[[id_column]])
  missing_ids <- setdiff(entity_or_counterparty_df[[id_column]], unique_ids)
  
  if (length(missing_ids) == 0) {
    return(paste0("OK: Every ", id_column, " from ", deparse(substitute(entity_or_counterparty_df)), " is present in the linking table"))
  } else {
    return(as.data.frame(missing_ids))
  }
}
check_id_inLinkingTable(link0$counterparty.entity, df0$counterparty, "id.counterparty") #OK
check_id_inLinkingTable(link0$counterparty.entity, df0$entity, "id.entity") #OK


#it measures, for a given ID_Counterparty, the number of ID_Entity in Link_Counterparty_Entity
check_entity_counts <- function(counterparties, link_c_e) {
  # Extract unique counterparty IDs and their corresponding n.entities values
  unique_counterparties <- unique(counterparties$id.counterparty)
  entity_counts <- counterparties$n.entities[match(unique_counterparties, counterparties$id.counterparty)]
  
  # Check if the count of appearances in another_df equals n.entities
  for (counterparty_id in unique_counterparties) {
    count_in_link_c_e <- sum(link_c_e$id.counterparty == counterparty_id)
    expected_count <- entity_counts[counterparty_id == unique_counterparties]
    if (count_in_link_c_e != expected_count) {
      cat("Mismatch for Counterparty in Link_Counterparty_Entity", counterparty_id, ": Expected", expected_count, "but found", count_in_link_c_e, "\n")
    }
  }
}
check_entity_counts(df0$counterparty, link0$counterparty.entity) #OK


#Uppercase letters allowed ("Mario Rossi" or "Random SRL" allowed)
check_name_format <- function(df) {
  not_lowercase <- df$name != tolower(df$name)
  if (any(not_lowercase)) {
    df$name_no_dots <- gsub("\\.", "", df$name)
    # Check if words in the "name" column are capitalized
    capitalized_words <- sapply(strsplit(df$name_no_dots[not_lowercase], "\\s+"), function(x) all(grepl("^[[:upper:]]", x)))
    # Check if part of the name is in all caps and matches a type of company
    caps_and_patterns <- grepl("\\b(SRL|SNCM|DI)\\b", df$name_no_dots[not_lowercase])
    
    cat("Rows with 'name' not in lowercase:", which(not_lowercase), "\n")
    cat("Rows with capitalized words:", which(capitalized_words), "\n")
    cat("Rows with 'SRL', 'SNCM', 'DI' patterns:", which(caps_and_patterns), "\n")
  } else {
    print("All 'name' values are in lowercase.")
  }
}
# # Example to see the code work
# df <- data.frame(
#   name = c("Some Company", "Another COMPANY", "S.R.L Corporation", "DI Solutions", "sncm Logistics")
# )
check_name_format(df0$counterparty) #OK


###-----------------------------------------------------------------------###
#-----                     Entities Table                              -----         
###-----------------------------------------------------------------------###
#The ID must exist in Link_Counterparty_Entity
check_id_inLinkingTable(link0$counterparty.entity, df0$entity, "id.entity") #OK


#For the cf.piva column, Unless NULL, there should not be duplicates
check_duplicates_cf.piva <- function(data, column) {
  notNull <- data[[column]][!is.na(data[[column]])]
  
  duplicates <- notNull[duplicated(notNull)]
  
  if (length(duplicates) == 0) {
    return(paste("There are no duplicates in the", column, "column."))
  } else {
    return(paste("Duplicate values found in the", column, "column:", toString(unique(duplicates))))
  }
}
#Dataframe to see the code work
# df <- data.frame(
#   cf.piva = c(123456789, 987654321, 123456789, 555555555, NA, 987654321),
#   other_column = c("A", "B", "C", "D", "E", "F")
# )
check_duplicates_cf.piva(df0$entity, "cf.piva") # Not OK but it's understandable why


#There must be at least one entity for every id_counterparty.
check_associations <- function(ids_in_counterparty, ids_in_linking_table, id_type) {
  if (n_distinct(ids_in_counterparty) == n_distinct(ids_in_linking_table)) {
    return(paste("There is at least one id for every", id_type))
  } else {
    missing_ids <- setdiff(ids_in_counterparty, ids_in_linking_table)
    return(paste("Some", id_type, "are not associated with at least one entity. Here's the list:", missing_ids))
  }
}
check_associations(df0$counterparty$id.counterparty, link0$counterparty.entity$id.counterparty, "id_counterparty") #OK


#'- if cf.piva is a fiscal code ==> individual;
# - if cf.piva is a p.iva ==> corporate, confidi, or other (NOT individual)
# - if cf.piva is N/A, no rules apply (there can't be inconsistency in this case)
cf.piva_and_type <- df0$entity %>% select(cf.piva, type.subject) %>% filter(nchar(cf.piva) == 16) 
non_individuals <- cf.piva_and_type %>% filter(type.subject != "individual")
if (nrow(non_individuals) == 0) {
  print("All CFs belong to individuals")
} else {
  print(paste("The following CFs are not classified as individuals:", toString(non_individuals$cf.piva)))
}
#OK


#sex, range_age, age, solvency_pf, income_pf 'Allowed only if type_subject == "individual"
check_columns_pf <- function(dataframe){
  df <- dataframe %>% filter(type.subject == "corporate") %>% select(id.entity, sex, age, range.age, solvency.pf, income.pf)
  #is_all_na <- all(apply(df, 2, function(col) all(is.na(col))))
  df <- df %>% filter(!is.na(sex) |!is.na(age) |!is.na(range.age) | !is.na(solvency.pf) | !is.na(income.pf)) %>% select(id.entity)
  if (nrow(df) == 0) {
    print("OK: There are no individual values in corporate rows")
  } else {
    message("Here are the entities that are corporate but have individual's values: ")
    return(df)
  }
}
check_columns_pf(df0$entity) #OK


#type_pg, status_pg, date_cessation 'Allowed only if type_subject != "individual"
check_columns_pf <- function(dataframe){
  df <- dataframe %>% filter(type.subject == "individual") %>% select(id.entity, type.pg, status.pg, date.cessation)
  #is_all_na <- all(apply(df, 2, function(col) all(is.na(col))))
  df <- df %>% filter(!is.na(type.pg) |!is.na(status.pg) |!is.na(date.cessation)) %>% select(id.entity)
  if (nrow(df) == 0) {
    print("OK: There are no corporate values in individual rows")
  } else {
    message("Here are the entities that are invididuals but have corporate's values: ")
    return(df)
  }
}
check_columns_pf(df0$entity) #OK


#GBV_Residual = Principal + Interest + Expenses + Penalties
check_gbv_sum <- function(dataframe){
  df <- dataframe %>% mutate(diff = gbv.original-principal-interest-penalties-expenses) %>%
    mutate(check_gbv = ifelse(is.na(diff),FALSE,ifelse(abs(diff)<0.5, TRUE, FALSE))) %>% 
    select(-diff) 
  df1 <- df %>% filter (check_gbv == FALSE) %>% select(id.loan)
  if (sum(df$check_gbv)==nrow(df)) {
    print("OK: GBV_Residual = Principal + Interest + Expenses + Penalties")
  } else {
    message("GBV_Residual != Principal + Interest + Expenses + Penalties. In the following loans: ")
    return(df1)
  }
}
check_gbv_sum(df0$loan) #No loan addds up, but it makes sense since interest and penalties are NAs

