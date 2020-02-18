# Efficient Data Management in R

### Programming and Debugging

##### Identifying center-left parties in the ESS

## ----auxiliary-data-----------------------------------------
## Main center-left parties
## Main center-left parties
aux <- list(
  AT = "SPÖ",
  BE = "SP.A",
  BG = "Balgarska sotsialisticheska partiya (BSP)"  ,
  CH = "Social Democratic Party / Socialist Party" ,
  CY = "Progressive Party of Working People (AKEL)" ,
  CZ = "ČSSD",
  DE = "SPD" ,
  EE = "Sotsiaaldemokraatlik Erakond",
  FI = "Social Democratic Party",
  FR = "PS (Parti Socialiste)",
  GB = "Labour",
  HU = "MSZP (Magyar Szocialista Párt)",
  IE = "Labour",
  IT = "Partido Democratico (PD)",
  NL = "Socialist Party",
  NO = "Arbeiderpartiet",
  PL = "Zjednoczona Lewica",
  RS = "Boris Tadic, Cedomir Jovanovic - Savez za bolju Srbiju - LDP, LSV, SDS",
  SI = "L - Levica"
) %>%
  melt() %>%
  rename(ess_party_name = value,
         cntry = L1)

## Match vote variables and country codes
vote_var <- ess %>%
  select(starts_with("prtv"),-prtvede1) %>%
  names()
vote_var_order <-
  sapply(aux$cntry, function (x)
    which(grepl(x, toupper(vote_var))))

## Add to auxiliary data frame
aux %<>%
  mutate(vote_var = vote_var[vote_var_order]) %>%
  mutate_if(is.factor, as.character)


##### Programming a function

## ----function-skeleton, eval = FALSE-------------------------------------
## ess_center_left <- function(micro_data, aux_data, group_var) {
##
##   ## A priori consistency checks
##   ...
##
##   ## Main function
##   ...
##
##   ## A posteriori consistency checks
##   ...
##
##   ## Return output
##   return(micro_data)
## }


###### A priori consistency checks

## ----function-priori, eval = FALSE---------------------------------------
## # 1) Check if group_var is a variable in micro_data and aux_data
## if (not(group_var %in% names(micro_data))) {
##   stop("group_var is not a variable in micro_data.")
## } else if (not(group_var %in% names(aux_data))) {
##   stop("group_var is not a variable in aux_data.")
## } else {
##
##   # 2) Unique values of group_var
##   group_vals_micro <- unique(micro_data[, group_var])
##   group_vals_aux <- unique(aux_data[, group_var])
##   group_vals_both <- group_vals_micro[group_vals_micro %in% group_vals_aux]
##
##   # 3) Missing values of group_var in micro_data and aux_data
##   group_vals_only_micro <-
##     group_vals_micro[not(group_vals_micro %in% group_vals_both)]
##   group_vals_only_aux <-
##     group_vals_aux[not(group_vals_aux %in% group_vals_both)]
##
##   # 4) Missing group_var values in micro_data or aux_data
##   if (length(group_vals_only_micro) > 0) {
##     warning(paste("Group values only in micro_data:",
##                   group_vals_only_micro, sep = " "))
##   }
##   if (length(group_vals_only_aux) > 0) {
##     warning(paste("Group values only in aux_data:",
##                   group_vals_only_aux, sep = " "))
##   }
## }


###### Main function

## ----function-main, eval = FALSE-----------------------------------------
## # Auxiliary ID
## micro_data$aux_id <- seq_len(nrow(micro_data))
##
## # List container for data frames containing v_center_left
## vote_recoded <- list()
##
## # Loop through groups
## for (j in group_vals_both) {
##   # Name of the group's vote choice variable
##   vote_var_j <- aux_data$vote_var[aux_data$cntry == j]
##
##   # Name of the group's  center left party (in native encoding)
##   ess_party_name_j <- aux_data$ess_party_name[aux_data$cntry == j]
##
##   # Generate v_center_left for this group
##   vote_recoded[[j]] <- micro_data %>%
##     filter(cntry == j) %>%                             # subset to group
##     select(aux_id, vote, vote_var_j) %>%               # select variables
##     rename(vote_choice = !!as.name(vote_var_j)) %>%    # rename vote choice
##     mutate(vote_choice = enc2native(vote_choice),      # harmonize encoding
##            v_center_left =                             # create v_center_left
##              ifelse(
##                is.na(vote) & is.na(vote_choice),
##                NA,                                     # missing information
##                ifelse(
##                  vote %in% c("No", "Not eligible to vote"),
##                  "Did not vote",                       # non-voters
##                  ifelse(vote_choice == ess_party_name_j,
##                         "Yes",                         # center-left voters
##                         "No")                          # voted for other party
##                )
##              ))
## }
##
## # Collapse list of data frames to a single data frame
## vote_recoded %<>%
##   bind_rows() %>%
##   select(aux_id, v_center_left)
##
## # Remove old versions of v_center_left if present
## if ("v_center_left" %in% names(micro_data)) {
##   micro_data %<>% select(-v_center_left)
## }
##
## # Add new variable to micro_data by aux_id
## micro_data %<>%
##   full_join(vote_recoded, by = "aux_id") %>%           # add new variable
##   select(-aux_id)                                      # drop auxiliary ID


###### A posteriori consistency checks

## ----function-posteriori, eval = FALSE-----------------------------------
## # Proportions of center left voters within each group
## sample_prop <- micro_data %>%
##   select(group_var, v_center_left) %>%
##   group_by(!!as.name(group_var)) %>%
##   summarize(prop_v_center_left = mean(v_center_left == "Yes", na.rm = TRUE))
##
## # Check if any group has 0% center left voters
## if (any(sample_prop$prop_v_center_left == 0)) {
##   warning(paste(
##     "No center-left voters in",
##     paste(sample_prop$cntry[sample_prop$prop_v_center_left == 0],
##           collapse = " "),
##     "- check your inputs!",
##     sep = " "
##   ))
## }


###### Full function

## ----function-full, eval = TRUE------------------------------------------
ess_center_left <- function(micro_data, aux_data, group_var) {
  ## A priori consistency checks
  # 1) Check if group_var is a variable in micro_data and aux_data
  if (not(group_var %in% names(micro_data))) {
    stop("group_var is not a variable in micro_data.")
  } else if (not(group_var %in% names(aux_data))) {
    stop("group_var is not a variable in aux_data.")
  } else {
    # 2) Unique values of group_var
    group_vals_micro <- unique(micro_data[, group_var])
    group_vals_aux <- unique(aux_data[, group_var])
    group_vals_both <-
      group_vals_micro[group_vals_micro %in% group_vals_aux]
    
    # 3) Missing values of group_var in micro_data and aux_data
    group_vals_only_micro <-
      group_vals_micro[not(group_vals_micro %in% group_vals_both)]
    group_vals_only_aux <-
      group_vals_aux[not(group_vals_aux %in% group_vals_both)]
    
    # 4) Missing group_var values in micro_data or aux_data
    if (length(group_vals_only_micro) > 0) {
      warning(paste(
        "Group values only in micro_data:",
        group_vals_only_micro,
        sep = " "
      ))
    }
    if (length(group_vals_only_aux) > 0) {
      warning(paste(
        "Group values only in aux_data:",
        group_vals_only_aux,
        sep = " "
      ))
    }
  }
  
  
  ## Main function
  # Auxiliary ID
  micro_data$aux_id <- seq_len(nrow(micro_data))
  
  # List container for data frames containing v_center_left
  vote_recoded <- list()
  
  # Loop through groups
  for (j in group_vals_both) {
    # Name of the group's vote choice variable
    vote_var_j <- aux_data$vote_var[aux_data$cntry == j]
    
    # Name of the group's  center left party (in native encoding)
    ess_party_name_j <- aux_data$ess_party_name[aux_data$cntry == j]
    
    # Generate v_center_left for this group
    vote_recoded[[j]] <- micro_data %>%
      filter(cntry == j) %>%                             # subset to group
      select(aux_id, vote, vote_var_j) %>%               # select variables
      rename(vote_choice = !!as.name(vote_var_j)) %>%    # rename vote choice
      mutate(
        vote_choice = enc2native(vote_choice),
        # harmonize encoding
        v_center_left =                             # create v_center_left
          ifelse(
            is.na(vote) & is.na(vote_choice),
            NA,
            # missing information
            ifelse(
              vote %in% c("No", "Not eligible to vote"),
              "Did not vote",
              # non-voters
              ifelse(vote_choice == ess_party_name_j,
                     "Yes",                         # center-left voters
                     "No")                          # voted for other party
            )
          )
      )
  }
  
  # Collapse list of data frames to a single data frame
  vote_recoded %<>%
    bind_rows() %>%
    select(aux_id, v_center_left)
  
  # Remove old versions of v_center_left if present
  if ("v_center_left" %in% names(micro_data)) {
    micro_data %<>% select(-v_center_left)
  }
  
  # Add new variable to micro_data by aux_id
  micro_data %<>%
    full_join(vote_recoded, by = "aux_id") %>%           # add new variable
    select(-aux_id)                                      # drop auxiliary ID
  
  
  ## A posteriori consistency checks
  # Proportions of center left voters within each group
  sample_prop <- micro_data %>%
    select(group_var, v_center_left) %>%
    group_by(!!as.name(group_var)) %>%
    summarize(prop_v_center_left = mean(v_center_left == "Yes", na.rm = TRUE))
  
  # Check if any group has 0% center left voters
  if (any(sample_prop$prop_v_center_left == 0)) {
    warning(paste(
      "No center-left voters in",
      paste(sample_prop$cntry[sample_prop$prop_v_center_left == 0],
            collapse = " "),
      "- check your inputs!",
      sep = " "
    ))
  }
  
  
  ## Return output
  return(micro_data)
}


###### Debugging

## ----apply-fun----------------------------------------------
ess %<>%
  ess_center_left(aux_data = aux, group_var = "cntry")


## ----inspect-problem----------------------------------------
ess %>%
  filter(cntry == "DE") %>%
  select(prtvede2) %>%
  table()

aux %>%
  filter(cntry == "DE")



## ----fix-problem--------------------------------------------
aux$ess_party_name[aux$cntry == "DE"] <-
  "Social Democratic Party (SPD)"

# Apply the function
ess %<>%
  ess_center_left(aux_data = aux, group_var = "cntry")


## ----save-data---------------------------------------------
save(ess, file = "processed-data/ess-proc.RData")