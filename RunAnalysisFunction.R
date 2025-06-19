
RunAnalysis <- function(
    current_data, 
    outcome_description, 
    outcome_measure, 
    var_prefix = "NULLXXXX",
    categorical = FALSE) {
  
  current_data <- dplyr::filter(current_data, !is.na(yi))
  
  mod_vars_list <- list(
    c("CG", "Complicated Grief"),
    c("age_deceased", "Age Deceased"),
    c("expected", "Expected Death"), 
    c("violent", "Violent Death"),
    c("percent_female", "Percent Female"),
    c("adult", "Adult"),
    c("location_us", "Location in US"),
    c("death_setting", "Death Setting"),
    c("intervention_setting", "Intervention Setting"),
    c("Anticipatory.death", "Anticipatory"),
    c("child_parent_spouse", "Parent, Child or Spouse")
  )
  #If grief or CG, remove complicated grief
  if(title == "Grief" | title == "Complicated grief") {
    mod_vars_list <- mod_vars_list[-1]
  }
  
  mod_vars <- 
    lapply(mod_vars_list, function(x) { return(x[[1]][[1]])}) %>%
    unlist()
  
  current_data <- current_data %>%
    dplyr::select(
      yi, vi, ID, Refid, all_of(outcome_measure), all_of(mod_vars), ni,
      intervention_type, starts_with(var_prefix), RoB, 
      CBT, writing, music, art, familytherapy
    ) %>%
    dplyr::filter(!is.na(yi), !is.na(vi))
  
  cat(
    glue::glue("\n\n**************", outcome_description, "*********\n\n" )
  )
  
  if (nrow(current_data) == 0) {
    cat(glue::glue("No studies for {outcome_description}.\n\n"))
    return(invisible(NULL))
  }
  
  
  
  cat(
    glue::glue(
      "Total N: {sum(current_data$ni, na.rm = TRUE)}\n\n"
    )
  )
  
  cat(glue::glue("Number of studies with n: {sum(!is.na(current_data$yi))}\n\n"))
  
  cat("\n")
  
  
  mod_vars <- 
    lapply(mod_vars_list, function(x) { return(x[[1]][[1]])}) %>%
    unlist()
  
  current_data <- current_data %>%
    dplyr::select(
      ID, Refid, all_of(outcome_measure), yi, vi, all_of(mod_vars),
      intervention_type, starts_with(var_prefix), RoB,
      CBT, writing, music, art, familytherapy
    )
  
  current_data %>%
    dplyr::filter(!is.na(yi)) %>%
    dplyr::select(ID, all_of(outcome_measure),
                  intervention_type
    ) %>% 
    knitr::kable() %>% print()
  
  
  cat("\n")
  
  # print IDs
  current_data %>%
    dplyr::select(Refid) %>%
    dplyr::arrange(Refid) %>%
    dplyr::mutate(Refid = paste0("{#", Refid, "}")) %>%
    unlist() %>%
    paste(collapse = '') %>% cat()
  
  cat("\n")
  
  fit_model <-  
    metafor::rma(yi = yi, vi = vi, data = current_data,
                 test="knha", slab = ID)
  
  print(fit_model)
  
  cat("\n###############  Remove High RoB Studies *********\n")
  
  
  current_data_no_high_risk <-
    current_data %>%
    dplyr::filter(
      RoB != "High risk"
    )
  
  if (nrow(current_data_no_high_risk > 0)) {
    fit_model_no_hrb <-  
      metafor::rma(yi = yi, vi = vi, data = current_data_no_high_risk,
                   test="knha", slab = ID)
    
    print(fit_model_no_hrb)
    
    
  } else {
    cat("\nRemoving high risk studies left no studies to analyze.\n")
  }
  
  
  cat("********** Analysis by intervention type ************\n")
  
  
  
  if (
    length(unique(current_data$intervention_type)) < nrow(current_data) &
    length(unique(current_data$intervention_type)) > 1
  ) {
    
    fit_model_intervention_type <-  
      metafor::rma(yi = yi, vi = vi, data = current_data,
                   test="knha", slab = ID, mods =~intervention_type)
    
    print(fit_model_intervention_type)
    
    forest(fit_model_intervention_type, atransf = exp,
           xlab = "Risk Ratio", refline = 0,
           mlab = "", main = "intervention type"
    )
    
  } else {
    cat("Not enough data for analysis by intervention type.")
  } 
  
  cat("\n********** End of Analysis by intervention type ************\n")
  
  
  if (categorical) {
    GetExpCoefs(fit_model)
    forest(fit_model, atransf = exp,
           xlab = "Risk Ratio", refline = 0,
           mlab = ""
    )
    
    funnel(fit_model)
    
    
  } else {
    forest(
      fit_model, 
      xlab = "Standardized Mean Difference",
      mlab = "")
    addpoly(fit_model, row=-1  # Place at the bottom
    )
    
    funnel(fit_model)
    
    forest(
      fit_model, 
      xlab = "Standardized Mean Difference",
      mlab = "", main = "intervention type")
  }  
  
  
  
  
  
  
  
  if (!categorical) {
    if (length(coef(fit_model)) == 1) {
      CalculatePooledSDEffect(
        model = fit_model,
        data = current_data,
        var_prefix = var_prefix
      )
    }
  }
  
  
  
  
  if (nrow(current_data) >= 3) {
    cat("Egger test")
    print(metafor::regtest(fit_model))
    cat("Begg test")
    print(metafor::ranktest(fit_model))
    cat("Trim and Fill\n")
    print(metafor::trimfill(fit_model))
  } else {
    cat("Not enough data for bias tests and trim/fill.\n")
  }
  
  # If intervention is all, do the subsets.
  # This is the major subgroups
  if (intervention == "all") {
    
    for (m in seq_along(mod_vars)) {
      cat("\n\n** ", mod_vars_list[m][[1]][[2]], " **\n\n")
      
      mod_var <- mod_vars[m]
      
      current_data_subset <- current_data 
      current_data_subset$mod_var <- current_data_subset[[mod_var]]
      current_data_subset <- current_data_subset %>% 
        dplyr::filter(!is.na(mod_var), mod_var != "N/A")
      
      
      
      # tests for enough data
      test_1 <- (
        length(unique(current_data_subset$mod_var)) <
          nrow(current_data_subset)) | 
        class(current_data_subset$mod_var) != "character"
      test_2 <- nrow(current_data_subset) > 2 & 
        length(unique(current_data_subset$mod_var)) > 1
      if (test_1 & test_2) {
        current_data_subset %>%
          dplyr::group_by(mod_var) %>%
          dplyr::summarise(n_studies = dplyr::n()) %>%
          dplyr::mutate(percent = (n_studies / sum(n_studies)) * 100)  %>%
          knitr::kable() %>% print()
        
        
        fit_model_mod <-  
          metafor::rma(yi = yi, vi = vi, data = current_data_subset,
                       test="knha", slab = ID, mods = ~ mod_var)
        print(fit_model_mod)
        
        # for complicated grief, do forest
        
        if (mod_var == "CG") {
          
          try({fit_model_2 <-  
            metafor::rma(
              yi = yi, vi = vi, 
              data = current_data %>% 
                dplyr::filter(CG == "Complicated grief" | CG == "Grief/CG"
                ),
              test="knha", slab = ID)
          print(forest(fit_model_2, main = "Complicated grief only"))
          })
        }
        
        
        if (outcome_measure == "severity_grief_measure_cont" & 
            mod_var == "CG") {
          cat("\nPsychotherapy only - complicated grief\n")
          
          data <- current_data %>% 
            dplyr::filter(
              intervention_type == "Psychotherapy",
              CG == "Complicated grief"
            )
          if (nrow(data) > 1) {
            fit_model_3 <-  
              metafor::rma(
                yi = yi, vi = vi, 
                data = data,
                test="knha", slab = ID
              )
            print(
              forest(fit_model_3, 
                     main = "Psychotherapy only - Complicated Grief")
            )
          } else {
            cat("Not enough data for psychotherapy ony in CG.\n")
          }
        }
        
        if (categorical) {
          GetExpCoefs(fit_model_mod)
        }
      } else {
        cat(
          glue::glue(
            "Not enough data for moderator analysis {mod_vars_list[m][[1]][[2]]}"
          )
        )
      }
    }
  }
}  
