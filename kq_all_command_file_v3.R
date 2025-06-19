library(dplyr)
library(glue)
library(here)
library(knitr)
library(metafor)
library(quarto)

# Change as appropriate
file_location <- "D:/documents/bereavement/github"

setwd(file_location)

d_orig <- read.csv(
  file.path(
    file_location,
    "distillersr-Bereavement_care_manuscript_2025-06-19-01-18-55.csv"
    )
)

# Combine Complicated Grief and Clinical diagnosis into one, and 
# remove category Grief and Complicated grief.
# First get counts.

d_orig <- d_orig %>%
  dplyr::mutate(
    prolonged_grief = 
      CG == "Clinical diagnosis" |
      CG == "Complicated grief", 
    CG = ifelse(CG == "Clinical diagnosis", NA, CG),
    CG = ifelse(CG == "Grief and complicated grief", "Grief/CG", CG)
  )


#################### KQ 3 and 4 combined #############


# KQ 3 refers to Key Question 3
# KQ 4 refers to Key Question 4.
# For the paper, these two questions were combined.

d_kq3_kq4 <- d_orig %>%
  dplyr::filter(KQ4 == "KQ4" | KQ3 == "KQ3")  %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    n_reviews = dplyr::n(),
    dupe = duplicated(ID)
  )



###################### KQ 3  #######################
d_kq3 <- d_orig %>%
  dplyr::filter(KQ3 == "KQ3")  %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    n_reviews = dplyr::n(),
    dupe = duplicated(ID)
  )


#### all
d <- d_kq3
intervention <- "all"
title <- "All"
# The global reassignment of d, intervention, and title within each loop and 
# before each quarto_render call works because save.image("kq.RData") 
# saves the current state of these variables for the Quarto document to load.
save.image("kq.RData")
quarto::quarto_render("kq3_4_v3.qmd", output_format  = "html",
                      output_file = "kq3_all.html")

d <- d_kq3_kq4
intervention <- "all"
title <- "All"
save.image("kq.RData")
quarto::quarto_render("kq3_4_v3.qmd", output_format  = "html",
                      output_file = "kq3_kq4_combined_all.html")



#### all - comparator
d <- d_kq3
intervention <- "all"
title <- "All vs Comparator"
save.image("kq.RData")
quarto::quarto_render("kq3_4_v3_comparator.qmd", output_format  = "html",
                      output_file = "kq3_all_comparator.html")

d <- d_kq3_kq4
intervention <- "all"
title <- "All vs Comparator"
save.image("kq.RData")
quarto::quarto_render("kq3_4_v3_comparator.qmd", output_format  = "html",
                      output_file = "kq3_kq4_all_comparator.html")



#### rct only
title <- "RCT Only"
d <- d_kq3 %>%
  dplyr::filter(study.design == "RCT")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_rct_all.html")
)


d <- d_kq3_kq4 %>%
  dplyr::filter(study.design == "RCT")
intervention <- "all"
title <- "All vs Comparator"
save.image("kq.RData")
quarto::quarto_render("kq3_4_v3_comparator.qmd", output_format  = "html",
                      output_file = "kq3_kq4_rct_all.html")


# Suicide
title <- "0% Female"
d <- d_kq3 %>%
  dplyr::filter(suicide == "suicide")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_suicide.html")
)

d <- d_kq3_kq4 %>%
  dplyr::filter(suicide == "suicide")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_kq4_suicide.html")
)


# Female = zero
title <- "0% Female"
d <- d_kq3 %>%
  dplyr::filter(percent_female == 0)
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_0_percent_female.html")
)

d <- d_kq3_kq4 %>%
  dplyr::filter(percent_female == 0)
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_kq4_0_percent_female.html")
)


# Female = 100
title <- "100% Female"
d <- d_kq3 %>%
  dplyr::filter(percent_female == 100)
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_100_percent_female.html")
)


d <- d_kq3_kq4 %>%
  dplyr::filter(percent_female == 100)
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_kq4_100_percent_female.html")
)

# Complicated Grief
title <- "Complicated grief"
d <- d_kq3 %>%
  dplyr::filter(CG == "Complicated grief")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_complicated_grief.html")
)


# Prolonged Grief
title <- "Prolonged grief"
d <- d_kq3_kq4 %>%
  dplyr::filter(prolonged_grief)
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_kq4_prolonged_grief.html")
)

d <- d_kq3_kq4 %>%
  dplyr::filter(CG == "Complicated grief")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_kq4_complicated_grief.html")
)


# grief failing, so remove for now
if (FALSE) {
  #  Grief
  title <- "Grief"
  d <- d_kq3 %>%
    dplyr::filter(CG == "Grief")
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3.qmd", output_format  = "html",
    output_file = glue::glue("kq3_grief.html")
  )
}

timing_categories <- c("Pre", "Acute", "6-12 months",  "More than 1 year")
for (t in timing_categories) {
  title <- glue::glue("Timing Category {t}")
  cat(title)
  d <- d_kq3 %>%
    dplyr::filter(timing_category == t)
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3.qmd", output_format  = "html",
    output_file = glue::glue("kq3_timing_{t}.html")
  )
}

for (t in timing_categories) {
  title <- glue::glue("Timing Category {t}")
  cat(title)
  d <- d_kq3_kq4 %>%
    dplyr::filter(timing_category == t)
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3.qmd", output_format  = "html",
    output_file = glue::glue("kq3_kq4_timing_{t}.html")
  )
}


interventions <- unique(d_kq3$intervention_type)
interventions <- interventions[interventions != ""]

interventions_kq3_kq4 <- unique(d_kq3_kq4$intervention_type)
interventions_kq3_kq4 <- interventions_kq3_kq4[interventions_kq3_kq4 != ""]

for (intervention in interventions) {
  print(intervention)
  title <- intervention
  d <- d_kq3 %>%
    dplyr::filter(KQ3 == "KQ3") %>%
    dplyr::filter(intervention_type == intervention)
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3.qmd", output_format  = "html",
    output_file = glue::glue("kq3_{intervention}.html")
  )
  
  d <- d_kq3
  title <- glue::glue("{intervention} vs Comparator")
  d <- d_kq3 %>%
    dplyr::filter(KQ3 == "KQ3") %>%
    dplyr::filter(intervention_type == intervention)
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3_comparator.qmd", output_format  = "html",
    output_file = glue::glue("kq3_{intervention}_comparator.html")
  )
  
  # rcts only
  title <- glue::glue("RCT only {intervention}")
  d <-  d %>%
    dplyr::filter(study.design == "RCT")
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3.qmd", output_format  = "html",
    output_file = glue::glue("kq3_rct_{intervention}.html")
  )
}



############# KQ3 KQ4 #####################
for (intervention in interventions_kq3_kq4) {
  print(intervention)
  title <- intervention
  d <- d_kq3_kq4 %>%
    dplyr::filter(intervention_type == intervention)
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3.qmd", output_format  = "html",
    output_file = glue::glue("kq3_kq4_{intervention}.html")
  )
  
  
  title <- intervention
  d <- d_kq3_kq4 %>%
    dplyr::filter(intervention_type == intervention) %>%
    dplyr::filter(prolonged_grief)
  save.image("kq.RData")
  if (nrow(d) > 0) {
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_prolonged_grief_{intervention}.html")
    )
  }
  title <- glue::glue("{intervention} vs Comparator")
  d <- d_kq3_kq4 %>%
    dplyr::filter(intervention_type == intervention)
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3_comparator.qmd", output_format  = "html",
    output_file = glue::glue("kq3_kq4_{intervention}_comparator.html")
  )
  
  # rcts only
  title <- glue::glue("RCT only {intervention}")
  d <-  d %>%
    dplyr::filter(study.design == "RCT")
  save.image("kq.RData")
  quarto::quarto_render(
    "kq3_4_v3.qmd", output_format  = "html",
    output_file = glue::glue("kq3_kq4_rct_{intervention}.html")
  )
}



############## Intervention subtypes




############# KQ3 KQ4 #####################
for (intervention in interventions_kq3_kq4) {
  print(intervention)
  title <- intervention
  d <- d_kq3_kq4 %>%
    dplyr::filter(intervention_type == intervention)
  if (nrow(d) > 0) {
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_{intervention}.html")
    )
  }
  
  
  title <- intervention
  d <- d_kq3_kq4 %>%
    dplyr::filter(intervention_type == intervention) %>%
    dplyr::filter(prolonged_grief)
  save.image("kq.RData")
  if (nrow(d) > 0) {
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_prolonged_grief_{intervention}.html")
    )
  }
  title <- glue::glue("{intervention} vs Comparator")
  d <- d_kq3_kq4 %>%
    dplyr::filter(intervention_type == intervention)
  if (nrow(d) > 0) {
    
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3_comparator.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_{intervention}_comparator.html")
    )
  }
  # rcts only
  title <- glue::glue("RCT only {intervention}")
  d <-  d %>%
    dplyr::filter(study.design == "RCT") %>%
    dplyr::filter(intervention_type == intervention)
  if (nrow(d) > 0) {
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_rct_{intervention}.html")
    )
  }
}


#### Minor subtypes

minor_subtypes <- c( "CBT", "writing", "music", "art", "familytherapy", 
                     "termination")


############# KQ3 KQ4 #####################
for (intervention in minor_subtypes) {
  print(intervention)
  title <- intervention
  d <- d_kq3_kq4 %>%
    dplyr::filter(!!sym(intervention) == intervention)
  if (nrow(d) > 0) {
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_{intervention}.html")
    )
  }
  
  
  title <- intervention
  d <- d_kq3_kq4 %>%
    dplyr::filter(!!sym(intervention) == intervention) %>%
    dplyr::filter(prolonged_grief)
  save.image("kq.RData")
  if (nrow(d) > 0) {
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_prolonged_grief_{intervention}.html")
    )
  }
  title <- glue::glue("{intervention} vs Comparator")
  d <- d_kq3_kq4 %>%
    dplyr::filter(!!sym(intervention) == intervention)
  if (nrow(d) > 0) {
    
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3_comparator.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_{intervention}_comparator.html")
    )
  }
  # rcts only
  title <- glue::glue("RCT only {intervention}")
  d <-  d %>%
    dplyr::filter(study.design == "RCT") %>%
    dplyr::filter(!!sym(intervention) == intervention)
  if (nrow(d) > 0) {
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq3_kq4_rct_{intervention}.html")
    )
  }
}







################ End of intervention subtypes




###################### KQ 4  #######################

d <- d_orig %>%
  dplyr::filter(KQ4 == "KQ4")  %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    n_reviews = dplyr::n(),
    dupe = duplicated(ID)
  )

# all
save.image("kq.RData")
intervention <- "all"

quarto::quarto_render("kq3_4_v3.qmd", output_format  = "html",
                      output_file = "kq4_all.html")


# rct only
d <- d %>%
  dplyr::filter(study.design == "RCT")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_rct_all.html")
)

# this doesn't run, so turning off for now
if (FALSE) {
  interventions <- unique(d$intervention_type)
  interventions <- interventions[interventions != ""]
  for (intervention in interventions) {
    d <- d_orig %>%
      dplyr::filter(KQ4 == "KQ4") %>%
      dplyr::filter(intervention_type == intervention)
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq4_{intervention}.html")
    )
    # rcts only
    d <- d %>%
      dplyr::filter(study.design == "RCT")
    save.image("kq.RData")
    quarto::quarto_render(
      "kq3_4_v3.qmd", output_format  = "html",
      output_file = glue::glue("kq4_rct_{intervention}.html")
    )
  }
}  


#### CBT
title <- "CBT Only"
d <- d_kq3 %>%
  dplyr::filter(CBT  == "CBT")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_cbt.html")
)


#### CBT
title <- "CBT Only"
d <- d_kq3_kq4 %>%
  dplyr::filter(CBT  == "CBT")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_kq4_cbt.html")
)






#### Family Therapy
title <- "FamilyTherapy"
d <- d_kq3 %>%
  dplyr::filter(familytherapy  == "familytherapy")
save.image("kq.RData")
quarto::quarto_render(
  "kq3_4_v3.qmd", output_format  = "html",
  output_file = glue::glue("kq3_familytherapy.html")
)



###################################################
## Forest plot Figure 2 with additional information
## Psychotherapy

d_kq3_kq4 <- d_orig %>%
  dplyr::filter(KQ4 == "KQ4" | KQ3 == "KQ3")  %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(Refid = paste(" {#", Refid, "}")) %>%
  dplyr::mutate(
    n_reviews = dplyr::n(),
    dupe = duplicated(ID)
  ) %>% dplyr::filter(intervention_type == "Psychotherapy")



d_severity_grief <- metafor::escalc(
  m1i = severity_grief_mean_int_cont,
  sd1i =severity_grief_SD_int_cont,
  n1i = severity_grief_n_int_cont,
  m2i = severity_grief_mean_ctrl_cont,
  sd2i = severity_grief_SD_ctrl_cont,
  n2i = severity_grief_n_ctrl_cont,
  append = TRUE,
  measure = "SMD",
  data = d_kq3_kq4) %>% 
  dplyr::mutate(CG = ifelse(CG == "Complicated grief", "CG", CG)) %>%
  dplyr::arrange(CG, ID)

d_severity_grief$ni <- attr(d_severity_grief$yi, which = 'ni')

d_severity_grief <- d_severity_grief %>%
  dplyr::filter(!is.na(yi)) %>%
  dplyr::mutate(
    percent_female = as.character(round(percent_female))
  ) %>%
  dplyr::mutate(
    percent_female = 
      ifelse(is.na(percent_female), "NA", percent_female)
  )


fit_model <-  
  metafor::rma(yi = yi, vi = vi, data = d_severity_grief,
               test="knha", slab = ID)

fit_model
num_studies <- fit_model$k
header_y_coord <- num_studies + 2 # Adjust this as needed for spacing

forest(
  fit_model, 
  ilab = cbind(percent_female, CG),
  ilab.xpos = c(1.1, 1.8),
  xlim = c(-6, 4),
  order = order(d_severity_grief$CG, ID)
)

text(c(1.1, 1.8), header_y_coord, c("% Female", "CG"))
text(c(-6), header_y_coord, c("Study"), adj = 0) 
text(2.2, header_y_coord, "Estimate", adj = 0)


###################################################
## Forest plot Figure 3 with additional information
## "Expert-facilitated support groups"

d_kq3_kq4 <- d_orig %>%
  dplyr::filter(KQ4 == "KQ4" | KQ3 == "KQ3")  %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(Refid = paste(" {#", Refid, "}")) %>%
  dplyr::mutate(
    n_reviews = dplyr::n(),
    dupe = duplicated(ID)
  ) %>% dplyr::filter(intervention_type == "Expert-facilitated support groups")



d_severity_grief <- metafor::escalc(
  m1i  = grief_symp_mean_int_cont,
  sd1i = grief_symp_SD_int_cont,
  n1i  = grief_symp_n_int_cont,
  m2i  = grief_symp_mean_ctrl_cont,
  sd2i = grief_symp_SD_ctrl_cont,
  n2i  = grief_symp_n_ctrl_cont,
  append = TRUE,
  measure = "SMD",
  data = d_kq3_kq4) %>% 
  dplyr::mutate(CG = ifelse(CG == "Complicated grief", "CG", CG)) %>%
  dplyr::arrange(CG, ID)

d_severity_grief$ni <- attr(d_severity_grief$yi, which = 'ni')

d_severity_grief <- d_severity_grief %>%
  dplyr::filter(!is.na(yi)) %>%
  dplyr::mutate(
    percent_female = as.character(round(percent_female))
  ) %>%
  dplyr::mutate(
    percent_female = 
      ifelse(is.na(percent_female), "NA", percent_female)
  )

fit_model <-  
  metafor::rma(yi = yi, vi = vi, data = d_severity_grief,
               test="knha", slab = ID)

fit_model
num_studies <- fit_model$k
header_y_coord <- num_studies + 2 # Adjust this as needed for spacing

forest(
  fit_model, 
  ilab = cbind(percent_female, CG),
  ilab.xpos = c(1.1, 1.8),
  xlim = c(-6, 4),
  order = order(d_severity_grief$CG, ID)
)

text(c(1.1, 1.8), header_y_coord, c("% Female", "CG"), adj = 0) # font=2 makes it bold
text(c(-6), header_y_coord, c("Study"), adj = 0) 
text(2.2, header_y_coord, "Estimate", adj = 0)