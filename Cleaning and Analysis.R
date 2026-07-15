# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(bit64)
library(here)
library(readxl)
library(gridExtra)
library(ggalluvial)
library(purrr)
library(tibble)
library(writexl)

here::set_here()
here()

# Read the dataset
dataset_complete <- read_excel(here("IMPACT Data.xlsx"))

n_rows_before <- nrow(dataset_complete)

##########EXCLUDE ACCOMMODATIONS################
#################################################

#Define the excluded accommodations
excluded_accommodation <- c("collective_centre", "hotel_hostel", "no_arrangement_for_now", 
                            "other", "own_accommodation", "pns", "(blank)", 
                            "back_to_habitual_residence" , "rented_accommodation")

# Filter the dataset to exclude rows with unwanted accommodations AND empty accommodation fields
filtered_dataset <- dataset_complete %>%
  filter(!accommodation %in% excluded_accommodation,  # Exclude listed accommodations
         !is.na(accommodation),                      # Exclude rows with NA accommodation
         accommodation != "")                        # Exclude rows where accommodation is an empty string


# Number of entries after filtering for accommodation
n_rows_after <- nrow(filtered_dataset)
n_unique_longit_ids_after <- n_distinct(filtered_dataset$LONGIT_ID)

cat("Rows before filtering:", n_rows_before, "\n")
cat("Rows after filtering:", n_rows_after, "\n")
cat("Unique LONGIT_IDs after filtering:", n_unique_longit_ids_after, "\n")


#Create categories based on accommodation 
filtered_dataset <- filtered_dataset %>%
  mutate(
    accommodation_category = case_when(
      accommodation == "other_accommodation_provided_authorities" ~ 1,  # Government accommodation
      accommodation == "provided_employer" ~ 2,                         # Private accommodation
      accommodation %in% c(
        "with_family_friends",
        "provided_volunteer",
        "provided_ngo"
      ) ~ 5,                                                            # Third sector
      TRUE ~ NA_real_
    )
  )

######### EVOLUTION OF DISTRIBUTION OF ACCOMMODATION ##############

first_last_dataset <- filtered_dataset %>%
  group_by(LONGIT_ID, round) %>%
  group_by(LONGIT_ID) %>%
  summarise(
    first_accommodation = first(accommodation_category),
    last_accommodation = last(accommodation_category)
  ) %>%
  ungroup()

first_last_dataset <- first_last_dataset %>%
  mutate(
    first_accommodation = recode(
      as.character(first_accommodation),
      "1" = "Public Sector",
      "2" = "Private Sector",
      "5" = "Third Sector"
    ),
    last_accommodation = recode(
      as.character(last_accommodation),
      "1" = "Public Sector",
      "2" = "Private Sector",
      "5" = "Third Sector"
    )
  )

transitions <- first_last_dataset %>%
  count(first_accommodation, last_accommodation)

sankey <- ggplot(
  transitions,
  aes(
    axis1 = first_accommodation,
    axis2 = last_accommodation,
    y = n
  )
) +
  geom_alluvium(aes(fill = factor(first_accommodation))) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(
    limits = c("First", "Last"),
    expand = c(.1, .1)
  ) +
  labs(
    x = NULL,
    y = "Number of people",
    fill = "Accommodation Category"
  ) +
  theme_minimal()

ggsave("sankey.png", sankey, width = 8, height = 12, dpi = 300)


##MAKE DATASET WITH FIRST AND DATASET WITH LAST ENTRY##
#######################################################


#### Highest Round Dataset (last entry per LONGIT_ID)
highest_round_dataset <- filtered_dataset %>%
  arrange(LONGIT_ID, desc(round)) %>%
  group_by(LONGIT_ID) %>%
  slice_head(n = 1) %>%
  ungroup()

highest_round_dataset <- highest_round_dataset %>%
  mutate(
    accommodation = recode(
      as.character(accommodation),
      "1" = "Public Sector",
      "2" = "Private Sector",
      "5" = "Third Sector"
    )
  )

highest_round_dataset %>%
  count(accommodation_category)

highest_round_dataset <- highest_round_dataset %>%
  select(LONGIT_ID, date, round, country, age, gender, occupation_now, accommodation, language_skill, discrimination,
         accommodation_category)

######### ACCOMMODATION DISTRIBUTION BY AGE FOR LAST ROUND###########

# Create age groups with specified categories, including blank/missing cases
age_accommodation_distribution <- highest_round_dataset %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 30 ~ "18-30",
    age >= 31 & age <= 40 ~ "31-40",
    age >= 41 & age <= 50 ~ "41-50",
    age >= 51 & age <= 64 ~ "51-64",
    age >= 65 ~ "65+",
    is.na(age) ~ "(blank)"
  )) %>%
  group_by(accommodation_category, age_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(accommodation_category) %>%
  mutate(
    percentage = round(100 * count / sum(count), 2)
  ) %>%
  ungroup()

############GENDER DISTRIB IN LAST ROUND######################

# Summarize gender distribution in the highest_round_dataset
gender_distribution <- highest_round_dataset %>%
  group_by(accommodation_category, gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(accommodation_category) %>%  
  mutate(
    percentage = round(count / sum(count) * 100, 2)
    ) %>%
  ungroup()



################# DEFINING "POSITIVE" OUTCOMES FOR INTEGRATION PARAMETERS ##############
##########################################################################################

# Define a function to classify based on given criteria
classify_language_skill <- function(skill) {
  if (is.na(skill) || skill %in% c("pns", "", "NA")) {
    return("excludable")
  } else if (skill %in% c("fair", "good", "very_good")) {
    return("positive")
  } else if (skill %in% c("poor", "very_poor")) {
    return("negative")
  } else {
    return("excludable")
  }
}

classify_discrimination <- function(discrimination) {
  if (is.na(discrimination) || discrimination %in% c("pns", "", "NA")) {
    return("excludable")
  } else if (discrimination == "no") {
    return("positive")
  } else if (discrimination == "yes") {
    return("negative")
  } else {
    return("excludable")
  }
}

classify_economic_integration <- function(occupation_now) {
  positive_jobs <- c("freelance", "own_business", "work_elsewhere_remote", "work_here", "work_ua_remote")
  negative_jobs <- c("caregiver_child", "caregiver_special_needs", "none", "not_working", "retired", "student", "volunteer")
  
  if (is.na(occupation_now) || occupation_now %in% c("pns", "", "NA")) {
    return("excludable")
  } else if (occupation_now %in% positive_jobs) {
    return("positive")
  } else if (occupation_now %in% negative_jobs) {
    return("negative")
  } else {
    return("excludable")
  }
}

classify_social_integration <- function(occupation_now) {
  positive_jobs <- c("caregiver_child", "caregiver_special_needs", "freelance", "own_business", "student", "volunteer", "work_elsewhere_remote", "work_here", "work_ua_remote")
  negative_jobs <- c("none", "not_working", "retired")
  
  if (is.na(occupation_now) || occupation_now %in% c("pns", "", "NA")) {
    return("excludable")
  } else if (occupation_now %in% positive_jobs) {
    return("positive")
  } else if (occupation_now %in% negative_jobs) {
    return("negative")
  } else {
    return("excludable")
  }
}


# Process the highest round dataset
highest_round_dataset <- highest_round_dataset %>%
  mutate(
    language_skill_status = sapply(language_skill, classify_language_skill),
    discrimination_status = sapply(discrimination, classify_discrimination),
    economic_integration_status = sapply(occupation_now, classify_economic_integration),
    social_integration_status = sapply(occupation_now, classify_social_integration)
  )


######## AMOUNT OF POS , NEG AND EXCLUDABLE PER DATASET PER VARIABLE OF INTEGRATION#########

# Function to summarize counts of statuses in a given dataset and column
summarize_counts <- function(data, column_name) {
  data %>%
    group_by(!!sym(column_name)) %>%            # Group by the specified column
    summarise(count = n()) %>%                  # Count occurrences
    ungroup() %>%
    pivot_wider(names_from = !!sym(column_name), values_from = count, values_fill = list(count = 0)) %>%
    mutate(total = positive + negative)
}

# Summarize counts for each dataset
print_counts <- function(data, dataset_name, vars) {
  cat("\nCounts for", dataset_name, ":\n")
  
  for (var in vars) {
    cat("\n", var, "\n", sep = "")
    print(summarize_counts(data, var))
  }
}

status_vars <- c(
  "discrimination_status",
  "language_skill_status",
  "economic_integration_status",
  "social_integration_status"
)

print_counts(highest_round_dataset, "Highest Round Dataset", status_vars)



#########AMOUNT OF PEOPLE PER ACCOMMODATION FOR EACH DATASET#######

summarize_counts_by_accommodation <- function(data, column_name) {
  data %>%
    count(accommodation_category, .data[[column_name]]) %>%
    pivot_wider(
      names_from = .data[[column_name]],
      values_from = n,
      values_fill = 0
    )
}

status_vars <- c(
  "discrimination_status",
  "language_skill_status",
  "economic_integration_status",
  "social_integration_status"
)

for (var in status_vars) {
  cat("\n", var, "\n", sep = "")
  print(summarize_counts_by_accommodation(highest_round_dataset, var))
}



all_counts <- map_dfr(
  status_vars,
  ~ summarize_counts_by_accommodation(highest_round_dataset, .x) %>%
    mutate(
      status_variable = .x,
      proportion_positive = positive / (positive + negative)
      ),
  .id = NULL
  )

all_counts <- all_counts %>%
  mutate(
  total = positive + negative)

print(all_counts)

#### CHI-SQUARE TESTS 

chi_results <- map(
  unique(all_counts$status_variable),
  function(var) {
    
    tbl <- all_counts %>%
      filter(status_variable == var) %>%
      select(accommodation_category, positive, negative) %>%
      column_to_rownames("accommodation_category") %>%
      as.matrix()
    
    chisq.test(tbl)
  }
)

names(chi_results) <- unique(all_counts$status_variable)
chi_results$discrimination_status
chi_results$language_skill_status
chi_results$economic_integration_status
chi_results$social_integration_status

chi_summary <- map_dfr(
  unique(all_counts$status_variable),
  function(var) {
    
    tbl <- all_counts %>%
      filter(status_variable == var) %>%
      select(accommodation_category, positive, negative) %>%
      column_to_rownames("accommodation_category") %>%
      as.matrix()
    
    test <- chisq.test(tbl)
    
    data.frame(
      indicator = var,
      chi_square = unname(test$statistic),
      df = unname(test$parameter),
      p_value = test$p.value
    )
  }
)

bonferroni_results <- lapply(
  unique(all_counts$status_variable),
  function(var) {
    
    dat <- all_counts %>%
      filter(status_variable == var)
    
    pairwise.prop.test(
      x = dat$positive,
      n = dat$total,
      p.adjust.method = "bonferroni"
    )
  }
)

bonferroni_df <- purrr::map_dfr(
  names(bonferroni_results),
  function(var) {
    
    pvals <- as.data.frame(as.table(bonferroni_results[[var]]$p.value))
    
    pvals %>%
      filter(!is.na(Freq)) %>%
      rename(
        group1 = Var1,
        group2 = Var2,
        p_value = Freq
      ) %>%
      mutate(
        outcome = var
      )
  }
)

bonferroni_df

#EXPORT ALL RELEVANT FILES
dfs <- list(
  ages = age_accommodation_distribution,
  genders = gender_distribution,
  count_summary = all_counts,
  chi_square_results = chi_summary,
  pairwise_comps = bonferroni_df)


for (name in names(dfs)) {
  write_xlsx(
    dfs[[name]],
    paste0(name, ".xlsx")
  )
}



