# Load necessary libraries
library(tidyverse)    # For data manipulation and piping
library(psych)        # For statistical functions (e.g., Cronbach's alpha)
library(MuMIn)        # For model selection and averaging
library(performance)  # For model performance metrics
library(readr)        # For reading data files
library(scales)       # For percent formatting
library(gtools)

# Read the CSV file into a raw data frame (without modifications)
data_RAW <- read_csv("data/data.csv")

# -----------------------------------------------
# Calculate Cronbach's Alpha for Different Scales
# -----------------------------------------------

# Compute Cronbach's alpha for the Self-Efficacy (EAG) scale
alpha_EAG <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA (missing values)
  select(starts_with("EAG_")) |>  # Select all columns starting with "EAG_"
  cronbachs_alpha()  # Compute Cronbach’s alpha for scale reliability

# Compute Cronbach's alpha for the Life-Satisfaction (SWLS) scale
alpha_SWLS <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA
  select(starts_with("SWLS_")) |>  # Select all columns starting with "SWLS_"
  cronbachs_alpha()

# Compute Cronbach's alpha for the Resilience (EBR) scale
alpha_EBR <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA
  select(starts_with("EBR_")) |>  # Select all columns starting with "EBR_"
  cronbachs_alpha()

# Compute Cronbach's alpha for the Depression (EBD) scale (after recoding responses)
alpha_EBD <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA
  select(starts_with("EBD_")) |>  # Select all columns starting with "EBD_"
  mutate(across(everything(), ~ ifelse(is.na(.x), NA, .x - 1))) |>  # Adjust scale values (-1 transformation)
  cronbachs_alpha()

# Compute Cronbach's alpha for the Social Support (MOS2) scale
alpha_MOS2 <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA
  select(starts_with("MOS2_")) |>  # Select all columns starting with "MOS2_"
  cronbachs_alpha()

# -----------------------------------------------
# Define PANAS Subscales (Positive & Negative Affect)
# -----------------------------------------------

# List of PANAS Positive Affect (PANAS_P) items
PANAS_P <- c("PANASB_1", "PANASB_3", "PANASB_5", "PANASB_9", 
             "PANASB_10", "PANASB_12", "PANASB_14", "PANASB_16", 
             "PANASB_17", "PANASB_19")

# List of PANAS Negative Affect (PANAS_N) items
PANAS_N <- c("PANASB_2", "PANASB_4", "PANASB_6", "PANASB_7",
             "PANASB_8", "PANASB_11", "PANASB_13", "PANASB_15",
             "PANASB_18", "PANASB_20")

# Compute Cronbach's alpha for PANAS Positive Affect (PANAS_P)
alpha_PANAS_P <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA
  select(all_of(PANAS_P)) |>  # Select PANAS_P variables
  cronbachs_alpha()

# Compute Cronbach's alpha for PANAS Negative Affect (PANAS_N)
alpha_PANAS_N <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA
  select(all_of(PANAS_N)) |>  # Select PANAS_N variables
  cronbachs_alpha()

# Compute Cronbach's alpha for Community_Cohesion (PCPS3) scale
alpha_PCPS3 <- data_RAW |> 
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>  # Replace 99 with NA
  select(starts_with("PCPS3_")) |>  # Select all columns starting with "PCPS3_"
  cronbachs_alpha()

# ------------------------------------------------------
# Data Preprocessing: Renaming, Recoding, and Filtering
# ------------------------------------------------------

data <- data_RAW |>
  # Rename columns to meaningful names
  rename(
    Age = SD1,
    City = SD2,
    Gender = SD3,
    Sexualientation = SD4,
    Sex = SD5,
    Ethnicity = SD6,
    Farmer = SD7,
    Marital_Status = SD8,
    SES = SD9, # Socioeconomic Status
    Education = SD10,
    Children = SD11,
    Housing = SD12,
    Cohabitant = SD13,
    Monthly_Income = SD14,
    Income_Source = SD15,
    Employment = SD16,
    Job = SD17,
    # Disabilities and difficulties
    Hearing_Difficulties = SD18_1,
    Speaking_Difficulties = SD18_2,
    Seeing_Difficulties = SD18_3,
    Moving_Difficulties = SD18_4,
    Grabing_Difficulties = SD18_5,
    Understanding_Difficulties = SD18_6,
    Interacting_Difficulties = SD18_7,
    # Lifetime Prevalence (LP) of substance use
    LP_Alcohol = SD19_1_A,
    LP_Cigarette = SD19_2_A,
    LP_Cannabis = SD19_3_A,
    LP_Cocaine = SD19_4_A,
    LP_Basuco = SD19_5_A,
    LP_Inhalant = SD19_6_A,
    LP_Ecstasy = SD19_7_A,
    LP_Psilocybin = SD19_8_A,
    LP_LSD = SD19_9_A,
    LP_Tranquilizer = SD19_10_A,
    LP_Popper = SD19_11_A,
    LP_Anfetamines = SD19_12_A,
    LP_Heroine = SD19_13_A,
    # Last Month (LM) substance use
    LM_Alcohol = SD19_1_B,
    LM_Cigarette = SD19_2_B,
    LM_Cannabis = SD19_3_B,
    LM_Cocaine = SD19_4_B,
    LM_Basuco = SD19_5_B,
    LM_Inhalant = SD19_6_B,
    LM_Ecstasy = SD19_7_B,
    LM_Psilocybin = SD19_8_B,
    LM_LSD = SD19_9_B,
    LM_TRAN = SD19_10_B,
    LM_Popper = SD19_11_B,
    LM_Anfetamines = SD19_12_B,
    LM_Heroine = SD19_13_B,
    # Last Week (LW) substance use
    LW_Alcohol = SD19_1_C,
    LW_Cigarette = SD19_2_C,
    LW_Cannabis = SD19_3_C,
    LW_Cocaine = SD19_4_C,
    LW_Basuco = SD19_5_C,
    LW_Inhalant = SD19_6_C,
    LW_Ecstasy = SD19_7_C,
    LW_Psilocybin = SD19_8_C,
    LW_LSD = SD19_9_C,
    LW_Tranquilizer = SD19_10_C,
    LW_Popper = SD19_11_C,
    LW_Anfetamines = SD19_12_C,
    LW_Heroine = SD19_13_C,
    Health = SD20_1,
    # Health and other variables
    Illness = SD21,
    Disease_Other = SD22_13_TEXT,
    PCPS1_4_Other = PCPS1_4_texto,
    eed1_7_Other = EED1_7_TEXT
  ) |>
  # Replace character "99" with NA for missing values
  mutate(across(where(is.character), ~ na_if(., "99"))) |>
  # Replace numeric 99 with NA for missing values
  mutate(across(where(is.numeric), ~ na_if(., 99))) |>
  # Recode gender categories into descriptive labels
  mutate(
    Gender = recode(
      Gender,
      "1" = "masculino",
      "2" = "femenino",
      "3" = "androgeno",
      "4" = "mujer trans",
      "5" = "hombre_trans",
      "6" = "trans femenino",
      "7" = "trans masculino",
      "8" = "queer",
      "9" = "no binario",
      "10" = "no se",
      "11" = "otro"
    )
  ) |>
  # Create a broader Gender category for analysis
  mutate(Gender = if_else(Gender %in% c(
    "femenino", "trans femenino",
    "tranSexual", "travesti", "mujer trans"
  ),
  "Mujer trans",
  if_else(Gender %in% c("masculino", "trans masculino", "hombre trans"),
          "Hombre trans",
          "No binario"
  )
  )) |>
  # Recode housing categories into descriptive labels
  mutate(
    Housing = recode(
      Housing,
      "1" = "Home-owner",
      "2" = "Renting (entire home)",
      "3" = "Living with family",
      "4" = "Shared rental (room)",
      "5" = "Without permanent housing"
    )
  ) |>
  # Recode substance use responses from text to binary (1 = Yes, 0 = No)
  mutate_at(
    c(
      "LP_Alcohol",
      "LP_Cigarette",
      "LP_Cannabis",
      "LP_Cocaine",
      "LP_Basuco",
      "LP_Inhalant",
      "LP_Ecstasy",
      "LP_Psilocybin",
      "LP_LSD",
      "LP_Tranquilizer",
      "LP_Popper",
      "LP_Anfetamines",
      "LP_Heroine",
      "LM_Alcohol",
      "LM_Cigarette",
      "LM_Cannabis",
      "LM_Cocaine",
      "LM_Basuco",
      "LM_Inhalant",
      "LM_Ecstasy",
      "LM_Psilocybin",
      "LM_LSD",
      "LM_TRAN",
      "LM_Popper",
      "LM_Anfetamines",
      "LM_Heroine",
      "LW_Alcohol",
      "LW_Cigarette",
      "LW_Cannabis",
      "LW_Cocaine",
      "LW_Basuco",
      "LW_Inhalant",
      "LW_Ecstasy",
      "LW_Psilocybin",
      "LW_LSD",
      "LW_Tranquilizer",
      "LW_Popper",
      "LW_Anfetamines",
      "LW_Heroine"
    ),
    ~ recode(.x, "1" = 1, "2" = 0)
  ) |>
  # Select only relevant variables
  select(
    -c(
      Codigo,
      ends_with("_TEXT"),
      Sexualientation,
      ends_with("_texto")
    )
  ) |>
  # Recode ethnicity, farmer status, marital status, SES, and education
  mutate(
    Ethnicity = recode(
      Ethnicity,
      "1" = "Indigenous",
      "2" = "Rrom",
      "3" = "Afro-Colombian",
      "4" = "Afro-Colombian",
      "5" = "Afro-Colombian",
      "6" = "Afro-Colombian",
      "7" = "Afro-Colombian",
      "8" = "Afro-Colombian",
      "9" = "None"
    )
  ) |>
  mutate(Farmer = recode(
    Farmer,
    "1" = "Yes",
    "2" = "No",
    "5" = NA_character_
  )) |>
  mutate(
    Marital_Status = recode(
      Marital_Status,
      "1" = "Married",
      "2" = "Single",
      "3" = "Widow/er",
      "4" = "Divorced",
      "5" = "Civil union",
      "6" = "Stable relationship"
    )
  ) |>
  mutate(
    SES = recode_factor(
      SES,
      "1" = "Low",
      "2" = "Low",
      "7" = "Low",
      "3" = "Middle-low",
      "4" = "Middle-high",
      "5" = "High",
      "6" = "High"
    )
  ) |>
  mutate(
    Education = recode_factor(
      Education,
      "1" = "No studies, illiterate",
      "2" = "No studies, literate",
      "3" = "Primary school (unfinished)",
      "4" = "Primary school",
      "5" = "Secondary school (unfinished)",
      "6" = "Secondary school",
      "7" = "Technical degree",
      "8" = "University (unfinished)",
      "9" = "University",
      "10" = "Postgraduate studies"
    )
  ) |>
  mutate(across(c(SD22_1:SD22_13, 
                  EED1_1,
                  EED1_2,
                  EED1_3,
                  EED1_4,
                  EED1_5,
                  EED1_6,
                  EED1_7,
                  EED2_1:EED2_5),
                ~ as.numeric(
                  recode(
                    as.character(.x),
                    "1" = "1",
                    "2" = "0",
                    .default = NA_character_,
                    .missing = NA_character_
                  )
                ))) |>
  # Convert disability variables to binary (1 = Has difficulty, 0 = No difficulty)
  mutate(across(
    ends_with("_Difficulties"),
    ~ case_when(.x == 99 ~ NA_real_, is.na(.x) ~ NA_real_, .x == 4 ~ 1, TRUE ~ 0)
  )) |>
  # Create a new variable 'difficulty_dichotomous' to indicate whether a person has
  # any difficulties across different categories
  mutate(Difficulty_Dichotomous = if_else(
    # If any of the difficulties variables (e.g., Hearing_Difficulties, Speaking_Difficulties, etc.) are NA
    rowSums(across(ends_with("_Difficulties"), ~ is.na(.))) > 0,
    NA_real_, # Assign NA if any difficulty is missing
    # Otherwise, check if all seven difficulties are marked as '1' (indicating impairment)
    if_else(rowSums(across(ends_with("_Difficulties"), ~ . == 1)) == 7,
            1, # Assign 1 if the person has all seven difficulties
            0 # Assign 0 otherwise
    )
  )) |>
  # Recode PCPS1_1 to PCPS1_5: Convert 1 to 1 (yes) and 2 to 0 (no), with NA for other values
  mutate(across(PCPS1_1:PCPS1_5, ~ case_when(
    . == 1 ~ 1, # Yes
    . == 2 ~ 0, # No
    TRUE ~ NA_real_ # Missing or other values
  ))) |>
  # Recode PCPS2_1 to PCPS2_5: Convert 1 to 0 (no engagement), and 2-5 to 1 (some engagement)
  mutate(across(PCPS2_1:PCPS2_5, ~ case_when(
    . == 1 ~ 0, # No engagement
    . %in% 2:5 ~ 1, # Some engagement
    TRUE ~ NA_real_ # Missing or other values
  ))) |>
  
  mutate(across(starts_with("EBD_"), ~ ifelse(is.na(.x), NA, .x - 1))) |> 
  # Compute aggregate variables summarizing different aspects
  mutate(
    # Count the number of substances used in the last month
    Polyconsumption_Month = rowSums(across(LM_Alcohol:LM_Heroine, ~.), na.rm = TRUE),
    # Count the number of reported diseases or health conditions
    Disease_Burden = rowSums(across(SD22_1:SD22_13, ~.), na.rm = TRUE),
    # Count the number of group memberships (sum of binary indicators)
    Group_Membership = rowSums(across(PCPS1_1:PCPS1_5, ~.), na.rm = TRUE),
    # Count the number of community engagement activities
    Community_Engagement = rowSums(across(PCPS2_1:PCPS2_5, ~.), na.rm = TRUE),
    # The following line is commented out: it would sum discrimination experiences
    Discrimination = rowSums(across(EED1_1:EED1_7, ~.), na.rm = TRUE),
    Discrimination = ifelse(Discrimination >= 1, 1, 0),
    
    Self_Efficacy = if_else(rowSums(!is.na(across(starts_with("EAG_")))) >= 5,
                            rowMeans(across(starts_with("EAG_")), na.rm = TRUE),
                            NA_real_),
    
    Life_Satisfaction = if_else(rowSums(!is.na(across(starts_with("SWLS_")))) >= 3,
                                rowMeans(across(starts_with("SWLS_")), na.rm = TRUE),
                                NA_real_),
    
    Resilience = if_else(rowSums(!is.na(across(starts_with("EBR_")))) >= 3,
                         rowMeans(across(starts_with("EBR_")), na.rm = TRUE),
                         NA_real_),
    
    Depression = if_else(rowSums(!is.na(across(starts_with("EBD_")))) >= 6,
                         rowMeans(across(starts_with("EBD_")), na.rm = TRUE),
                         NA_real_),
    
    Social_Support = if_else(rowSums(!is.na(across(starts_with("MOS2_")))) >= 10,
                             rowMeans(across(starts_with("MOS2_")), na.rm = TRUE),
                             NA_real_),
    
    Positive_Affect = if_else(rowSums(!is.na(across(all_of(PANAS_P)))) >= 8,
                              rowMeans(across(all_of(PANAS_P)), na.rm = TRUE),
                              NA_real_),
    
    Negative_Affect = if_else(rowSums(!is.na(across(all_of(PANAS_N)))) >= 9,
                              rowMeans(across(all_of(PANAS_N)), na.rm = TRUE),
                              NA_real_),                          
    
    Community_Cohesion = if_else(rowSums(!is.na(across(starts_with("PCPS3_")))) >= 2,
                                 rowMeans(across(starts_with("PCPS3_")), na.rm = TRUE),
                                 NA_real_),
    
  ) |>
  select(Age, Gender, Ethnicity, Marital_Status, SES, Education, Housing, 
         Health, Polyconsumption_Month:Community_Cohesion) |>
  # Convert categorical variables Housing to Job into factors
  mutate(Housing = as.factor(Housing)) |>
  # Convert all remaining character variables to factors
  mutate_if(is.character, as.factor) |> 
  
  filter(Age >= 18)

# ------------------------------------------------------
# Analyze and Visualize Missing Data
# ------------------------------------------------------

# Create a summary of missing data for each variable in the dataset
Missing_data <- data |>
  # Summarize across all columns, counting the number of NA values in each column
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  # Convert the summary from wide format (one row, many columns) to long format
  pivot_longer(
    everything(),               # Select all columns
    names_to = "Variable",      # Store column names in a new variable "Variable"
    values_to = "NA_count"      # Store the count of NAs in a new variable "NA_count"
  ) |> 
  # Compute the proportion of missing values for each variable
  mutate(Proportion = NA_count / dim(data)[1])  # Divide NA count by total number of rows in 'data'


# ------------------------------------------------------
# Create a Bar Plot of Missing Data Proportions
# ------------------------------------------------------

ggplot(Missing_data, aes(
  x = fct_reorder(Variable, Proportion, .desc = TRUE), # Reorder variables from highest to lowest missingness
  y = Proportion,
  fill = Proportion  # Use fill color to indicate proportion of missing data
)) +
  geom_col() +  # Create bar plot
  # Apply color gradient: Green (low missing data) → Yellow (moderate) → Red (high missing data)
  scale_fill_gradientn(
    colors = c("green", "yellow", "red"),  # Define color range
    labels = percent_format(accuracy = 1)  # Convert legend values to percentage format
  ) + 
  # Convert Y-axis (proportion of missing data) to a percentage scale
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Add axis labels
  labs(
    y = "Percentage of Missing Data",  # Label for Y-axis
    x = "Variable"  # Label for X-axis
  ) +
  # Use a minimal theme for a cleaner visual appearance
  theme_minimal() +
  # Rotate X-axis labels for better readability
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ------------------------------------------------------
# Final Data Cleaning: Remove Variables & Drop Rows with Missing Data
# ------------------------------------------------------

# Modelo Life Satisfaction----
dat_LS <- data |> 
  select(Life_Satisfaction, Age, Gender, Ethnicity, Marital_Status, Education, Housing, 
         Self_Efficacy, Community_Cohesion, Depression, Social_Support,
         Polyconsumption_Month, Disease_Burden, Discrimination, Group_Membership, Community_Engagement)

# Datos perdidos antes de imputación
dat_LS |> 
  summarise(across(everything(), ~ sum(is.na(.))))

# Base de datos
dat_LS <- dat_LS |> 
  drop_na()

global_LS <- lm(Life_Satisfaction ~ Age + Gender + Ethnicity + Marital_Status + Education +
                  Housing + Self_Efficacy + Community_Cohesion + Depression + 
                  Social_Support + Polyconsumption_Month + Disease_Burden + Discrimination +
                  Group_Membership + Community_Engagement,
                data = dat_LS,
                na.action = "na.fail")

# Dredge
dr_LS<- dredge(global_LS, 
               trace = 2 #para ver barra de progreso
)

plot(dr_LS[1:100,])

# Modelo promedio
avg_LS <- model.avg(dr_LS, subset = delta < 2, fit = TRUE)
summary(avg_LS)



xlb <- cbind(summary(avg_LS)$coefmat.full,
             confint(avg_LS, full = TRUE),
             gather(as.data.frame(summary(avg_LS)$coef.nmod)))
xlb$avmod <- deparse(substitute(avg_LS))
xlb$value <- xlb$value/max(xlb$value)

xlb$sig <- stars.pval(xlb$`Pr(>|z|)`)
xlb$sig[xlb$sig == "." ] <- "†"
xlb$avmod <- factor(xlb$avmod)

# Replace and format term names from model tables
xlb$key <- str_remove_all(xlb$key, "scale")
xlb$key <- str_remove_all(xlb$key, "\\(")
xlb$key <- str_remove_all(xlb$key, "\\)")

lvs <- as.character(unique(xlb$key))
xlb$key <- factor(xlb$key, levels = lvs)
#create data frame with number of averaged models
nMods <- dim(avg_LS$msTable)[1]
#create plot
ggplot(xlb, aes(x = key, y = Estimate)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(aes(size = value, color = value), alpha = 0.5) +
  geom_errorbar(aes(ymin = `2.5 %`,
                    ymax = `97.5 %`),
                colour = "black", width=.1) +
  geom_point(size = 1) +
  theme_bw() +
  labs(x = NULL, y = "Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size_continuous(range = c(2,8),
                        breaks= seq(0, 1, by = 0.2)) +
  guides(size = guide_legend(title = "Importance"),
         color = guide_legend(title = "Importance")) +
  scale_x_discrete(labels = levels(xlb$key),
                   expand = c(0, 0.5)) +
  scale_colour_gradient(low = "deepskyblue2",
                        high = "brown2",
                        breaks= seq(0, 1, by = 0.2)) +
  geom_text(aes(label = sig), y = xlb$`97.5 %`, vjust = -0.4) +
  geom_text(aes(x = Inf,
                y = -Inf,
                label = paste("n mod = ", nMods)),
            fontface = "italic",
            size = 3,
            hjust = 1.1,
            vjust = -0.5,
            inherit.aes = FALSE)






# Modelo PANAS_P----
dat_PANAS_P <- data |> 
  select(Positive_Affect, Age, Gender, Ethnicity, Marital_Status, Education, Housing,
         Self_Efficacy, Community_Cohesion, Depression, Social_Support,
         Polyconsumption_Month, Disease_Burden, Discrimination, Group_Membership, Community_Engagement)

# Datos perdidos antes de imputación
dat_PANAS_P |> 
  summarise(across(everything(), ~ sum(is.na(.))))

# Base de datos
dat_PANAS_P <- dat_PANAS_P |> 
  drop_na()

global_PANAS_P <- lm(Positive_Affect ~ Age + Gender + Ethnicity + Marital_Status + Education +
                       Housing + Self_Efficacy + Community_Cohesion + Depression +
                       Social_Support + Polyconsumption_Month + Disease_Burden + Discrimination +
                       Group_Membership + Community_Engagement,
                     data = dat_PANAS_P,
                     na.action = "na.fail")

# Dredge
dr_PANAS_P <- dredge(global_PANAS_P, 
                     trace = 2 #para ver barra de progreso
)

plot(dr_PANAS_P[1:100,])

# Modelo promedio
avg_PANAS_P <- model.avg(dr_PANAS_P, subset = delta < 2, fit = TRUE)
summary(avg_PANAS_P)



xlb <- cbind(summary(avg_PANAS_P)$coefmat.full,
             confint(avg_PANAS_P, full = TRUE),
             gather(as.data.frame(summary(avg_PANAS_P)$coef.nmod)))
xlb$avmod <- deparse(substitute(avg_PANAS_P))
xlb$value <- xlb$value/max(xlb$value)

xlb$sig <- stars.pval(xlb$`Pr(>|z|)`)
xlb$sig[xlb$sig == "." ] <- "†"
xlb$avmod <- factor(xlb$avmod)

# Replace and format term names from model tables
xlb$key <- str_remove_all(xlb$key, "scale")
xlb$key <- str_remove_all(xlb$key, "\\(")
xlb$key <- str_remove_all(xlb$key, "\\)")

lvs <- as.character(unique(xlb$key))
xlb$key <- factor(xlb$key, levels = lvs)
#create data frame with number of averaged models
nMods <- dim(avg_PANAS_P$msTable)[1]
#create plot
ggplot(xlb, aes(x = key, y = Estimate)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(aes(size = value, color = value), alpha = 0.5) +
  geom_errorbar(aes(ymin = `2.5 %`,
                    ymax = `97.5 %`),
                colour = "black", width=.1) +
  geom_point(size = 1) +
  theme_bw() +
  labs(x = NULL, y = "Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size_continuous(range = c(2,8),
                        breaks= seq(0, 1, by = 0.2)) +
  guides(size = guide_legend(title = "Importance"),
         color = guide_legend(title = "Importance")) +
  scale_x_discrete(labels = levels(xlb$key),
                   expand = c(0, 0.5)) +
  scale_colour_gradient(low = "deepskyblue2",
                        high = "brown2",
                        breaks= seq(0, 1, by = 0.2)) +
  geom_text(aes(label = sig), y = xlb$`97.5 %`, vjust = -0.4) +
  geom_text(aes(x = Inf,
                y = -Inf,
                label = paste("n mod = ", nMods)),
            fontface = "italic",
            size = 3,
            hjust = 1.1,
            vjust = -0.5,
            inherit.aes = FALSE)




# Modelo PANAS_N----
dat_PANAS_N <- data |> 
  select(Negative_Affect, Age, Gender, Ethnicity, Marital_Status, Education, Housing,
         Self_Efficacy, Community_Cohesion, Depression, Social_Support,
         Polyconsumption_Month, Disease_Burden, Discrimination, Group_Membership, Community_Engagement)

# Datos perdidos antes de imputación
dat_PANAS_N |> 
  summarise(across(everything(), ~ sum(is.na(.))))

# Base de datos
dat_PANAS_N <- dat_PANAS_N |> 
  drop_na()

global_PANAS_N <- lm(Negative_Affect ~ Age + Gender + Ethnicity + Marital_Status + Education +
                       Housing + Self_Efficacy + Community_Cohesion + Depression +
                       Social_Support + Polyconsumption_Month + Disease_Burden + Discrimination +
                       Group_Membership + Community_Engagement,
                     data = dat_PANAS_N,
                     na.action = "na.fail")

# Dredge
dr_PANAS_N <- dredge(global_PANAS_N, 
                     trace = 2 #para ver barra de progreso
)

plot(dr_PANAS_N[1:100,])

# Modelo promedio
avg_PANAS_N <- model.avg(dr_PANAS_N, subset = delta < 2, fit = TRUE)
summary(avg_PANAS_N)





xlb <- cbind(summary(avg_PANAS_N)$coefmat.full,
             confint(avg_PANAS_N, full = TRUE),
             gather(as.data.frame(summary(avg_PANAS_N)$coef.nmod)))
xlb$avmod <- deparse(substitute(avg_PANAS_N))
xlb$value <- xlb$value/max(xlb$value)

xlb$sig <- stars.pval(xlb$`Pr(>|z|)`)
xlb$sig[xlb$sig == "." ] <- "†"
xlb$avmod <- factor(xlb$avmod)

# Replace and format term names from model tables
xlb$key <- str_remove_all(xlb$key, "scale")
xlb$key <- str_remove_all(xlb$key, "\\(")
xlb$key <- str_remove_all(xlb$key, "\\)")

lvs <- as.character(unique(xlb$key))
xlb$key <- factor(xlb$key, levels = lvs)
#create data frame with number of averaged models
nMods <- dim(avg_PANAS_N$msTable)[1]
#create plot
ggplot(xlb, aes(x = key, y = Estimate)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(aes(size = value, color = value), alpha = 0.5) +
  geom_errorbar(aes(ymin = `2.5 %`,
                    ymax = `97.5 %`),
                colour = "black", width=.1) +
  geom_point(size = 1) +
  theme_bw() +
  labs(x = NULL, y = "Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size_continuous(range = c(2,8),
                        breaks= seq(0, 1, by = 0.2)) +
  guides(size = guide_legend(title = "Importance"),
         color = guide_legend(title = "Importance")) +
  scale_x_discrete(labels = levels(xlb$key),
                   expand = c(0, 0.5)) +
  scale_colour_gradient(low = "deepskyblue2",
                        high = "brown2",
                        breaks= seq(0, 1, by = 0.2)) +
  geom_text(aes(label = sig), y = xlb$`97.5 %`, vjust = -0.4) +
  geom_text(aes(x = Inf,
                y = -Inf,
                label = paste("n mod = ", nMods)),
            fontface = "italic",
            size = 3,
            hjust = 1.1,
            vjust = -0.5,
            inherit.aes = FALSE)


# Pruebas ggeffects----
## más info en https://strengejacke.github.io/ggeffects/reference/predict_response.html?q=mumin#averaged-model-predictions-package-mumin-

library(ggeffects)

predict_response(avg_PANAS_N, terms = "Education") |> 
  ggplot(aes(x, predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


predict_response(avg_PANAS_N, terms = "Age") |> 
  ggplot(aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)


ggpubr::ggarrange(plotlist = predict_response(avg_PANAS_P) |> 
                    plot())

summary(avg_LS)

pred_LS <- predict_response(avg_LS, terms = "Community_Cohesion")

pred_LS |> 
  gt

ggpubr::ggarrange(plotlist = pred_LS |> 
                    plot())

# TOST----
## más info en https://easystats.github.io/parameters/reference/equivalence_test.lm.html
library(parameters)

equivalence_test(global_PANAS_P) |> 
  plot()
