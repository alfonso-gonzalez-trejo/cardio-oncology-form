# Cardio-oncology Practice in Latin America
# Author: Alfonso Gonzalez-Trejo
# June 2026
# Rstudio Version 2025.09.2+418
##### Initial: Library and WD####################################################
library(janitor)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(nortest)
library(forcats)
library(gtsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggsci)
library(scales)
library(ggplot2)
library(UpSetR)
library(ggpubr)
library(nnet)
library(MASS)
library(flextable)
library(cowplot)

library(DataExplorer)
library(ggalluvial)
library(sandwich)
library(lmerTest)
library(lme4)
library(broom.mixed)
library(gt)

library(broom)
library(colorspace)
#Working Directory
setwd("~/Documents/SS/Cardio-oncology form")
##### Database: Import##########################################################
datasets <- load("cardio_oncology_form.RData")

##### Analysis: Table 1 ########################################################
#Variable review -> asymmetrical with positive skew
summary(df_filtered$age) #Median 45 and Mean 47.56, not similar 
hist(df_filtered$age) #asymmetrical with positive skew
lillie.test(df$age) # p = 0.0032 so asymmetrical
#Table 1
tab1 <- df_filtered %>%
  dplyr::select(
    age, sex, has_cardio_onc_unit_cat, has_cardio_onc_unit_dicho, specialty, years_physician, years_cardio_onc_num, workplace_type, training_cardio_onc, formal_training_res,
    weekly_patients,
    refer_service_1, 
    refer_reason_1, 
    cancer_type_1, 
    drug_top1, 
    symptom_1,
    rf_1, rf_2, rf_3, rf_4, rf_5,
    cardioprotector_1,
    uses_hfa_icos_dicho, has_echo_2d, has_troponinas, has_pro_bnp,
    has_strain, has_echo_3d, has_cmr, has_angiotc, has_medicina_nuclear,
    has_hf_unit, has_arrhythmia_unit, has_tamo,
  ) %>%
  tbl_summary(
    by = has_cardio_onc_unit_dicho,
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    percent = "column",
    label = c(
      age ~ "Age (years, IQR)",
      sex ~ "Sex (n, %)",
      has_cardio_onc_unit_cat ~ "Cardio-oncology Unit and alternatives (n, %)",
      years_cardio_onc_num ~ "Years in Cardio-oncology (years, IQR)",
      formal_training_res ~ "Formal training during residency (n, %)",
      specialty ~ "Specialty (n, %)",
      years_physician ~ "Years in practice (years)",
      workplace_type ~ "Practice setting (n, %)",
      training_cardio_onc ~ "Cardio-oncology training (n, %)",
      weekly_patients ~ "Weekly cardio-oncology patient volume (n, %)",
      refer_service_1 ~ "Top referring service (n, %)",
      refer_reason_1 ~ "Main referral reason (n, %)",
      cancer_type_1 ~ "Most common cancer referred (n, %)",
      drug_top1 ~ "Most common therapy referred (n, %)",
      symptom_1 ~ "Most common presenting symptom (n, %)",
      rf_1 ~ "First most common cardiovascular risk factor (n, %)",
      rf_2 ~ "Second most common cardiovascular risk factor (n, %)",
      rf_3 ~ "Third most common cardiovascular risk factor (n, %)",
      rf_4 ~ "Fourth most common cardiovascular risk factor (n, %)",
      rf_5 ~ "Fifth most common cardiovascular risk factor (n, %)",
      cardioprotector_1 ~ "Most commonly used cardioprotective medication (n, %)",
      uses_hfa_icos_dicho ~ "Uses HFA-ICOS risk score (n, %)",
      
      has_tamo ~ "Availability of HSCT unit (TAMO) (n, %)",
      has_echo_2d ~ "Availability of 2D echocardiography (n, %)",
      has_echo_3d ~ "Availability of 3D echocardiography (n, %)",
      has_strain ~ "Availability of strain imaging (n, %)",
      has_medicina_nuclear ~ "Availability of nuclear medicine (n, %)",
      has_angiotc ~ "Availability of CT angiography (n, %)",
      has_cmr ~ "Availability of cardiac MRI (n, %)",
      has_troponinas ~ "Availability of troponins (n, %)",
      has_pro_bnp ~ "Availability of BNP/NT-proBNP (n, %)",
      has_hf_unit ~ "Availability of heart failure unit (n, %)",
      has_arrhythmia_unit ~ "Availability of arrhythmia unit (n, %)"
    ),
    sort = list(
      all_categorical() ~ "frequency",
      c(weekly_patients) ~ "alphanumeric"
    ),
    missing = "ifany"
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      c(sex, formal_training_res, has_angiotc, has_arrhythmia_unit, has_cmr, has_echo_3d,
         has_hf_unit, has_medicina_nuclear, has_pro_bnp, has_strain, has_tamo,
         has_troponinas) ~ "chisq.test",
      c(has_cardio_onc_unit_cat, specialty, workplace_type, training_cardio_onc,
        weekly_patients, refer_reason_1, cancer_type_1, drug_top1, symptom_1,
        rf_1, rf_2, rf_3, rf_4, rf_5,
        cardioprotector_1, uses_hfa_icos_dicho, has_echo_2d) ~ "fisher.test",
      refer_service_1 ~ "chisq.test"
    ),
    test.args = list(
      refer_service_1 ~ list(simulate.p.value = TRUE, B = 2000)
    ),
    pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 1: Descriptive characteristics in the study population**") %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Presence of Cardio-Oncology Unit**"
  ) %>%
  modify_table_styling(
    columns = c(label, starts_with("stat_")),
    footnote = "**Continuous variables:** Median (IQR); **Categorical variables:** n (%)"
  )


tab1
#tab1 %>% as_flex_table()%>% flextable::save_as_docx(path="Figures/Manuscript/Table 1 - Descriptive characteristics in the study population.docx")
#Post-hoc analysis for multi-level variables in Table 1#
#Sex: sex
chisq.test(table(df_filtered$sex == "Male",
                 df_filtered$has_cardio_onc_unit_dicho))
chisq.test(table(df_filtered$sex == "Female",
                 df_filtered$has_cardio_onc_unit_dicho))
#Cardio-oncology Unit and alternatives
fisher.test(table(df_filtered$has_cardio_onc_unit_cat == "Cardio-oncology unit",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$has_cardio_onc_unit_cat == "No (Cardiology)",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$has_cardio_onc_unit_cat == "No (Other)",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$has_cardio_onc_unit_cat == "No (Oncology)",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$has_cardio_onc_unit_cat == "No (Private practice)",
                  df_filtered$has_cardio_onc_unit_dicho))
#Specialty: specialty
fisher.test(table(df_filtered$specialty == "Cardiology",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$specialty == "Internal Medicine",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$specialty == "Oncology",
                  df_filtered$has_cardio_onc_unit_dicho))
#Practice setting: workplace_type
fisher.test(table(df_filtered$workplace_type == "Private Hospital",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$workplace_type == "2nd level Hospital (Regional, University, General)",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$workplace_type == "National Institute / High Specialty Institute",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$workplace_type == "Social Security",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$workplace_type == "Foundation Hospitals",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$workplace_type == "Private Practice",
                  df_filtered$has_cardio_onc_unit_dicho))
#Cardio-oncology training: training_cardio_onc
fisher.test(table(df_filtered$training_cardio_onc == "Postgraduate Diploma",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$training_cardio_onc == "Short structured training (Course, structured training, congress)",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$training_cardio_onc == "None",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$training_cardio_onc == "Master/MSc",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$training_cardio_onc == "Residency",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$training_cardio_onc == "Other",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$training_cardio_onc == "Self-directed",
                  df_filtered$has_cardio_onc_unit_dicho))
#Weekly cardio-oncology patient volume: weekly_patients
fisher.test(table(df_filtered$weekly_patients == "1-5",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$weekly_patients == "6-10",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$weekly_patients == "11-15",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$weekly_patients == "16-20",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$weekly_patients == "21-25",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$weekly_patients == "> 25",
                  df_filtered$has_cardio_onc_unit_dicho))
## Top referring service: refer_service_1
chisq.test(table(df_filtered$refer_service_1 == "Oncology",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
chisq.test(table(df_filtered$refer_service_1 == "Hematology",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
chisq.test(table(df_filtered$refer_service_1 == "Oncologic Surgery",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
chisq.test(table(df_filtered$refer_service_1 == "Gynecology",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
chisq.test(table(df_filtered$refer_service_1 == "Internal Medicine",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
chisq.test(table(df_filtered$refer_service_1 == "None",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
chisq.test(table(df_filtered$refer_service_1 == "Surgery",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
chisq.test(table(df_filtered$refer_service_1 == "Radiotherapy",
                 df_filtered$has_cardio_onc_unit_dicho),
           simulate.p.value = TRUE, B = 2000)
## Main referral reason: refer_reason_1
fisher.test(table(df_filtered$refer_reason_1 == "Prevention / protocol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$refer_reason_1 == "Cardiotoxicity screening",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$refer_reason_1 == "Cardiotoxicity follow-up",
                  df_filtered$has_cardio_onc_unit_dicho))
## Most common cancer referred: cancer_type_1
fisher.test(table(df_filtered$cancer_type_1 == "Breast cancer",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cancer_type_1 == "Leukemia (acute or chronic)",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cancer_type_1 == "Lymphoma",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cancer_type_1 == "Multiple myeloma",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cancer_type_1 == "Prostate cancer",
                  df_filtered$has_cardio_onc_unit_dicho))
## Most common therapy referred: drug_top1
fisher.test(table(df_filtered$drug_top1 == "Anthracyclines",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$drug_top1 == "Trastuzumab",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$drug_top1 == "5-Fluorouracil",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$drug_top1 == "Anti-estrogens",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$drug_top1 == "Taxanes",
                  df_filtered$has_cardio_onc_unit_dicho))
## Most common presenting symptom: symptom_1
fisher.test(table(df_filtered$symptom_1 == "Dyspnea",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$symptom_1 == "Asymptomatic",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$symptom_1 == "Palpitations",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$symptom_1 == "Angina",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$symptom_1 == "Fatigue",
                  df_filtered$has_cardio_onc_unit_dicho))
## First most common cardiovascular risk factor: rf_1
fisher.test(table(df_filtered$rf_1 == "Hypertension",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_1 == "High cholesterol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_1 == "Sedentary lifestyle",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_1 == "Obesity",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_1 == "Smoking",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_1 == "Diabetes",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_1 == "None",
                  df_filtered$has_cardio_onc_unit_dicho))
## Second most common cardiovascular risk factor: rf_2
fisher.test(table(df_filtered$rf_2 == "Diabetes",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_2 == "High cholesterol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_2 == "Hypertension",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_2 == "Obesity",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_2 == "Smoking",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_2 == "Sedentary lifestyle",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_2 == "None",
                  df_filtered$has_cardio_onc_unit_dicho))
## Third most common cardiovascular risk factor: rf_3
fisher.test(table(df_filtered$rf_3 == "Diabetes",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_3 == "High cholesterol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_3 == "Obesity",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_3 == "Sedentary lifestyle",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_3 == "Smoking",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_3 == "Hypertension",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_3 == "Chronic kidney disease",
                  df_filtered$has_cardio_onc_unit_dicho))
## Fourth most common cardiovascular risk factor: rf_4
fisher.test(table(df_filtered$rf_4 == "Obesity",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_4 == "High cholesterol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_4 == "Diabetes",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_4 == "Sedentary lifestyle",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_4 == "Smoking",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_4 == "Prior myocarditis",
                  df_filtered$has_cardio_onc_unit_dicho))
## Fifth most common cardiovascular risk factor: rf_5
fisher.test(table(df_filtered$rf_5 == "Sedentary lifestyle",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_5 == "Chronic kidney disease",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_5 == "Smoking",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_5 == "Obesity",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_5 == "Diabetes",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_5 == "High cholesterol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_5 == "Congenital heart disease",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$rf_5 == "None",
                  df_filtered$has_cardio_onc_unit_dicho))
## Most commonly used cardioprotective medication: cardioprotector_1
fisher.test(table(df_filtered$cardioprotector_1 == "Enalapril",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cardioprotector_1 == "Carvedilol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cardioprotector_1 == "SGLT2 inhibitors",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cardioprotector_1 == "Bisoprolol",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cardioprotector_1 == "Atorvastatin",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cardioprotector_1 == "Dexrazoxane",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cardioprotector_1 == "Candesartan",
                  df_filtered$has_cardio_onc_unit_dicho))
fisher.test(table(df_filtered$cardioprotector_1 == "Spironolactone",
                  df_filtered$has_cardio_onc_unit_dicho))
##### Analysis: Figure 2. Maps per region ######################
# 1) Country -> region
country_to_region <- c(
  "Argentina" = "South America",
  "Bolivia" = "South America",
  "Brazil" = "South America",
  "Chile" = "South America",
  "Colombia" = "South America",
  "Cuba" = "Caribbean",
  "Ecuador" = "South America",
  "Mexico" = "Mexico & Central America",
  "Paraguay" = "South America",
  "Peru" = "South America",
  "Dominican Republic" = "Caribbean",
  "Uruguay" = "South America",
  "Venezuela" = "South America",
  "Panama" = "Mexico & Central America",
  "Costa Rica" = "Mexico & Central America"
)

# 2) Count respondents per country
counts_total_country <- df_filtered %>%
  mutate(country = fct_drop(country)) %>%
  count(country, name = "n") %>%
  mutate(
    country_chr = as.character(country),
    region = unname(country_to_region[country_chr])
  )

# 3) Count respondents per region
counts_total_region <- counts_total_country %>%
  count(region, wt = n, name = "n") %>%
  mutate(
    total_n = sum(n, na.rm = TRUE),
    pct = n / total_n
  )

# 4) Manual mapping: Country -> ISO3
es_to_iso3 <- c(
  "Argentina" = "ARG",
  "Bolivia" = "BOL",
  "Brazil" = "BRA",
  "Chile" = "CHL",
  "Colombia" = "COL",
  "Cuba" = "CUB",
  "Ecuador" = "ECU",
  "Mexico" = "MEX",
  "Paraguay" = "PRY",
  "Peru" = "PER",
  "Dominican Republic" = "DOM",
  "Uruguay" = "URY",
  "Venezuela" = "VEN",
  "Panama" = "PAN",
  "Costa Rica" = "CRI"
)

counts_total_country <- counts_total_country %>%
  mutate(
    iso3 = unname(es_to_iso3[country_chr])
  )

# Optional sanity check
counts_total_country %>% filter(is.na(iso3) | is.na(region))

# 5) Map data
world_total <- ne_countries(scale = "medium", returnclass = "sf")

map_df_filtered_region <- world_total %>%
  left_join(counts_total_country, by = c("iso_a3" = "iso3")) %>%
  left_join(
    counts_total_region %>%
      dplyr::select(region, pct_region = pct),
    by = "region"
  ) %>%
  filter(continent %in% c("South America", "North America")) %>%
  filter(!iso_a3 %in% c("USA", "CAN")) %>%
  mutate(
    n = replace_na(n, 0L),
    pct_region = replace_na(pct_region, 0),
    legend_lab = if_else(
      n == 0L,
      "Non-respondents",
      paste0(region, " (", percent(pct_region, accuracy = 0.1), ")")
    ),
    legend_lab = fct_relevel(
      fct_reorder(legend_lab, pct_region, .desc = TRUE),
      "Non-respondents",
      after = Inf
    )
  )

# 6) Palette
labs_present <- levels(map_df_filtered_region$legend_lab)
labs_resp <- labs_present[labs_present != "Non-respondents"]

k <- length(labs_resp)
u <- ggsci::pal_d3("category20c")(max(3, k))

pal <- c(
  setNames(u[seq_len(k)], labs_resp),
  "Non-respondents" = "white"
)

pal <- alpha(pal, 0.70)

# 7) Plot
Figure2 <- ggplot(map_df_filtered_region) +
  geom_sf(aes(fill = legend_lab), color = "black", linewidth = 0.1) +
  coord_sf(xlim = c(-120, -35), ylim = c(-60, 35), expand = FALSE) +
  scale_fill_manual(values = pal, drop = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank()
  ) +
  labs(
    title = "Geographic distribution of survey respondents across Latin America and the Caribbean",
    subtitle = "Share of respondents by region (%)"
  ) +
  guides(fill = guide_legend(override.aes = list(color = "black", linewidth = 0.2)))

Figure2
ggsave(Figure2, filename = "Figures/Manuscript/Figure 2.png", width = 27, height = 25, units=c("cm"), dpi = 300,limitsize = FALSE)
##### Analysis: Figure 3. Sankey plot (developmental pathways of cardio-oncology practice in LATAM) ########################
# Definition categorization
# Training pathway
df_filtered$training_formality_cat <- dplyr::case_when(
  df_filtered$training_cardio_onc == "None" ~ "None",
  df_filtered$training_cardio_onc %in% c(
    "Self-directed",
    "Short structured training (Course, structured training, congress)",
    "Other"
  ) ~ "Informal training",
  df_filtered$training_cardio_onc == "Postgraduate Diploma" ~ "Postgraduate Diploma",
  df_filtered$training_cardio_onc == "Residency" ~ "Res.",
  df_filtered$training_cardio_onc == "Master/MSc" ~ "MSc",
  TRUE ~ NA_character_
)

df_filtered$training_formality_cat <- factor(
  df_filtered$training_formality_cat,
  levels = c(
    "None",
    "Informal training",
    "Postgraduate Diploma",
    "Res.",
    "MSc"
  )
)
# Cardio-oncology experience
df_filtered$co_experience_cat <- cut(
  df_filtered$years_cardio_onc_num,
  breaks = c(-Inf, 2, 8, Inf),
  labels = c("0–2 years",
             "3–8 years",
             ">8 years"),
  right = TRUE
)

df_filtered$co_experience_cat <- factor(
  df_filtered$co_experience_cat,
  levels = c("0–2 years",
             "3–8 years",
             ">8 years")
)

# Cardio-oncology unit
df_filtered$unit_setting_cat <- factor(
  ifelse(df_filtered$has_cardio_onc_unit_dicho == "Yes",
         "Cardio-oncology unit",
         "No Cardio-oncology unit"),
  levels = c("No Cardio-oncology unit",
             "Cardio-oncology unit")
)

# Check categories
table(df_filtered$training_formality_cat)
table(df_filtered$co_experience_cat)
table(df_filtered$unit_setting_cat)

# Sankey plot
# Summarize flows
sankey_df <- df_filtered %>%
  count(
    training_formality_cat,
    co_experience_cat,
    unit_setting_cat,
    name = "n"
  )

# Plot
Figure3 <- ggplot(
  sankey_df,
  aes(
    y = n,
    axis1 = training_formality_cat,
    axis2 = co_experience_cat,
    axis3 = unit_setting_cat
  )
) +
  geom_alluvium(
    aes(fill = training_formality_cat),
    stat = "alluvium",
    width = 1/12,
    alpha = 0.72
  ) +
  geom_stratum(
    stat = "stratum",
    width = 1/12,
    color = "grey30",
    fill = "grey95"
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3.5,
    color = "black",
    fontface = "bold",
    angle = 90
  ) +
  scale_fill_manual(
    values = c(
      "None" = "grey70",
      "Informal training" = "#B7C7D6",
      "Postgraduate Diploma" = "#41B6C4",
      "Res." = "#225EA8",
      "MSc" = "#31A354"
    )
  ) +
  scale_x_discrete(
    limits = c(
      "Training pathway",
      "Cardio-oncology specialist's experience",
      "Cardio-oncology unit presence"
    ),
    expand = c(0.15, 0.05)
  ) +
  labs(
    y = "Number of respondents",
    x = NULL,
    fill = "Training pathway"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank()
  )

Figure3
#ggsave(Figure3, filename = "Figures/Manuscript/Figure 3.png", width = 40, height = 25, units=c("cm"), dpi = 300,limitsize = FALSE)
##### Analysis: Figure 4. Bar plot guideline-practices ########################
df_filtered$uses_hfa_icos_dicho
df_filtered$has_strain
df_filtered$has_echo_3d
df_filtered$hfa_icos_use
df_filtered$gls_available
df_filtered$echo_3d_available
df_filtered$unit_setting_cat
# Variable categorization
df_filtered <- df_filtered %>%
  mutate(
    hfa_icos_use = ifelse(uses_hfa_icos_dicho == "Yes", 1, 0),
    gls_available = ifelse(has_strain == 1, 1, 0),
    echo_3d_available = ifelse(has_echo_3d == 1, 1, 0)
  )

implementation_long <- df_filtered %>%
  dplyr::select(
    unit_setting_cat,
    hfa_icos_use,
    gls_available,
    echo_3d_available
  ) %>%
  tidyr::pivot_longer(
    cols = c(hfa_icos_use, gls_available, echo_3d_available),
    names_to = "practice",
    values_to = "present"
  ) %>%
  mutate(
    practice = dplyr::recode(
      practice,
      hfa_icos_use = "HFA-ICOS risk score use",
      gls_available = "GLS availability",
      echo_3d_available = "3D echocardiography availability"
    ),
    practice = factor(
      practice,
      levels = c(
        "HFA-ICOS risk score use",
        "GLS availability",
        "3D echocardiography availability"
      )
    )
  )

implementation_plot_df <- implementation_long %>%
  group_by(practice, unit_setting_cat) %>%
  summarise(
    n_present = sum(present == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group_total = case_when(
      unit_setting_cat == "Cardio-oncology unit" ~ sum(df_filtered$unit_setting_cat == "Cardio-oncology unit"),
      unit_setting_cat == "No Cardio-oncology unit" ~ sum(df_filtered$unit_setting_cat == "No Cardio-oncology unit")
    ),
    
    percent_group = 100 * n_present / group_total,
    percent_total = 100 * n_present / nrow(df_filtered)
  )
#Figure 4A
total_labels_A <- implementation_plot_df %>%
  group_by(practice) %>%
  summarise(
    total_percent = sum(percent_total),
    total_n = sum(n_present),
    .groups = "drop"
  )

Figure4A <- ggplot(
  implementation_plot_df,
  aes(x = percent_total, y = practice, fill = unit_setting_cat)
) +
  geom_col(width = 0.65, color = "white") +
  
  geom_text(
    aes(
      label = paste0(round(percent_group, 1), "%")
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.6,
    fontface = "bold"
  ) +
  
  geom_text(
    data = total_labels_A,
    aes(
      x = total_percent + 2,
      y = practice,
      label = paste0(round(total_percent, 1), "%")
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.8,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = c(
      "No Cardio-oncology unit" = "#E68613",
      "Cardio-oncology unit" = "#0072B2"
    )
  ) +
  
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 105),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Implementation of guideline-based cardio-oncology practices",
    x = NULL,
    y = NULL,
    fill = "Practice setting"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "grey30")
  )
Figure4A

# Panel B: Main referral reason
referral_plot_df <- df_filtered %>%
  group_by(refer_reason_1, unit_setting_cat) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    group_total = case_when(
      unit_setting_cat == "Cardio-oncology unit" ~ sum(df_filtered$unit_setting_cat == "Cardio-oncology unit"),
      unit_setting_cat == "No Cardio-oncology unit" ~ sum(df_filtered$unit_setting_cat == "No Cardio-oncology unit")
    ),
    percent_group = 100 * n / group_total,
    percent_total = 100 * n / nrow(df_filtered),
    refer_reason_1 = dplyr::recode(
      refer_reason_1,
      "Cardiotoxicity screening" = "CTR-CVT screening",
      "Cardiotoxicity follow-up" = "CTR-CVT follow-up",
      "Prevention / protocol" = "Prevention / protocol"
    ),
    
    refer_reason_1 = factor(
      refer_reason_1,
      levels = c(
        "Prevention / protocol",
        "CTR-CVT screening",
        "CTR-CVT follow-up"
      )
    )
  )

total_labels_B <- referral_plot_df %>%
  group_by(refer_reason_1) %>%
  summarise(
    total_percent = sum(percent_total),
    .groups = "drop"
  )

Figure4B <- ggplot(
  referral_plot_df,
  aes(x = percent_total, y = refer_reason_1, fill = unit_setting_cat)
) +
  geom_col(width = 0.65, color = "white") +
  geom_text(
    aes(
      label = ifelse(
        percent_group >= 8,
        paste0(round(percent_group, 1), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.6,
    fontface = "bold"
  ) +
  geom_text(
    data = total_labels_B,
    aes(
      x = total_percent + 2,
      y = refer_reason_1,
      label = paste0(round(total_percent, 1), "%")
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.8,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "No Cardio-oncology unit" = "#E68613",
      "Cardio-oncology unit" = "#0072B2"
    )
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 105),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Primary referral patterns according to cardio-oncology setting",
    x = "Overall proportion of respondents",
    y = NULL,
    fill = "Practice setting"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "grey30"),
    legend.title = element_text(face = "bold")
  )



Figure4B
# Combined figure
Figure4 <- cowplot::plot_grid(
  Figure4A,
  Figure4B,
  ncol = 1,
  labels = c("A", "B"),
  align = "v",
  axis = "l",
  rel_heights = c(1, 1)
)

Figure4

Figure4
# Export
ggsave(Figure4, filename = "Figures/Manuscript/Figure 4.png", width = 25, height = 20, units=c("cm"), dpi = 300,limitsize = FALSE)

##### Analysis: Figure 5. Bar plot by overall and resources ####################
vars <- c(
  "has_tamo","has_echo_2d","has_echo_3d","has_strain","has_medicina_nuclear",
  "has_angiotc","has_cmr","has_troponinas","has_pro_bnp","has_hf_unit","has_arrhythmia_unit"
)

labels_map <- c(
  has_tamo = "HSCT unit (TAMO)",
  has_echo_2d = "2D echocardiography",
  has_echo_3d = "3D echocardiography",
  has_strain = "Strain imaging *",
  has_medicina_nuclear = "Nuclear medicine *",
  has_angiotc = "CT angiography",
  has_cmr = "Cardiac MRI",
  has_troponinas = "Troponins",
  has_pro_bnp = "BNP/NT-proBNP",
  has_hf_unit = "Heart failure unit",
  has_arrhythmia_unit = "Arrhythmia unit *"
)

group_levels <- c("Overall", "Cardio-oncology unit", "No cardio-oncology unit")

pal3 <- ggsci::pal_nejm()(3)
names(pal3) <- group_levels

long_total <- df_filtered %>%
  dplyr::select(has_cardio_onc_unit_dicho, all_of(vars)) %>%
  mutate(
    has_cardio_onc_unit_dicho = as.character(has_cardio_onc_unit_dicho),
    across(all_of(vars), ~ as.numeric(as.character(.x)))
  ) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "x"
  ) %>%
  mutate(
    group = if_else(
      has_cardio_onc_unit_dicho == "Yes",
      "Cardio-oncology unit",
      "No cardio-oncology unit"
    )
  )

sum_df_filtered <- bind_rows(
  long_total %>%
    group_by(variable) %>%
    summarise(
      n = sum(!is.na(x)),
      n_yes = sum(x == 1, na.rm = TRUE),
      pct_yes = 100 * n_yes / n,
      .groups = "drop"
    ) %>%
    mutate(group = "Overall"),
  
  long_total %>%
    group_by(group, variable) %>%
    summarise(
      n = sum(!is.na(x)),
      n_yes = sum(x == 1, na.rm = TRUE),
      pct_yes = 100 * n_yes / n,
      .groups = "drop"
    )
)

order_levels_total <- sum_df_filtered %>%
  filter(group == "Overall") %>%
  arrange(pct_yes) %>%
  pull(variable)

sum_df_filtered <- sum_df_filtered %>%
  mutate(
    group = factor(group, levels = group_levels),
    label = dplyr::recode(variable, !!!labels_map),
    label = factor(label, levels = dplyr::recode(order_levels_total, !!!labels_map))
  )

pos <- position_dodge2(width = 0.8, reverse = TRUE)

Figure5 <- ggplot(sum_df_filtered, aes(x = pct_yes, y = label, fill = group)) +
  geom_col(position = pos, width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_yes)),
    position = pos,
    hjust = -0.05,
    size = 3.3
  ) +
#Eliminated aes(label = sprintf("%.1f%% (%d/%d)", pct_yes, n_yes, n))
  
  scale_fill_manual(values = pal3, breaks = group_levels) +
  scale_x_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Cardio-oncology resource availability",
    subtitle = "Overall and by presence of a cardio-oncology unit (%)",
    x = "Percentage reporting availability",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

Figure5

#ggsave(Figure5, filename = "Figures/Manuscript/Figure 5.png", width = 28, height = 20, units=c("cm"), dpi = 300,limitsize = FALSE)
##### Analysis: Linear Regression models (Resource availability score) #######################################
# Resources score creation
df_filtered <- df_filtered %>%
  mutate(
    across(all_of(vars), ~ as.numeric(as.character(.x)))
  ) %>%
  rowwise() %>%
  mutate(
    resources_n_answered = sum(!is.na(c_across(all_of(vars)))),
    resources_n_yes      = sum(c_across(all_of(vars)) == 1, na.rm = TRUE),
    
    # raw sum (will be NA if everything is NA)
    resources_sum = if_else(resources_n_answered > 0, resources_n_yes, NA_real_),
    
    # scaled score to 0-11 even if some items are missing
    resources_score = if_else(
      resources_n_answered > 0,
      (resources_n_yes / resources_n_answered) * length(vars),
      NA_real_
    )
  ) 
#Releveling
df_filtered$has_cardio_onc_unit_dicho <- relevel(
  factor(df_filtered$has_cardio_onc_unit_dicho),
  ref = "No"
)
#Univariable models
m1 <- lm(resources_score ~ years_cardio_onc_num, data = df_filtered); tbl_regression(m1)
m2 <- lm(resources_score ~ has_cardio_onc_unit_dicho, data = df_filtered); tbl_regression(m2)
m3 <- lm(resources_score ~ weekly_patients_cluster, data = df_filtered); tbl_regression(m3)
#Multivariable models
m_multi_total <- lm(resources_score ~ weekly_patients_cluster + has_cardio_onc_unit_dicho + years_cardio_onc_num, data = df_filtered); summary(m_multi_total); tbl_regression(m_multi_total)
#R2 performance
performance::r2(m1)
performance::r2(m2)
performance::r2(m3)
performance::r2(m_multi_total)
#Assumptions evaluations
pm1 <- performance::check_model(m1)
pm2 <- performance::check_model(m2)
pm3 <- performance::check_model(m3)
pm_multi <- performance::check_model(m_multi_total)
# Models to be used
# Univariable table
tbl_uni <- tbl_uvregression(data = df_filtered, method = lm, y = resources_score, include = c(years_cardio_onc_num,has_cardio_onc_unit_dicho, weekly_patients_cluster)) %>% bold_p(t = 0.05)

# Multivariable model
m_multi_total <- lm(resources_score ~ years_cardio_onc_num + has_cardio_onc_unit_dicho + weekly_patients_cluster, data = df_filtered)

tbl_multi <- tbl_regression(m_multi_total) %>% bold_p(t = 0.05)

# Final merged table
tbl_final <- tbl_merge(tbls = list(tbl_uni, tbl_multi), tab_spanner = c("**Univariable model**", "**Multivariable model**")) %>% modify_caption("**Linear regression for resource availability score**")

tbl_final
# Export to Word
as_flex_table(tbl_final) %>% save_as_docx(path = "Figures/Manuscript/Table 2.docx")

# Export Assumptions evaluations
png("Figures/Manuscript/Supplementary Figure 1.png",
    width = 40, height = 25, units = "cm", res = 300)
print(pm_multi)
dev.off()

##### Analysis: Logistic regression model (Presence cardio-oncology unit)####
#Setting references and variable creation
df_filtered$has_cardio_onc_unit_dicho <- relevel(df_filtered$has_cardio_onc_unit_dicho, ref = "No")
df_filtered$weekly_patients_cluster <- factor(df_filtered$weekly_patients_cluster, levels = c("Low (1–5)", "Mid (6–25)", "High (>25)"), ordered = TRUE)
df_filtered <- df_filtered %>%
  mutate(
    training_simple = case_when(
      training_cardio_onc %in% c("Postgraduate Diploma", "Master/MSc", "Residency") ~ "Formal training", 
      training_cardio_onc %in% c("Short structured training (Course, structured training, congress)", "Self-directed", "Other") ~ "Informal training", 
      training_cardio_onc %in% c("None") ~ "None", TRUE ~ NA_character_))
df_filtered$training_simple <- factor(df_filtered$training_simple, levels = c("None", "Informal training", "Formal training"))
df_filtered$uses_hfa_icos_dicho_n <- factor(df_filtered$uses_hfa_icos_dicho, levels = c("No", "Yes"))
#Univariable models
m4 <- glm(has_cardio_onc_unit_dicho ~ years_cardio_onc_num, data = df_filtered, family = binomial); tbl_regression(m4, exponentiate = TRUE)
m5 <- glm(has_cardio_onc_unit_dicho ~ years_physician, data = df_filtered, family = binomial); tbl_regression(m5, exponentiate = TRUE)
m6 <- glm(has_cardio_onc_unit_dicho ~ sex, data = df_filtered, family = binomial); tbl_regression(m6, exponentiate = TRUE)
m7 <- glm(has_cardio_onc_unit_dicho ~ training_simple, data = df_filtered, family = binomial); tbl_regression(m7, exponentiate = TRUE)
#Multivariable models
m_multi_total_2 <- glm(has_cardio_onc_unit_dicho ~ years_cardio_onc_num + years_physician + sex + training_simple, data = df_filtered, family = binomial); tbl_regression(m_multi_total_2, exponentiate = TRUE)
#R2 performance
performance::r2(m4)
performance::r2(m5)
performance::r2(m6)
performance::r2(m_multi_total_2)
#Assumptions evaluations
pm1 <- performance::check_model(m4)
pm2 <- performance::check_model(m5)
pm3 <- performance::check_model(m6)
pm4 <- performance::check_model(m_multi_total_2)
# Models to be used
# Univariable table
tbl_uni_2 <- tbl_uvregression(data = df_filtered, method = glm, y = has_cardio_onc_unit_dicho, include = c(years_cardio_onc_num, years_physician, sex, training_simple), method.args = list(family = binomial), exponentiate = TRUE) %>% bold_p(t = 0.05)

# Multivariable model
m_multi_total_2 <- glm(has_cardio_onc_unit_dicho ~ years_cardio_onc_num + years_physician + sex + training_simple, data = df_filtered, family = binomial)
tbl_multi_2 <- tbl_regression(m_multi_total_2, exponentiate = TRUE) %>% bold_p(t = 0.05)

# Final merged table
tbl_final_2 <- tbl_merge(tbls = list(tbl_uni_2, tbl_multi_2), tab_spanner = c("**Univariable model**", "**Multivariable model**")) %>% modify_caption("**Binary logistic regression for presence of a cardio-oncology unit**")
tbl_final_2

# Export to Word
as_flex_table(tbl_final_2) %>% save_as_docx(path = "Figures/Manuscript/Table 3.docx")

# Export Assumptions evaluations
png("Figures/Manuscript/Supplementary Figure 2.png",
    width = 40, height = 25, units = "cm", res = 300)
print(pm4)
dev.off()

##### Sensitivity analysis: Mixed-effects linear regression (Resource availability score) #########################################
# Resources score creation
df <- df %>%
  mutate(
    across(all_of(vars), ~ as.numeric(as.character(.x)))
  ) %>%
  rowwise() %>%
  mutate(
    resources_n_answered = sum(!is.na(c_across(all_of(vars)))),
    resources_n_yes      = sum(c_across(all_of(vars)) == 1, na.rm = TRUE),
    
    # raw sum (will be NA if everything is NA)
    resources_sum = if_else(resources_n_answered > 0, resources_n_yes, NA_real_),
    
    # scaled score to 0-11 even if some items are missing
    resources_score = if_else(
      resources_n_answered > 0,
      (resources_n_yes / resources_n_answered) * length(vars),
      NA_real_
    )
  ) 
#Releveling
df$has_cardio_onc_unit_dicho <- relevel(
  factor(df$has_cardio_onc_unit_dicho),
  ref = "No"
)
# Mixed-effects linear regression models
m1_all<-lmerTest::lmer(resources_score ~ years_cardio_onc_num + (1|institution_id), data = df); jtools::summ(m1_all,confint=T)
m2_all<-lmerTest::lmer(resources_score ~ has_cardio_onc_unit_dicho + (1|institution_id), data = df); jtools::summ(m2_all,confint=T)
m3_all<-lmerTest::lmer(resources_score ~ weekly_patients_cluster  + (1|institution_id), data = df); jtools::summ(m3_all,confint=T)
m4_all<-lmerTest::lmer(resources_score ~ weekly_patients_cluster+ has_cardio_onc_unit_dicho  + years_cardio_onc_num + (1|institution_id), data = df); jtools::summ(m4_all,confint=T)


# Models to be used
# Univariable models
tbl_uni_mixed <- tbl_uvregression(
  data = df,
  method = lmerTest::lmer,
  y = resources_score,
  include = c(years_cardio_onc_num,
              has_cardio_onc_unit_dicho,
              weekly_patients_cluster),
  formula = "{y} ~ {x} + (1 | institution_id)"
) %>%
  bold_p(t = 0.05)
# Multivariable models
m4_all <- lmerTest::lmer(
  resources_score ~ weekly_patients_cluster +
    has_cardio_onc_unit_dicho +
    years_cardio_onc_num +
    (1 | institution_id),
  data = df
)

tbl_multi_mixed <- tbl_regression(m4_all) %>%
  bold_p(t = 0.05)

tbl_final_mixed <- tbl_merge(
  tbls = list(tbl_uni_mixed, tbl_multi_mixed),
  tab_spanner = c("**Univariable mixed model**", "**Multivariable mixed model**")
) %>%
  modify_caption("**Mixed-effects linear regression for resource availability score**")

tbl_final_mixed
# Exporting tables
as_flex_table(tbl_final_mixed) %>% save_as_docx(path = "Figures/Manuscript/Supplementary Table 4.docx")
#Assumptions evaluations
model_check_m4_all <- performance::check_model(m4_all)
performance::check_singularity(m4_all)
# Export model evaluations
png("Figures/Manuscript/Supplementary Figure 3.png",
    width = 40, height = 25, units = "cm", res = 300)
print(model_check_m4_all)
dev.off()

##### Sensitivity analysis: Mixed-effects binary logistic regression (Presence of cardio-oncology unit) #########################################
#Setting references and variable creation
df$has_cardio_onc_unit_dicho <- relevel(df$has_cardio_onc_unit_dicho, ref = "No")
df$weekly_patients_cluster <- factor(df$weekly_patients_cluster, levels = c("Low (1–5)", "Mid (6–25)", "High (>25)"), ordered = F)
df <- df %>%
  mutate(
    training_simple = case_when(
      training_cardio_onc %in% c("Postgraduate Diploma", "Master/MSc", "Residency") ~ "Formal training", 
      training_cardio_onc %in% c("Short structured training (Course, structured training, congress)", "Self-directed", "Other") ~ "Informal training", 
      training_cardio_onc %in% c("None") ~ "None", TRUE ~ NA_character_))
df$training_simple <- factor(df$training_simple, levels = c("None", "Informal training", "Formal training"))
df$uses_hfa_icos_dicho_n <- factor(df$uses_hfa_icos_dicho, levels = c("No", "Yes"))
# Mixed-effects binary logistic regression models
m5_all<-lme4::glmer(has_cardio_onc_unit_dicho ~ years_cardio_onc_num + (1|institution_id), data = df, family = binomial); jtools::summ(m5_all,confint=T, exp = T)
m6_all<-lme4::glmer(has_cardio_onc_unit_dicho ~ years_physician + (1|institution_id), data = df, family = binomial); jtools::summ(m6_all,confint=T, exp = T)
m7_all<-lme4::glmer(has_cardio_onc_unit_dicho ~ sex  + (1|institution_id), data = df, family = binomial); jtools::summ(m7_all,confint=T, exp = T)
m8_all<-lme4::glmer(has_cardio_onc_unit_dicho ~ training_simple  + (1|institution_id), data = df, family = binomial); jtools::summ(m8_all,confint=T, exp = T)
m9_all<-lme4::glmer(has_cardio_onc_unit_dicho ~ years_cardio_onc_num + years_physician + sex + training_simple + (1|institution_id), data = df, family = binomial); jtools::summ(m9_all,confint=T, exp = T)

# Models to be used
# Univariable mixed-effects logistic regression models
tbl_uni_glmer <- tbl_uvregression(
  data = df,
  method = lme4::glmer,
  y = has_cardio_onc_unit_dicho,
  include = c(
    years_cardio_onc_num,
    years_physician,
    sex,
    training_simple
  ),
  formula = "{y} ~ {x} + (1 | institution_id)",
  method.args = list(family = binomial),
  exponentiate = TRUE,
  tidy_fun = broom.mixed::tidy
) %>%
  bold_p(t = 0.05)

# Multivariable mixed-effects logistic regression model
m_multi_glmer <- glmer(
  has_cardio_onc_unit_dicho ~
    years_cardio_onc_num +
    years_physician +
    sex +
    training_simple +
    (1 | institution_id),
  data = df,
  family = binomial
)

tbl_multi_glmer <- tbl_regression(
  m_multi_glmer,
  exponentiate = TRUE,
  tidy_fun = broom.mixed::tidy
) %>%
  bold_p(t = 0.05)

# Final merged table
tbl_final_glmer <- tbl_merge(
  tbls = list(tbl_uni_glmer, tbl_multi_glmer),
  tab_spanner = c(
    "**Univariable mixed model**",
    "**Multivariable mixed model**"
  )
) %>%
  modify_caption(
    "**Mixed-effects binary logistic regression for presence of a cardio-oncology unit**"
  )

tbl_final_glmer
# Exporting tables
as_flex_table(tbl_final_glmer) %>% save_as_docx(path = "Figures/Manuscript/Supplementary Table 5.docx")
# Evaluating model
model_check_glmer <- performance::check_model(m_multi_glmer)
performance::check_singularity(m_multi_glmer)
performance::check_overdispersion(m_multi_glmer)
# Export model evaluations
png("Figures/Manuscript/Supplementary Figure 4.png", width = 40, height = 25, units = "cm", res = 300)
print(model_check_glmer)
dev.off()

#### Sensitivity analysis: Linear regression (Resource availability score) with greatest experience specialist ##############
# Filtering specialist with the greatest experience in cardio-oncology
df_sensitivity <- df %>%
  group_by(institution_id) %>%
  arrange(desc(years_cardio_onc_num), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Resources score creation
vars <- c(
  "has_tamo","has_echo_2d","has_echo_3d","has_strain","has_medicina_nuclear",
  "has_angiotc","has_cmr","has_troponinas","has_pro_bnp","has_hf_unit","has_arrhythmia_unit"
)
df_sensitivity <- df_sensitivity %>%
  mutate(
    across(all_of(vars), ~ as.numeric(as.character(.x)))
  ) %>%
  rowwise() %>%
  mutate(
    resources_n_answered = sum(!is.na(c_across(all_of(vars)))),
    resources_n_yes      = sum(c_across(all_of(vars)) == 1, na.rm = TRUE),
    
    # raw sum (will be NA if everything is NA)
    resources_sum = if_else(resources_n_answered > 0, resources_n_yes, NA_real_),
    
    # scaled score to 0-11 even if some items are missing
    resources_score = if_else(
      resources_n_answered > 0,
      (resources_n_yes / resources_n_answered) * length(vars),
      NA_real_
    )
  ) 
#Releveling
df_sensitivity$has_cardio_onc_unit_dicho <- relevel(
  factor(df_sensitivity$has_cardio_onc_unit_dicho),
  ref = "No"
)
#Univariable models
ms1 <- lm(resources_score ~ years_cardio_onc_num, data = df_sensitivity); tbl_regression(ms1)
ms2 <- lm(resources_score ~ has_cardio_onc_unit_dicho, data = df_sensitivity); tbl_regression(ms2)
ms3 <- lm(resources_score ~ weekly_patients_cluster, data = df_sensitivity); tbl_regression(ms3)
#Multivariable models
m_multisens_total <- lm(resources_score ~ weekly_patients_cluster + has_cardio_onc_unit_dicho + years_cardio_onc_num, data = df_sensitivity); summary(m_multisens_total); tbl_regression(m_multisens_total)
#R2 performance
performance::r2(ms1)
performance::r2(ms2)
performance::r2(ms3)
performance::r2(m_multisens_total)
#Assumptions evaluations
pms1 <- performance::check_model(ms1)
pms2 <- performance::check_model(ms2)
pms3 <- performance::check_model(ms3)
pms_multi <- performance::check_model(m_multisens_total)
# Models to be used
# Univariable table
tbl_uni_sens <- tbl_uvregression(data = df_sensitivity, method = lm, y = resources_score, include = c(years_cardio_onc_num,has_cardio_onc_unit_dicho, weekly_patients_cluster)) %>% bold_p(t = 0.05)

# Multivariable model
m_multi_total_sens <- lm(resources_score ~ years_cardio_onc_num + has_cardio_onc_unit_dicho + weekly_patients_cluster, data = df_sensitivity)

tbl_multi_sens <- tbl_regression(m_multi_total_sens) %>% bold_p(t = 0.05)

# Final merged table
tbl_final_sens <- tbl_merge(tbls = list(tbl_uni_sens, tbl_multi_sens), tab_spanner = c("**Univariable model**", "**Multivariable model**")) %>% modify_caption("**Linear regression for resource availability score**")

tbl_final_sens
# Export to Word
as_flex_table(tbl_final_sens) %>% save_as_docx(path = "Figures/Manuscript/Supplementary Table 6.docx")

# Export Assumptions evaluations
png("Figures/Manuscript/Supplementary Figure 5.png",
    width = 40, height = 25, units = "cm", res = 300)
print(pms_multi)
dev.off()

#### Citation ##################################################################
citation("nortest")
citation("gtsummary")
citation("rnaturalearth")
citation("ggplot2")
citation("performance")
citation("lme4")
citation("lmtest")


