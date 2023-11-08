# import libraries
library(dplyr)

ncdb_recode <- function(df) {
#########################################
  df$PUF_CASE_ID
#########################################


#########################################
  df$PUF_FACILITY_ID <- as.factor(df$PUF_FACILITY_ID)

  code_dict <- setNames(paste0("Facility ",
  seq_along(levels(df$PUF_FACILITY_ID))),
  levels(df$PUF_FACILITY_ID))

  df$Facility <- code_dict[df$PUF_FACILITY_ID]
#########################################


#########################################
  df$FACILITY_TYPE_CD <-
    factor(
      df$FACILITY_TYPE_CD,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Community Cancer Program",
        "Comprehensive Community Cancer Program",
        "Academic/Research Program (includes NCI-designated comprehensive cancer centers)",
        "Integrated Network Cancer Program"
      )
    )
#########################################


#########################################
  df$FACILITY_LOCATION_CD <-
    factor(
      df$FACILITY_LOCATION_CD,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "New England",
        "Middle Atlantic",
        "South Atlantic",
        "East North Central",
        "East South Central",
        "West North Central",
        "West South Central",
        "Mountain",
        "Pacific"
      )
    )
#########################################


#########################################
  df$PUF_MULT_SOURCE <-
    factor(
      df$PUF_MULT_SOURCE,
      levels = c(0, 1),
      labels = c(
        'Only one CoC facility reported this case to NCDB',
        'Records pertaining to this case submitted to NCDB by more than one CoC facility'
      )
    )
#########################################


#########################################
  df$AGE[df$AGE == 999] <- NA 
#########################################


#########################################
  df$SEX <-
    factor(df$SEX,
           levels = c(1, 2),
           labels = c("Male", "Female"))
#########################################


#########################################
  df$RACE <-
    factor(
      df$RACE,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 25, 26, 27, 28, 30, 31, 32, 96, 97, 98, 99),
      labels = c(
        "White",
        "Black",
        "American Indian, Aleutian, or Eskimo",
        "Chinese",
        "Japanese",
        "Filipino",
        "Hawaiian",
        "Korean",
        "Vietnamese",
        "Laotian",
        "Hmong",
        "Kampuchean (including Khmer and Cambodian)",
        "Thai",
        "Asian Indian or Pakistani, NOS (formerly code 09)",
        "Asian Indian",
        "Pakistani",
        "Micronesian, NOS",
        "Chamorran",
        "Guamanian, NOS",
        "Polynesian, NOS",
        "Tahitian",
        "Samoan",
        "Tongan",
        "Melanesian, NOS",
        "Fiji Islander",
        "New Guinean",
        "Other Asian, including Asian, NOS and Oriental, NOS",
        "Pacific Islander, NOS",
        "Other",
        "Unknown"
      )
    )
#########################################


#########################################
  df$SPANISH_HISPANIC_ORIGIN <-
    factor(
      df$SPANISH_HISPANIC_ORIGIN,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "Non-Spanish; non-Hispanic",
        "Mexican (includes Chicano)",
        "Puerto Rican",
        "Cuban",
        "South or Central America (except Brazil)",
        "Other specified Spanish/Hispanic origin (includes European)",
        "Spanish, NOS; Hispanic, NOS; Latino, NOS (There is evidence other than surname or maiden name that the person is Hispanic, but he/she cannot be assigned to any category of 1-5)",
        "Spanish surname only (The only evidence of the person's Hispanic origin is surname or maiden name, and there is no contrary evidence that the person is not Hispanic)",
        "Dominican Republic (for use with patients who were diagnosed with cancer on January 1, 2005, or later)",
        "Unknown whether Spanish or not; not stated in patient record"
      )
    )
#########################################


#########################################
  df$INSURANCE_STATUS <-
    factor(
      df$INSURANCE_STATUS,
      levels = c(0, 1, 2, 3, 4, 9),
      labels = c(
        "Not Insured",
        "Private Insurance/Managed Care",
        "Medicaid",
        "Medicare",
        "Other Government",
        "Insurance Status unknown"
      )
    )
#########################################


#########################################
  df$NO_HSD_QUAR_00 <-
    factor(
      df$NO_HSD_QUAR_00,
      levels = c(1, 2, 3, 4),
      labels = c("29% or more",
                 "20% - 28.9%",
                 "14% - 19.9%",
                 "Less than 14%")
    )
#########################################


#########################################
  df$NO_HSD_QUAR_12 <-
    factor(
      df$NO_HSD_QUAR_12,
      levels = c(1, 2, 3, 4),
      labels = c("21% or more",
                 "13%-20.9%",
                 "7%-12.9%",
                 "Less than 7%")
    )
#########################################


#########################################
  df$NO_HSD_QUAR_2016 <-
    factor(
      df$NO_HSD_QUAR_2016,
      levels = c(1, 2, 3, 4),
      labels = c("17.6% or more",
                 "10.9% - 17.5%",
                 "6.3% - 10.8%",
                 "Less than 6.3%")
    )
#########################################


#########################################
  df$NO_HSD_QUAR_2020 <-
    factor(
      df$NO_HSD_QUAR_2020,
      levels = c(1, 2, 3, 4),
      labels = c("15.3% or more",
                 "9.1% - 15.2%",
                 "5.0% - 9.0%",
                 "Less than 5.0%")
    )
#########################################


#########################################
  df$MED_INC_QUAR_00 <-
    factor(
      df$MED_INC_QUAR_00,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than $30,000",
        "$30,000-$34,999",
        "$35,000-$45,999",
        "$46,000 or more"
      )
    )
#########################################


#########################################
  df$MED_INC_QUAR_12 <-
    factor(
      df$MED_INC_QUAR_12,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than $38,000",
        "$38,000-$47,999",
        "$48,000-$62,999",
        "$63,000 or more"
      )
    )
#########################################


#########################################
  df$MED_INC_QUAR_2016 <-
    factor(
      df$MED_INC_QUAR_2016,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than $40,227",
        "$40,227 - $50,353",
        "$50,354 - $63,332",
        "63,333+")
    )
#########################################


#########################################
  df$MED_INC_QUAR_2020 <-
    factor(
      df$MED_INC_QUAR_2020,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than $46,277",
        "$46,277 - $57,856",
        "$57,857 - $74,062",
        "$74,063+")
    )
#########################################


#########################################
  df$UR_CD_03 <-
    factor(
      df$UR_CD_03,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        # Metro counties
        "Counties in metro areas of 1 million population or more",
        "Counties in metro areas of 250,000 to 1 million population",
        "Counties in metro areas of fewer than 250,000 population",
        # Urban Counties
        "Urban population of 20,000 or more, adjacent to a metro area.",
        "Urban population of 20,000 or more, not adjacent to a metro area.",
        "Urban population of 2,500 to 19,999, adjacent to a metro area.",
        "Urban population of 2,500 to 19,999, not adjacent to a metro area.",
        # Rural Counties
        "Completely rural or less than 2,500 urban population, adjacent to a metro area",
        "Completely rural or less than 2,500 urban population, not adjacent to a metro area"

      )
    )
#########################################


#########################################
  df$UR_CD_13 <-
    factor(
      df$UR_CD_13,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        # Metro counties
        "Metro areas >= 1 million",
        "Metro areas of 250,000 to 1 million",
        "Metro areas of fewer than 250,000",
        # Urban Counties
        "Urban areas of 20,000 or more, adjacent to a metro area",
        "Urban population of 20,000 or more, not adjacent to a metro area",
        "Urban population of 2,500 to 19,999, adjacent to a metro area",
        "Urban population of 2,500 to 19,999, not adjacent to a metro area",
        # Rural Counties
        "Completely rural or less than 2,500 urban population, adjacent to a metro area",
        "Completely rural or less than 2,500 urban population, not adjacent to a metro area"
      )
    )
#########################################


#########################################
  #URBAN_RURAL
  # Calculated column to group the UR_CD_13 values by Metro/Urban/Rural
  df$URBAN_RURAL <- NA
  df$URBAN_RURAL[df$UR_CD_13 %in% c(
    "Metro areas >= 1 million",
    "Metro areas of 250,000 to 1 million",
    "Metro areas of fewer than 250,000"
  )] <- 0
  df$URBAN_RURAL[df$UR_CD_13 %in% c(
    "Urban areas of 20,000 or more, adjacent to a metro area.",
    "Urban population of 20,000 or more, not adjacent to a metro area.",
    "Urban population of 2,500 to 19,999, adjacent to a metro area.",
    "Urban population of 2,500 to 19,999, not adjacent to a metro area."
  )] <- 1
  df$URBAN_RURAL[df$UR_CD_13 %in% c(
    "Completely rural or less than 2,500 urban population, adjacent to a metro area",
    "Completely rural or less than 2,500 urban population, not adjacent to a metro area"
  )] <- 2

  df$URBAN_RURAL <-
    factor(
      df$URBAN_RURAL,
      levels = c(0, 1, 2),
      labels = c("Metro",
                 "Urban",
                 "Rural")
    )
#########################################


#########################################
  df$PUF_MEDICAID_EXPN_CODE <-
    factor(
      df$PUF_MEDICAID_EXPN_CODE,
      levels = c(0, 1, 2, 3, 9),
      labels = c("Non-Expansion States",
                 "January 2014 Expansion States",
                 "Early Expansion States (2010 - 2013)",
                 "Late Expansion States (after Jan 2014)",
                 "Suppressed for Ages 0-39")
    )
#########################################


#########################################
  df$CROWFLY
#########################################


#########################################
  df$CDCC_TOTAL_BEST <-
    factor(
      df$CDCC_TOTAL_BEST,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Total Charlson Score of 0",
        "Total Charlson score of 1",
        "Total Charlson score of 2",
        "Total Charlson score of 3 or more"
      )
    )
#########################################


#########################################
  df$SARSCOV2_POS <-
    factor(
      df$SARSCOV2_POS,
      levels = c(0, 1, 9),
      labels = c(
        "Patient did not test positive for active SARS-CoV-2: No positive test",
        "Patient tested positive for active SARS-CoV-2; test positive on at least one test",
        "Unknown if tested; test done, results unknown"
      )
    )
#########################################


#########################################
  df$SARSCOV2_POS_DAYS
#########################################


#########################################
  df$SARSCOV2_TEST <-
    factor(
      df$SARSCOV2_TEST,
      levels = c(0, 1, 9),
      labels = c(
        "Patient not tested for SARS-CoV-2: facility records support that patient did not undergo pre-admit or in-hospital testing",
        "Patient tested for active SARS-CoV-2",
        "Unknown if patient tested for SARS-CoV-2/No facility record of preadmit hospital testing of SARS-CoV-2"
      )
    )
#########################################


#########################################
  df$SEQUENCE_NUMBER 
#########################################


#########################################
  df$CLASS_OF_CASE <-
    factor(
      df$CLASS_OF_CASE,
      levels = c(00, 10, 11, 12, 13, 14, 20, 21, 22),
      labels = c(
        "Diagnosis at the reporting facility and all treatment or a decision not to treat was done elsewhere.",
        "Initial diagnosis at the reporting facility, and part or all of first course treatment or a decision not to treat was at the reporting facility, NOS.",
        "Initial diagnosis in a staff physician's office and part of first course treatment was done at the reporting facility.",
        "Initial diagnosis in a staff physician's office and all of first course treatment or a decision not to treate was done at the reporting facility.",
        "Initial diagnosis at the reporting facility and part of first course treatment was done at the reporting facility; part of first course treatment was done elsewhere.",
        "Initial diagnosis at the reporting facility and all of first course treatment or a decision not to treat was done at the reporting facility.",
        "Initial diagnosis elsewhere and all or part of first course treatment or a decision not to treat was done at the reporting facility, NOS.",
        "Initial diagnosis elsewhere and part of first course treatment was done at the reporting facility; part of first course treatment was done elsewhere.",
        "Initial diagnosis elsewhere and all of first course treatment or a decision not to treat was done at the reporting facility."
      )
    )
#########################################


#########################################
  df$YEAR_OF_DIAGNOSIS
#########################################


#########################################
  df$PRIMARY_SITE
#########################################


#########################################
  df$LATERALITY <-
    factor(
      df$LATERALITY,
      levels = c(0, 1, 2, 3, 4, 5, 9),
      labels = c(
        "Organ is not considered to be a paired site.",
        "Origin of primary is right.",
        "Origin of primary is left.",
        "Only one side involved, right or left origin not specified.",
        "Bilateral involvement, side of origin unknown, stated to be a single primary. This includes: Both ovaries simultaneously involved with a single histology, Bilateral retinoblastomas, Bilateral Wilms tumors",
        "Midline of a paired site tumors",
        "Paired site, but lateral origin unknown; midline tumor."
      )
    )
#########################################


#########################################
df$HISTOLOGY
#########################################


#########################################
  df$BEHAVIOR <-
    factor(
      df$BEHAVIOR,
      levels = c(0, 1, 2, 3),
      labels = c(
        "Benign Benign",
        "Borderline",
        "In situ",
        "Invasive or microinvasive"
      )
    )
#########################################


#########################################
  df$GRADE <-
    factor(
      df$GRADE,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "Grade I,1,i Well differentiated; differentiated, NOS",
        "Grade II,2,ii,I/III, or 1/3 (Moderately differentiated; moderately well differentiated; intermediate differentiation)",
        "Grade III,3,iii, II/III, or 2/3 (Poorly differentiated)",
        "Grade IV,4,iv,III/III, or 3/3 (Undifferentiated; anaplastic)",
        "T cell; T-precursor",
        "B cell; pre-B; B-precursor",
        "Null cell; non T-non B",
        "NK (natural killer) cell (effective with diagnosis 1/1/95 and after)",
        "Cell type not determined, not stated or not applicable;unknown primaries; high grade dysplasia (adenocarcinoma in situ)"
      )
    )
#########################################


#########################################
  df$GRADE_RECODE <- NA
  df$GRADE_RECODE[df$GRADE %in% c(1, 2)] <- 0
  df$GRADE_RECODE[df$GRADE %in% c(3, 4)] <- 1
  df$GRADE_RECODE[df$GRADE %in% c(5, 6, 7, 8, 9)] <- 2
  df$GRADE_RECODE <-
    factor(
      df$GRADE_RECODE,
      levels = c(0, 1, 2),
      labels = c("Low (I/II)",
                 "High (III/IV)",
                 "Other")
    )
#########################################


#########################################
  df$DIAGNOSTIC_CONFIRMATION <-
    factor(
      df$DIAGNOSTIC_CONFIRMATION,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "Positive histology", #Histologic confirmation (tissue microscopically examined)
        "Positive cytology", #	Cytologic confirmation (no tissue microscopically examined; fluid cells microscopically examined
        "Postive histology PLUS positive immunophenotyping and/or positive genetic studies", #Histology is positive for cancer, and there are also positive immunophenotyping and/or genetic test results. Use this code only for histology range 9590-9992 where the year of diagnosis is 2010 or later
        "Positive microscopic confirmation, method not specified", #Microscopic confirmation is all that is known. It is unknown if the cells were from histology or cytology
        "Positive laboratory test/marker study", #A clinical diagnosis of cancer is based on laboratory tests/marker studies which are clinically diagnostic for cancer. This includes alpha-fetoprotein for liver cancer and abnormal electrophoretic spike for multiple myeloma. Elevated PSA is nondiagnostic of cancer. If the physician uses the PSA as a basis for diagnosing prostate cancer with no other workup, record as code 5. (Adapted from SEER.)
        "Direct visualization without microscopic confirmation", #The tumor was visualized during a surgical/endoscopic procedure only with no tissue resected for microscopic examination
        "Radiography and other imaging techniques without microscopic confirmation", #The malignancy was reported by the physician from an imaging technique report only
        "Clinical diagnosis only", # (Other than 5,6,7) The malignancy was reported by the physician in the medical record
        "A statement of malignancy was reported in the medical record" #A statement of malignancy was reported in the medical record, but there is no statement of how the cancer was diagnosed (usually Class of Case 3)
      )
    )
#########################################


#########################################
  df$REGIONAL_NODES_EXAMINED <-
    # when 95, 96, 97, 98, or 99 make NA
    ifelse(df$REGIONAL_NODES_EXAMINED %in% c(95, 96, 97, 98, 99), NA, df$REGIONAL_NODES_EXAMINED)
#########################################


#########################################
  df$REGIONAL_NODES_EXAMINED_notes <- 
  ifelse(df$REGIONAL_NODES_EXAMINED %in% c(95), "No regional nodes removed, but aspiration or core biopsy of regional nodes was performed",
          ifelse(df$REGIONAL_NODES_EXAMINED %in% c(96), "Regional lymph node removal was documented as sampling, and the number of nodes is unknown/not stated",
                ifelse(df$REGIONAL_NODES_EXAMINED %in% c(97), "Regional lymph node removal was documented as dissection, and the number of nodes is unknown/notstated",
                        ifelse(df$REGIONAL_NODES_EXAMINED %in% c(98), "Regional lymph nodes surgically removed but number of lymph nodes unknown or not stated, and not documented as sampling or dissection; nodes were examined, but the number is unknown",
                              ifelse(df$REGIONAL_NODES_EXAMINED %in% c(99), "Unknown if regional nodes examined. Not applicable or negative. Not stated in patient record.", NA)))))
#########################################


#########################################
  df$REGIONAL_NODES_POSITIVE <-
    # when 95, 97, 98, or 99 make NA
    ifelse(df$REGIONAL_NODES_POSITIVE %in% c(95, 97, 98, 99), NA, df$REGIONAL_NODES_POSITIVE)
#########################################


#########################################
  df$REGIONAL_NODES_POSITIVE_notes <-
  ifelse(df$REGIONAL_NODES_POSITIVE %in% c(95), "Positive aspiration or core biopsy of lymph node(s)",
          ifelse(df$REGIONAL_NODES_POSITIVE %in% c(97), "Positive nodes are documented, but the number are unspecified",
                ifelse(df$REGIONAL_NODES_POSITIVE %in% c(98), "No nodes examined",
                        ifelse(df$REGIONAL_NODES_POSITIVE %in% c(99), "It is unknown whether nodes are positive; not applicable; not stated in patient record", NA))))
#########################################


#########################################
  df$SLN_EXAM <- 
  ifelse(df$SLN_EXAM %in% c(95,98,99), NA, SLN_EXAM)
#########################################


#########################################
  df$SLN_POS <-
  ifelse(df$SLN_POS %in% c(95,97,98,99), NA, SLN_POS)
#########################################


#########################################
  df$SLN_EXAM_notes <-
  ifelse(df$SLN_EXAM %in% c(95), "No sentinel nodes were removed, but aspiration of sentinel node(s) was performed",
          ifelse(df$SLN_EXAM %in% c(98), "Sentinel lymph nodes were biopsied, but the number is unknown",
                ifelse(df$SLN_EXAM %in% c(99), "It is unknown whether sentinel nodes were examined; not applicable or negative; not stated in patient record", NA)))
#########################################


#########################################
  df$SLN_POS_notes <-
  ifelse(df$SLN_POS %in% c(95), "Positive aspiration of sentinel lymph node(s) was performed",
          ifelse(df$SLN_POS %in% c(97), "Positive sentinel nodes are documented, but the number is unspecified",
                ifelse(df$SLN_POS %in% c(98), "No sentinel nodes were biopsied",
                        ifelse(df$SLN_POS %in% c(99), "It is unknown whether sentinel nodes are positive; not applicable; not stated in patient record", NA))))
#########################################


#########################################
  df$SENTINEL_LNBX_STARTED_DAY
#########################################


#########################################
  df$REG_LN_DISS_STARTED_DAY
#########################################


#########################################
  df$RX_SUMM_DXSTG_PROC <-
    factor(
      df$RX_SUMM_DXSTG_PROC,
      levels = c(0, 1, 2, 3, 4, 5, 7, 9),
      labels = c(
        "No surgical diagnosis or staging performed",
        "Biopsy done to site other than primary",
        "Biopsy done to the primary site",
        "Surgical exploration only, no biopsy or treatment",
        "Surgical procedure with bypass, no biopsy",
        "Exploratory procedure with biopsy of primary or other site",
        "Procedure done, type unknown",
        "No information on diagnosis or staging"
      )
    )
#########################################


#########################################
  df$RX_HOSP_DXSTG_PROC <-
    factor(
      as.numeric(df$RX_HOSP_DXSTG_PROC),
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "No surgical diagnostic or staging procedure was performed",
        "A biopsy (incisional, needle, or aspiration) was done to a site other than the primary. No exploratory procedure was done",
        "A biopsy (incisional, needle, or aspiration) was done to the primary site",
        "A surgical exploration only. The patient was not biopsied or treated",
        "A surgical procedure with a bypass was performed, but no biopsy was done",
        "An exploratory procedure was performed, and a biopsy of either the primary site or another site was done",
        "A bypass procedure was performed, and a biopsy of either the primary site or another site was done",
        "A procedure was done, but the type of procedure is unknown",
        "No information of whether a diagnostic or staging procedure was performed"
      )
    )
#########################################


#########################################
  df$DX_STAGING_PROC_DAYS
#########################################


#########################################
  df$TNM_CLIN_T <- trimws(df$TNM_CLIN_T, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_CLIN_N <- trimws(df$TNM_CLIN_N, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_CLIN_M <- trimws(df$TNM_CLIN_M, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_T <- trimws(df$TNM_PATH_T, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_N <- trimws(df$TNM_PATH_N, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_M <- trimws(df$TNM_PATH_M, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_CLIN_STAGE_GROUP <- trimws(df$TNM_CLIN_STAGE_GROUP, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  df$TNM_PATH_STAGE_GROUP <- trimws(df$TNM_PATH_STAGE_GROUP, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

  df$TNM_CLIN_T <-
    factor(
      df$TNM_CLIN_T,
      levels = c(
        "cX",
        "c0",
        "cA",
        "pIS",
        "pISPU",
        "pISPD",
        "c1MI",
        "c1",
        "c1A",
        "c1A1",
        "c1A2",
        "c1B",
        "c1B1",
        "c1B2",
        "c1C",
        "c1D",
        "c2",
        "c2A",
        "c2A1",
        "c2A2",
        "c2B",
        "c2C",
        "c2D",
        "c3",
        "c3A",
        "c3B",
        "c3C",
        "c3D",
        "c4",
        "c4A",
        "c4B",
        "c4C",
        "c4D",
        "c4E",
        "88"
      ),
      labels = c(
        "cTX",
        "cT0",
        "cTa",
        "pTis",
        "pTispu",
        "pTispd",
        "cT1mic",
        "cT1",
        "cT1a",
        "cT1a1",
        "cT1a2",
        "cT1b",
        "cT1b1",
        "cT1b2",
        "cT1c",
        "cT1d",
        "cT2",
        "cT2a",
        "cT2a1",
        "cT2a2",
        "cT2b",
        "cT2c",
        "cT2d",
        "cT3",
        "cT3a",
        "cT3b",
        "cT3c",
        "cT3d",
        "cT4",
        "cT4a",
        "cT4b",
        "c4c",
        "c4d",
        "c4e",
        "Not applicable"
      )
    )

  df$cT_RECODE <- NA
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cTX")] <- 0
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT0", "cTa")] <- 1
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT1", "cT1a", "cT1a1", "cT1a2", "ct1b", "ct1b1", "ct1b2", "cT1c", "cT1d")] <- 2
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT2", "cT2a", "cT2a1", "cT2a2", "cT2b", "cT2c", "cT2d")] <- 3
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT3", "cT3a", "cT3b", "cT3c", "cT3d")] <- 4
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT4")] <- 5
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT4a")] <-6
  df$cT_RECODE[df$TNM_CLIN_T %in% c("cT4b")] <- 7
  df$cT_RECODE[df$TNM_CLIN_T %in% c("c4c")] <- 8
  df$cT_RECODE[df$TNM_CLIN_T %in% c("c4d")] <- 9
  df$cT_RECODE[df$TNM_CLIN_T %in% c("c4e")] <- 10
  df$cT_RECODE[df$TNM_CLIN_T %in% c("pTis", "pTispu", "pTispd")] <- 11 ## Look into how this is used/what to call these



  df$cT_RECODE <-
    factor(
      df$cT_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
      labels = c("Tx",
                 "T0",
                 "T1",
                 "T2",
                 "T3",
                 "T4",
                 "T4a",
                 "T4b",
                 "T4c",
                 "T4d",
                 "T4e",
                 "cIs or similar")
    )
#########################################



#########################################
  df$TNM_CLIN_N <-
    factor(
      df$TNM_CLIN_N,
      levels = c(
        "cX",
        "c0",
        "c0I-",
        "c0I+",
        "c0M-",
        "c0M+",
        "c1MI",
        "c0A",
        "c0B",
        "c1",
        "c1A",
        "c1B",
        "c1C",
        "c2",
        "c2A",
        "c2B",
        "c2C",
        "c3",
        "c3A",
        "c3B",
        "c3C",
        "c4",
        "88"
      ),
      labels = c(
        "cNX",
        "cN0",
        "cN0i-",
        "cN0i+",
        "cN0m-",
        "cN0m+",
        "cN1mi",
        "cN0a",
        "cN0b",
        "cN1",
        "cN1a",
        "cN1b",
        "cN1c",
        "cN2",
        "cN2a",
        "cN2b",
        "cN2c",
        "cN3",
        "cN3a",
        "cN3b",
        "cN3c",
        "cN4",
        "Not applicable"
      )
    )


  df$cN_RECODE <- NA
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cNX")] <- 0
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN0", "cN0i-", "cN0i+", "cN0m-", "cN0m+", "cN0a", "cN0b")] <- 1
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN1mi", "cN1", "cN1a", "cN1b", "cN1c")] <- 2
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2", "cN2a", "cN2b", "cN2c")] <- 3
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2a")] <- 4
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2b")] <- 5
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN2c")] <- 6
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN3", "cN3a", "cN3b", "cN3c")] <- 7
  df$cN_RECODE[df$TNM_CLIN_N %in% c("cN4")] <- 8


  df$cN_RECODE <-
    factor(
      df$cN_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7),
      labels = c("NX",
                 "N0",
                 "N1",
                 "N2A",
                 "N2B",
                 "N2C",
                 "N3",
                 "N4")
    )
#########################################


#########################################
  df$TNM_CLIN_M <-
    factor(
      df$TNM_CLIN_M,
      levels = c("cX", "c0", "c0I+", "c1", "c1A", "c1B", "c1C", "c1D", "88"),
      labels = c(
        "cMX",
        "cM0",
        "cM0(i+)",
        "cM1",
        "cM1a",
        "cM1b",
        "cM1c",
        "cM1d",
        "Not applicable (not defined)"
      )
    )



  df$cM_RECODE <- NA
  df$cM_RECODE[df$TNM_CLIN_M %in% c("cMX")] <- 0
  df$cM_RECODE[df$TNM_CLIN_M %in% c("cM0", "cM0(i+)")] <- 1
  df$cM_RECODE[df$TNM_CLIN_M %in% c("cM1", "cM1a", "cM1b", "cM1c", "cM1d")] <- 2
  df$cM_RECODE[df$TNM_CLIN_M %in% c("Not applicable (not defined)")] <- ""


  df$cM_RECODE <-
    factor(
      df$cM_RECODE,
      levels = c(0, 1, 2),
      labels = c("Mx",
                 "M0",
                 "M1"
      )
    )
#########################################


#########################################
  df$TNM_CLIN_STAGE_GROUP <-
    factor(
      df$TNM_CLIN_STAGE_GROUP,
      levels = c(
        "0",
        "0A",
        "0IS",
        "1",
        "1A",
        "1A1",
        "1A2",
        "1B",
        "1B1",
        "1B2",
        "1C",
        "1S",
        "2",
        "2A",
        "2A1",
        "2A2",
        "2B",
        "2C",
        "3",
        "3A",
        "3B",
        "3C",
        "3C1",
        "3C2",
        "4",
        "4A",
        "4A1",
        "4A2",
        "4B",
        "4C",
        "OC",
        "88",
        "99"
      ),
      labels = c(
        "cStage 0",
        "cStage 0A",
        "cStage 0is",
        "cStage I",
        "cStage IA",
        "cStage IA1",
        "cStage IA2",
        "cStage IB",
        "cStage IB1",
        "cStage IB2",
        "cStage IC",
        "cStage IS",
        "cStage II",
        "cStage IIA",
        "cStage IIA1",
        "cStage IIA2",
        "cStage IIB",
        "cStage IIC",
        "cStage III",
        "cStage IIIA",
        "cStage IIIB",
        "cStage IIIC",
        "cStage IIIC1",
        "cStage IIIC2",
        "cStage IV",
        "cStage IVA",
        "cStage IVA1",
        "cStage IVA2",
        "cStage IVB",
        "cStage IVC",
        "Occult",
        "Not applicable",
        "Unknown"
      )
    )


  df$cSTAGE_RECODE <- NA
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage 0","cStage 0A","cStage 0is")] <- 0
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage I","cStage IA","cStage IA1","cStage IA2","cStage IB","cStage IB1","cStage IB2","cStage IC","cStage IS")] <- 1
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage II","cStage IIA","cStage IIA1","cStage IIA2","cStage IIB","cStage IIC")] <- 2
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage III","cStage IIIA","cStage IIIB","cStage IIIC","cStage IIIC1","cStage IIIC2")] <- 3
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("cStage IV", "cStage IVA","cStage IVA1","cStage IVA2","cStage IVB","cStage IVC")] <- 4
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("Occult")] <- 5
  df$cSTAGE_RECODE[df$TNM_CLIN_STAGE_GROUP %in% c("Not applicable", "Unknown")] <- 6


  df$cSTAGE_RECODE <-
    factor(
      df$cSTAGE_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c("cStage 0",
                 "cStage I",
                 "cStage II",
                 "cStage III",
                 "cStage IV",
                 "Occult",
                 "Not applicable/Unknown")
    )
#########################################


#########################################
  df$TNM_PATH_T <-
    factor(
      df$TNM_PATH_T,
      levels = c(
        "pX",
        "p0",
        "pA",
        "pIS",
        "pISPU",
        "pISPD",
        "p1MI",
        "p1",
        "p1A",
        "p1A1",
        "p1A2",
        "p1B",
        "p1B1",
        "p1B2",
        "p1C",
        "p1D",
        "p2",
        "p2A1",
        "p2A2",
        "p2B",
        "p2C",
        "p2D",
        "p3",
        "p3A",
        "p3B",
        "p3C",
        "p3D",
        "p4",
        "p4A",
        "p4B",
        "p4C",
        "p4D",
        "p4E",
        "88"
        ),
      labels = c(
        "pTX",
        "pT0",
        "pTa",
        "pTis",
        "pTispu",
        "pTispd",
        "pT1mic",
        "pT1",
        "pT1a",
        "pT1a1",
        "pT1a2",
        "pT1b",
        "pT1b1",
        "pT1b2",
        "pT1c",
        "pT1d",
        "pT2",
        "pT2a1",
        "pT2a2",
        "pT2b",
        "pT2c",
        "pT2d",
        "pT3",
        "pT3a",
        "pT3b",
        "pT3c",
        "pT3d",
        "pT4",
        "pT4a",
        "pT4b",
        "p4c",
        "p4d",
        "p4e",
        "Not applicable"
      )
    )

  df$pT_RECODE <- NA
  df$pT_RECODE[df$TNM_PATH_T %in% c("pTX")] <- 0
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT0", "pTa")] <- 1
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT1", "pT1a", "pT1a1", "pT1a2", "pT1b", "pT1b1", "pT1b2", "pT1c", "pT1d")] <- 2
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT2", "pT2a", "pT2a1", "pT2a2", "pT2b", "pT2c", "pT2d")] <- 3
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT3", "pT3a", "pT3b", "pT3c", "pT3d")] <- 4
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT4")] <- 5
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT4a")] <-6
  df$pT_RECODE[df$TNM_PATH_T %in% c("pT4b")] <- 7
  df$pT_RECODE[df$TNM_PATH_T %in% c("c4c")] <- 8
  df$pT_RECODE[df$TNM_PATH_T %in% c("c4d")] <- 9
  df$pT_RECODE[df$TNM_PATH_T %in% c("c4e")] <- 10
  df$pT_RECODE[df$TNM_PATH_T %in% c("pTis", "pTispu", "pTispd")] <- 11 ## Look into how this is used/what to call these



  df$pT_RECODE <-
    factor(
      df$pT_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
      labels = c("Tx",
                 "T0",
                 "T1",
                 "T2",
                 "T3",
                 "T4",
                 "T4a",
                 "T4b",
                 "T4c",
                 "T4d",
                 "T4e",
                 "cIs or similar")
    )
#########################################


#########################################
  df$TNM_PATH_N <-
    factor(
      df$TNM_PATH_N,
      levels = c(
        "pX",
        "p0",
        "p0I-",
        "p0I+",
        "p0M-",
        "p0M+",
        "p1MI",
        "p0A",
        "p0B",
        "p1",
        "p1A",
        "p1B",
        "p1C",
        "p2",
        "p2A",
        "p2B",
        "p2C",
        "p3",
        "p3A",
        "p3B",
        "p3C",
        "p4",
        "88"
      ),
      labels = c(
        "pNX",
        "pN0",
        "pN0i-",
        "pN0i+",
        "pN0m-",
        "pN0m+",
        "pN1mi",
        "pN0a",
        "pN0b",
        "pN1",
        "pN1a",
        "pN1b",
        "pN1c",
        "pN2",
        "pN2a",
        "pN2b",
        "pN2c",
        "pN3",
        "pN3a",
        "pN3b",
        "pN3c",
        "pN4",
        "Not applicable"
      )
    )



  df$pN_RECODE <- NA
  df$pN_RECODE[df$TNM_PATH_N %in% c("pNX")] <- 0
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN0", "pN0i-", "pN0i+", "pN0m-", "pN0m+", "pN0a", "pN0b")] <- 1
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN1mi", "pN1", "pN1a", "pN1b", "pN1c")] <- 2
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN2", "pN2a", "pN2b", "pN2c")] <- 3
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN3", "pN3a", "pN3b", "pN3c")] <- 4
  df$pN_RECODE[df$TNM_PATH_N %in% c("pN4")] <- 5


  df$pN_RECODE <-
    factor(
      df$pN_RECODE,
      levels = c(0, 1, 2, 3, 4, 5),
      labels = c("NX",
                 "N0",
                 "N1",
                 "N2",
                 "N3",
                 "N4")
    )
#########################################



#########################################
  df$TNM_PATH_M <-
    factor(
      df$TNM_PATH_M,
      levels = c(
        "p0",
        "p0I+",
        "p1",
        "p1A",
        "p1B",
        "p1C",
        "p1D",
        "88"
      ),
      labels = c(
        "pM0",
        "pM0(i+)",
        "pM1",
        "pM1a",
        "pM1b",
        "pM1c",
        "pM1d",
        "Not appliable"
      )
    )


  df$pM_RECODE <- NA
  df$pM_RECODE[df$TNM_PATH_M %in% c("pMX")] <- 0
  df$pM_RECODE[df$TNM_PATH_M %in% c("pM0", "pM0(i+)")] <- 1
  df$pM_RECODE[df$TNM_PATH_M %in% c("pM1", "pM1a", "pM1b", "pM1c", "pM1d")] <- 2
  df$pM_RECODE[df$TNM_PATH_M %in% c("Not applicable (not defined)")] <- ""


  df$pM_RECODE <-
    factor(
      df$pM_RECODE,
      levels = c(0, 1, 2),
      labels = c("Mx",
                 "M0",
                 "M1"
      )
    )
#########################################


#########################################
  df$TNM_PATH_STAGE_GROUP <-
    factor(
      df$TNM_PATH_STAGE_GROUP,
      levels = c(
        "0",
        "0A",
        "0IS",
        "1",
        "1A",
        "1A1",
        "1A2",
        "1B",
        "1B1",
        "1B2",
        "1C",
        "IS",
        "2",
        "2A",
        "2A1",
        "2A2",
        "2B",
        "2C",
        "3",
        "3A",
        "3B",
        "3C",
        "3C1",
        "3C2",
        "4",
        "4A",
        "4A1",
        "4A2",
        "4B",
        "4C",
        "OC",
        "88",
        "99"
      ),
      labels = c(
        "pStage 0",
        "pStage 0A",
        "pStage 0is",
        "pStage I",
        "pStage IA",
        "pStage IA1",
        "pStage IA2",
        "pStage IB",
        "pStage IB1",
        "pStage IB2",
        "pStage IC",
        "pStage IS",
        "pStage II",
        "pStage IIA",
        "pStage IIA1",
        "pStage IIA2",
        "pStage IIB",
        "pStage IIC",
        "pStage III",
        "pStage IIIA",
        "pStage IIIB",
        "pStage IIIC",
        "pStage IIIC1",
        "pStage IIIC2",
        "pStage IV",
        "pStage IVA",
        "pStage IVA1",
        "pStage IVA2",
        "pStage IVB",
        "pStage IVC",
        "Occult",
        "Not applicable",
        "Unknown"
        )
    )

  df$pSTAGE_RECODE <- NA
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage 0","pStage 0A","pStage 0is")] <- 0
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage I","pStage IA","pStage IA1","pStage IA2","pStage IB","pStage IB1","pStage IB2","pStage IC","pStage IS")] <- 1
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage II","pStage IIA","pStage IIA1","pStage IIA2","pStage IIB","pStage IIC")] <- 2
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage III","pStage IIIA","pStage IIIB","pStage IIIC","pStage IIIC1","pStage IIIC2")] <- 3
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("pStage IV", "pStage IVA","pStage IVA1","pStage IVA2","pStage IVB","pStage IVC")] <- 4
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("Occult")] <- 5
  df$pSTAGE_RECODE[df$TNM_PATH_STAGE_GROUP %in% c("Not applicable", "Unknown")] <- 6


  df$pSTAGE_RECODE <-
    factor(
      df$pSTAGE_RECODE,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c("pStage 0",
                 "pStage I",
                 "pStage II",
                 "pStage III",
                 "pStage IV",
                 "Occult",
                 "Not applicable/Unknown")
    )
#########################################



#########################################
  df$TNM_EDITION_NUMBER <-
    factor(
      df$TNM_EDITION_NUMBER,
      levels = c(
        "0",
        "5",
        "6",
        "7",
        "88",
        "99"
      ),
      labels = c(
        "Not staged", #cases that have AJCC staging scheme and staging was not done
        "5th Edition",
        "6th Edition",
        "7th Edition",
        "Not applicable", #cases that do not have an AJCC staging scheme
        "Staged, edition unknown" #prior to the 5th edition
      )
    )
#########################################


#########################################
  df$ANALYTIC_STAGE_GROUP <-
    factor(
      df$ANALYTIC_STAGE_GROUP,
      levels = c(0, 1, 2, 3, 4, 5, 8, 9),
      labels = c(
        "Stage 0",
        "Stage I",
        "Stage II",
        "Stage III",
        "Stage IV",
        "Occult (lung only)",
        "AJCC Staging not applicable",
        "AJCC Stage group unknown"
      )
    )
#########################################


#########################################
  df$METS_AT_DX_BONE <-
    factor(
      df$CS_METS_DX_BONE,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no bone metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether bone is involved; Not documented in patient record
      )
    )
#########################################


#########################################
  df$METS_AT_DX_BRAIN <-
    factor(
      df$METS_AT_DX_BRAIN,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no brain metastases"
        "Yes",
        "Not applicable",
        "Unknown" # unknown  whether brain is involved; Not documented in patient record
      )
    )
#########################################


#########################################
  df$METS_AT_DX_LIVER <-
    factor(
      df$CS_METS_DX_LIVER,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no liver metastases"
        "Yes",
        "Not applicable",
        "Unknown" # whether liver is involved; Not documented in patient record
      )
    )
#########################################


#########################################
  df$METS_AT_DX_LUNG <-
    factor(
      df$METS_AT_DX_LUNG,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no lung metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether lung is involved; Not documented in patient record
      )
    )
#########################################


#########################################
  df$METS_AT_DX_OTHER <-
    factor(
      df$METS_AT_DX_OTHER,
      levels = c(0, 1, 2, 8, 9),
      labels = c(
        "None", # no other metastases
        "Yes", # distant metastases in known site(s) other than bone, brain, liver, lung or distant lymph nodes.
        "Generalized metastases", #such as carcinomatosis
        "Not applicable",
        "Unknown" # whether other metastatic site is involved; Not documented in patient record
      )
    )
#########################################


#########################################
  df$METS_AT_DX_DISTANT_LN <-
    factor(
      df$METS_AT_DX_DISTANT_LN,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no distant LN metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether distant LN are involved; Not documented in patient record
      )
    )
#########################################






  #CS_EXTENSION
  # Identifies contiguous growth (extension) of the primary tumor within the organ or origin or its direct
  # extension into neighboring organs. For some sites such as ovary, discontinuous metastasis is coded in CS Extension.
  # CS extension codes are found here: http://ncdbpuf.facs.org/?q=node/370
  # TODO: create function for CS values

  # separate function here to decode using this http://ncdbpuf.facs.org/sites/default/files/cs/cs_head_neck_replpgs01.02.00.pdf

  #CS_TUMOR_SIZEEXT_EVAL
  # Records how the codes for the two items, CS Tumor Size and CS Extension were determined,
  # based on the diagnostic methods employed.
  # TODO: pull info from here: http://web2.facs.org/cstage0205/mouthother/MouthOther_cpa.html

  ### LYMPH_VASCULAR_INVASION
  # Indicates the presence or absence of tumor cells in lymphatic channels (not lymph
  # nodes) or blood vessels within the primary tumor as noted microscopically by the
  # pathologist. This data item is separate from the CS data items but is included in this
  # manual because of its relationship to the Collaborative Stage Data Collection
  # System. Lymph-vascular invasion is an item of interest to both pathologists and
  # clinicians and is mentioned in many chapters of the AJCC Cancer Staging Manual,
  # seventh edition. This field is required for mapping of T in some sites, such as testis
  # and penis.

  df$LYMPH_VASCULAR_INVASION <-
    factor(
      df$LYMPH_VASCULAR_INVASION,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", #http://web2.facs.org/cstage0205/mouthother/MouthOther_cpa.html
        "Present", #Lymph-vascular invasion is present or identified",
        "Not applicable",
        "Unknown" # unknown if lymph-vascular invasion is present, or indeterminant"
      )
    )



  ### CS_METS_AT_DX - CS Mets at DX
  # Identifies whether there is metastatic involvement of distant site(s) at the time of
  # diagnosis.
  # codes found at http://web2.facs.org/205/nasalcavity/NasalCavity_hpb.html
  # TODO: large effort to make this applicable to all NCDB sites since each CS variable
  # has site-specific values.
  # df$CS_METS_AT_DX <-
  #   factor(
  #     as.numeric(df$CS_METS_AT_DX),
  #     levels = c(00, 10, 40, 50, 60, 99),
  #     labels = c(
  #       "No distant metastasis",
  #       "Distant lymph node(s)",
  #       "Distant metastases except distant lymph nodes",
  #       "Distant metastasis plus distant lymph nodes",
  #       "Distant metastasis, NOS",
  #       "Unknown"
  #     )
  #   )


  ### CS_METS_DX_BONE - CS Mets at DX-Bone
  # Identifies whether there is metastatic involvement of distant site(s) at the time of
  # diagnosis

  df$CS_METS_DX_BONE <-
    factor(
      df$CS_METS_DX_BONE,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no bone metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether bone is involved; Not documented in patient record
      )
    )


  ### CS_METS_DX_Brain - CS Mets at DX-Brain
  # Identifies the presence of distant metastatic involvement of the bone at the time of
  # diagnosis

  df$CS_METS_DX_BRAIN <-
    factor(
      df$CS_METS_DX_BRAIN,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no brain metastases"
        "Yes",
        "Not applicable",
        "Unknown" # unknown  whether brain is involved; Not documented in patient record
      )
    )


  ### CS_METS_DX_Liver - CS Mets at DX-Liver
  # Identifies the presence of distant metastatic involvement of the liver at the time of
  # diagnosis

  df$CS_METS_DX_LIVER <-
    factor(
      df$CS_METS_DX_LIVER,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no liver metastases"
        "Yes",
        "Not applicable",
        "Unknown" # whether liver is involved; Not documented in patient record
      )
    )



  ### CS_METS_DX_LUNG - CS Mets at DX-LUNG
  # Identifies the presence of distant metastatic involvement of the lung at the time of
  # diagnosis

  df$CS_METS_DX_LUNG <-
    factor(
      df$CS_METS_DX_LUNG,
      levels = c(0, 1, 8, 9),
      labels = c(
        "None", # no lung metastases
        "Yes",
        "Not applicable",
        "Unknown" # whether lung is involved; Not documented in patient record
      )
    )


  #CS_SITESPECIFIC_FACTORs
  # TODO: http://ncdbpuf.facs.org/?q=node/370 find way to do this

  #CS_METS_EVAL
  # Records how the code for CS Mets at DX was determined based on the diagnostic methods employed.
  # TODO: do with other CS codes

  #TUMOR_SIZE
  # Describes the largest dimension of the diameter of the primary tumor in millimeters(mm).


  #CS_VERSION_LATEST
  #This is the version number of the most recent derivation of CS data items in the record.
  # TODO

  #TREATMENT####

  #RX_SUMM_TREATMENT_STATUS - Treatment Status
  # This item summarizes whether the patient received any treatment or was under
  # active surveillance.
  df$RX_SUMM_TREATMENT_STATUS <-
    factor(
      df$RX_SUMM_TREATMENT_STATUS,
      levels = c(0, 1, 2, 9),
      labels = c(
        "No treatment given",
        "Treatment given",
        "Active surveillance (watchful waiting)",
        "Unknown if treatment given"
      )
    )


  #DX_RX_STARTED_DAYS
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which treatment [surgery, radiation, systemic, or other therapy] (NAACCR
  # Item #1270) of the patient began at any facility


  #DX_SURG_STARTED_DAYS - First Surgical Procedure, days from dx
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date the first treatment surgery was performed (NAACCR Item #1200). The surgery
  # may be primary site surgery (NAACCR Item #1290), regional lymph node surgery
  # (NAACCR Item #1292) or other regional or distant surgery (NAACCR Item #1294).
  # Incisional biopsies are not coded as treatment surgery.


  #DX_DEFSURG_STARTED_DAYS - Definitive Surgical Procedure, days from dx
  # The number of days between the date of diagnosis (NAACCR Item #390) and the
  # date on which the most definitive surgical procedure was performed on the primary
  # site (NAACCR Item #3170).


  #RX_SUMM_SURG_PRIM_SITE - Surgical procedure of the primary site
  # Records the surgical procedure performed to the primary site at any facility.
  # "00-None No surgical procedure of primary site. Diagnosed at autopsy."
  # "10--19 Site-specific codes; tumor destruction Tumor destruction, no pathologic specimen produced.  Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "20--80-Site-specific codes; resection Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure. "
  # "90-Surgery, NOS A surgical procedure to the primary site was done, but no information on the type of surgical procedure is provided. "
  # "98-Site-specific codes; special Special code. Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "99-Unknown Patient record does not state whether a surgical procedure of the primary site was performed and no information is available. Death certificate only."
  # TODO

  df$RX_SUMM_SURG_PRIM_SITE <-
    factor(
      df$RX_SUMM_SURG_PRIM_SITE,
      levels = c(0,1),
      labels = c("Tumor destruction", "Tumor resection")
    )



  #RX_HOSP_SURG_PRIM_SITE
  # This item records the surgical procedure performed to the primary site at the facility
  # that submitted this record
  # Records the surgical procedure performed to the primary site at any facility.
  # "00-None No surgical procedure of primary site. Diagnosed at autopsy."
  # "10-19 Site-specific codes; tumor destruction Tumor destruction, no pathologic specimen produced.  efer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "20--80-Site-specific codes; resection Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure. "
  # "90-Surgery, NOS A surgical procedure to the primary site was done, but no information on the type of surgical procedure is provided. "
  # "98-Site-specific codes; special Special code. Refer to Surgery of the Primary Site Codes for the correct site-specific code for the procedure."
  # "99-Unknown Patient record does not state whether a surgical procedure of the primary site was performed and no information is available. Death certificate only."


  #RX_HOSP_SURG_APPR_2010 - Surgical Approach ONLY USED AFTER 2010
  # This item is used to monitor patterns and trends in the adoption and utilization of
  # minimally-invasive surgical techniques.

  df$RX_HOSP_SURG_APPR_2010 <-
    factor(
      df$RX_HOSP_SURG_APPR_2010,
      levels = c(0, 1, 2, 3, 4, 5, 9),
      labels = c(
        "No surgical procedure of primary site at this facility.",
        "Robotic assisted.",
        "Robotic converted to open.",
        "Endoscopic or laparoscopic.",
        "Endoscopic or laparoscopic converted to open.",
        "Open or approach unspecified.",
        "Unknown whether surgery was performed at this facility"
      )
    )

  #RX_SUMM_SURGICAL_MARGINS - Surgical Margins
  # Records the final status of the surgical margins after resection of the primary
  # tumor.

    df$RX_SUMM_SURGICAL_MARGINS <-
    factor(
      df$RX_SUMM_SURGICAL_MARGINS,
      levels = c(0, 1, 2, 3, 7, 8, 9),
      labels = c(
        "No residual tumor", # All margins are grossly and microscopically negative
        "Residual tumor, NOS", #Involvement is indicated, but not otherwise specified
        "Microscopic residual tumor", # Cannot be seen by the naked eye
        "Macroscopic residual tumor", #Gross tumor of the primary site which is visible to the naked eye
        "Margins not evaluable", # Cannot be assessed (indeterminate)
        "No primary site surgery", # No surgical procedure of the primary site. Diagnosed at autopsy
        "Unknown or not applicable" # It is unknown whether a surgical procedure to the primary site was performed; death certificate-only; for lymphomas with a lymph node primary site; an unknown or ill-defined primary; or for hematopoietic, reticuloendothelial, immunoproliferative, or myeloproliferative disease"
      )
    )

  df$MARGINS_RECODE <- NA
  df$MARGINS_RECODE[df$RX_SUMM_SURGICAL_MARGINS %in% c("No residual tumor")] <-    0
  df$MARGINS_RECODE[df$RX_SUMM_SURGICAL_MARGINS %in% c(
    "Residual tumor, NOS",
    "Microscopic residual tumor",
    "Macroscopic residual tumor"  )] <- 1
  df$MARGINS_RECODE[df$RX_SUMM_SURGICAL_MARGINS %in% c(
    "MARGINS_RECODE not evaluable",
    "No primary site surgery",
    "Unknown or not applicable")] <- 2

  df$MARGINS_RECODE <-
    factor(
      df$MARGINS_RECODE,
      levels = c(0, 1, 2),
      labels = c("Negative margin",
                 "Positive margin",
                 "Indeterminate/NA")
    )


  #RX_SUMM_SCOPE_REG_LN_SUR
  # Identifies the removal, biopsy, or aspiration of regional lymph node(s) at the time of
  # surgery of the primary site or during a separate surgical event.

  df$RX_SUMM_SCOPE_REG_LN_SUR <-
    factor(
      df$RX_SUMM_SCOPE_REG_LN_SUR,
      levels = c(0, 1, 9),
      labels = c(
        "No regional lymph node surgery",
        "Regional lymph node surgery",
        "Unknown if regional lymph node surgery performed"
      )
    )



  #RX_SUMM_SCOPE_REG_LN_2012
  # Identifies the removal, biopsy, or aspiration of regional lymph node(s) at the time of
  # surgery of the primary site or during a separate surgical event.
  # TODO bad link on site, can't find values

  # df$RX_SUMM_SCOPE_REG_LN_2012 <-
  #   factor(
  #     df$RX_SUMM_SCOPE_REG_LN_2012,
  #     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
  #     labels = c(""
  #     )
  #   )




  #RX_SUMM_SURG_OTH_REGDIS
  # Records the surgical removal of distant lymph nodes or other tissue(s)/organ(s)
  # beyond the primary site.

  df$RX_SUMM_SURG_OTH_REGDIS <-
    factor(
      df$RX_SUMM_SURG_OTH_REGDIS,
      levels = c(0, 1, 2, 3, 4, 5, 9),
      labels = c(
        "None", # No nonprimary surgical site resection was performed. Diagnosed at autopsy.",
        "Nonprimary surgical procedure performed", # Nonprimary surgical resection to other site(s), unknown if whether the site(s) is regional or distant.",
        "Nonprimary surgical procedure to other regional sites", # Resection of regional site.",
        "Nonprimary surgical procedure to distant lymph node(s)", # Resection of distant lymph node(s) .",
        "Nonprimary surgical procedure to distant site", # Resection of distant site.",
        "Combination of codes", # Any combination of surgical procedures 2, 3, or 4.",
        "Unknown" # It is unknown whether any surgical procedure of a non primary site was performed. Death certificate only."
      )
    )


  #SURG_DISCHARGE_DAYS
  # The number of days between the date the most definitive surgical procedure was
  # performed on the primary site (NAACCR Item #3170) and the date the patient was
  # discharged following primary site surgery (NAACCR Item #3180)

   (df$SURG_DISCHARGE_DAYS) < "Surgical Inpatient Stay, Days from Surgery"

  #READM_HOSP_30_DAYS
  # Records a readmission to the same hospital, for the same illness, within 30 days of
  # discharge following hospitalization for surgical resection of the primary site

  df$READM_HOSP_30_DAYS <-
    factor(
      df$READM_HOSP_30_DAYS,
      levels = c(0, 1, 2, 3, 9),
      labels = c(
        "None", #surgical procedure of the primary site was performed, or the patient was not readmitted to the same hospital within 30 days of discharge.
        "Unplanned", # A patient was surgically treated and was readmitted to the same hospital within 30 days of being discharged. This readmission was unplanned.
        "Planned", # A patient was surgically treated and was then readmitted to the same hospital within 30 days of-being discharged. This readmission was planned (chemotherapy port insertion, revision of colostomy, etc.)
        "Planned + unplanned", # A patient was surgically treated and, within 30 days of being discharged, the patient had both a planned and an unplanned readmission to the same hospital
        "Unknown" # Unknown whether surgery of the primary site was recommended or performed. It is unknown whether the patient was readmitted to the same hospital within 30 days of discharge
      )
    )


  #REASON_FOR_NO_SURGERY
  # Records the reason that no surgery was performed on the primary site.
  df$REASON_FOR_NO_SURGERY <-
    factor(
      df$REASON_FOR_NO_SURGERY,
      levels = c(0, 1, 2, 5, 6, 7, 8, 9),
      labels = c(
        "Surgery performed", # surgery of the primary site was performed
        "Not part of planned treatment", # Surgery of the primary site was not performed because it was not part of the planned first course treatment
        "Contraindicated, risk factors", # Surgery of the primary site was not recommended/performed because it was contraindicated due to patient risk factors (comorbid conditions, advanced age, etc.)
        "Recommended, patient deceased", # Surgery of the primary site was not performed because the patient died prior to planned or recommended surgery.
        "Recommended, no reason provided", # Surgery of the primary site was not performed; it was recommended by the patient=s physician, but was not performed as part of the first course of therapy. No reason was noted in patient record.
        "Recommended, refused by patient", # Surgery of the primary site was not performed; it was recommended by the patient=s physician, but this treatment was refused by the patient, the patient=s family member, or the patient=s guardian. The refusal was noted in patient record.
        "Recommended, unknown if performed", # Surgery of the primary site was recommended, but it is unknown if it was performed. Further follow-up is recommended.
        "Unknown if recommended or performed" # It is unknown whether surgery of the primary site was recommended or performed. Diagnosed at autopsy or death certificate only."
      )
    )


  # Calculated field to determine if the patient had surgery
  df$ANY_SURGERY <- NA
  df$ANY_SURGERY[df$REASON_FOR_NO_SURGERY %in% c("Surgery performed")] <-
    0

  df$ANY_SURGERY[df$REASON_FOR_NO_SURGERY %in% c(
    "Not part of planned treatment",
    "Contraindicated, risk factors",
    "Recommended, patient deceased",
    "Recommended, no reason provided",
    "Recommended, refused by patient",
    "Recommended, unknown if performed",
    "Unknown if recommended or performed"
  )] <- 1

  df$ANY_SURGERY <-
    factor(df$ANY_SURGERY,
           levels = c(0, 1),
           labels = c("Surgery",
                      "No Surgery"))


  # calculated variable to combine surgery and margins
  df$JOINT_SURG_MARGINS <- factor(
    paste(df$ANY_SURGERY, df$MARGINS_RECODE),
    levels = c(
      "No Surgery Indeterminate/NA",
      "Surgery Indeterminate/NA",
      "Surgery NA",
      "Surgery Negative margin",
      "Surgery Positive margin"
    )
  )


  df$SURGERY_MARGINS <- NA
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("No Surgery Indeterminate/NA")] <- 0
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("Surgery Negative margin")] <- 1
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("Surgery Positive margin")] <- 2
  df$SURGERY_MARGINS[df$JOINT_SURG_MARGINS %in% c("Surgery Indeterminate/NA",
                                                  "Surgery NA")] <- 3


  df$SURGERY_MARGINS <-
    factor(
      df$SURGERY_MARGINS,
      levels = c(0, 1, 2, 3),
      labels = c(
        "No Surgery",
        "Surgery - Margins",
        "Surgery + Margins",
        "Surgery Margins Unknown"
      )
    )


  
  #PALLIATIVE_CARE
  # Identifies any care provided in an effort to palliate or alleviate symptoms. Palliative
  # care is performed to relieve symptoms and may include surgery, radiation therapy,
  # systemic therapy (chemotherapy, hormone therapy, or other systemic drugs),
  # and/or other pain management therapy

  df$PALLIATIVE_CARE <-
    factor(
      df$PALLIATIVE_CARE,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "None", #No palliative care provided. Diagnosed at autopsy.",
        "Surgery", #(which may involve a bypass procedure) to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Radiation therapy", # to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Chemotherapy, hormone therapy, or other systemic", # drugs to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Pain management", #Patient received or was referred for pain management therapy with no other palliative care.",
        "Combination of surgery, systemic without pain management", #Any combination of codes 1, 2, and/or 3 without code 4.
        "Combination of surgery, systemic with pain management", #	Any combination of codes 1, 2, and/or 3 with code 4.
        "Recommended, no information available", #Palliative care was performed or referred, but no information on the type of procedure is available in the patient record. Palliative care was provided that does not fit the descriptions for codes 1–6.",
        "Unknown" #It is unknown if palliative care was performed or referred; not stated in patient record."
      )
    )


  #PALLIATIVE_CARE_HOSP
  # Identifies any care provided in an effort to palliate or alleviate symptoms at the reporting facility.
  # Palliative care is performed to relieve symptoms and may include surgery, radiation therapy, systemic
  #therapy (chemotherapy, hormone therapy, or other systemic drugs), and/or other pain management therapy.
  #This data item was added to the 2015 PUF (data released in Fall 2017), and does not appear in prior versions of the PUF data.

  df$PALLIATIVE_CARE_HOSP <-
    factor(
      df$PALLIATIVE_CARE_HOSP,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "None", #No palliative care provided. Diagnosed at autopsy.",
        "Surgery", #(which may involve a bypass procedure) to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Radiation therapy", # to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Chemotherapy, hormone therapy, or other systemic", # drugs to alleviate symptoms, but no attempt to diagnose, stage, or treat the primary tumor is made.",
        "Pain management", #Patient received or was referred for pain management therapy with no other palliative care.",
        "Combination of surgery, systemic without pain management", #Any combination of codes 1, 2, and/or 3 without code 4.
        "Combination of surgery, systemic with pain management", #	Any combination of codes 1, 2, and/or 3 with code 4.
        "Recommended, no information available", #Palliative care was performed or referred, but no information on the type of procedure is available in the patient record. Palliative care was provided that does not fit the descriptions for codes 1–6.",
        "Unknown" #It is unknown if palliative care was performed or referred; not stated in patient record."
      )
    )



#OUTCOMES####

  #PUF_30_DAY_MORT_CD - Thirty Day Mortality
  # This item indicates mortality within 30 days of the most definitive primary site
  # surgery

  df$PUF_30_DAY_MORT_CD <-
    factor(
      df$PUF_30_DAY_MORT_CD,
      levels = c(0, 1, 9),
      labels = c(
        "Alive", #or died more than 30 days after surgery performed",
        "Dead", #Patient died 30 or fewer days after surgery performed",
        "Alive, < 30 days FU" #Patient alive with fewer than 30 days of follow-up, surgery date missing, or last contact date missing"
      )
    )



  #PUF_90_DAY_MORT_CD - Nintey day mortality
  # This item indicates mortality within 90 days after the most definitive primary site
  # surgery

  df$PUF_90_DAY_MORT_CD <-
    factor(
      df$PUF_90_DAY_MORT_CD,
      levels = c(0, 1, 9),
      labels = c(
        "Alive", #Patient alive, or died more than 90 days after surgery performed",
        "Dead", #Patient died 90 or fewer days after surgery performed",
        "Alive, < 90 days FU" #or surgery date missing, or last contact date missing"
      )
    )



  #DX_LASTCONTACT_DEATH_MONTHS
  # The number of months between the date of diagnosis (NAACCR Item #390) and
  # the date on which the patient was last contacted or died (NAACCR Item #1750).


  #PUF_VITAL_STATUS - PUF Vital Status
  # Records the vital status of the patient as of the date entered in Date of Last Contact
  # or Death (NAACCR Item #1750), which is the status of the patient at the end of
  # Elapsed Months Date of Diagnosis to Date of Last Contact or Death in the PUF.

  df$PUF_VITAL_STATUS <-
    factor(
      df$PUF_VITAL_STATUS,
      levels = c(0, 1),
      labels = c("Dead",
                 "Alive")
    )

  # R Survival object defaults to 0 = Alive, 1 = dead, recode to  use


  #OUTPUT####
  #returns recoded dataframe
  df$RECODED_STATUS <- NA
  df$RECODED_STATUS[df$PUF_VITAL_STATUS == "Dead"] <-  1
  df$RECODED_STATUS[df$PUF_VITAL_STATUS == "Alive"] <- 0

  df

}
NCDBTableOne <- function(df){
# If a vector of variables is not passed, use this as default
  tableOne <-
    CreateTableOne(
      vars = c(
        "AGE_GROUP",
        "SEX",
        "RACE",
        "CDCC_TOTAL_BEST",
        "INSURANCE_STATUS",
        "MED_INC_QUAR_12",
        "NO_HSD_QUAR_12",
        "URBAN_RURAL",
        "FACILITY_TYPE_CD",
        "GRADE_RECODE",
        "pSTAGE_RECODE",
        "cSTAGE_RECODE",
        "PRIMARY_SITE",
        "SURGERY_MARGINS",
        "ANY_RADIATION",
        "ANY_CHEMO",
        "DX_RX_STARTED_DAYS"
      ),
      data = df
    )
  tableOne

}



ncdb_rename <- function(df) {
  df <- df %>%
    rename(case_id = PUF_CASE_ID,
           facility = PUF_FACILITY_ID,
           type_of_facility = FACILITY_TYPE_CD)
  return(df)
}