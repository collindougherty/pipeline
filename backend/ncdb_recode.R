# import libraries
library(dplyr)

ncdb_recode <- function(df) {

  #########################################
  df$PUF_CASE_ID <- as.character(df$PUF_CASE_ID)
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
    ifelse(df$SLN_EXAM %in% c(95,98,99), NA, df$SLN_EXAM)
  #########################################


  #########################################
  df$SLN_POS <-
    ifelse(df$SLN_POS %in% c(95,97,98,99), NA, df$SLN_POS)
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


  #########################################
  df$TUMOR_SIZE_SUMMARY_2016[df$TUMOR_SIZE_SUMMARY_2016 == 999] <- NA
  df$TUMOR_SIZE_SUMMARY_2016[df$TUMOR_SIZE_SUMMARY_2016 == 998] <- NA
  df$TUMOR_SIZE_SUMMARY_2016[df$TUMOR_SIZE_SUMMARY_2016 == 990] <- NA
  #########################################


  #########################################
  df$AJCC_TNM_CLIN_T
  #########################################


  #########################################
  df$AJCC_TNM_CLIN_T_SFX <-
  factor(
    df$AJCC_TNM_CLIN_T_SFX,
    levels = c("(m)", "(s)"),
    labels = c(
      "Multiple synchronous tumors OR Multifocal tumor",
      "Solitary tumor"
    )
  )
  #########################################


  #########################################
  df$AJCC_TNM_CLIN_N 
  #########################################


  #########################################
  df$AJCC_TNM_CLIN_N_SFX <-
  factor(
    df$AJCC_TNM_CLIN_N_SFX,
    levels = c("(sn)", "(f)"),
    labels = c(
      "Sentinel node procedure with or without FNA or core needle biopsy",
      "FNA or core needly biopsy only"
    )
  )
  #########################################


  #########################################
  df$AJCC_TNM_CLIN_M
  #########################################


  #########################################
  df$AJCC_TNM_CLIN_STG_GRP
  #########################################


  #########################################
  df$AJCC_TNM_PATH_T
  #########################################


  #########################################
  df$AJCC_TNM_PATH_T_SFX <-
  factor(
    df$AJCC_TNM_PATH_T_SFX,
    levels = c("(m)", "(s)"),
    labels = c(
      "Multiple synchronous tumors OR Multifocal tumor",
      "Solitary tumor"
    )
  )
  #########################################


  #########################################
  df$AJCC_TNM_PATH_N
  #########################################


  #########################################
  df$AJCC_TNM_PATH_N_SFX <-
  factor(
    df$AJCC_TNM_PATH_N_SFX,
    levels = c("(sn)", "(f)"),
    labels = c(
      "Sentinel node procedure with or without FNA or core needle biopsy",
      "FNA or core needly biopsy only"
    )
  )
  #########################################


  #########################################
  df$AJCC_TNM_PATH_M
  #########################################


  #########################################
  df$AJCC_TNM_PATH_STG_GRP
  #########################################


  #########################################
  df$AJCC_TNM_POST_PATH_T
  #########################################


  #########################################
  df$AJCC_TNM_POST_PATH_T_SFX <-
  factor(
    df$AJCC_TNM_PATH_T_SFX,
    levels = c("(m)", "(s)"),
    labels = c(
      "Multiple synchronous tumors OR Multifocal tumor",
      "Solitary tumor"
    )
  )
  #########################################


  #########################################
  df$AJCC_TNM_POST_PATH_N
  #########################################


  #########################################
  df$AJCC_TNM_POST_PATH_N_SFX <-
  factor(
    df$AJCC_TNM_PATH_N_SFX,
    levels = c("(sn)", "(f)"),
    labels = c(
      "Sentinel node procedure with or without FNA or core needle biopsy",
      "FNA or core needly biopsy only"
    )
  )
  #########################################


  #########################################
  df$AJCC_TNM_POST_PATH_M
  #########################################


  #########################################
  df$AJCC_TNM_POST_PATH_STG_GRP
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_1
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_2
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_3
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_4
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_5
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_6
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_7
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_8
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_9
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_10
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_11
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_12
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_13
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_14
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_15
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_16
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_17
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_18
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_19
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_20
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_21
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_22
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_23
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR24
  #########################################


  #########################################
  df$CS_SITE_SPECIFIC_FACTOR_25
  #########################################


  #########################################
  #CS_EXTENSION
  # Identifies contiguous growth (extension) of the primary tumor within the organ or origin or its direct
  # extension into neighboring organs. For some sites such as ovary, discontinuous metastasis is coded in CS Extension.
  # CS extension codes are found here: http://ncdbpuf.facs.org/?q=node/370
  # TODO: create function for CS values
  # separate function here to decode using this http://ncdbpuf.facs.org/sites/default/files/cs/cs_head_neck_replpgs01.02.00.pdf
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
  #CS_METS_EVAL
  # Records how the code for CS Mets at DX was determined based on the diagnostic methods employed.
  # TODO: do with other CS codes
  #########################################


  #########################################
  df$TUMOR_SIZE
  #########################################


  #########################################
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
  #########################################


  #########################################
  df$DX_RX_STARTED_DAYS
  #########################################


  #########################################
  df$DX_SURG_STARTED_DAYS
  #########################################


  #########################################
  df$DX_DEFSURG_STARTED_DAYS
  #########################################


  #########################################
 #RX_SUMM_SURG_PRIM_SITE
  #########################################


  #########################################
#RX_HOSP_SURG_PRIM_SITE
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
  #RX_SUMM_SCOPE_REG_LN_2012
  #########################################


  #########################################
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
  #########################################


  #########################################
df$SURG_DISCHARGE_DAYS
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
  df$DX_RAD_STARTED_DAYS
  #########################################


  #########################################
  df$RAD_LOCATION_OF_RX <-
    factor(
      df$RAD_LOCATION_OF_RX,
      levels = c(0, 1, 2, 3, 4, 8, 9),
      labels = c(
        "No radiation treatment", # No radiation therapy was administered to the patient
        "All radiation treatment at this facility", # All radiation therapy was administered at the reporting facility
        "Regional treatment at this facility, boost elsewhere", # Regional treatment was administered at the reporting facility; a boost dose was administered elsewhere
        "Boost radiation at this facility, regional elsewhere", # Regional treatment was administered elsewhere; a boost dose was administered at the reporting facility
        "All radiation treatment elsewhere", # All radiation therapy was administered elsewhere
        "Other", # Radiation therapy was administered, but the pattern does not fit the above categories
        "Unknown" # Radiation therapy was administered, but the location of the treatment facility is unknown or not stated in patient record; it is unknown whether radiation therapy was administered
      )
    )
  #########################################


  #########################################
  # df$PHASE_I_RT_TRIAL <- factor(df$PHASE_I_RT_TRIAL,
  #   levels = c("00", "01", "02", "03", "04", "05", "06", "07", "09", "10", "11", "12",
  #             "13", "14", "20", "21", "22", "23", "24", "25", "26", "29", "30", "31",
  #             "32", "39", "40", "41", "42", "50", "51", "52", "53", "54", "55", "56",
  #             "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68",
  #             "70", "71", "72", "73", "80", "81", "82", "83", "84", "85", "86", "88",
  #             "90", "91", "92", "93", "94", "95", "97", "98", "99"),
  #   labels = c("No radiation treatment", "Neck lymph node regions", "Thoracic lymph node regions",
  #             "Neck and thoracic lymph node regions", "Breast/Chest wall lymph node regions",
  #             "Abdominal lymph nodes", "Pelvic lymph nodes", "Abdominal and pelvic lymph nodes",
  #             "Lymph node region NOS", "Eye/orbit/optic nerve", "Pituitary", "Brain", 
  #             "Brain (limited)", "Spinal cord", "Nasopharynx", "Oral cavity", "Oropharynx",
  #             "Larynx (glottis) or hypopharynx", "Sinuses/Nasal tract", "Parotid or other salivary glands",
  #             "Thyroid", "Head and neck (NOS)", "Lung or bronchus", "Mesothelium",
  #             "Thymus", "Chest/lung (NOS)", "Breast (whole)", "Breast (partial)", "Chest wall",
  #             "Esophagus", "Stomach", "Small bowel", "Colon", "Rectum", "Anus", "Liver",
  #             "Biliary tree or gallbladder", "Pancreas or hepatopancreatic ampulla", "Abdomen (NOS)",
  #             "Bladder (whole)", "Bladder (partial)", "Kidney", "Ureter", "Prostate (whole)",
  #             "Prostate (partial)", "Urethra", "Penis", "Testicle or scrotum",
  #             "Ovaries or fallopian tubes", "Uterus or cervix", "Vagina", "Vulva", "Skull",
  #             "Spine/vertebral bodies", "Shoulder", "Ribs", "Hip", "Pelvic bones",
  #             "Pelvis (NOS, nonvisceral)", "Extremity bone, NOS", "Skin", "Soft Tissue",
  #             "Hemibody", "Whole body", "Mantle, minimantle (obsolete after 2017)",
  #             "Lower extended field (obsolete after 2017)", "Invalid historical FORDS value",
  #             "Other", "Unknown"))
  #########################################


  #########################################
  df$PHASE_I_RT_TO_LN <-
    factor(
      df$PHASE_I_RT_TO_LN,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 88, 99),
      labels = c(
        "No radiation to draining lymph nodes",
        "Neck lymph node regions",
        "Thoracic lymph node regions",
        "Neck and thoracic lymph node regions",
        "Breast/ Chest wall lymph node regions",
        "Abdominal lymph nodes",
        "Pelvic lymph nodes",
        "Abdominal and pelvic lymph nodes",
        "Lymph node region, NOS",
        "Not applicable; Radiation primary treatment is lymph nodes",
        "Unknown if any radiation treatment to draining lymph nodes; Unknown if radiation treatment administered"
      )
    )
  #########################################


  #########################################
  # df$PHASE_I_RT_TO_MODALITY <-
  #   factor(
  #     df$PHASE_I_RT_TO_MODALITY,
  #     levels = c(
  #       0,
  #       1,
  #       2,
  #       3,
  #       4,
  #       5,
  #       6,
  #       7,
  #       8,
  #       9,
  #       10,
  #       11,
  #       12,
  #       13,
  #       14,
  #       15,
  #       16,
  #       98,
  #       99
  #     ),
  #     labels = c(
  #       "No radiation treatment",
  #       "External beam, NOS",
  #       "External beam, photons",
  #       "External beam, protons",
  #       "External beam, electrons",
  #       "External beam, neutrons",
  #       "External beam, carbon ions",
  #       "Brachytherapy, NOS",
  #       "Brachytherapy, intracavitary, LDR",
  #       "Brachytherapy, intracavitary, HDR",
  #       "Brachytherapy, interstitial, LDR",
  #       "Brachytherapy, interstitial, HDR",
  #       "Brachytherapy, electronic",
  #       "Radioisotopes, NOS",
  #       "Radioisotopes, Radium-223",
  #       "Radioisotopes, Strontium-89",
  #       "Radioisotopes, Strontium-90",
  #       "Radiation Rx administered, Rx modality unknown",
  #       "Radiation treatment modality unknown; Unknown if radiation treatment administered"
  #     )
  #   )
  #########################################


  #########################################
  df$PHASE_I_BEAM_TECH <- 
    factor(df$PHASE_I_BEAM_TECH,
    levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "88", "98", "99"),
    labels = c(
      "No radiation treatment",
      "External beam, NOS",
      "Low energy x-ray/photon therapy",
      "2-D therapy",
      "Conformal or 3-D conformal therapy",
      "Intensity modulated therapy",
      "Stereotactic radiotherapy or radiosurgery, NOS",
      "Stereotactic radiotherapy or radiosurgery, robotic",
      "Stereotactic radiotherapy or radiosurgery, Gamma Knife",
      "CT-guided online adaptive therapy",
      "MR-guided online adaptive therapy",
      "Not applicable",
      "Other, NOS",
      "Unknown"
    )
  )
  #########################################


  #########################################
  df$PHASE_I_DOSE_FRACT <-
  # if its 99998 or 99999 00200 00150 or 00220, then mark it NA
    ifelse(df$PHASE_I_DOSE_FRACT %in% c(99998, 99999), NA, df$PHASE_I_DOSE_FRACT)
  #########################################


  #########################################
  df$PHASE_I_NUM_FRACT <- 
    ifelse(df$PHASE_I_NUM_FRACT %in% c(999), NA, df$PHASE_I_NUM_FRACT)
  #########################################


  #########################################
  df$PHASE_I_TOTAL_DOSE <- 
    ifelse(df$PHASE_I_TOTAL_DOSE %in% c(999998, 999999), NA, df$PHASE_I_TOTAL_DOSE)
  #########################################


  #########################################
  df$PHASE_II_RT_VOLUME <- factor(df$PHASE_II_RT_VOLUME, 
  levels = c("00", "", "01", "02", "03", "04", "05", "06", "07", "09", "10", "11", "12", 
             "13", "14", "20", "21", "22", "23", "24", "25", "26", "29", "30", "31", "32", 
             "39", "40", "41", "42", "50", "51", "52", "53", "54", "55", "56", "57", "58", 
             "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "70", "71", "72", 
             "73", "80", "81", "82", "83", "84", "85", "86", "88", "90", "91", "92", "93", 
             "94", "95", "97", "98", "99"),
  labels = c(
    "No radiation treatment", 
    "No radiation treatment",  # for the blank code
    "Neck lymph node regions", 
    "Thoracic lymph node regions", 
    "Neck and thoracic lymph node regions", 
    "Breast/Chest wall lymph node regions", 
    "Abdominal lymph nodes", 
    "Pelvic lymph nodes", 
    "Abdominal and pelvic lymph nodes", 
    "Lymph node region NOS", 
    "Eye/orbit/optic nerve", 
    "Pituitary", 
    "Brain", 
    "Brain (limited)", 
    "Spinal cord", 
    "Nasopharynx", 
    "Oral cavity", 
    "Oropharynx", 
    "Larynx (glottis) or hypopharynx", 
    "Sinuses/Nasal tract", 
    "Parotid or other salivary glands", 
    "Thyroid", 
    "Head and neck (NOS)", 
    "Lung or bronchus", 
    "Mesothelium", 
    "Thymus", 
    "Chest/lung (NOS)", 
    "Breast (whole)", 
    "Breast (partial)", 
    "Chest wall", 
    "Esophagus", 
    "Stomach", 
    "Small bowel", 
    "Colon", 
    "Rectum", 
    "Anus", 
    "Liver", 
    "Biliary tree or gallbladder", 
    "Pancreas or hepatopancreatic ampulla", 
    "Abdomen (NOS)", 
    "Bladder (whole)", 
    "Bladder (partial)", 
    "Kidney", 
    "Ureter", 
    "Prostate (whole)", 
    "Prostate (partial)", 
    "Urethra", 
    "Penis", 
    "Testicle or scrotum", 
    "Ovaries or fallopian tubes", 
    "Uterus or cervix", 
    "Vagina", 
    "Vulva", 
    "Skull", 
    "Spine/vertebral bodies", 
    "Shoulder", 
    "Ribs", 
    "Hip", 
    "Pelvic bones", 
    "Pelvis (NOS, nonvisceral)", 
    "Extremity bone, NOS", 
    "Skin", 
    "Soft Tissue", 
    "Hemibody", 
    "Whole body", 
    "Mantle, minimantle (obsolete after 2017)", 
    "Lower extended field (obsolete after 2017)", 
    "Invalid historical FORDS value", 
    "Other", 
    "Unknown"
  )
)
  #########################################


  #########################################
  df$PHASE_II_RT_TO_LN <-
    factor(
      df$PHASE_II_RT_TO_LN,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 88, 99),
      labels = c(
        "No radiation to draining lymph nodes",
        "Neck lymph node regions",
        "Thoracic lymph node regions",
        "Neck and thoracic lymph node regions",
        "Breast/ Chest wall lymph node regions",
        "Abdominal lymph nodes",
        "Pelvic lymph nodes",
        "Abdominal and pelvic lymph nodes",
        "Lymph node region, NOS",
        "Not applicable; Radiation primary treatment is lymph nodes",
        "Unknown if any radiation treatment to draining lymph nodes; Unknown if radiation treatment administered"
      )
    )
  #########################################


  #########################################
  df$PHASE_II_RT_MODALITY <-
    factor(
      df$PHASE_II_RT_MODALITY,
      levels = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        98,
        99
      ),
      labels = c(
        "No radiation treatment",
        "External beam, NOS",
        "External beam, photons",
        "External beam, protons",
        "External beam, electrons",
        "External beam, neutrons",
        "External beam, carbon ions",
        "Brachytherapy, NOS",
        "Brachytherapy, intracavitary, LDR",
        "Brachytherapy, intracavitary, HDR",
        "Brachytherapy, interstitial, LDR",
        "Brachytherapy, interstitial, HDR",
        "Brachytherapy, electronic",
        "Radioisotopes, NOS",
        "Radioisotopes, Radium-223",
        "Radioisotopes, Strontium-89",
        "Radioisotopes, Strontium-90",
        "Radiation Rx administered",
        "Radiation treatment unknown"
      )
    )
  #########################################


  #########################################
  df$PHASE_II_BEAM_TECH <- factor(df$PHASE_II_BEAM_TECH,
    levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
               "88", "98", "99"),
    labels = c(
      "No radiation treatment",
      "External beam, NOS",
      "Low energy x-ray/photon therapy",
      "2-D therapy",
      "Conformal or 3-D conformal therapy",
      "Intensity modulated therapy",
      "Stereotactic radiotherapy or radiosurgery, NOS",
      "Stereotactic radiotherapy or radiosurgery, robotic",
      "Stereotactic radiotherapy or radiosurgery, Gamma Knife",
      "CT-guided online adaptive therapy",
      "MR-guided online adaptive therapy",
      "Not applicable",
      "Other, NOS",
      "Unknown"
    )
  )
  #########################################


  #########################################
  df$PHASE_II_DOSE_FRACT <-
    ifelse(df$PHASE_II_DOSE_FRACT %in% c(99998, 99999), NA, df$PHASE_II_DOSE_FRACT)
  #########################################


  #########################################
  df$PHASE_II_NUM_FRACT <-
    ifelse(df$PHASE_II_NUM_FRACT %in% c(999), NA, df$PHASE_II_NUM_FRACT)
  #########################################


  #########################################
  df$PHASE_II_TOTAL_DOSE <-
    ifelse(df$PHASE_II_TOTAL_DOSE %in% c(999998, 999999), NA, df$PHASE_II_TOTAL_DOSE)
  #########################################


  #########################################
df$PHASE_III_RT_VOLUME <- factor(df$PHASE_III_RT_VOLUME, 
  levels = c("00", "", "01", "02", "03", "04", "05", "06", "07", "09", "10", "11", "12", 
             "13", "14", "20", "21", "22", "23", "24", "25", "26", "29", "30", "31", "32", 
             "39", "40", "41", "42", "50", "51", "52", "53", "54", "55", "56", "57", "58", 
             "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "70", "71", "72", 
             "73", "80", "81", "82", "83", "84", "85", "86", "88", "90", "91", "92", "93", 
             "94", "95", "97", "98", "99"),
  labels = c(
    "No radiation treatment", 
    "No radiation treatment",  # for the blank code
    "Neck lymph node regions", 
    "Thoracic lymph node regions", 
    "Neck and thoracic lymph node regions", 
    "Breast/Chest wall lymph node regions", 
    "Abdominal lymph nodes", 
    "Pelvic lymph nodes", 
    "Abdominal and pelvic lymph nodes", 
    "Lymph node region NOS", 
    "Eye/orbit/optic nerve", 
    "Pituitary", 
    "Brain", 
    "Brain (limited)", 
    "Spinal cord", 
    "Nasopharynx", 
    "Oral cavity", 
    "Oropharynx", 
    "Larynx (glottis) or hypopharynx", 
    "Sinuses/Nasal tract", 
    "Parotid or other salivary glands", 
    "Thyroid", 
    "Head and neck (NOS)", 
    "Lung or bronchus", 
    "Mesothelium", 
    "Thymus", 
    "Chest/lung (NOS)", 
    "Breast (whole)", 
    "Breast (partial)", 
    "Chest wall", 
    "Esophagus", 
    "Stomach", 
    "Small bowel", 
    "Colon", 
    "Rectum", 
    "Anus", 
    "Liver", 
    "Biliary tree or gallbladder", 
    "Pancreas or hepatopancreatic ampulla", 
    "Abdomen (NOS)", 
    "Bladder (whole)", 
    "Bladder (partial)", 
    "Kidney", 
    "Ureter", 
    "Prostate (whole)", 
    "Prostate (partial)", 
    "Urethra", 
    "Penis", 
    "Testicle or scrotum", 
    "Ovaries or fallopian tubes", 
    "Uterus or cervix", 
    "Vagina", 
    "Vulva", 
    "Skull", 
    "Spine/vertebral bodies", 
    "Shoulder", 
    "Ribs", 
    "Hip", 
    "Pelvic bones", 
    "Pelvis (NOS, nonvisceral)", 
    "Extremity bone, NOS", 
    "Skin", 
    "Soft Tissue", 
    "Hemibody", 
    "Whole body", 
    "Mantle, minimantle (obsolete after 2017)", 
    "Lower extended field (obsolete after 2017)", 
    "Invalid historical FORDS value", 
    "Other", 
    "Unknown"
  )
)
  #########################################


  #########################################
  df$PHASE_III_RT_TO_LN <-
    factor(
      df$PHASE_III_RT_TO_LN,
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 88, 99),
      labels = c(
        "No radiation to draining lymph nodes",
        "Neck lymph node regions",
        "Thoracic lymph node regions",
        "Neck and thoracic lymph node regions",
        "Breast/ Chest wall lymph node regions",
        "Abdominal lymph nodes",
        "Pelvic lymph nodes",
        "Abdominal and pelvic lymph nodes",
        "Lymph node region, NOS",
        "Not applicable; Radiation primary treatment is lymph nodes",
        "Unknown if any radiation treatment to draining lymph nodes; Unknown if radiation treatment administered"
      )
    )
  #########################################


  #########################################
  df$PHASE_III_RT_MODALITY <-
    factor(
      df$PHASE_III_RT_MODALITY,
      levels = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        98,
        99
      ),
      labels = c(
        "No radiation treatment",
        "External beam, NOS",
        "External beam, photons",
        "External beam, protons",
        "External beam, electrons",
        "External beam, neutrons",
        "External beam, carbon ions",
        "Brachytherapy, NOS",
        "Brachytherapy, intracavitary, LDR",
        "Brachytherapy, intracavitary, HDR",
        "Brachytherapy, interstitial, LDR",
        "Brachytherapy, interstitial, HDR",
        "Brachytherapy, electronic",
        "Radioisotopes, NOS",
        "Radioisotopes, Radium-223",
        "Radioisotopes, Strontium-89",
        "Radioisotopes, Strontium-90",
        "Radiation Rx administered, Rx modality unknown",
        "Radiation treatment modality unknown; Unknown if radiation treatment administered"
      )
    )
  #########################################


  #########################################
  df$PHASE_III_BEAM_TECH <- factor(df$PHASE_III_BEAM_TECH,
    levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
               "88", "98", "99"),
    labels = c(
      "No radiation treatment",
      "External beam, NOS",
      "Low energy x-ray/photon therapy",
      "2-D therapy",
      "Conformal or 3-D conformal therapy",
      "Intensity modulated therapy",
      "Stereotactic radiotherapy or radiosurgery, NOS",
      "Stereotactic radiotherapy or radiosurgery, robotic",
      "Stereotactic radiotherapy or radiosurgery, Gamma Knife",
      "CT-guided online adaptive therapy",
      "MR-guided online adaptive therapy",
      "Not applicable",
      "Other, NOS",
      "Unknown"
    )
  )
  #########################################


  #########################################
  df$PHASE_III_DOSE_FRACT <-
    ifelse(df$PHASE_III_DOSE_FRACT %in% c(99998, 99999), NA, df$PHASE_III_DOSE_FRACT)
  #########################################


  #########################################
  df$PHASE_III_NUM_FRACT <-
    ifelse(df$PHASE_III_NUM_FRACT %in% c(999), NA, df$PHASE_III_NUM_FRACT)
  #########################################


  #########################################
  df$PHASE_III_TOTAL_DOSE <-
    ifelse(df$PHASE_III_TOTAL_DOSE %in% c(999998, 999999), NA, df$PHASE_III_TOTAL_DOSE)
  #########################################


  #########################################
  df$NUMBER_PHASES_RAD_RX <- 
    ifelse(df$NUMBER_PHASES_RAD_RX %in% c(99), NA, df$NUMBER_PHASES_RAD_RX)
  #########################################


  #########################################
  df$RAD_RX_DISC_EARLY <- 
  factor(
    df$RAD_RX_DISC_EARLY,
    levels = c(0, 1, 2, 3, 4, 5, 6, 7, 99),
    labels = c(
      "No radiation treatment",
      "Radiation treatment completed as prescribed",
      "Radiation treatment discontinued early; toxicity",
      "Radiation treatment discontinued early; contraindicated to other patient risk factors (comorbid conditions, advanced age, progression of tumor prior to planned radiation, etc.",
      "Radiation treatment discontinued early; patient decision",
      "Radiation treatment discontinued early; family decision",
      "Radiation treatment discontinued early; patient expired",
      "Radiation treatment discontinued early; reason not documented",
      "Unknown if radiation treatment discontinued; Unknown whether radiation therapy administered"
    )
  )
  #########################################


  #########################################
  df$TOTAL_DOSE <- 
    ifelse(df$TOTAL_DOSE %in% c(999998, 999999), NA, df$TOTAL_DOSE)
  #########################################


  #########################################
  # df$RX_SUMM_SURGRAD_SEQ <- 
  # factor(
  #   df$RX_SUMM_SURGGRAD_SEQ,
  #   levels = c(0, 2, 3, 4, 5, 6, 7, 9),
  #   labels = c(
  #     "No radiation therapy and/or surgical procedures",
  #     "Radiation therapy before surgery",
  #     "Radiation therapy after surgery",
  #     "Radiation therapy both before and after surgery",
  #     "Intraoperative radiation therapy",
  #     "Intraoperative radiation therapy with other therapy administered before or after surgery",
  #     "Surgery both before and after radiation",
  #     "Sequence unknown"
  #   )
  # )
  #########################################


  #########################################
  df$RAD_ELAPSED_RX_DAYS <- 
    ifelse(df$RAD_ELAPSED_RX_DAYS %in% c(999), NA, df$RAD_ELAPSED_RX_DAYS)
  #########################################


  #########################################
  df$REASON_FOR_NO_RADIATION <-
    factor(
      df$REASON_FOR_NO_RADIATION,
      levels = c(0, 1, 2, 5, 6, 7, 8, 9),
      labels = c(
        "Radiation therapy was administered",
        "Radiation therapy was not administered because it was not part of the planned first course treatment",
        "Radiation therapy was not recommended/administeredbecause it was contraindicated due to other patient risk factors (comorbid conditions, advanced age, etc.)",
        "Radiation therapy was not administered because the patient died priorto planned or recommended therapy",
        "Radiation therapy was not administered; it was recommended by the patient’s physician, but was not administered as part of first course treatment. No reason was noted in patient record",
        "Radiation therapy was not administered; it was recommended by the patient’s physician, but this treatment was refused by the patient, the patient’s family member, orthe patient’s guardian. The refusal was noted in patient record",
        "Radiation therapy was recommended, but it is unknown whetherit was administered",
        "It is unknown if radiation therapy was recommended or administered"
      )
    )
  #########################################


  #########################################
  df$DX_SYSTEMIC_STARTED_DAYS
  #########################################


  #########################################
  df$RX_SUMM_CHEMO <- 
  factor(
    df$RX_SUMM_CHEMO,
    levels = c(0, 1, 2, 3, 82, 85, 86, 87, 88, 99),
    labels = c(
      "None, chemotherapy was not part of the planned first course of therapy",
      "Chemotherapy administered as first course therapy, but the type and number of agents is not documented in patient record",
      "Single-agent chemotherapy administered as first course therapy",
      "Multiagent chemotherapy administered as first course therapy",
      "Chemotherapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age, progression of tumor prior to administration, etc.)",
      "Chemotherapy was not administered because the patient died priorto planned orrecommended therapy",
      "Chemotherapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record",
      "Chemotherapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, or the patient's guardian. The refusal was noted in patient record",
      "Chemotherapy was recommended, but it is unknown whether it was administered",
      "It is unknown if chemotherapy was recommended or administered"
    )
  )
  #########################################


  #########################################
  df$RX_HOSP_CHEMO <- 
    factor(
      df$RX_HOSP_CHEMO,
      levels = c(0, 1, 2, 3, 99),
      labels = c(
        "None, chemotherapy was not part of the planned first course of therapy",
        "Chemotherapy administered as first course therapy, but the type and number of agents is not documented in patient record",
        "Single-agent chemotherapy administered as first course therapy",
        "Multiagent chemotherapy administered as first course therapy",
        "It is unknown whether a chemotherapeutic agent(s) was recommended or administered because it is not stated in patient record"
      )
    )
  #########################################


  #########################################
  df$DX_CHEMO_STARTED_DAYS
  #########################################


  #########################################
  df$RX_SUMM_HORMONE <- 
  factor(
    df$RX_SUMM_HORMONE,
    levels = c(0, 1, 2, 3, 82, 85, 86, 87, 88, 99),
    labels = c(
      "None, hormone therapy was not part of the planned first course of therapy",
      "Hormone therapy administered as first course therapy, but the type and number of agents is not documented in patient record",
      "Single-agent hormone therapy administered as first course therapy",
      "Multiagent hormone therapy administered as first course therapy",
      "Hormone therapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age, progression of tumor prior to administration)",
      "Hormone therapy was not administered because the patient died priorto planned orrecommended therapy",
      "Hormone therapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record",
      "Hormone therapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, orthe patient's guardian. The refusal was noted in patient record",
      "Hormone therapy was recommended, but it is unknown if it was administered",
      "It is unknown whether a hormonal agent(s) was recommended or administered because it is not stated in patient record"
    )
  )
  #########################################


  #########################################
  df$RX_HOSP_HORMONE <-
    factor(
      df$RX_HOSP_HORMONE,
      levels = c(0, 1, 99),
      labels = c(
        "None, hormone therapy was not part of the planned first course of therapy",
        "Hormone therapy administered as first course therapy",
        "It is unknown whether a hormonal agent(s) was recommended or administered because it is not stated in patient record"
      )
    )
  #########################################


  #########################################
  df$DX_HORMONE_STARTED_DAYS
  #########################################


  #########################################
  df$RX_SUMM_IMMUNOTHERAPY <- 
    factor(
      df$RX_SUMM_IMMUNOTHERAPY,
      levels = c(0, 1, 2, 3, 82, 85, 86, 87, 88, 99),
      labels = c(
        "None, immunotherapy was not part of the planned first course of therapy",
        "Immunotherapy administered as first course therapy, but the type and number of agents is not documented in patient record",
        "Single-agent immunotherapy administered as first course therapy",
        "Multiagent immunotherapy administered as first course therapy",
        "Immunotherapy was not recommended/administered because it was contraindicated due to patient risk factors (ie, comorbid conditions, advanced age, progression of tumor prior to administration)",
        "Immunotherapy was not administered because the patient died priorto planned orrecommended therapy",
        "Immunotherapy was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record",
        "Immunotherapy was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, orthe patient's guardian. The refusal was noted in patient record",
        "Immunotherapy was recommended, but it is unknown if it was administered",
        "It is unknown whether an immunotherapeutic agent(s) was recommended or administered because it is not stated in patient record"
      )
    )
  #########################################


  #########################################
  df$RX_HOSP_IMMUNOTHERAPY <-
    factor(
      df$RX_HOSP_IMMUNOTHERAPY,
      levels = c(0, 1, 99),
      labels = c(
        "None, immunotherapy was not part of the planned first course of therapy",
        "Immunotherapy administered as first course therapy",
        "It is unknown whether an immunotherapeutic agent(s) was recommended or administered because it is not stated in patient record"
      )
    )
  #########################################


  #########################################
  df$DX_IMMUNOTHERAPY_STARTED_DAYS
  #########################################


  #########################################
  df$RX_SUMM_TRNSPLNT_ENDO <-
    factor(
      df$RX_SUMM_TRNSPLNT_ENDO,
      levels = c(0, 10, 11, 12, 20, 30, 40, 82, 85, 86, 87, 88, 99),
      labels = c(
        "No transplant procedure or endocrine therapy was administered as part of first course therapy",
        "A bonemarrow transplant procedure was administered, but the type was not specified",
        "Bone marrow transplant - autologous",
        "Bone marrow transplant - allogeneic",
        "Stem cell harvest and infusion. Umbilical cord stem cell transplant, with blood from one or multiple umbilical cords.",
        "Endocrine surgery and/or endocrine radiation therapy",
        "Combination of endocrine surgery and/orradiation with a transplant procedure. (Combination of codes 30 and 10, 11, 12, or 20)",
        "Hematologic transplant and/or endocrine surgery/radiation was not recommended/administered because it was contraindicated due to patient risk factors (i.e, comorbid conditions, advanced age, progression of disease prior to administration, etc.)",
        "Hematologic transplant and/or endocrine surgery/radiation was not administered because the patient died priorto planned orrecommended therapy",
        "Hematologic transplant and/or endocrine surgery/radiation was not administered. It was recommended by the patient's physician, but was not administered as part of the first course of therapy. No reason was stated in patient record",
        "Hematologic transplant and/or endocrine surgery/radiation was not administered. It was recommended by the patient's physician, but this treatment was refused by the patient, a patient's family member, orthe patient's guardian. The refusal was noted in patient record",
        "Hematologic transplant and/or endocrine surgery/radiation was recommended, but it is unknown if it was administered",
        "It is unknown whether hematologic transplant and/or endocrine surgery/radiation was recommended or administered because it is not stated in patient record"
      )
    )
  #########################################


  #########################################
  df$RX_SUMM_SYSTEMIC_SUR_SEQ <- 
    factor(
      df$RX_SUMM_SYSTEMIC_SUR_SEQ,
      levels = c(0, 2, 3, 4, 5, 6, 7, 9),
      labels = c(
        "No systemic therapy and/or surgical procedures",
        "Systemic therapy before surgery",
        "Systemic therapy after surgery",
        "Systemic therapy both before and after surgery",
        "Intraoperative systemic therapy",
        "Intraoperative systemic therapy with other systemic therapy administered before or after surgery",
        "Surgery both before and after systemic therapy",
        "Sequence unknown"
      )
    )
  #########################################


  #########################################
  df$RX_SUMM_OTHER <- 
    factor(
      df$RX_SUMM_OTHER,
      levels = c(0, 1, 2, 3, 6, 7, 8, 9),
      labels = c(
        "None",
        "Other",
        "Other Experimental",
        "Other Double Blind",
        "Other Unproven",
        "Refusal",
        "Recommended, unknown if administered",
        "Unknown"
      )
    )
  #########################################


  #########################################
  df$RX_HOSP_OTHER <- 
    factor(
      df$RX_HOSP_OTHER,
      levels = c(0, 1, 2, 3, 6, 7, 8, 9),
      labels = c(
        "None",
        "Other",
        "Other Experimental",
        "Other Double Blind",
        "Other Unproven",
        "Refusal",
        "Recommended, unknown if administered",
        "Unknown"
      )
    )
  #########################################


  #########################################
  df$DX_IMMUNOTHERAPY_STARTED_DAYS
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
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
  #########################################


  #########################################
  df$DX_LASTCONTACT_DEATH_MONTHS <-
    ifelse(df$DX_LASTCONTACT_DEATH_MONTHS %in% c(9999), NA, df$DX_LASTCONTACT_DEATH_MONTHS)
  #########################################


  #########################################
  df$PUF_VITAL_STATUS <-
    factor(
      df$PUF_VITAL_STATUS,
      levels = c(0, 1),
      labels = c("Dead",
                 "Alive")
    )
  #########################################


  # lets return a df which removes the columns we have not edited yet
  # that would be SEQUENCE_NUMBER, PRIMARY_SITE, HISTOLOGY, Grade_Clin, Grade_Path, Grade_Path_Post, CS_VERSION_ENCODED, CS_EXTENSION, CS_METS_AT_DX, CS_METS_EVAL, RX_SUMM_SURG_PRIM_SITE, and CS_SITESPECIFIC_FACTOR_1-25

  new_df <- df[, !(names(df) %in% c("SEQUENCE_NUMBER", "PRIMARY_SITE", "HISTOLOGY", "Grade_Clin", "Grade_Path", "Grade_Path_Post", 
  "CS_VERSION_ENCODED", "CS_EXTENSION", "CS_METS_AT_DX", "CS_METS_EVAL", "RX_SUMM_SURG_PRIM_SITE", "CS_SITESPECIFIC_FACTOR_1",
  "CS_SITESPECIFIC_FACTOR_2", "CS_SITESPECIFIC_FACTOR_3", "CS_SITESPECIFIC_FACTOR_4", "CS_SITESPECIFIC_FACTOR_5", "CS_SITESPECIFIC_FACTOR_6",
  "CS_SITESPECIFIC_FACTOR_7", "CS_SITESPECIFIC_FACTOR_8", "CS_SITESPECIFIC_FACTOR_9", "CS_SITESPECIFIC_FACTOR_10", "CS_SITESPECIFIC_FACTOR_11",
  "CS_SITESPECIFIC_FACTOR_12", "CS_SITESPECIFIC_FACTOR_13", "CS_SITESPECIFIC_FACTOR_14", "CS_SITESPECIFIC_FACTOR_15", "CS_SITESPECIFIC_FACTOR_16",
  "CS_SITESPECIFIC_FACTOR_17", "CS_SITESPECIFIC_FACTOR_18", "CS_SITESPECIFIC_FACTOR_19", "CS_SITESPECIFIC_FACTOR_20", "CS_SITESPECIFIC_FACTOR_21",
  "CS_SITESPECIFIC_FACTOR_22", "CS_SITESPECIFIC_FACTOR_23", "CS_SITESPECIFIC_FACTOR_24", "CS_SITESPECIFIC_FACTOR_25"))]

  return(new_df)
}



ncdb_drop_ids <- function(df) {
  drop_variables <- sapply(df, function(col) length(levels(col)) > 100)
  df <- df[, -which(drop_variables)]
  return(df)
}


ncdb_rename <- function(df) {
  names(df)[names(df) == "PUF_CASE_ID"] <- "case_id"
  names(df)[names(df) == "PUF_FACILITY_ID"] <- "facility"
  names(df)[names(df) == "FACILITY_TYPE_CD"] <- "type_of_facility"
  return(df)
}