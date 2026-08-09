# Behind the scenes

In previous vignettes we have seen numerous R functions that can help us
to add a cohort of interest to a cdm reference. When our cdm reference
is to tables in a database, as is often the code, our R code will have
been translated to SQL that is run against tables in the databases (for
more details on how this is all implemented see
<https://ohdsi.github.io/Tidy-R-programming-with-OMOP/>).

Let’s again work with Eunomia and get the codes needed to create a set
of drug cohorts.

``` r

library(omock)
library(CodelistGenerator)
library(PatientProfiles)
library(CohortConstructor)
library(dplyr)

cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")

drug_codes <- getDrugIngredientCodes(
  cdm = cdm, 
  name = c("acetaminophen", "amoxicillin", "diclofenac", "simvastatin", "warfarin")
)
```

To capture all the SQL executed as we use CohortConstructor functions we
can set a global option. For this, we just need to point to a directory
in which we’ll save each SQL statement run behind the scenes. Note that
in this example we’re using duckdb so the SQL is for this database
management system. If you were running on a different type of database
the SQL would be adapted accordingly.

``` r

dir_sql <- file.path(tempdir(), "sql_folder")
dir.create(dir_sql)
options("omopgenerics.log_sql_path" = dir_sql)

cdm$drugs <- conceptCohort(cdm, 
                           conceptSet = drug_codes,
                           exit = "event_end_date",
                           name = "drugs")

# print sql in order they were saved
files <- file.info(list.files(dir_sql, full.names = TRUE))
sorted_files <- rownames(files[order(files$ctime),])
for(i in seq_along(sorted_files)) {
  cat(paste0("### ", sorted_files[i], "\n\n"))
  sql_with_quotes <- paste0('"', paste(readLines(sorted_files[i]), collapse = '\n'), '"')
  cat(sql_with_quotes, "\n```\n\n")
}
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00001_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.040 seconds
#> sql: <SQL>
#>   SELECT DISTINCT concept.concept_id AS concept_id
#>   FROM concept
#>   INNER JOIN results.test_og_003_1786285434
#>     ON (concept.concept_id = test_og_003_1786285434.concept_id)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00002_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.038 seconds
#> sql: <SQL>
#>   SELECT vocabulary_version
#>   FROM vocabulary
#>   WHERE (vocabulary_id = 'None')" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00003_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: tmp_001_og_004_1786285434
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_uploadCohortCodelist
#> time_taken: 0.131 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     TRY_CAST(concept_id AS INTEGER) AS concept_id,
#>     LOWER(domain_id) AS domain_id
#>   FROM (
#>     SELECT test_tmp_001_og_004_1786285434.*, domain_id
#>     FROM results.test_tmp_001_og_004_1786285434
#>     LEFT JOIN concept
#>       ON (test_tmp_001_og_004_1786285434.concept_id = concept.concept_id)
#>   ) AS q01" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00004_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.029 seconds
#> sql: <SQL>
#>   SELECT domain_id, COUNT(*) AS n
#>   FROM results.test_tmp_001_og_004_1786285434
#>   GROUP BY domain_id" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00005_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.024 seconds
#> sql: <SQL>
#>   SELECT DISTINCT domain_id
#>   FROM results.test_tmp_001_og_004_1786285434" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00006_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.028 seconds
#> sql: <SQL>
#>   SELECT COUNT(*) AS n
#>   FROM results.test_tmp_001_og_004_1786285434
#>   WHERE (domain_id IN ('drug'))" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00007_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: tmp_001_temp_codelist_cohort_id
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_tempCodelistCohortId_
#> time_taken: 0.056 seconds
#> sql: <SQL>
#>   SELECT DISTINCT cohort_definition_id, concept_id
#>   FROM results.test_tmp_001_og_004_1786285434
#>   WHERE (domain_id IN ('drug'))" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00008_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: tmp_001_temp_codelist
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_tempCodelist_
#> time_taken: 0.041 seconds
#> sql: <SQL>
#>   SELECT DISTINCT concept_id
#>   FROM results.test_tmp_001_temp_codelist_cohort_id" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00009_on_2026_08_09_at_14_23_54.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: tmp_001_og_005_1786285435_1
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_tempCohort_
#> time_taken: 0.078 seconds
#> sql: <SQL>
#>   SELECT
#>     person_id AS subject_id,
#>     drug_concept_id AS concept_id,
#>     drug_exposure_start_date AS cohort_start_date,
#>     drug_exposure_end_date AS cohort_end_date
#>   FROM drug_exposure
#>   INNER JOIN results.test_tmp_001_temp_codelist
#>     ON (drug_exposure.drug_concept_id = test_tmp_001_temp_codelist.concept_id)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00010_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: tmp_001_og_005_1786285435_1
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_tempCohort_
#> time_taken: 0.209 seconds
#> sql: <SQL>
#>   SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#>   FROM results.test_tmp_001_og_005_1786285435_1
#>   INNER JOIN results.test_tmp_001_temp_codelist_cohort_id
#>     ON (test_tmp_001_og_005_1786285435_1.concept_id = test_tmp_001_temp_codelist_cohort_id.concept_id)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00011_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.030 seconds
#> sql: <SQL>
#>   SELECT COUNT(*) AS n
#>   FROM (
#>     SELECT *
#>     FROM results.test_tmp_001_og_005_1786285435_1
#>     LIMIT 1
#>   ) AS q01" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00012_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: drugs
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_conceptCohort_reduce_
#> time_taken: 0.064 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     subject_id,
#>     cohort_start_date,
#>     COALESCE(cohort_end_date, cohort_start_date) AS cohort_end_date
#>   FROM results.test_tmp_001_og_005_1786285435_1" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00013_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.033 seconds
#> sql: <SQL>
#>   SELECT COUNT(*) AS n
#>   FROM (
#>     SELECT *
#>     FROM results.test_drugs
#>     LIMIT 1
#>   ) AS q01" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00014_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.041 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     COUNT(*) AS number_records,
#>     COUNT(DISTINCT row(subject_id)) AS number_subjects
#>   FROM results.test_drugs
#>   GROUP BY cohort_definition_id" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00015_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.020 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM results.test_drugs
#>   LIMIT 1" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00016_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.025 seconds
#> sql: <SQL>
#>   SELECT DISTINCT cohort_definition_id
#>   FROM results.test_drugs" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00017_on_2026_08_09_at_14_23_55.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: drugs
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_fulfillCohortReqs_observationJoin_
#> time_taken: 0.116 seconds
#> sql: <SQL>
#>   SELECT
#>     test_drugs.*,
#>     observation_period_id,
#>     observation_period_start_date,
#>     observation_period_end_date
#>   FROM results.test_drugs
#>   LEFT JOIN observation_period
#>     ON (test_drugs.subject_id = observation_period.person_id)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00018_on_2026_08_09_at_14_23_56.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: drugs
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_fulfillCohortReqs_useRecordsBeforeObservation_
#> time_taken: 0.125 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     subject_id,
#>     cohort_start_date,
#>     CASE WHEN (observation_period_end_date >= cohort_end_date) THEN cohort_end_date WHEN NOT (observation_period_end_date >= cohort_end_date) THEN observation_period_end_date END AS cohort_end_date
#>   FROM results.test_drugs
#>   WHERE
#>     (cohort_start_date >= observation_period_start_date) AND
#>     (cohort_start_date <= observation_period_end_date)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00019_on_2026_08_09_at_14_23_56.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.044 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     COUNT(*) AS number_records,
#>     COUNT(DISTINCT row(subject_id)) AS number_subjects
#>   FROM results.test_drugs
#>   WHERE (cohort_definition_id IN (1, 2, 3, 4, 5))
#>   GROUP BY cohort_definition_id" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00020_on_2026_08_09_at_14_23_56.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.020 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM results.test_drugs
#>   LIMIT 1" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00021_on_2026_08_09_at_14_23_56.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.024 seconds
#> sql: <SQL>
#>   SELECT DISTINCT cohort_definition_id
#>   FROM results.test_drugs" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00022_on_2026_08_09_at_14_23_56.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: drugs
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_fulfillCohortReqs_filterStart_
#> time_taken: 0.079 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM results.test_drugs
#>   WHERE (NOT((cohort_start_date IS NULL)))" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00023_on_2026_08_09_at_14_23_56.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.044 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     COUNT(*) AS number_records,
#>     COUNT(DISTINCT row(subject_id)) AS number_subjects
#>   FROM results.test_drugs
#>   WHERE (cohort_definition_id IN (1, 2, 3, 4, 5))
#>   GROUP BY cohort_definition_id" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00024_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.020 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM results.test_drugs
#>   LIMIT 1" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00025_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.024 seconds
#> sql: <SQL>
#>   SELECT DISTINCT cohort_definition_id
#>   FROM results.test_drugs" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00026_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: drugs
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_fulfillCohortReqs_filterStartEnd_
#> time_taken: 0.103 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     subject_id,
#>     cohort_start_date,
#>     CASE WHEN (cohort_start_date <= cohort_end_date) THEN cohort_end_date WHEN NOT (cohort_start_date <= cohort_end_date) THEN cohort_start_date END AS cohort_end_date
#>   FROM results.test_drugs" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00027_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.030 seconds
#> sql: <SQL>
#>   SELECT COUNT(*) AS n
#>   FROM (
#>     SELECT *
#>     FROM results.test_drugs
#>     LIMIT 5
#>   ) AS q01" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00028_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: og_010_1786285437
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_joinOverlap_workingTbl_
#> time_taken: 0.125 seconds
#> sql: <SQL>
#>   SELECT *, -1 AS date_id
#>   FROM (
#>     SELECT cohort_definition_id, subject_id, cohort_start_date AS date
#>     FROM results.test_drugs
#>   ) AS q01
#>   
#>   UNION ALL
#>   
#>   SELECT *, 1 AS date_id
#>   FROM (
#>     SELECT cohort_definition_id, subject_id, cohort_end_date AS date
#>     FROM results.test_drugs
#>   ) AS q01" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00029_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: og_011_1786285437
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_joinOverlap_colapse_
#> time_taken: 0.335 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     subject_id,
#>     MIN(date) AS cohort_start_date,
#>     MAX(date) AS cohort_end_date
#>   FROM (
#>     SELECT
#>       cohort_definition_id,
#>       subject_id,
#>       date,
#>       date_id,
#>       cum_id,
#>       SUM(TRY_CAST(era_id AS INTEGER)) OVER (PARTITION BY cohort_definition_id, subject_id ORDER BY date, date_id ROWS UNBOUNDED PRECEDING) AS era_id
#>     FROM (
#>       SELECT
#>         *,
#>         SUM(date_id) OVER (PARTITION BY cohort_definition_id, subject_id ORDER BY date, date_id ROWS UNBOUNDED PRECEDING) AS cum_id,
#>         CASE WHEN (date_id = -1) THEN 1 WHEN NOT (date_id = -1) THEN 0 END AS era_id
#>       FROM results.test_og_010_1786285437
#>     ) AS q01
#>     WHERE (cum_id = 0 OR (cum_id = -1 AND date_id = -1))
#>   ) AS q01
#>   GROUP BY cohort_definition_id, subject_id, era_id" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00030_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: compute
#> schema: results
#> prefix: test_
#> name: drugs
#> temporary: FALSE
#> overwrite: TRUE
#> log_prefix: CohortConstructor_joinOverlap_relocate_
#> time_taken: 0.041 seconds
#> sql: <SQL>
#>   SELECT DISTINCT *
#>   FROM results.test_og_011_1786285437" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00031_on_2026_08_09_at_14_23_57.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.044 seconds
#> sql: <SQL>
#>   SELECT
#>     cohort_definition_id,
#>     COUNT(*) AS number_records,
#>     COUNT(DISTINCT row(subject_id)) AS number_subjects
#>   FROM results.test_drugs
#>   WHERE (cohort_definition_id IN (1, 2, 3, 4, 5))
#>   GROUP BY cohort_definition_id" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00032_on_2026_08_09_at_14_23_58.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.020 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM results.test_drugs
#>   LIMIT 1" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00033_on_2026_08_09_at_14_23_58.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.025 seconds
#> sql: <SQL>
#>   SELECT DISTINCT cohort_definition_id
#>   FROM results.test_drugs" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00034_on_2026_08_09_at_14_23_58.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.023 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM results.test_drugs
#>   LIMIT 1" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00035_on_2026_08_09_at_14_23_58.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.025 seconds
#> sql: <SQL>
#>   SELECT DISTINCT cohort_definition_id
#>   FROM results.test_drugs" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00036_on_2026_08_09_at_14_23_58.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.055 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM (
#>     SELECT
#>       subject_id,
#>       CASE WHEN (cohort_end_date < cohort_start_date) THEN 1 WHEN NOT (cohort_end_date < cohort_start_date) THEN 0 END AS end_before_start
#>     FROM results.test_drugs
#>   ) AS q01
#>   WHERE (end_before_start = 1)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00037_on_2026_08_09_at_14_23_58.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.107 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM (
#>     SELECT
#>       subject_id,
#>       CASE WHEN (cohort_end_date >= next_cohort_start_date) THEN 1 WHEN NOT (cohort_end_date >= next_cohort_start_date) THEN 0 END AS overlap
#>     FROM (
#>       SELECT
#>         *,
#>         LEAD(cohort_start_date, 1, NULL) OVER (PARTITION BY cohort_definition_id, subject_id ORDER BY cohort_start_date) AS next_cohort_start_date
#>       FROM results.test_drugs
#>     ) AS q01
#>   ) AS q01
#>   WHERE (overlap = 1)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00038_on_2026_08_09_at_14_23_58.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.062 seconds
#> sql: <SQL>
#>   SELECT *
#>   FROM (
#>     SELECT
#>       subject_id,
#>       CASE WHEN ((cohort_definition_id IS NULL) OR (subject_id IS NULL) OR (cohort_start_date IS NULL) OR (cohort_end_date IS NULL)) THEN 1 WHEN NOT ((cohort_definition_id IS NULL) OR (subject_id IS NULL) OR (cohort_start_date IS NULL) OR (cohort_end_date IS NULL)) THEN 0 END AS missing
#>     FROM results.test_drugs
#>   ) AS q01
#>   WHERE (missing = 1)" 
#> ```
#> 
#> ### /tmp/RtmptlTUGP/sql_folder/logged_query_00039_on_2026_08_09_at_14_23_59.txt
#> 
#> "type: collect
#> schema: results
#> prefix: test_
#> time_taken: 0.132 seconds
#> sql: <SQL>
#>   SELECT COUNT(*) AS n
#>   FROM (
#>     SELECT test_drugs.*
#>     FROM results.test_drugs
#>     WHERE NOT EXISTS (
#>       SELECT 1 FROM (
#>       SELECT
#>         test_drugs.*,
#>         observation_period_start_date,
#>         observation_period_end_date
#>       FROM results.test_drugs
#>       INNER JOIN observation_period
#>         ON (test_drugs.subject_id = observation_period.person_id)
#>       WHERE (test_drugs.cohort_start_date >= observation_period.observation_period_start_date AND test_drugs.cohort_start_date <= observation_period.observation_period_end_date AND test_drugs.cohort_end_date >= observation_period.observation_period_start_date AND test_drugs.cohort_end_date <= observation_period.observation_period_end_date)
#>   ) AS RHS
#>       WHERE
#>         (test_drugs.cohort_definition_id = RHS.cohort_definition_id) AND
#>         (test_drugs.subject_id = RHS.subject_id) AND
#>         (test_drugs.cohort_start_date = RHS.cohort_start_date) AND
#>         (test_drugs.cohort_end_date = RHS.cohort_end_date)
#>     )
#>   ) AS q01" 
#> ```
```

If we want even more detail, we also have the option to see the
execution plan along with the SQL.

``` r

dir_explain <- file.path(tempdir(), "explain_folder")
dir.create(dir_explain)
options("omopgenerics.log_sql_explain_path" = dir_explain)

cdm$drugs <- cdm$drugs |> 
  requireIsFirstEntry()

files <- list.files(dir_explain, full.names = TRUE)
file_names <- list.files(dir_explain, full.names = FALSE)

for(i in seq_along(files)) {
  cat(paste0("### ", file_names[i], "\n\n"))
  sql_with_quotes <- paste0('"', paste(readLines(files[i]), collapse = '\n'), '"')
  cat(sql_with_quotes, "\n```\n\n")
}
```
