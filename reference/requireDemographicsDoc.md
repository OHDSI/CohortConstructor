# Helper for consistent documentation of arguments in `requireDemographics`.

Helper for consistent documentation of arguments in
`requireDemographics`.

## Arguments

- ageRange:

  A list of vectors specifying minimum and maximum age.

- sex:

  Can be "Both", "Male" or "Female".

- minPriorObservation:

  A minimum number of continuous prior observation days in the database.

- minFutureObservation:

  A minimum number of continuous future observation days in the
  database.

- indexDate:

  Variable in cohort that contains the date to compute the demographics
  characteristics on which to restrict on.

- requirementInteractions:

  If TRUE, cohorts will be created for all combinations of ageGroup,
  sex, and daysPriorObservation. If FALSE, only the first value
  specified for the other factors will be used. Consequently, order of
  values matters when requirementInteractions is FALSE.
