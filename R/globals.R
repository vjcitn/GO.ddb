# R/globals.R

utils::globalVariables(c(
  # column names used as bare symbols in dplyr verbs across the package
  "id",
  "label",
  "definition",
  "ontology",
  "deprecated",
  "subject",
  "predicate",
  "object",
  "value",
  "ancestor_id",
  "descendant_id",
  # operator translated by dbplyr to SQL LIKE
  "%like%"
))
