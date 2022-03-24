library(Capr)

df <- getConceptIdDetails(conceptIds = c(1310149,40241331),
                          connection = connection,
                          vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                          mapToStandard = TRUE)


tst <- conceptSet(df) %>%
  conceptSetItem(includeDescendants = c(T,F)) %>%
  conceptSetExpression() %>%
  component()

comp <- component(Name = "test", ComponentType = "ConceptSetExpression",
                  ConceptSetExpression = list(tst))
