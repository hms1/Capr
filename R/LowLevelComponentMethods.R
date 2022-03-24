# generic for component
setGeneric('component', function(x,
                                 Name = NA_character_,
                                 Description = NA_character_,
                                 ComponentType = NA_character_){
  standardGeneric("component")
  })

#component create for a ConceptSetExpression

setMethod("component", signature(x = "ConceptSetExpression",
                                 Name = "ANY",
                                 Description = "ANY",
                                 ComponentType = "ANY"),
          function(x,
                   Name = NA_character_,
                   Description = NA_character_,
                   ComponentType = NA_character_) {

            #create the meta data
            md <- new("MetaData",
                      ComponentType = "ConceptSetExpression",
                      Name = Name,
                      Description = Description)

            #Fill out component for conceptSetExpression Type
            Limit <- list()
            CriteriaExpression <- list()
            ConceptSetExpression <- list(x)

            #create component
            comp <- new("Component",
                        MetaData = md,
                        CriteriaExpression = CriteriaExpression,
                        Limit = Limit,
                        ConceptSetExpression = ConceptSetExpression)
            return(comp)

          })



