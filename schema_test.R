remotes::install_github("ropensci/jsonvalidate")
remotes::install_github("nmfs-fish-tools/data_dictionary")
library(jsonvalidate)
library(stockassessmentdictionary)


schema <- '{
    "$schema": "http://json-schema.org/draft-04/schema#",
    "title": "StockAssessmentDataDictionary",
    "description": "A json schema",
    "type": "object",
    "properties": {
        "name": {
            "description": "Name of the term",
            "type": "string",
            "minLength": 1
        },
        "description": {
            "description": "Description of the term",
            "type": "string",
            "minLength": 1
        },
        "Examples": {
            "description": "Detailed information illustrating something that is characteristic of its kind or a general rule of thumb such that readers can grasp the concept of the term",
            "type": "string"
        },
        "ICES code type": {
            "description": "ICES code type from https://vocab.ices.dk/",
            "type": "string"
        },
        "Units": {
            "description": "Unit of the term",
            "type": "string"
        },
        "Raionale": {
            "description": "Reasons for why the term was included",
            "type": "string"
        },
        "Alternatives": {
            "description": "Synonyms along with the potential reference for what framework uses the alternative. Sources for each alternative, in parentheses, will help build the language translator.",
            "type": "string"
        },
        "Range of possible values": {
            "description": "min and max values of the term",
            "type": "string"
        }
    },
    "required": ["name", "description"]
}'

biomass <- '{
    "name": "Biomass (B)",
    "description": "Model predicted weight (mt) of all fish within a stock. If referring to a certain portion of the stock, it should be made clear what portion of the stock the biomass pertains to using a modifying word or clause. ",
    "Examples": " spawning stock biomass, age three plus biomass, exploitable biomass, January 1 biomass",
    "ICES code type": " StockSizeIndicator",
    "Units": " mt",
    "Rationale": " Biomass is often undefined in terms of what year classes and sexes it includes. Typically, the age classes that are included will precede the word “biomass” in the sentence. ",
    "Alternatives": " total biomass, abundance which is in numbers not mt, BMWETWT (ICES), B_index (ICES), Biomass index",
    "Range of possible values": ""
  }'

ssb <- '{
    "name": "Spawning Stock Biomass (SSB)",
    "description": "",
    "Examples": " ",
    "ICES code type": "",
    "Units": " mt",
    "Rationale": " This quantity is key to projecting populations assuming these are the fish contributing to reproduction. ",
    "Alternatives": " NA Spawning Stock Output (eggs)",
    "Range of possible values": " zero to infinity"
  }'

validator <- jsonvalidate::json_validator(schema)
validator("{}", verbose = TRUE, error = TRUE)
validator(biomass, verbose = TRUE, error = TRUE)
validator(ssb, verbose = TRUE, error = TRUE)


json_out_path <- "inst/extdata/top20.json"
in_json <- read_json(json_out_path)

for (i in 1:length(in_json)){
  
  data <- toJSON(in_json[[i]], auto_unbox=TRUE)
  cat(validator(data, verbose = TRUE, error = TRUE))
  print(in_json[[i]]$name)
  
}


