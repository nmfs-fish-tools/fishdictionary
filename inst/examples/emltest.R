require(EML)
attributes <- read.csv("lookupEMLattributes.csv")
factors <- read.csv("lookupEMLfactors.csv")
attributes

attributeList <- 
  set_attributes(attributes, 
                 col_classes = c("numeric",
                                 "Date",
                                 "character"))

keywordSet <- eml$keywordSet(keywordThesaurus = "LTER controlled vocabulary",
                             keyword = list("population dynamics", "fishes", "marine"))

geographicDescription <- "West Coast US"
coverage <- 
  set_coverage(begin = '2012-06-01', end = '2021-10-15',
               sci_names = "Eopsetta jordani",
               geographicDescription = geographicDescription,
               west = -122.44, east = -117.15, 
               north = 37.38, south = 30.00)

christine <- eml$creator(
  individualName = eml$individualName(
    givenName = "Christine", 
    surName = "Stawitz"),
  electronicMailAddress = "christine.stawitz@noaa.gov")
abstract <- set_TextType(text="Here is an abstract about this data source.")

methods <- set_methods("methods.md")
  
physical <- set_physical("landings.csv")
  
  
my_eml <- eml$eml(
packageId = uuid::UUIDgenerate(),  
system = "uuid",
dataset = eml$dataset(
  title = "Landings",
  creator = christine,
  contact = christine,
  pubDate = "2021",
  keywordSet = keywordSet,
  coverage = coverage,
  dataTable = eml$dataTable(
    entityName = "landings.csv",
    entityDescription = "table of fishery landings",
    physical = physical,
    attributeList = attributeList)
))

eml_validate(my_eml)
