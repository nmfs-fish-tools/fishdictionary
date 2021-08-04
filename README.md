# data_dictionary
A flexible data dictionary for fisheries


Explanation of column headers:
- Description​: Write a brief definition, stated in the singular, that could stand alone from other element definitions.
- Name​: What is the name used in your file? This is our proposed replacement for existing names.
- Purpose: the role this value plays in the model. This can be type scalar (an input value ex: number of years), data (input data e.g. catch), derived quantity (a model output that is a function of parameters e.g. spawning stock biomass), parameter (an estimated or fixed model parameter e.g. steepness), or index (an index used to structure the model or reference data e.g. year, age, fleet).
- Data type​: This is the programmatic type e.g. how C++ would classify a value. For example, indicate varchar, integer, date, etc.
- Unit type: This is the unit. Could be the same as data type (e.g. integer) or different (e.g. data type is double, unit type is centimeters)
- Length​: For example, the maximum length for Excel is 255, so indicate 255 or less.
- Index1, Index2, Index3: how this value may be expanded (e.g. to provide sex- or fleet-specific values)
- MAS: how this is named in MAS
- SS: how this is named in SS
- r4ss: how this is named in r4ss
- AMAK input: name in AMAK input file
- AMAK output: name in AMAK output (ForR.rep)
- Acceptable values (not yet filled out): List all acceptable values, separated by pipes ( | ). This may be a field name or a range of values.
- Required (not yet filled out): Enter y/n to indicate whether this field is required.
- Accepts null value (not yet filled out): This required to run calculations on your data. Indicate y/n if null value is allowable.
(A note on null values: Null is the absence of a recorded value for a field. A null value differs from a value of zero in that zero may represent the measure of an attribute, while a null value indicates that no measurement has been taken.)
