library(dplyr)

#loon lake names
unique(loonBlood$Lake)

#fish lake names
unique(fishData$WATERWAY)

#fixing loon lake names (removing parenthesis that explain a location within a lake)
loonBlood[, LakeFixed := Lake]
loonBlood[grep("Rabbit", Lake), LakeFixed := "Rabbit"]
loonBlood[grep("Mantrap", Lake), LakeFixed := "Mantrap"]
loonBlood[grep("Big Birch", Lake), LakeFixed := "Big Birch"]
loonBlood[grep("Arrowhead", Lake), LakeFixed := "Arrowhead"]
loonBlood[grep("Monongalia", Lake), LakeFixed := "Monongalia"]

#fixing fish lake names (removing parenthesis that explain a location within a lake)
fishData[grep]

#fixed loon lake names
unique(loonBlood$LakeFixed)

#add lowercase lake names to each table
loonBlood[, LakeFixedLower := sapply(LakeFixed, tolower)]
fishData[, WaterwayLower := sapply(WATERWAY, tolower)]

#loon lakes not found in fish data
unique(loonBlood[!loonBlood$LakeFixedLower %in% fishData$WaterwayLower,"LakeFixedLower"])

#finding "missing" lakes in fish data
unique(fishData$WaterwayLower[grep("black", fishData$WaterwayLower)]) # "blackduck"    "blackwater"   "blackhawk"    "black island" "black duck"
unique(fishData$WaterwayLower[grep("south|tamarac", fishData$WaterwayLower)]) # "tamarack" "north tamarack" "south twin" "south center" "south lindstrom" "south fowl" "south" "upper south long" "lower south long" "south mcdougal" "south turtle" "south lida"
unique(fishData$WaterwayLower[grep("north|tamarac", fishData$WaterwayLower)]) # "tamarack" "north tamarack" "north center" "north fowl" "northern light" "north" "north long" "north star" "north mcdougal" "north twin" "north turtle" "north lida" "north star steel" "north brown's"   
unique(fishData$WaterwayLower[grep("mckeown", fishData$WaterwayLower)]) # none (is this misspelled?)
unique(fishData$WaterwayLower[grep("point", fishData$WaterwayLower)]) # "many point" "disappointment" "sand point"
unique(fishData$WaterwayLower[grep("eagles|nest", fishData$WaterwayLower)]) # "nest" "eagles nest #4" "eagles nest #1" "eagles nest #3"
unique(fishData$WaterwayLower[grep("east|vermilion", fishData$WaterwayLower)]) # "east moore" "east twin" "east toqua" "east rush" "east pike" "east bearskin" "east pope" "east fox" "rabbit (east portion)" "east crooked" "east solomon" "big stone nwr east pool" "east" "east chub" "east graham" "east leaf" "east battle" "east lost" "east spirit" "east vadnais" "vermilion" "little vermilion" "beast" "east lake sylvia"
unique(fishData$WaterwayLower[grep("arr|ow|head", fishData$WaterwayLower)]) # none
