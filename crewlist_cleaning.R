library(data.table)

crewlist <- fread("~/Documents/Projects/whalerDB/whalemenDB_cleaned.tsv", na.strings = "", check.names = TRUE)
grep("[a-zA-Z]", crewlist$VesselNumber)
crewlist[28260:28265,]
crewlist[grep("[a-zA-Z]", crewlist$VesselNumber), "VesselNumber"] <- "100"
crewlist$VesselNumber <- as.numeric(crewlist$VesselNumber)

grep("[^0-9]", crewlist$Age, value = TRUE)
crewlist$Age <- as.numeric(gsub("[^0-9]", "", crewlist$Age))

statepool <- c(state.abb, state.name)
crewlist$USstate[crewlist$Residence.2 %in% statepool] <- crewlist$Residence.2[crewlist$Residence.2 %in% statepool]
crewlist$USstate[crewlist$Residence.3 %in% statepool] <- crewlist$Residence.3[crewlist$Residence.3 %in% statepool]
crewlist$USstate[crewlist$Residence_orig %in% c("New Bedford", "Boston", "Acushnet", "Dartmouth", "Fairhaven", "Nantucket", "Westport", "Martha's Vineyard", "Gay Head", "North Dartmouth", "South Boston", "Cambridgeport")] <- "MA"
crewlist$USstate[crewlist$Residence_orig %in% c("Providence", "Tiverton", "Newport", "Bristol")] <- "RI"
crewlist$USstate[crewlist$Residence_orig %in% c("Chicago")] <- "IL"
crewlist$USstate[crewlist$Residence_orig %in% c("New York City", "Troy", "Albany", "Long Island", "Brooklyn")] <- "NY"
crewlist$USstate[crewlist$Residence_orig %in% c("Philadelphia", "Penn", "Philadephia")] <- "PA"
crewlist$USstate[crewlist$Residence_orig %in% c("Baltimore")] <- "MD"
crewlist$USstate[crewlist$Residence_orig %in% c("Portland")] <- "ME"
crewlist$USstate[crewlist$Residence_orig %in% c("New London")] <- "CT"
crewlist$USstate[crewlist$Residence.1 %in% statepool] <- crewlist$Residence.1[crewlist$Residence.1 %in% statepool]

MAtowns <- read.table("~/Documents/Projects/whalerDB/MAtowns", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
MAtowns <- MAtowns$Town
crewlist$USstate[crewlist$Residence_orig %in% MAtowns & is.na(crewlist$USstate)] <- "MA"

# map state abbrevs to IDs
NENYtowns <- readr::read_tsv("~/Documents/Projects/whalerDB/NENYtowns.txt")
NENYtowns$NAME <- gsub("\\s+(CDP|city|village|borough)\\s*", "", NENYtowns$NAME)
NENYtowns <- NENYtowns[, c("NAME", "USPS")]
dups <- NENYtowns$NAME[duplicated(NENYtowns$NAME)]
uniqueNENYtowns <- as.data.frame(NENYtowns[!(NENYtowns$NAME %in% dups),])
colnames(uniqueNENYtowns) <- c("Residence_orig", "tmpstate")

nations <- read.table("~/Documents/Projects/whalerDB/nationlist", sep = "\t", header = TRUE)
nations <- as.character(nations$Nation)
nations <- nations[-which(nations == "United States")]
nations <- c(nations, "England", "Scotland", "Foreigner", "Wales", "Portuguese")
crewlist$USstate[crewlist$Residence_orig %in% nations & is.na(crewlist$USstate)] <- "II"
crewlist$USstate[crewlist$Residence_orig %in% c("Azores", "Brava", "Fayal", "Fogo", "Flores")] <- "II"

crewlist$Nation <- rep(NA, nrow(crewlist))
crewlist$Nation[!is.na(crewlist$USstate) & crewlist$USstate == "II"] <- crewlist$Residence_orig[!is.na(crewlist$USstate) & crewlist$USstate == "II"]

tmp <- merge(crewlist, uniqueNENYtowns, by = "Residence_orig", all.x = TRUE)
tmp$USstate[is.na(tmp$USstate)] <- tmp$tmpstate[is.na(tmp$USstate)]
crewlist <- tmp
crewlist$tmpstate <- NULL
crewlist$tmpstate <- state.name[match(crewlist$USstate, state.abb)]
crewlist$USstate[!is.na(crewlist$tmpstate)] <- crewlist$tmpstate[!is.na(crewlist$tmpstate)]

#x <- crewlist[is.na(crewlist$Residence.2) & is.na(crewlist$Residence.3) & is.na(crewlist$USstate) & !is.na(crewlist$Residence_orig),]
#sort(table(x$Residence_orig))

### Prep to Write Out
colnames(crewlist) <- gsub("Residence_orig", "Residence", colnames(crewlist))
crewlist <- crewlist[,c(2:7, 1, 8:12, 16:22)]

write.table(crewlist,"~/Documents/Projects/whalerDB/whalemenDB_cleaned_again.tsv", sep = "\t")
