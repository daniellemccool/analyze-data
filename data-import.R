library(data.table)
library(lubridate)
b2b <- readRDS("~/Dropbox/Data/BCAC Case Data/b2b.with.updated.bcac.7122015.RDS")


b2b[, CBC := 0]
b2b[Bilateral == 1, CBC := 1]

b2b_cbc <- b2b[study %in% b2b[CBC == 1, .N, study][N > 5, study]]
b2b_cbc[Bilateral %in% c(0, 1), .N, .(study, Bilateral)][, .N, study]
No
b2b_cbc[study == "ICICLE", .N, Bilateral]
b2b_cbc[study == "ICICLE" & is.na(Bilateral), Bilateral := '0']

b2b_cbc <- b2b_cbc[!Bilateral == 2]

b2b_cbc[DateDiag1 < (DateEnter - years(5)), .N, study]

b2b_cbc[study == "erasmus", .N, Chemo_adjuvant]
b2b_cbc[study == "erasmus", Chemo_adjuvant := as.numeric(Chemo_adjuvant) - 1]

b2b_cbc[study == "hebon", .N, Chemo_adjuvant]
b2b_cbc[study == "hebon" & is.na(Chemo_adjuvant), Chemo_adjuvant := 0]

b2b_cbc[, .N, .((DateDiag1 < (DateEnter - months(6))), CBC)][, .(D1.before.DE.window = DateDiag1, CBC, perc = (N / sum(N)))]

b2b_cbc[, .N, DateEnter][order(N)]
b2b_cbc[, DateDiag2 - DateEnter]
b2b_cbc[DateDiag2 != "7000-07-06" & !is.na(DateEnter), .(DateDiag2 - DateEnter)][V1 < 0, .N]
b2b_cbc[!is.na(Chemo_adjuvant) & !is.na(CBC), .N, .(Chemo_adjuvant, study, CBC)]

b2b_cbc[study == "erasmus", Chemo_adjuvant := Chemo_adjuvant - 1]
b2b_cbc[study == "erasmus", .N, Chemo_adjuvant]
b2b_cbc[study == "hebon", .N, Chemo_adjuvant]
b2b_cbc[study == "hebon" & is.na(Chemo_adjuvant), Chemo_adjuvant := 0]

b2b_cbc[CBC == 1, TFU := BilateralTime]
b2b_cbc[CBC != 1, .N, YearsToStatus][order(YearsToStatus)]

b2b_cbc[CBC == 1, .N, TFU][order(N)]

b2b_cbc[is.na(YearsToStatus), table(is.na(DateDiag1), is.na(DateLastFU), deparse.level = 2)]


b2b_cbc <- b2b_cbc[!(YearsToStatus < 0) | is.na(YearsToStatus)]

b2b_cbc[is.na(YearsToStatus) & CBC == 0, YearsToStatus := as.period(DateLastFU - DateDiag1) / years(1)]
b2b_cbc <- b2b_cbc[!(is.na(YearsToStatus) & is.na(TFU))]

b2b_cbc[BilateralTime == 0, BilateralTime]
b2b_cbc <- b2b_cbc[BilateralTime != 0]

b2b_cbc[, .N, TFU]
b2b_cbc[CBC == 0, TFU := YearsToStatus]
b2b_cbc <- b2b_cbc[study %in% b2b_cbc[CBC == 1, .N, study][N > 5, study]]


tab <- b2b_cbc[!is.na(DateEnter) & CBC == 1,  table(DateEnter < DateDiag2)/length(DateEnter < DateDiag2), .(study)]

with(b2b_cbc2, prop.table(DateEnter < DateDiag2)/length(DateEnter < DateDiag2))
tab <- b2b_cbc[,  table(DateEnter > DateDiag1)/length(DateEnter > DateDiag1 ), .(study, CBC)]
b2b_cbc[,  table(DateEnter > DateDiag1), .(study, CBC)]
b2b_cbc[,  (DateEnter > DateDiag1)/length(DateEnter > DateDiag1), .(study, CBC)]

b2b_cbc2[, DEafterD1 := 0]
b2b_cbc2[DateEnter > DateDiag1, DEafterD1 := 1]

b2b_cbc2[, DEafterD2 := 0]
b2b_cbc2[CBC == 1 & DateEnter > DateDiag2, DEafterD2 := 1]


b2b_cbc2[, DEwindowD1 := 0]
b2b_cbc2[(DateEnter - months(6)) > DateDiag1, DEwindowD1 := 1]

b2b_cbc2[, .N, .(study, CBC, DEafterD1)]
b2b_cbc2[, DEafterD1 := factor(DEafterD1)]
b2b_cbc2$DEafterD1
tab <- b2b_cbc2[!is.na(DateEnter), .("DEBefore" = table(DEafterD1, deparse.level = 1)[1]/length(DEafterD1), "DEAfter"= table(DEafterD1, deparse.level = 2)[2]/length(DEafterD1)), .(study, CBC)]
b2b_cbc2[!is.na(DateEnter), .("DEBefore" = table(DEafterD1, deparse.level = 1)[1]/length(DEafterD1), "DEAfter"= table(DEafterD1, deparse.level = 2)[2]/length(DEafterD1)), .(study, CBC)]

b2b_cbc2[CBC == 1 & DEafterD2 == 1, .N, study]
with(b2b_cbc2[study == "BIGGS" & CBC == 1], (table(DEafterD1, deparse.level = 2)))

532/(295+532)
b2b_cbc2[CBC == 0, .N, study]
b2b_cbc2[, study := factor(study)]
?table
tabulate(as.factor(c('1', '0', '1'), nbins = 2))
b2b_cbc2[CBC == 1, .N, DEafterD2]
b2b_cbc2[CBC == 1, .N, .(study, DEafterD2)][DEafterD2 == 1]
b2b_cbc2[CBC == 1, .N, .(study)]
b2b_cbc2[, sum(DEwindowD1), study]


b2b_cbc2[]

d <- head(b2b_cbc2$DateDiag1)
e <- head(b2b_cbc2$DateEnter)

b2b_cbc2[]

d
length(1:1)

mode(b2b_cbc2$study)

tab <- tab[is.na(V1), V1 := 0]
tab2 <- 
  with(tab, dcast(tab, study ~ CBC, value.var = c(`DE Before Diag1`, `DE Before Diag2`)))

names(tab2) <- c("Study", "No CBC", "CBC")

tab2[is.na(`No CBC`), `No CBC` := 0]
tab2[is.na(CBC), CBC := 0]
b2b_cbc[, prop.table((DateEnter > DateDiag1))]
tab2

b2b_cbc[, (DateEnter < DateDiag2)/length(DateEnter < DateDiag2), study]
tab
?data.table
table(b2b_cbc$DateEnter < b2b_cbc$DateDiag2)[1]
b2b_cbc[study == "BCFR-PA", .N, .(DateEnter > DateDiag1, CBC)]
/length(b2b_cbc$DateEnter < b2b_cbc$DateDiag1)
?table
with(b2b_cbc,
     ftable(((DateEnter > DateDiag1))/length(DateEnter > DateDiag1)), study, CBC))

dcast(b2b_cbc[, mean(AgeDiag1, na.rm = TRUE), .(study, CBC)], study ~ CBC, value.var = "V1")
b2b_cbc[study == "BOCS", ]
b2b[study == "POSH", .N, .(DateEnter > DateDiag2)]
b2b_cbc[study == "BCFR-PA", DateEnter > DateDiag1, .(DateEnter, DateDiag1)]

saveRDS(b2b_cbc, "~/RProjects/data-verzameling/import-data/combined-data/b2b_cbc.1282015.RDS")
saveRDS(b2b, "~/RProjects/data-verzameling/import-data/combined-data/b2b.1282015.RDS")
