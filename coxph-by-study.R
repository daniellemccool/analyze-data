library(survival)
library(forestplot)

res <- vector("list", length(b2b_cbc[!is.na(Chemo_adjuvant) & study %nin% c("HERPACC", "POSH", "NBCS"), unique(study)]))
names(res) <- b2b_cbc[!is.na(Chemo_adjuvant)& study %nin% c("HERPACC", "POSH", "NBCS"), unique(study)]
for (i in b2b_cbc[!is.na(Chemo_adjuvant)& study %nin% c("HERPACC", "POSH", "NBCS"), unique(study)][seq_along(b2b_cbc[!is.na(Chemo_adjuvant)& study %nin% c("HERPACC", "POSH", "NBCS"), unique(study)])]) {
  fit <- tryCatch(coxph(Surv(TFU, CBC) ~ Chemo_adjuvant, data = b2b_cbc[study == i], x = TRUE), error=function(e) e, warning=function(w) w)  
  res[[i]] <- fit
}
res[["total"]] <-   fit <- try(coxph(Surv(TFU, CBC) ~ Chemo_adjuvant, data = b2b_cbc))
res <- res[!sapply(res, function(X) any(grepl('Warning', X)))]
mean <- exp(sapply(res, `[[`, 1))
lower <- sapply(res, FUN = function(X){exp(X$coefficients - sqrt(X$var)*1.96)})
upper <- sapply(res, FUN = function(X){exp(X$coefficients + sqrt(X$var)*1.96)})
plot.new()
forestplot(mean = c(mean), lower = c(lower), upper = c(upper),
           labeltext = names(res),
           tabletext = cbind(c(names(res)), c(unname(mean))),
           new_page = TRUE,
           xlog = TRUE,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           title = "Family History 1st Degree Relatives",
           is.summary = c(rep(FALSE, length(res) - 1), TRUE))
b2b_cbc[study == "POSH", .N, .(Chemo_adjuvant, CBC)]
BSUCH
b2b_cbc[!is.na(Chemo_adjuvant), .N, .(study, Chemo_adjuvant, CBC)][order(study, CBC, Chemo_adjuvant)]

b2b_cbc[study == "SKKDKFZS" & is.na(Chemo_adjuvant), Chemo_adjuvant := 0]
b2b_cbc[study == "SASBAC" & is.na(Chemo_adjuvant), Chemo_adjuvant := 0]
b2b_cbc[study == "ORIGO" & is.na(Chemo_adjuvant), Chemo_adjuvant := 0]
b2b_cbc[study == "BIGGS" & is.na(Chemo_adjuvant), Chemo_adjuvant := 0]


library(Hmisc)
N <- b2b_cbc[!is.na(Chemo_adjuvant) & study %nin% c("HERPACC", "POSH", "NBCS"), .N, study]
CBC_nfh <- b2b_cbc[!is.na(Chemo_adjuvant) & CBC == 1 & Chemo_adjuvant == 0 & study %nin% c("HERPACC", "POSH", "NBCS"), .N, .(study, Chemo_adjuvant)][, .(study, NFH0 = N)]
CBC_fh <- b2b_cbc[!is.na(Chemo_adjuvant) & CBC == 1 & Chemo_adjuvant == 1 & study %nin% c("HERPACC", "POSH", "NBCS"), .N, .(study, Chemo_adjuvant)][, .(study, NFH1 = N)]
b2b_cbc[!is.na(Chemo_adjuvant)   & study %nin% c("HERPACC", "POSH", "NBCS"), .N, ]

setkey(N, study)
setkey(CBC_nfh, study)
setkey(CBC_fh, study)
tabletext <- as.matrix(N[CBC_fh[CBC_nfh]])
tabletext <- rbind(tabletext, c("Total", "51552", "1121", "2512"))
tabletext <- rbind(c("Study", "N", "CBC(FH)", "CBC(NFH)"), tabletext)
b2b_cbc$Chemo_adjuvant
plot.new()
forestplot(tabletext, 
           mean = c(NA, mean), lower = c(NA, lower), upper = c(NA, upper),
           hrzl_lines = gpar(col="#444444"),
           new_page = TRUE,
           xlog = TRUE,
           clip = c(0.25, 4.0),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           title = "Family History 1st Degree Relatives",
           is.summary = c(TRUE, rep(FALSE, length(res) - 1), TRUE))
