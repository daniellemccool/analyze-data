library(survival)
library(forestplot)

res <- vector("list", length(b2b_cbc[!is.na(AgeDiag1) & study != "xx", unique(study)]))
names(res) <- b2b_cbc[!is.na(AgeDiag1) & study != "xx", unique(study)]
for (i in b2b_cbc[!is.na(AgeDiag1)  & study != "xx", unique(study)][seq_along(b2b_cbc[!is.na(AgeDiag1)  & study != "xx", unique(study)])]) {
  fit <- tryCatch(coxph(Surv(TFU, CBC) ~ AgeDiag1, data = b2b_cbc[study == i], x = TRUE), error=function(e) e, warning=function(w) w)  
  res[[i]] <- fit
}
res[["total"]] <-   fit <- try(coxph(Surv(TFU, CBC) ~ AgeDiag1, data = b2b_cbc))
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
           title = "Age at first breast cancer",
           is.summary = c(rep(FALSE, length(res) - 1), TRUE))
b2b_cbc[study == "kConFab/AOCS", .N, .(AgeDiag1, CBC)]
b2b_cbc[study == "kConFab/AOCS" & is.na(AgeDiag1), AgeDiag1 := 0]
BSUCH
b2b_cbc[!is.na(AgeDiag1), .N, .(study, AgeDiag1, CBC)][order(study, CBC, AgeDiag1)]

b2b_cbc[study == "SKKDKFZS" & is.na(AgeDiag1), AgeDiag1 := 0]
b2b_cbc[study == "SASBAC" & is.na(AgeDiag1), AgeDiag1 := 0]
b2b_cbc[study == "ORIGO" & is.na(AgeDiag1), AgeDiag1 := 0]
b2b_cbc[study == "BIGGS" & is.na(AgeDiag1), AgeDiag1 := 0]


library(Hmisc)
N <- b2b_cbc[!is.na(AgeDiag1) , .N, study]
CBC_CT <- b2b_cbc[!is.na(AgeDiag1) & CBC == 1 & AgeDiag1 == 0 , .N, .(study, AgeDiag1)][, .(study, NFH0 = N)]
CBC_NCT <- b2b_cbc[!is.na(AgeDiag1)  & CBC == 1 & AgeDiag1 == 1 , .N, .(study, AgeDiag1)][, .(study, NFH1 = N)]
b2b_cbc[!is.na(AgeDiag1)   & CBC == 1 , .N, .(AgeDiag1)]
mean
setkey(N, study)
setkey(CBC_CT, study)
setkey(CBC_NCT, study)
tabletext <- as.matrix(N[CBC_CT[CBC_NCT]])
tabletext <- rbind(tabletext, c("Total", "73854", "1050", "2588"))
tabletext <- rbind(c("Study", "N", "CBC(CT)", "CBC(NCT)"), tabletext)
b2b_cbc$AgeDiag1
plot.new()
forestplot(tabletext, 
           mean = c(NA, mean), lower = c(NA, lower), upper = c(NA, upper),
           hrzl_lines = gpar(col="#444444"),
           new_page = TRUE,
           xlog = TRUE,
           clip = c(0.25, 4.0),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           title = "Adjuvant Chemotherapy",
           is.summary = c(TRUE, rep(FALSE, length(res) - 1), TRUE))
length(mean)
tabletext
