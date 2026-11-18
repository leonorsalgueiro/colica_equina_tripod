data_extracao <- Sys.Date()
pasta_raw <- file.path("data-raw", data_extracao)
dir.create(pasta_raw, recursive = TRUE, showWarnings = FALSE)

base <- "https://archive.ics.uci.edu/ml/machine-learning-databases/horse-colic/"
ficheiros <- c("horse-colic.data", "horse-colic.test", "horse-colic.names")
for (f in ficheiros) {
  download.file(paste0(base, f), file.path(pasta_raw, f), mode = "wb")
}

writeLines(
  c(paste("Data de extração:", data_extracao),
    paste("Fonte:", base)),
  file.path(pasta_raw, "LEIAME.txt")
)

Sys.chmod(file.path(pasta_raw, ficheiros), mode = "0444")
cat(readLines(file.path(pasta_raw, "horse-colic.names")), sep = "\n")

colica.data <- read.table(
  file.path(pasta_raw, "horse-colic.data"),
  header = FALSE, na.strings = "?"
)
dim(colica.data)
sum(colica.data$V23 == 2, na.rm = TRUE)
sum(colica.data$V23 == 3, na.rm = TRUE)

colica.test <- read.table(
  file.path(pasta_raw, "horse-colic.test"),
  header = FALSE, na.strings = "?"
)
dim(colica.test)
sum(colica.test$V23 == 2, na.rm = TRUE)
sum(colica.test$V23 == 3, na.rm = TRUE)

variaveis <- c("surgery","age","hospital_number",              
               "rectal_temperature","pulse","respiratory_rate","temp_extremities",
               "peripheral_pulse","mucous_membranes","capillary_refill_time","pain",          
               "peristalsis","abdominal_distension","nasogastric_tube","nasogastric_reflux",
               "nasogastric_reflux_ph","rectal_exam_feces","abdomen","packed_cell_volume",
               "total_protein","abdominocentesis_appearance","abdomcentesis_total_protein",
               "outcome","surgical_lesion","lesion_1","lesion_2","lesion_3","cp_data"                        
)
names(colica.data)<- variaveis
install.packages("dplyr")
library(dplyr)

colica.data.names<- colica.data |> mutate(
  surgery = factor(surgery, levels = 1:2,
                   labels = c("yes", "no")),
  
  age = factor(age, levels = c(1, 9),
               labels = c("adult", "young")),
  
  temp_extremities = factor(temp_extremities, levels = 1:4,
                            labels = c("normal", "warm", "cool", "cold")),
  
  peripheral_pulse = factor(peripheral_pulse, levels = 1:4,
                            labels = c("normal", "increased", "reduced", "absent")),
  
  mucous_membranes = factor(mucous_membranes, levels = 1:6,
                            labels = c("normal_pink", "bright_pink", "pale_pink",
                                       "pale_cyanotic", "bright_red", "dark_cyanotic")),
  
  capillary_refill_time = factor(capillary_refill_time, levels = 1:2,
                                 labels = c("<3s", ">=3s")),
  
  pain = factor(pain, levels = 1:5,
                labels = c("alert_no_pain", "depressed", "intermittent_mild",
                           "intermittent_severe", "continuous_severe")),
  
  peristalsis = factor(peristalsis, levels = 1:4,
                       labels = c("hypermotile", "normal", "hypomotile", "absent")),
  
  abdominal_distension = factor(abdominal_distension, levels = 1:4,
                                labels = c("none", "slight", "moderate", "severe")),
  
  nasogastric_tube = factor(nasogastric_tube, levels = 1:3,
                            labels = c("none", "slight", "significant")),
  
  nasogastric_reflux = factor(nasogastric_reflux, levels = 1:3,
                              labels = c("none", ">1L", "<1L")),
  
  rectal_exam_feces = factor(rectal_exam_feces, levels = 1:4,
                             labels = c("normal", "increased", "decreased", "absent")),
  
  abdomen = factor(abdomen, levels = 1:5,
                   labels = c("normal", "other", "firm_feces_large_intestine",
                              "distended_small_intestine", "distended_large_intestine")),
  
  abdominocentesis_appearance = factor(abdominocentesis_appearance, levels = 1:3,
                                       labels = c("clear", "cloudy", "serosanguinous")),
  
  outcome = factor(outcome, levels = 1:3,
                   labels = c("lived", "died", "euthanized")),
  
  surgical_lesion = factor(surgical_lesion, levels = 1:2,
                           labels = c("yes", "no")),
  
  cp_data = factor(cp_data, levels = 1:2,
                   labels = c("yes", "no"))
)

install.packages("forcats")
library(forcats)
colica.data.outcome <- colica.data.names |>
  mutate(outcome = fct_collapse(outcome, died = c("died", "euthanized")))
table(colica.data.outcome$outcome, useNA = "ifany")


install.packages("purrr")
install.packages("tibble")
library(purrr)
library(tibble)

tabela_geral <- tibble(
  variavel = names(colica.data.outcome),
  tipo     = map_chr(colica.data.outcome, ~ class(.x)[1]),
  n_na     = map_int(colica.data.outcome, ~ sum(is.na(.x))),
  percent_na   = round(100 * n_na / nrow(colica.data.outcome), 1),
  valores_observados = map_chr(colica.data.outcome, ~ {
    if (is.factor(.x)) {
      paste(levels(.x), collapse = " | ")
    } else if (is.numeric(.x)) {
      paste0(min(.x, na.rm = TRUE), " a ", max(.x, na.rm = TRUE))
    } else ""
  })
)
knitr::kable(tabela_geral)
write.csv(tabela_geral, "tabela_geral.csv", row.names = FALSE)
which(is.na(colica.data.outcome$outcome))

pred_dados<- colica.data.outcome |>
  select(hospital_number, age, rectal_temperature, pulse, respiratory_rate, temp_extremities, peripheral_pulse, mucous_membranes, capillary_refill_time, pain, peristalsis, abdominal_distension, nasogastric_tube, nasogastric_reflux, nasogastric_reflux_ph, rectal_exam_feces, abdomen, packed_cell_volume, total_protein, abdominocentesis_appearance, abdomcentesis_total_protein, outcome)

na_rows <- rowSums(is.na(pred_dados))
tabela_na_casos <- data.frame(
  caso = seq_along(na_rows),
  n_na = as.integer(na_rows))

sum(tabela_na_casos$n_na == 0)
which(tabela_na_casos$n_na == 0)
casos_completos <- pred_dados[tabela_na_casos$n_na == 0, ]

casos_repetidos <- pred_dados |>
  group_by(hospital_number) |>
  filter(n() > 1) |>
  ungroup() |>
  arrange(hospital_number)
sum(duplicated(pred_dados))
casos_repetidos$hospital_number[duplicated(casos_repetidos)]

dados_amostra_reduct <- colica.data.outcome |>
  filter(!is.na(outcome)) |>                      
  distinct(hospital_number, .keep_all = TRUE)
nrow(dados_amostra_reduct) 

four_pred<- dados_amostra_reduct|> 
  select(hospital_number, pulse, capillary_refill_time, rectal_temperature, packed_cell_volume, outcome)

preditores <- c("pulse", "capillary_refill_time", "rectal_temperature", "packed_cell_volume")

four_pred |>
  mutate(n_na = rowSums(is.na(across(all_of(preditores))))) |>
  filter(n_na == 4)
four_pred |>
  mutate(n_na = rowSums(is.na(across(all_of(preditores))))) |>
  count(n_na)

four_pred <- four_pred |>
  mutate(n_na = rowSums(is.na(across(all_of(preditores))))) |>
  filter(n_na < 4) |>       
  select(-n_na)

nrow(four_pred)

install.packages("pmsampsize")
library(pmsampsize)

dados_died <- four_pred |>
  mutate(evento = as.integer(outcome %in% "died"))
prev_died <- mean(dados_died$evento)
n_ev_died <- sum(dados_died$evento)

lnL_null_died <- prev_died * log(prev_died) + (1 - prev_died) * log(1 - prev_died)
max_r2cs_died <- 1 - exp(2 * lnL_null_died)

size_died<- pmsampsize(
  type        = "b",            
  csrsquared  = 0.15 * max_r2cs_died,
  parameters  = 4,
  prevalence  = prev_died,
  shrinkage   = 0.9 
)
size_died$results_table

n_obs_died <- nrow(dados_died)

res <- sapply(1:4, function(p) {
  out <- pmsampsize(type = "b",
                    csrsquared = 0.15 * max_r2cs_died,
                    parameters = p,
                    prevalence = prev_died)
  out$sample_size
})

data.frame(parametros = 1:4, N_necessario = res) |>
  mutate(suficiente = N_necessario <= n_obs_died)

n <- nrow(dados_died)
prev_died
margem <- 1.96 * sqrt(prev_died * (1 - prev_died) / n)
margem
binom.test(n_ev_died, n)$conf.int

n <- 278; p <- 4
r2 <- 0.15 * max_r2cs_died
f <- function(S) S - (1 + p / (n * log(1 - r2 / S)))
uniroot(f, c(0.5, 0.999))$root

install.packages("mice")
install.packages("rms")
library(mice)
library(rms)

table(four_pred$outcome, useNA = "ifany")
dados <- four_pred |>
  mutate(evento = as.integer(outcome %in% "died"),
         capillary_refill_time = factor(as.character(capillary_refill_time),levels = c("<3s", ">=3s"))
  )
table(dados$evento, useNA = "ifany")

set.seed(2026)
B <- 500
dados_imp <- dados |> select(-any_of(c("hospital_number", "outcome")))

m <- 35
imp <- mice(dados_imp, m = m, maxit = 20, seed = 2026, printFlag = FALSE)

imp$loggedEvents
plot(imp)
densityplot(imp)

form <- evento ~ pulse + capillary_refill_time + rectal_temperature + packed_cell_volume

ajustes  <- with(imp, glm(evento ~ pulse + capillary_refill_time + rectal_temperature + packed_cell_volume,family = binomial))

pool_fit <- pool(ajustes)
tab <- summary(pool_fit, conf.int = TRUE, exponentiate = TRUE)
tab <- tab[-1, ]

tabela <- data.frame(
  Variável = c("Pulso (bpm)",
               "TRC ≥3s (vs <3s)",
               "T retal (°C)",
               "PCV (%)"),
  OR        = round(tab$estimate, 2),
  `IC 95%`  = paste0(round(tab$conf.low, 2), " – ", round(tab$conf.high, 2)),
  p         = ifelse(tab$p.value < 0.001, "< 0,001",
                     format(round(tab$p.value, 3), nsmall = 3, decimal.mark = ",")),
  check.names = FALSE
)

val_list <- lapply(seq_len(m), \(i) {
  d   <- complete(imp, i)
  fit <- lrm(form, data = d, x = TRUE, y = TRUE)
  validate(fit, B = B)
})

cols <- c("index.orig", "optimism", "index.corrected")
med  <- Reduce(`+`, lapply(val_list, \(v) unclass(v)[, cols])) / m

resultados <- rbind(
  "AUC (C)" = c(0.5 * (med["Dxy", "index.orig"] + 1),
                0.5 * med["Dxy", "optimism"],
                0.5 * (med["Dxy", "index.corrected"] + 1)),
  "Declive de calibração"    = med["Slope", ],
  "Intercepto de calibração" = med["Intercept", ],
  "Brier"                    = med["B", ]
)
colnames(resultados) <- c("aparente", "otimismo", "corrigido")
round(resultados, 3)

pred <- sapply(seq_len(m), \(i) {
  d   <- complete(imp, i)
  fit <- lrm(form, data = d, x = TRUE, y = TRUE)
  predict(fit, type = "fitted")
})
p_med <- rowMeans(pred)

png("calibracao.png", width = 1600, height = 2200, res = 250)

layout(matrix(1:2, ncol = 1), heights = c(2, 1.2))
cal_list <- lapply(seq_len(min(m, 10)), \(i) {
  d   <- complete(imp, i)
  fit <- lrm(form, data = d, x = TRUE, y = TRUE)
  calibrate(fit, B = B)
})
par(mar = c(0.5, 4, 1, 1), mgp = c(2.5, 0.7, 0))
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxt = "n",
     xlab = "", ylab = "Proporção observada")
abline(0, 1, lty = 2)
for (cal in cal_list) {
  lines(cal[, "predy"], cal[, "calibrated.corrected"],
        col = adjustcolor("red", alpha.f = 0.4))
}

par(mar = c(4.5, 4, 0.5, 1), mgp = c(2.5, 0.7, 0))
h_risco <- hist(p_med, breaks = seq(0, 1, 0.05), main = "",
                xlab = "Risco previsto", ylab = "N.º de casos", col = "grey80")

S <- min(1, resultados["Declive de calibração", "corrigido"])
cat("Declive de calibração corrigido (S):", round(S, 3), "\n")

beta <- setNames(pool_fit$pooled$estimate, pool_fit$pooled$term)  
if (S < 0.9) {
  interceptos <- sapply(seq_len(m), \(i) {
    d  <- complete(imp, i)
    X  <- model.matrix(~ pulse + capillary_refill_time + rectal_temperature +
                         packed_cell_volume, data = d)[, -1]
    lp <- as.vector(X %*% beta[-1])
    coef(glm(d$evento ~ offset(S * lp), family = binomial))[1]
  })
  beta_final <- c("(Intercept)" = mean(interceptos), S * beta[-1])
} else {
  beta_final <- beta
}
round(beta_final, 4)
tabela_beta <- data.frame(
  Termo = c("Intercept",
            "Pulso (bpm)",
            "TRC ≥3 s (vs <3 s)",
            "T retal (°C)",
            "PCV (%)"),
  Coeficiente = round(unname(beta_final), 4),
  OR = c(NA, round(exp(unname(beta_final[-1])), 2))
)

cat("Exatidão de 'sobrevive sempre':", round(1 - prev_died, 3), "\n")
cat("Brier do modelo nulo (só prevalência):", round(prev_died * (1 - prev_died), 3), "\n")
cat("Brier do modelo (corrigido):", round(resultados["Brier", "corrigido"], 3), "\n")

dados_cc <- na.omit(dados_imp)
nrow(dados_cc)   

fit_cc <- lrm(form, data = dados_cc, x = TRUE, y = TRUE)
val_cc <- validate(fit_cc, B = B)
val_cc
cat("AUC corrigida (casos completos):",
    round(0.5 * (val_cc["Dxy", "index.corrected"] + 1), 3), "\n")

#formula------------------------------------------------------------------------
risco <- function(pulso, trc, temp, pcv) {
  logit <- 18.3766 + 0.0208 * pulso + 0.5247 * trc -
    0.6202 * temp + 0.0687 * pcv
  1 / (1 + exp(-logit))
}
--------------------------------------------------------------------------------