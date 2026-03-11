################################################################################
#                                                                              #
#   ANALISIS PROPERTI PSIKOMETRIK INTELLIGENZ STRUKTUR TEST (IST)              #
#   ================================================================           #
#   Paradigma: Psikometrika Klasik (CTT) & Psikometrika Modern (IRT)           #
#                                                                              #
#   Subtes IST:                                                                #
#     SE (Satzergaenzung)      - Melengkapi Kalimat      - 20 item dikotomis   #
#     WA (Wortauswahl)         - Melengkapi Kata         - 20 item dikotomis   #
#     AN (Analogien)           - Persamaan Kata          - 20 item dikotomis   #
#     GE (Gemeinsamkeiten)     - Persamaan Sifat         - 16 item politomis   #
#     RA (Rechenaufgaben)      - Berhitung               - 20 item dikotomis   #
#     ZR (Zahlenreihen)        - Deret Angka             - 20 item dikotomis   #
#     FA (Figurenauswahl)      - Memilih Bentuk          - 20 item dikotomis   #
#     WU (Wuerfelaufgaben)     - Latihan Balok           - 20 item dikotomis   #
#     ME (Merkaufgaben)        - Latihan Simbol          - 20 item dikotomis   #
#                                                                              #
#   Output: Laporan PDF komprehensif dengan grafik dan tabel                   #
#                                                                              #
################################################################################

# ==============================================================================
# 0. INSTALASI DAN PEMUATAN PAKET
# ==============================================================================

required_packages <- c(
  "psych",       # CTT: reliabilitas, analisis item, analisis faktor
  "ltm",         # IRT: model Rasch, 2PL, 3PL
  "mirt",        # IRT: model multidimensi, GRM, PCM
  "CTT",         # CTT: analisis item klasik
  "ggplot2",     # Visualisasi
  "gridExtra",   # Pengaturan layout grafik
  "reshape2",    # Transformasi data
  "corrplot",    # Matriks korelasi
  "knitr",       # Tabel
  "kableExtra",  # Format tabel
  "rmarkdown",   # Render PDF
  "dplyr",       # Manipulasi data
  "tidyr",       # Tidy data
  "RColorBrewer",# Palet warna
  "readxl",      # Baca Excel
  "lavaan"       # CFA (Confirmatory Factor Analysis)
)

# Instal paket yang belum ada
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE)
  }
}

# Muat semua paket
invisible(lapply(required_packages, library, character.only = TRUE))

cat("============================================================\n")
cat("  ANALISIS PSIKOMETRIK IST - SEMUA PAKET BERHASIL DIMUAT\n")
cat("============================================================\n\n")

# ==============================================================================
# 1. IMPOR DAN PERSIAPAN DATA
# ==============================================================================

cat(">>> [1] Mengimpor dan mempersiapkan data...\n")

# --- Baca data utama ---
data_raw <- read.csv("data_ist.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

cat("   Jumlah responden:", nrow(data_raw), "\n")
cat("   Jumlah variabel :", ncol(data_raw), "\n")

# --- Definisi subtes ---
subtests <- list(
  SE = paste0("SE", sprintf("%02d", 1:20)),
  WA = paste0("WA", sprintf("%02d", 1:20)),
  AN = paste0("AN", sprintf("%02d", 1:20)),
  GE = paste0("GE", sprintf("%02d", 1:16)),
  RA = paste0("RA", sprintf("%02d", 1:20)),
  ZR = paste0("ZR", sprintf("%02d", 1:20)),
  FA = paste0("FA", sprintf("%02d", 1:20)),
  WU = paste0("WU", sprintf("%02d", 1:20)),
  ME = paste0("ME", sprintf("%02d", 1:20))
)

subtest_names <- c(
  SE = "Satzergaenzung (Melengkapi Kalimat)",
  WA = "Wortauswahl (Melengkapi Kata)",
  AN = "Analogien (Persamaan Kata)",
  GE = "Gemeinsamkeiten (Persamaan Sifat)",
  RA = "Rechenaufgaben (Berhitung)",
  ZR = "Zahlenreihen (Deret Angka)",
  FA = "Figurenauswahl (Memilih Bentuk)",
  WU = "Wuerfelaufgaben (Latihan Balok)",
  ME = "Merkaufgaben (Latihan Simbol)"
)

subtest_types <- c(
  SE = "dikotomis", WA = "dikotomis", AN = "dikotomis",
  GE = "politomis", RA = "dikotomis", ZR = "dikotomis",
  FA = "dikotomis", WU = "dikotomis", ME = "dikotomis"
)

# --- Norma berdasarkan kelompok usia (dari NORMA_IST_NEW.xlsx) ---
norma_subtes <- data.frame(
  subtes = rep(names(subtests), each = 8),
  usia = rep(c("12 - 19 Tahun","20 - 27 Tahun","28 - 35 Tahun","36 - 43 Tahun",
               "44 - 51 Tahun","52 - 59 Tahun","60 - 67 Tahun","68 - 75 Tahun"), 9),
  mean = c(
    8.782,10.78,10.539,10.524,10.368,8.751,10.581,7.000,   # SE
    9.607,11.573,11.596,11.639,11.624,9.641,12.161,8.000,  # WA
    6.496,8.751,8.61,8.715,8.495,6.758,8.742,6.000,        # AN
    8.761,11.516,11.657,11.588,11.332,9.31,10.968,8.000,   # GE
    4.094,6.959,6.544,6.502,6.257,4.739,8.032,4.000,       # RA (ME di IST)
    6.938,10.737,10.332,10.063,9.516,6.681,10.258,6.000,   # ZR (RA di IST)
    8.472,8.558,8.64,8.814,8.759,8.09,8.774,7.000,         # FA (ZR)
    9.369,9.878,9.713,9.582,9.577,8.735,9.161,7.000,       # WU (FA)
    12.536,15.402,15.032,14.828,14.307,12.364,11.935,10.000 # ME (WU)
  ),
  sd = c(
    2.905,3.188,3.017,3.101,3.171,3.187,2.838,2.500,       # SE
    2.966,3.186,3.076,3.039,3.012,3.297,2.806,2.500,       # WA
    2.911,3.523,3.305,3.284,3.373,3.149,3.759,2.500,       # AN
    4.335,4.954,4.605,4.412,4.179,4.631,4.293,3.500,       # GE
    3.362,4.877,4.372,4.056,3.647,3.776,4.004,3.000,       # RA
    4.271,5.651,5.33,4.94,4.781,4.495,5.105,4.000,         # ZR
    2.839,3.316,3.362,3.293,3.209,3.005,2.459,2.500,       # FA
    3.515,4.5,4.383,4.159,3.992,3.527,4.18,3.500,          # WU
    5.23,5.524,5.613,5.596,5.663,5.714,5.813,4.500         # ME
  ),
  stringsAsFactors = FALSE
)

norma_total <- data.frame(
  usia = c("12 - 19 Tahun","20 - 27 Tahun","28 - 35 Tahun","36 - 43 Tahun",
           "44 - 51 Tahun","52 - 59 Tahun","60 - 67 Tahun","68 - 75 Tahun"),
  mean = c(75.055, 94.154, 92.662, 92.256, 90.234, 75.069, 90.613, 65.667),
  sd   = c(21.34, 27.338, 25.192, 23.563, 22.841, 23.352, 23.599, 19.553),
  stringsAsFactors = FALSE
)

# --- Persiapan data item ---
# Pastikan semua item numerik
all_items <- unlist(subtests)
for (col in all_items) {
  if (col %in% names(data_raw)) {
    data_raw[[col]] <- as.numeric(data_raw[[col]])
  }
}

# Hitung skor per subtes
for (s in names(subtests)) {
  data_raw[[paste0("skor_", s)]] <- rowSums(data_raw[, subtests[[s]]], na.rm = TRUE)
}
data_raw$skor_total <- rowSums(data_raw[, paste0("skor_", names(subtests))], na.rm = TRUE)

# --- Deskripsi demografi ---
cat("   Distribusi jenis kelamin:\n")
print(table(data_raw$jk))
cat("\n   Distribusi kelompok usia:\n")
print(table(data_raw$rentus))
cat("\n   Distribusi pendidikan:\n")
print(table(data_raw$pendidikan))

cat("\n>>> Data berhasil dimuat dan disiapkan.\n\n")

# ==============================================================================
# 2. MEMBUAT DIREKTORI OUTPUT
# ==============================================================================

dir.create("output_ist", showWarnings = FALSE)
dir.create("output_ist/grafik", showWarnings = FALSE)

# ==============================================================================
# 3. FUNGSI UTILITAS
# ==============================================================================

# Fungsi klasifikasi Wechsler
klasifikasi_wechsler <- function(iq) {
  cut(iq,
      breaks = c(-Inf, 69, 79, 89, 109, 119, 129, Inf),
      labels = c("Extremely Low", "Borderline", "Low Average",
                 "Average", "High Average", "Superior", "Very Superior"))
}

# Fungsi untuk menghasilkan tabel item analysis CTT
item_analysis_ctt <- function(data_items, subtes_name) {
  ia <- suppressWarnings(psych::alpha(data_items, check.keys = TRUE))

  # Identifikasi item yang di-reverse oleh check.keys
  reversed_items <- names(which(ia$keys == -1))
  if (length(reversed_items) > 0) {
    cat("   [CATATAN] Item berkorelasi negatif (di-reverse untuk analisis):",
        paste(reversed_items, collapse = ", "), "\n")
  }

  item_stats <- data.frame(
    Item = colnames(data_items),
    Mean = round(colMeans(data_items, na.rm = TRUE), 3),
    SD = round(apply(data_items, 2, sd, na.rm = TRUE), 3),
    r_item_total = round(ia$item.stats$r.cor, 3),
    r_drop = round(ia$item.stats$r.drop, 3),
    alpha_if_deleted = round(ia$alpha.drop$raw_alpha, 3),
    reversed = colnames(data_items) %in% reversed_items,
    stringsAsFactors = FALSE
  )
  # Untuk dikotomis, Mean = proporsi benar = tingkat kesulitan
  if (all(data_items %in% c(0, 1), na.rm = TRUE)) {
    item_stats$p_difficulty <- item_stats$Mean
    item_stats$kategori_difficulty <- ifelse(
      item_stats$p_difficulty < 0.20, "Sangat Sulit",
      ifelse(item_stats$p_difficulty < 0.40, "Sulit",
             ifelse(item_stats$p_difficulty < 0.60, "Sedang",
                    ifelse(item_stats$p_difficulty < 0.80, "Mudah", "Sangat Mudah"))))
  }
  item_stats$kategori_daya_beda <- ifelse(
    item_stats$r_item_total < 0.10, "Sangat Rendah",
    ifelse(item_stats$r_item_total < 0.20, "Rendah",
           ifelse(item_stats$r_item_total < 0.30, "Cukup",
                  ifelse(item_stats$r_item_total < 0.40, "Baik", "Sangat Baik"))))
  return(item_stats)
}

# ==============================================================================
# 4. ANALISIS PSIKOMETRIKA KLASIK (CTT) PER SUBTES
# ==============================================================================

cat(">>> [2] Memulai Analisis Psikometrika Klasik (CTT)...\n\n")

ctt_results <- list()
reliability_summary <- data.frame(
  Subtes = character(), Nama = character(), Jumlah_Item = integer(),
  Alpha = numeric(), Alpha_Std = numeric(), Mean_r_IT = numeric(),
  Mean_Skor = numeric(), SD_Skor = numeric(), SEM = numeric(),
  stringsAsFactors = FALSE
)

for (s in names(subtests)) {
  cat("   Menganalisis subtes:", s, "-", subtest_names[s], "\n")
  
  items <- data_raw[, subtests[[s]]]
  
  # Reliabilitas (Cronbach's Alpha)
  alpha_res <- suppressWarnings(psych::alpha(items, check.keys = TRUE))

  # Analisis item
  ia <- item_analysis_ctt(items, s)

  # Split-half reliability
  sh <- suppressWarnings(psych::splitHalf(items, raw = TRUE))
  
  ctt_results[[s]] <- list(
    alpha = alpha_res,
    item_analysis = ia,
    split_half = sh,
    descriptives = psych::describe(items)
  )
  
  reliability_summary <- rbind(reliability_summary, data.frame(
    Subtes = s,
    Nama = subtest_names[s],
    Jumlah_Item = length(subtests[[s]]),
    Alpha = round(alpha_res$total$raw_alpha, 3),
    Alpha_Std = round(alpha_res$total$std.alpha, 3),
    Mean_r_IT = round(mean(ia$r_item_total, na.rm = TRUE), 3),
    Mean_Skor = round(mean(rowSums(items, na.rm = TRUE)), 2),
    SD_Skor = round(sd(rowSums(items, na.rm = TRUE)), 2),
    SEM = round(sd(rowSums(items, na.rm = TRUE)) * sqrt(1 - alpha_res$total$raw_alpha), 2),
    stringsAsFactors = FALSE
  ))
}

cat("\n--- Ringkasan Reliabilitas Semua Subtes ---\n")
print(reliability_summary[, c("Subtes","Jumlah_Item","Alpha","Alpha_Std","Mean_r_IT","SEM")])

# ==============================================================================
# 5. ANALISIS PSIKOMETRIKA MODERN (IRT) PER SUBTES
# ==============================================================================

cat("\n>>> [3] Memulai Analisis Item Response Theory (IRT)...\n\n")

irt_results <- list()

for (s in names(subtests)) {
  cat("   IRT untuk subtes:", s, "\n")
  
  items <- data_raw[, subtests[[s]]]
  
  tryCatch({
    if (subtest_types[s] == "dikotomis") {
      # --- Model Rasch (1PL) ---
      rasch_mod <- mirt(items, model = 1, itemtype = "Rasch", verbose = FALSE)
      
      # --- Model 2PL ---
      twopl_mod <- mirt(items, model = 1, itemtype = "2PL", verbose = FALSE)
      
      # Perbandingan model
      anova_res <- tryCatch(anova(rasch_mod, twopl_mod), error = function(e) NULL)
      
      # Ekstrak parameter item
      rasch_params <- coef(rasch_mod, IRTpars = TRUE, simplify = TRUE)$items
      twopl_params <- coef(twopl_mod, IRTpars = TRUE, simplify = TRUE)$items
      
      # Item fit
      rasch_fit <- mirt::itemfit(rasch_mod, fit_stats = "S_X2", na.rm = TRUE)
      twopl_fit <- mirt::itemfit(twopl_mod, fit_stats = "S_X2", na.rm = TRUE)
      
      # Person fit
      person_fit_rasch <- mirt::personfit(rasch_mod)
      
      # Model fit indices
      rasch_m2 <- tryCatch(M2(rasch_mod, type = "C2"), error = function(e) NULL)
      twopl_m2 <- tryCatch(M2(twopl_mod, type = "C2"), error = function(e) NULL)
      
      irt_results[[s]] <- list(
        type = "dikotomis",
        rasch = rasch_mod,
        twopl = twopl_mod,
        anova = anova_res,
        rasch_params = rasch_params,
        twopl_params = twopl_params,
        rasch_fit = rasch_fit,
        twopl_fit = twopl_fit,
        person_fit = person_fit_rasch,
        rasch_m2 = rasch_m2,
        twopl_m2 = twopl_m2
      )
      
    } else {
      # --- Model GRM (Graded Response Model) untuk GE (politomis) ---
      grm_mod <- mirt(items, model = 1, itemtype = "graded", verbose = FALSE)
      
      # --- Model PCM (Partial Credit Model) ---
      pcm_mod <- mirt(items, model = 1, itemtype = "gpcm", verbose = FALSE)
      
      grm_params <- coef(grm_mod, IRTpars = TRUE, simplify = TRUE)$items
      
      grm_fit <- mirt::itemfit(grm_mod, fit_stats = "S_X2", na.rm = TRUE)
      
      irt_results[[s]] <- list(
        type = "politomis",
        grm = grm_mod,
        pcm = pcm_mod,
        grm_params = grm_params,
        grm_fit = grm_fit
      )
    }
  }, error = function(e) {
    cat("     [PERINGATAN] Gagal estimasi IRT untuk", s, ":", conditionMessage(e), "\n")
    irt_results[[s]] <<- list(type = subtest_types[s], error = conditionMessage(e))
  })
}

cat("\n>>> IRT selesai untuk semua subtes.\n\n")

# ==============================================================================
# 6. ANALISIS STRUKTUR FAKTOR (CFA)
# ==============================================================================

cat(">>> [4] Analisis Struktur Faktor...\n")

# Hitung skor per subtes untuk CFA
skor_subtests <- data_raw[, paste0("skor_", names(subtests))]
names(skor_subtests) <- names(subtests)

# Korelasi antar subtes
cor_matrix <- cor(skor_subtests, use = "pairwise.complete.obs")

# Exploratory Factor Analysis
efa_result <- tryCatch({
  psych::fa(cor_matrix, nfactors = 3, rotate = "varimax", n.obs = nrow(data_raw), fm = "ml")
}, error = function(e) {
  psych::fa(cor_matrix, nfactors = 3, rotate = "varimax", n.obs = nrow(data_raw), fm = "pa")
})

# KMO & Bartlett
kmo_res <- psych::KMO(cor_matrix)

# CFA: Model 3 Faktor IST (Verbal, Numerik, Figural)
cfa_model <- '
  Verbal  =~ SE + WA + AN + GE
  Numerik =~ RA + ZR + ME
  Figural =~ FA + WU
'

cfa_fit <- tryCatch({
  lavaan::cfa(cfa_model, data = skor_subtests, std.lv = TRUE)
}, error = function(e) {
  cat("   [INFO] CFA mungkin kurang stabil dengan N kecil\n")
  NULL
})

cat("   KMO:", round(kmo_res$MSA, 3), "\n")

# ==============================================================================
# 7. VISUALISASI
# ==============================================================================

cat("\n>>> [5] Membuat visualisasi...\n")

# --- 7.1 Grafik distribusi skor per subtes ---
pdf("output_ist/grafik/01_distribusi_skor.pdf", width = 14, height = 10)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
for (s in names(subtests)) {
  skor <- data_raw[[paste0("skor_", s)]]
  hist(skor, main = paste("Distribusi Skor", s),
       xlab = "Skor", ylab = "Frekuensi",
       col = "steelblue", border = "white",
       breaks = seq(min(skor, na.rm = TRUE) - 0.5,
                    max(skor, na.rm = TRUE) + 0.5, by = 1))
  abline(v = mean(skor, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
  legend("topright", legend = paste("M =", round(mean(skor, na.rm = TRUE), 1)),
         col = "red", lty = 2, lwd = 2, cex = 0.8, bty = "n")
}
dev.off()

# --- 7.2 Matriks korelasi antar subtes ---
pdf("output_ist/grafik/02_korelasi_subtes.pdf", width = 8, height = 8)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.8,
         tl.col = "black", tl.srt = 45,
         title = "Matriks Korelasi Antar Subtes IST",
         mar = c(0, 0, 2, 0),
         col = colorRampPalette(c("#2166AC","white","#B2182B"))(100))
dev.off()

# --- 7.3 Tingkat Kesulitan & Daya Beda per subtes ---
pdf("output_ist/grafik/03_item_difficulty_discrimination.pdf", width = 14, height = 18)
par(mfrow = c(5, 2), mar = c(4, 5, 3, 1))
for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis
  
  # Tingkat kesulitan (mean)
  cols <- ifelse(ia$Mean < 0.20, "#B2182B",
          ifelse(ia$Mean < 0.40, "#EF8A62",
          ifelse(ia$Mean < 0.60, "#F7F7F7",
          ifelse(ia$Mean < 0.80, "#67A9CF", "#2166AC"))))
  
  barplot(ia$Mean, names.arg = ia$Item, main = paste(s, "- Tingkat Kesulitan (Mean)"),
          ylab = "Proporsi Benar / Mean", col = "steelblue",
          las = 2, cex.names = 0.7, ylim = c(0, max(ia$Mean, na.rm = TRUE) * 1.2))
  abline(h = 0.5, col = "red", lty = 2)
  
  # Daya beda (r item-total)
  cols_disc <- ifelse(ia$r_item_total < 0.20, "red",
               ifelse(ia$r_item_total < 0.30, "orange", "forestgreen"))
  barplot(ia$r_item_total, names.arg = ia$Item,
          main = paste(s, "- Daya Beda (r item-total terkoreksi)"),
          ylab = "Korelasi Item-Total", col = cols_disc,
          las = 2, cex.names = 0.7, ylim = c(min(0, min(ia$r_item_total, na.rm = TRUE) - 0.05), 1))
  abline(h = c(0.20, 0.30), col = c("red", "orange"), lty = 2)
  legend("topright",
         legend = c("r < 0.20 (Rendah)", "0.20-0.30 (Cukup)", "> 0.30 (Baik)"),
         fill = c("red", "orange", "forestgreen"), cex = 0.7, bty = "n")
}
dev.off()

# --- 7.4 ICC (Item Characteristic Curves) IRT ---
cat("   Membuat grafik IRT...\n")

for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    pdf(paste0("output_ist/grafik/04_ICC_", s, ".pdf"), width = 12, height = 8)
    
    if (irt_results[[s]]$type == "dikotomis") {
      # ICC dari model 2PL
      plot(irt_results[[s]]$twopl, type = "trace",
           main = paste("Item Characteristic Curves -", s, "(2PL)"),
           par.settings = list(superpose.line = list(lwd = 2)))
    } else {
      # ICC dari model GRM
      plot(irt_results[[s]]$grm, type = "trace",
           main = paste("Category Response Curves -", s, "(GRM)"),
           par.settings = list(superpose.line = list(lwd = 2)))
    }
    dev.off()
  }
}

# --- 7.5 Item Information Curves (IIC) ---
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    pdf(paste0("output_ist/grafik/05_IIF_", s, ".pdf"), width = 12, height = 8)
    
    mod <- if (irt_results[[s]]$type == "dikotomis") irt_results[[s]]$twopl else irt_results[[s]]$grm
    plot(mod, type = "infotrace",
         main = paste("Item Information Functions -", s))
    dev.off()
  }
}

# --- 7.6 Test Information Function per subtes ---
pdf("output_ist/grafik/06_TIF_semua_subtes.pdf", width = 14, height = 10)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    mod <- if (irt_results[[s]]$type == "dikotomis") irt_results[[s]]$twopl else irt_results[[s]]$grm
    theta <- seq(-4, 4, by = 0.01)
    info <- testinfo(mod, Theta = matrix(theta, ncol = 1))
    se <- 1 / sqrt(info)
    
    plot(theta, info, type = "l", col = "steelblue", lwd = 2,
         main = paste("Test Information Function -", s),
         xlab = expression(theta), ylab = "Information / SE",
         ylim = c(0, max(info) * 1.2))
    lines(theta, se, col = "red", lwd = 2, lty = 2)
    legend("topright", legend = c("Information", "SE"),
           col = c("steelblue", "red"), lty = c(1, 2), lwd = 2, cex = 0.8, bty = "n")
  }
}
dev.off()

# --- 7.7 Wright Map (Person-Item Map) untuk model Rasch ---
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error) &&
      irt_results[[s]]$type == "dikotomis") {
    
    pdf(paste0("output_ist/grafik/07_WrightMap_", s, ".pdf"), width = 10, height = 8)
    
    mod <- irt_results[[s]]$rasch
    theta_est <- fscores(mod, method = "EAP")[, 1]
    diff_params <- irt_results[[s]]$rasch_params[, "b"]
    
    par(mfrow = c(1, 2), mar = c(4, 2, 3, 1))
    
    # Person distribution
    hist(theta_est, breaks = 20, main = paste("Wright Map -", s),
         xlab = expression(theta), ylab = "", col = "lightblue",
         border = "white", horizontal = FALSE, xlim = c(-4, 4))
    
    # Item locations
    plot(diff_params, 1:length(diff_params), pch = 18, cex = 1.5, col = "darkred",
         main = "Item Difficulty", xlab = expression(delta),
         ylab = "", yaxt = "n", xlim = c(-4, 4))
    axis(2, at = 1:length(diff_params), labels = names(diff_params), las = 1, cex.axis = 0.7)
    abline(v = mean(theta_est), col = "blue", lty = 2, lwd = 2)
    
    dev.off()
  }
}

# --- 7.8 Reliabilitas ringkasan barplot ---
pdf("output_ist/grafik/08_reliabilitas_subtes.pdf", width = 10, height = 6)
par(mar = c(5, 4, 3, 1))
bp <- barplot(reliability_summary$Alpha, names.arg = reliability_summary$Subtes,
              main = "Koefisien Reliabilitas (Cronbach's Alpha) per Subtes",
              ylab = "Alpha", col = ifelse(reliability_summary$Alpha >= 0.7, "forestgreen",
                                           ifelse(reliability_summary$Alpha >= 0.5, "orange", "red")),
              ylim = c(0, 1), las = 1)
text(bp, reliability_summary$Alpha + 0.03, labels = reliability_summary$Alpha, cex = 0.9)
abline(h = c(0.7, 0.5), col = c("forestgreen", "orange"), lty = 2)
legend("bottomright", legend = c("Alpha >= 0.70 (Baik)", "0.50-0.70 (Cukup)", "< 0.50 (Rendah)"),
       fill = c("forestgreen", "orange", "red"), cex = 0.8, bty = "n")
dev.off()

# --- 7.9 Boxplot skor subtes ---
pdf("output_ist/grafik/09_boxplot_skor.pdf", width = 10, height = 6)
skor_df <- data_raw[, paste0("skor_", names(subtests))]
names(skor_df) <- names(subtests)
boxplot(skor_df, main = "Distribusi Skor per Subtes IST",
        ylab = "Skor", col = brewer.pal(9, "Set3"),
        las = 1, notch = FALSE)
dev.off()

# --- 7.10 Scree Plot ---
pdf("output_ist/grafik/10_scree_plot.pdf", width = 8, height = 6)
ev <- eigen(cor_matrix)
plot(1:length(ev$values), ev$values, type = "b", pch = 19, col = "steelblue",
     main = "Scree Plot - Eigenvalue Subtes IST",
     xlab = "Komponen", ylab = "Eigenvalue", lwd = 2)
abline(h = 1, col = "red", lty = 2)
text(1:length(ev$values), ev$values + 0.15,
     labels = round(ev$values, 2), cex = 0.8)
dev.off()

# --- 7.11 Profil skor deskriptif ---
pdf("output_ist/grafik/11_profil_skor_deskriptif.pdf", width = 10, height = 6)
mean_scores <- sapply(names(subtests), function(s) mean(data_raw[[paste0("skor_", s)]], na.rm = TRUE))
max_scores <- sapply(names(subtests), function(s) length(subtests[[s]]) * 
                       ifelse(s == "GE", 2, 1))
pct_scores <- mean_scores / max_scores * 100
barplot(pct_scores, names.arg = names(subtests),
        main = "Rata-rata Persentase Skor per Subtes IST",
        ylab = "Persentase (%)", col = "steelblue",
        ylim = c(0, 100), las = 1)
abline(h = 50, col = "red", lty = 2)
text(seq(0.7, by = 1.2, length.out = 9), pct_scores + 3,
     labels = paste0(round(pct_scores, 1), "%"), cex = 0.8)
dev.off()

cat("   Semua visualisasi selesai dibuat.\n")

# ==============================================================================
# 8. MEMBUAT LAPORAN PDF
# ==============================================================================

cat("\n>>> [6] Membuat laporan PDF...\n")

# --- Buat file R Markdown ---
rmd_content <- '---
title: "LAPORAN ANALISIS PROPERTI PSIKOMETRIK"
subtitle: "Intelligenz Struktur Test (IST)"
author: "Analisis Psikometri Otomatis"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  pdf_document:
    toc: true
    toc_depth: 3
    number_sections: true
    fig_caption: true
    latex_engine: xelatex
    keep_tex: false
header-includes:
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{float}
  - \\usepackage{colortbl}
  - \\usepackage{geometry}
  - \\geometry{margin=1in}
  - \\usepackage{fancyhdr}
  - \\pagestyle{fancy}
  - \\fancyhead[L]{Laporan Psikometrik IST}
  - \\fancyhead[R]{\\thepage}
  - \\fancyfoot[C]{}
fontsize: 11pt
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.pos = "H", fig.align = "center",
                      out.width = "95%", dpi = 150)
library(psych)
library(mirt)
library(ltm)
library(CTT)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(corrplot)
library(knitr)
library(kableExtra)
library(dplyr)
library(RColorBrewer)
```

\\newpage

# PENDAHULUAN

## Tentang Tes IST

Intelligenz Struktur Test (IST) merupakan tes inteligensi yang dikembangkan oleh Rudolf Amthauer untuk mengukur struktur kecerdasan individu. IST terdiri dari 9 subtes yang mengukur aspek-aspek inteligensi yang berbeda:

| No | Subtes | Nama Lengkap | Jumlah Item | Tipe Skor |
|----|--------|-------------|-------------|-----------|
| 1 | SE | Satzergaenzung (Melengkapi Kalimat) | 20 | Dikotomis (0/1) |
| 2 | WA | Wortauswahl (Melengkapi Kata) | 20 | Dikotomis (0/1) |
| 3 | AN | Analogien (Persamaan Kata) | 20 | Dikotomis (0/1) |
| 4 | GE | Gemeinsamkeiten (Persamaan Sifat) | 16 | Politomis (0/1/2) |
| 5 | RA | Rechenaufgaben (Berhitung) | 20 | Dikotomis (0/1) |
| 6 | ZR | Zahlenreihen (Deret Angka) | 20 | Dikotomis (0/1) |
| 7 | FA | Figurenauswahl (Memilih Bentuk) | 20 | Dikotomis (0/1) |
| 8 | WU | Wuerfelaufgaben (Latihan Balok) | 20 | Dikotomis (0/1) |
| 9 | ME | Merkaufgaben (Latihan Simbol) | 20 | Dikotomis (0/1) |

## Cakupan Analisis

Laporan ini mencakup:

1. **Psikometrika Klasik (CTT)**: Reliabilitas, analisis item (tingkat kesulitan, daya beda), Standard Error of Measurement.
2. **Psikometrika Modern (IRT)**: Model Rasch, 2PL, GRM; Item fit, Test Information Function, ICC.
3. **Analisis Struktur Faktor**: EFA, CFA, validitas konstruk.
4. **Visualisasi**: Grafik distribusi, ICC, TIF, Wright Map, korelasi, dll.

## Deskripsi Sampel

```{r deskripsi_sampel}
cat("Jumlah responden:", nrow(data_raw), "\\n")
cat("Jumlah item total:", length(unlist(subtests)), "\\n\\n")

# Tabel demografi
demo_jk <- as.data.frame(table(data_raw$jk))
names(demo_jk) <- c("Jenis Kelamin", "Frekuensi")
demo_jk$Persentase <- round(demo_jk$Frekuensi / sum(demo_jk$Frekuensi) * 100, 1)

demo_usia <- as.data.frame(table(data_raw$rentus))
names(demo_usia) <- c("Kelompok Usia", "Frekuensi")
demo_usia$Persentase <- round(demo_usia$Frekuensi / sum(demo_usia$Frekuensi) * 100, 1)

demo_pend <- as.data.frame(table(data_raw$pendidikan))
names(demo_pend) <- c("Pendidikan", "Frekuensi")
demo_pend$Persentase <- round(demo_pend$Frekuensi / sum(demo_pend$Frekuensi) * 100, 1)

kable(demo_jk, caption = "Distribusi Jenis Kelamin", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)

kable(demo_usia, caption = "Distribusi Kelompok Usia", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)

kable(demo_pend, caption = "Distribusi Pendidikan", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)
```

\\newpage

# ANALISIS PSIKOMETRIKA KLASIK (CTT)

## Ringkasan Reliabilitas

```{r reliabilitas_tabel}
rel_tbl <- reliability_summary[, c("Subtes","Nama","Jumlah_Item","Alpha","Alpha_Std",
                                    "Mean_r_IT","Mean_Skor","SD_Skor","SEM")]
names(rel_tbl) <- c("Subtes","Nama","n Item","Alpha","Alpha Std","Mean r.it",
                     "Mean Skor","SD Skor","SEM")
kable(rel_tbl, caption = "Reliabilitas dan Statistik Deskriptif per Subtes",
      booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position","scale_down"), font_size = 9)
```

```{r plot_reliabilitas, fig.cap="Koefisien Reliabilitas per Subtes", fig.height=4, fig.width=8}
ggplot(reliability_summary, aes(x = Subtes, y = Alpha, fill = Alpha >= 0.7)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Alpha), vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = c(0.5, 0.7), linetype = "dashed",
             color = c("orange","forestgreen")) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "salmon"),
                    labels = c("< 0.70", ">= 0.70"), name = "Kriteria") +
  ylim(0, 1) +
  labs(title = "Koefisien Cronbach\'s Alpha per Subtes IST",
       x = "Subtes", y = "Alpha") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

**Interpretasi:**

- Koefisien Alpha >= 0.70 menunjukkan reliabilitas yang memadai untuk pengukuran kelompok.
- Alpha >= 0.80 dianggap baik, dan >= 0.90 dianggap sangat baik.
- SEM (Standard Error of Measurement) menunjukkan tingkat presisi pengukuran.

\\newpage

## Analisis Item per Subtes

```{r item_analysis_loop, results="asis"}
for (s in names(subtests)) {
  cat("\\n### Subtes", s, "-", subtest_names[s], "\\n\\n")
  
  ia <- ctt_results[[s]]$item_analysis
  
  display_cols <- c("Item","Mean","SD","r_item_total","r_drop","alpha_if_deleted","kategori_daya_beda")
  if ("p_difficulty" %in% names(ia)) {
    display_cols <- c(display_cols[1:3], "p_difficulty", "kategori_difficulty", display_cols[4:7])
  }
  
  ia_display <- ia[, display_cols[display_cols %in% names(ia)]]
  
  print(kable(ia_display, caption = paste("Analisis Item CTT -", s),
              booktabs = TRUE, digits = 3, row.names = FALSE) %>%
          kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))
  
  cat("\\n**Alpha keseluruhan:", round(ctt_results[[s]]$alpha$total$raw_alpha, 3), "**\\n\\n")
  
  # Identifikasi item bermasalah
  problem_items <- ia$Item[ia$r_item_total < 0.20]
  if (length(problem_items) > 0) {
    cat("*Item dengan daya beda rendah (r < 0.20):*", paste(problem_items, collapse = ", "), "\\n\\n")
  }
  if ("reversed" %in% names(ia)) {
    rev_items <- ia$Item[ia$reversed == TRUE]
    if (length(rev_items) > 0) {
      cat("*Item berkorelasi negatif (di-reverse):*", paste(rev_items, collapse = ", "), "\\n\\n")
    }
  }
  
  cat("\\n\\newpage\\n")
}
```

## Distribusi Skor per Subtes

```{r distribusi_skor, fig.cap="Distribusi Skor per Subtes IST", fig.height=9, fig.width=12}
skor_long <- reshape2::melt(data_raw[, paste0("skor_", names(subtests))],
                             variable.name = "Subtes", value.name = "Skor")
skor_long$Subtes <- gsub("skor_", "", skor_long$Subtes)

ggplot(skor_long, aes(x = Skor)) +
  geom_histogram(aes(y = after_stat(density)), fill = "steelblue",
                 color = "white", bins = 15) +
  geom_density(color = "red", linewidth = 0.8) +
  facet_wrap(~ Subtes, scales = "free", ncol = 3) +
  labs(title = "Distribusi Skor per Subtes IST",
       x = "Skor", y = "Densitas") +
  theme_minimal()
```

```{r boxplot_skor, fig.cap="Boxplot Skor per Subtes", fig.height=5, fig.width=9}
ggplot(skor_long, aes(x = Subtes, y = Skor, fill = Subtes)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Boxplot Skor Subtes IST", x = "Subtes", y = "Skor") +
  theme_minimal() +
  theme(legend.position = "none")
```

\\newpage

## Statistik Deskriptif Skor

```{r descriptive_scores}
desc_tbl <- data.frame(
  Subtes = names(subtests),
  N = sapply(names(subtests), function(s) sum(!is.na(data_raw[[paste0("skor_", s)]]))),
  Min = sapply(names(subtests), function(s) min(data_raw[[paste0("skor_", s)]], na.rm = TRUE)),
  Max = sapply(names(subtests), function(s) max(data_raw[[paste0("skor_", s)]], na.rm = TRUE)),
  Mean = sapply(names(subtests), function(s) round(mean(data_raw[[paste0("skor_", s)]], na.rm = TRUE), 2)),
  SD = sapply(names(subtests), function(s) round(sd(data_raw[[paste0("skor_", s)]], na.rm = TRUE), 2)),
  Median = sapply(names(subtests), function(s) median(data_raw[[paste0("skor_", s)]], na.rm = TRUE)),
  Skewness = sapply(names(subtests), function(s) round(psych::describe(data_raw[[paste0("skor_", s)]])$skew, 3)),
  Kurtosis = sapply(names(subtests), function(s) round(psych::describe(data_raw[[paste0("skor_", s)]])$kurtosis, 3))
)

kable(desc_tbl, caption = "Statistik Deskriptif Skor per Subtes",
      booktabs = TRUE, row.names = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 9)
```

\\newpage

# ANALISIS PSIKOMETRIKA MODERN (IRT)

## Ringkasan Model IRT

Untuk subtes dikotomis (SE, WA, AN, RA, ZR, FA, WU, ME), digunakan model **Rasch (1PL)** dan **2PL**.
Untuk subtes politomis (GE), digunakan model **Graded Response Model (GRM)**.

```{r irt_model_comparison, results="asis"}
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    cat("\\n### Subtes", s, "-", subtest_names[s], "\\n\\n")
    
    if (irt_results[[s]]$type == "dikotomis") {
      # Parameter 2PL
      params <- as.data.frame(round(irt_results[[s]]$twopl_params, 3))
      params$Item <- rownames(params)
      params <- params[, c("Item", "a", "b", "g", "u")]
      names(params) <- c("Item", "Diskriminasi (a)", "Kesulitan (b)", "Guessing (g)", "Upper (u)")
      
      print(kable(params, caption = paste("Parameter IRT 2PL -", s),
                  booktabs = TRUE, row.names = FALSE, digits = 3) %>%
              kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))
      
      # Item fit
      fit_df <- irt_results[[s]]$twopl_fit
      fit_display <- data.frame(
        Item = fit_df$item,
        S_X2 = round(fit_df$S_X2, 3),
        df = fit_df$df.S_X2,
        p.value = round(fit_df$p.S_X2, 4)
      )
      fit_display$Status <- ifelse(fit_display$p.value < 0.05, "Misfit *", "Fit")
      
      print(kable(fit_display, caption = paste("Item Fit (S-X2) -", s, "(2PL)"),
                  booktabs = TRUE, row.names = FALSE) %>%
              kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))
      
      # Perbandingan Rasch vs 2PL
      if (!is.null(irt_results[[s]]$anova)) {
        av <- irt_results[[s]]$anova
        cat("\\n**Perbandingan Model Rasch vs 2PL:**\\n\\n")
        cat("- AIC Rasch:", round(av$AIC[1], 2), "| AIC 2PL:", round(av$AIC[2], 2), "\\n")
        cat("- BIC Rasch:", round(av$BIC[1], 2), "| BIC 2PL:", round(av$BIC[2], 2), "\\n")
        if (!is.na(av$p[2])) {
          cat("- Likelihood Ratio Test: p =", round(av$p[2], 4), "\\n")
          if (av$p[2] < 0.05) {
            cat("- **Model 2PL lebih baik dari Rasch (p < 0.05)**\\n")
          } else {
            cat("- **Model Rasch cukup memadai (p >= 0.05)**\\n")
          }
        }
      }
      
      # Model fit M2
      if (!is.null(irt_results[[s]]$twopl_m2)) {
        m2 <- irt_results[[s]]$twopl_m2
        cat("\\n**Model Fit 2PL:**\\n")
        cat("- M2 =", round(m2$M2, 2), ", df =", m2$df, ", p =", round(m2$p, 4), "\\n")
        cat("- RMSEA =", round(m2$RMSEA, 4), "\\n")
        cat("- SRMSR =", round(m2$SRMSR, 4), "\\n")
        cat("- TLI =", round(m2$TLI, 4), "\\n")
        cat("- CFI =", round(m2$CFI, 4), "\\n")
      }
      
    } else {
      # GRM parameter
      params <- as.data.frame(round(irt_results[[s]]$grm_params, 3))
      params$Item <- rownames(params)
      params <- params[, c("Item", setdiff(names(params), "Item"))]
      
      print(kable(params, caption = paste("Parameter IRT GRM -", s),
                  booktabs = TRUE, row.names = FALSE, digits = 3) %>%
              kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))
      
      # GRM fit
      fit_df <- irt_results[[s]]$grm_fit
      fit_display <- data.frame(
        Item = fit_df$item,
        S_X2 = round(fit_df$S_X2, 3),
        df = fit_df$df.S_X2,
        p.value = round(fit_df$p.S_X2, 4)
      )
      fit_display$Status <- ifelse(fit_display$p.value < 0.05, "Misfit *", "Fit")
      
      print(kable(fit_display, caption = paste("Item Fit (S-X2) -", s, "(GRM)"),
                  booktabs = TRUE, row.names = FALSE) %>%
              kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))
    }
    
    cat("\\n\\newpage\\n")
  }
}
```

## Grafik Item Characteristic Curves (ICC)

```{r icc_plots, results="asis", fig.height=6, fig.width=10}
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    cat("\\n### ICC -", s, "\\n\\n")
    
    if (irt_results[[s]]$type == "dikotomis") {
      print(plot(irt_results[[s]]$twopl, type = "trace",
                 main = paste("Item Characteristic Curves -", s, "(2PL)")))
    } else {
      print(plot(irt_results[[s]]$grm, type = "trace",
                 main = paste("Category Response Curves -", s, "(GRM)")))
    }
    cat("\\n\\newpage\\n")
  }
}
```

## Test Information Functions (TIF)

```{r tif_plots, fig.cap="Test Information Function per Subtes", fig.height=9, fig.width=12}
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    mod <- if (irt_results[[s]]$type == "dikotomis") irt_results[[s]]$twopl else irt_results[[s]]$grm
    theta <- seq(-4, 4, by = 0.01)
    info <- testinfo(mod, Theta = matrix(theta, ncol = 1))
    se_val <- 1 / sqrt(info)
    
    plot(theta, info, type = "l", col = "steelblue", lwd = 2,
         main = paste("TIF -", s),
         xlab = expression(theta), ylab = "Information / SE",
         ylim = c(0, max(info) * 1.2))
    lines(theta, se_val, col = "red", lwd = 2, lty = 2)
    legend("topright", legend = c("Info", "SE"),
           col = c("steelblue", "red"), lty = c(1, 2), lwd = 2, cex = 0.7, bty = "n")
  }
}
```

## Wright Map (Person-Item Map) - Subtes Dikotomis

```{r wright_maps, results="asis", fig.height=5, fig.width=9}
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error) &&
      irt_results[[s]]$type == "dikotomis") {
    
    cat("\\n### Wright Map -", s, "\\n\\n")
    
    mod <- irt_results[[s]]$rasch
    theta_est <- fscores(mod, method = "EAP")[, 1]
    diff_params <- irt_results[[s]]$rasch_params[, "b"]
    
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    
    hist(theta_est, breaks = 15, main = paste("Distribusi Theta -", s),
         xlab = expression(theta), col = "lightblue", border = "white", xlim = c(-4, 4))
    abline(v = mean(theta_est), col = "red", lty = 2, lwd = 2)
    
    plot(diff_params, 1:length(diff_params), pch = 18, cex = 1.5, col = "darkred",
         main = paste("Lokasi Kesulitan Item -", s),
         xlab = expression(delta), ylab = "", yaxt = "n", xlim = c(-4, 4))
    axis(2, at = 1:length(diff_params), labels = names(diff_params), las = 1, cex.axis = 0.6)
    abline(v = mean(theta_est), col = "blue", lty = 2, lwd = 2)
    
    cat("\\n")
  }
}
```

\\newpage

# ANALISIS STRUKTUR FAKTOR

## Korelasi Antar Subtes

```{r korelasi_plot, fig.cap="Matriks Korelasi Antar Subtes IST", fig.height=6, fig.width=7}
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.8,
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#2166AC","white","#B2182B"))(100))
```

```{r korelasi_tabel}
cor_df <- as.data.frame(round(cor_matrix, 3))
kable(cor_df, caption = "Matriks Korelasi Antar Subtes IST",
      booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position","scale_down"), font_size = 9)
```

## Scree Plot dan Eigenvalue

```{r scree, fig.cap="Scree Plot Eigenvalue", fig.height=4, fig.width=7}
ev <- eigen(cor_matrix)
scree_df <- data.frame(Komponen = 1:length(ev$values), Eigenvalue = ev$values)

ggplot(scree_df, aes(x = Komponen, y = Eigenvalue)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(Eigenvalue, 2)), vjust = -0.8, size = 3) +
  scale_x_continuous(breaks = 1:9) +
  labs(title = "Scree Plot - Eigenvalue Subtes IST",
       x = "Komponen", y = "Eigenvalue") +
  theme_minimal()
```

**Keterangan:** Jumlah faktor yang direkomendasikan berdasarkan kriteria Kaiser (eigenvalue > 1): **`r sum(ev$values > 1)`** faktor.

## KMO dan Bartlett Test

```{r kmo}
cat("KMO Measure of Sampling Adequacy:", round(kmo_res$MSA, 3), "\\n")
cat("\\nKMO per variabel:\\n")
kmo_item <- data.frame(Subtes = names(kmo_res$MSAi), KMO = round(kmo_res$MSAi, 3))
kable(kmo_item, caption = "KMO per Subtes", booktabs = TRUE, row.names = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)
```

## Exploratory Factor Analysis (EFA)

```{r efa_results}
# Loading matrix
loadings_df <- as.data.frame(unclass(efa_result$loadings))
loadings_df <- round(loadings_df, 3)
loadings_df$Communality <- round(efa_result$communality, 3)

kable(loadings_df, caption = "Factor Loadings (EFA - 3 Faktor, Varimax Rotation)",
      booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 9)

# Variance explained
cat("\\n**Proporsi varians yang dijelaskan:**\\n\\n")
var_exp <- efa_result$Vaccounted
if (!is.null(var_exp)) {
  var_df <- as.data.frame(round(var_exp, 3))
  print(kable(var_df, booktabs = TRUE, digits = 3) %>%
          kable_styling(latex_options = c("hold_position"), font_size = 9))
}
```

## Confirmatory Factor Analysis (CFA)

Model CFA 3-faktor IST:

- **Verbal**: SE, WA, AN, GE
- **Numerik**: RA, ZR, ME
- **Figural**: FA, WU

```{r cfa_results}
if (!is.null(cfa_fit)) {
  fit_indices <- fitMeasures(cfa_fit, c("chisq","df","pvalue","cfi","tli",
                                         "rmsea","rmsea.ci.lower","rmsea.ci.upper",
                                         "srmr","aic","bic"))
  fit_df <- data.frame(
    Indeks = c("Chi-square","df","p-value","CFI","TLI","RMSEA",
               "RMSEA CI Lower","RMSEA CI Upper","SRMR","AIC","BIC"),
    Nilai = round(as.numeric(fit_indices), 4),
    Kriteria = c("-","-","> 0.05","> 0.90","> 0.90","< 0.08",
                 "-","-","< 0.08","Lebih kecil lebih baik","Lebih kecil lebih baik")
  )
  
  kable(fit_df, caption = "Fit Indices CFA Model 3-Faktor IST",
        booktabs = TRUE, row.names = FALSE) %>%
    kable_styling(latex_options = c("hold_position"), full_width = FALSE)
  
  cat("\\n**Standardized Factor Loadings:**\\n\\n")
  std_loads <- standardizedSolution(cfa_fit)
  std_loads_display <- std_loads[std_loads$op == "=~",
                                  c("lhs","rhs","est.std","se","pvalue")]
  names(std_loads_display) <- c("Faktor","Subtes","Loading Std","SE","p-value")
  std_loads_display[, 3:5] <- round(std_loads_display[, 3:5], 3)
  
  print(kable(std_loads_display, caption = "Standardized Factor Loadings CFA",
              booktabs = TRUE, row.names = FALSE) %>%
          kable_styling(latex_options = c("hold_position"), full_width = FALSE))
  
  # Factor correlations
  cat("\\n**Korelasi Antar Faktor:**\\n\\n")
  fac_cor <- std_loads[std_loads$op == "~~" & std_loads$lhs != std_loads$rhs &
                         std_loads$lhs %in% c("Verbal","Numerik","Figural"),
                       c("lhs","rhs","est.std")]
  names(fac_cor) <- c("Faktor 1","Faktor 2","Korelasi")
  if (nrow(fac_cor) > 0) {
    fac_cor$Korelasi <- round(fac_cor$Korelasi, 3)
    print(kable(fac_cor, booktabs = TRUE, row.names = FALSE) %>%
            kable_styling(latex_options = c("hold_position"), full_width = FALSE))
  }
  
} else {
  cat("CFA tidak dapat diestimasi (kemungkinan karena ukuran sampel kecil).\\n")
}
```

\\newpage

# RINGKASAN DAN REKOMENDASI

## Ringkasan Reliabilitas

```{r ringkasan_reliabilitas}
rel_summary <- reliability_summary[, c("Subtes","Alpha","SEM")]
rel_summary$Interpretasi <- ifelse(rel_summary$Alpha >= 0.9, "Sangat Baik",
                             ifelse(rel_summary$Alpha >= 0.8, "Baik",
                             ifelse(rel_summary$Alpha >= 0.7, "Memadai",
                             ifelse(rel_summary$Alpha >= 0.6, "Cukup",
                             ifelse(rel_summary$Alpha >= 0.5, "Kurang", "Rendah")))))

kable(rel_summary, caption = "Ringkasan Reliabilitas dan Interpretasi",
      booktabs = TRUE, row.names = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)
```

## Ringkasan Item Bermasalah

```{r item_bermasalah}
problem_df <- data.frame(Subtes = character(), Item = character(),
                         Masalah = character(), Nilai = numeric(),
                         stringsAsFactors = FALSE)

for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis
  
  # Daya beda rendah
  low_disc <- ia[ia$r_item_total < 0.20, ]
  if (nrow(low_disc) > 0) {
    problem_df <- rbind(problem_df, data.frame(
      Subtes = s, Item = low_disc$Item,
      Masalah = "Daya beda < 0.20",
      Nilai = low_disc$r_item_total, stringsAsFactors = FALSE))
  }
  
  # Item berkorelasi negatif (di-reverse)
  if ("reversed" %in% names(ia)) {
    rev_items <- ia[ia$reversed == TRUE, ]
    if (nrow(rev_items) > 0) {
      problem_df <- rbind(problem_df, data.frame(
        Subtes = s, Item = rev_items$Item,
        Masalah = "Korelasi negatif (reversed)",
        Nilai = rev_items$r_item_total, stringsAsFactors = FALSE))
    }
  }

  # Item terlalu mudah / sulit (untuk dikotomis)
  if ("p_difficulty" %in% names(ia)) {
    too_easy <- ia[ia$p_difficulty > 0.95, ]
    too_hard <- ia[ia$p_difficulty < 0.05, ]
    
    if (nrow(too_easy) > 0) {
      problem_df <- rbind(problem_df, data.frame(
        Subtes = s, Item = too_easy$Item,
        Masalah = "Terlalu mudah (p > 0.95)",
        Nilai = too_easy$p_difficulty, stringsAsFactors = FALSE))
    }
    if (nrow(too_hard) > 0) {
      problem_df <- rbind(problem_df, data.frame(
        Subtes = s, Item = too_hard$Item,
        Masalah = "Terlalu sulit (p < 0.05)",
        Nilai = too_hard$p_difficulty, stringsAsFactors = FALSE))
    }
  }
}

if (nrow(problem_df) > 0) {
  problem_df$Nilai <- round(problem_df$Nilai, 3)
  kable(problem_df, caption = "Daftar Item Bermasalah",
        booktabs = TRUE, row.names = FALSE) %>%
    kable_styling(latex_options = c("hold_position"), full_width = FALSE)
} else {
  cat("Tidak ditemukan item bermasalah berdasarkan kriteria yang ditetapkan.\\n")
}
```

## Rekomendasi

Berdasarkan hasil analisis psikometrik di atas, berikut beberapa rekomendasi:

1. **Item dengan daya beda rendah** (r < 0.20) perlu ditinjau ulang, baik dari segi konten maupun kunci jawaban.
2. **Item yang terlalu mudah atau terlalu sulit** (p < 0.05 atau p > 0.95) perlu direvisi agar distribusi tingkat kesulitan lebih merata.
3. **Item yang menunjukkan misfit** pada analisis IRT perlu dievaluasi apakah mengukur konstruk yang sama.
4. **Reliabilitas** subtes dengan Alpha < 0.70 perlu ditingkatkan, misalnya dengan menambahkan item berkualitas atau merevisi item yang ada.
5. **Struktur faktor** IST sebaiknya dikonfirmasi dengan sampel yang lebih besar untuk memastikan model 3-faktor (Verbal, Numerik, Figural) sesuai.

---

*Laporan ini dihasilkan secara otomatis menggunakan R.*
*Tanggal: `r format(Sys.Date(), "%d %B %Y")`*
'

# Tulis file Rmd
writeLines(rmd_content, "output_ist/laporan_psikometrik_IST.Rmd")

cat("   File Rmd berhasil dibuat.\n")

# --- Render ke PDF ---
tryCatch({
  rmarkdown::render("output_ist/laporan_psikometrik_IST.Rmd",
                    output_file = "Laporan_Psikometrik_IST.pdf",
                    output_dir = "output_ist",
                    envir = globalenv(),
                    quiet = TRUE)
  cat(">>> LAPORAN PDF BERHASIL DIBUAT: output_ist/Laporan_Psikometrik_IST.pdf\n")
}, error = function(e) {
  cat(">>> [INFO] Render PDF memerlukan LaTeX (tinytex). Jalankan:\n")
  cat("   install.packages('tinytex'); tinytex::install_tinytex()\n")
  cat("   Kemudian re-run bagian render.\n")
  cat("   Error:", conditionMessage(e), "\n")
})

# ==============================================================================
# 9. SIMPAN HASIL ANALISIS KE FILE
# ==============================================================================

cat("\n>>> [7] Menyimpan hasil ke file...\n")

# Simpan reliability summary
write.csv(reliability_summary, "output_ist/reliabilitas_summary.csv", row.names = FALSE)

# Simpan item analysis per subtes
for (s in names(subtests)) {
  write.csv(ctt_results[[s]]$item_analysis,
            paste0("output_ist/item_analysis_", s, ".csv"), row.names = FALSE)
}

# Simpan IRT parameters
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    if (irt_results[[s]]$type == "dikotomis") {
      write.csv(as.data.frame(irt_results[[s]]$twopl_params),
                paste0("output_ist/irt_params_2PL_", s, ".csv"))
      write.csv(as.data.frame(irt_results[[s]]$rasch_params),
                paste0("output_ist/irt_params_Rasch_", s, ".csv"))
    } else {
      write.csv(as.data.frame(irt_results[[s]]$grm_params),
                paste0("output_ist/irt_params_GRM_", s, ".csv"))
    }
  }
}

# Simpan korelasi
write.csv(round(cor_matrix, 3), "output_ist/korelasi_subtes.csv")

cat("\n")
cat("============================================================\n")
cat("  ANALISIS SELESAI!                                        \n")
cat("============================================================\n")
cat("  File output tersedia di folder: output_ist/              \n")
cat("                                                            \n")
cat("  Isi folder:                                               \n")
cat("  - Laporan_Psikometrik_IST.pdf    (Laporan lengkap)       \n")
cat("  - laporan_psikometrik_IST.Rmd    (File source Rmd)       \n")
cat("  - reliabilitas_summary.csv                                \n")
cat("  - item_analysis_[SUBTES].csv     (per subtes)            \n")
cat("  - irt_params_[MODEL]_[SUBTES].csv                        \n")
cat("  - korelasi_subtes.csv                                     \n")
cat("  - grafik/                         (Semua grafik PDF)      \n")
cat("============================================================\n")
