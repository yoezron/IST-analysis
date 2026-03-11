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

# Fungsi untuk menghasilkan tabel item analysis CTT (komprehensif)
item_analysis_ctt <- function(data_items, subtes_name) {
  ia <- suppressWarnings(psych::alpha(data_items, check.keys = TRUE))

  # Identifikasi item yang di-reverse oleh check.keys
  reversed_items <- names(which(ia$keys == -1))
  if (length(reversed_items) > 0) {
    cat("   [CATATAN] Item berkorelasi negatif (di-reverse untuk analisis):",
        paste(reversed_items, collapse = ", "), "\n")
  }

  # Skor total per responden
  total_score <- rowSums(data_items, na.rm = TRUE)
  n_resp <- nrow(data_items)

  # --- Analisis kelompok atas-bawah (27%) ---
  n27 <- max(1, floor(n_resp * 0.27))
  rank_order <- order(total_score, decreasing = TRUE)
  idx_upper <- rank_order[1:n27]
  idx_lower <- rank_order[(n_resp - n27 + 1):n_resp]

  # Proporsi benar kelompok atas & bawah, dan indeks diskriminasi D
  p_upper <- colMeans(data_items[idx_upper, , drop = FALSE], na.rm = TRUE)
  p_lower <- colMeans(data_items[idx_lower, , drop = FALSE], na.rm = TRUE)
  D_index <- p_upper - p_lower

  # --- Point-biserial / korelasi item-total (uncorrected) ---
  r_pbis <- sapply(seq_len(ncol(data_items)), function(i) {
    cor(data_items[, i], total_score, use = "pairwise.complete.obs")
  })

  # --- Item reliability index = r_it * SD_item ---
  sd_items <- apply(data_items, 2, sd, na.rm = TRUE)
  item_rel_index <- ia$item.stats$r.cor * sd_items

  item_stats <- data.frame(
    Item = colnames(data_items),
    Mean = round(colMeans(data_items, na.rm = TRUE), 3),
    SD = round(sd_items, 3),
    r_pbis = round(r_pbis, 3),
    r_item_total = round(ia$item.stats$r.cor, 3),
    r_drop = round(ia$item.stats$r.drop, 3),
    alpha_if_deleted = round(ia$alpha.drop$raw_alpha, 3),
    p_upper = round(p_upper, 3),
    p_lower = round(p_lower, 3),
    D_index = round(D_index, 3),
    item_rel_index = round(item_rel_index, 3),
    reversed = colnames(data_items) %in% reversed_items,
    stringsAsFactors = FALSE
  )

  # Untuk dikotomis, Mean = proporsi benar = tingkat kesulitan
  is_dichotomous <- all(data_items %in% c(0, 1), na.rm = TRUE)
  if (is_dichotomous) {
    item_stats$p_difficulty <- item_stats$Mean
    item_stats$kategori_difficulty <- ifelse(
      item_stats$p_difficulty < 0.20, "Sangat Sulit",
      ifelse(item_stats$p_difficulty < 0.40, "Sulit",
             ifelse(item_stats$p_difficulty < 0.60, "Sedang",
                    ifelse(item_stats$p_difficulty < 0.80, "Mudah", "Sangat Mudah"))))
  }

  # Kategori daya beda berdasarkan r_item_total (corrected)
  item_stats$kategori_daya_beda <- ifelse(
    item_stats$r_item_total < 0.10, "Sangat Rendah",
    ifelse(item_stats$r_item_total < 0.20, "Rendah",
           ifelse(item_stats$r_item_total < 0.30, "Cukup",
                  ifelse(item_stats$r_item_total < 0.40, "Baik", "Sangat Baik"))))

  # Kategori indeks diskriminasi D
  item_stats$kategori_D <- ifelse(
    item_stats$D_index < 0.10, "Sangat Rendah",
    ifelse(item_stats$D_index < 0.20, "Rendah",
           ifelse(item_stats$D_index < 0.30, "Cukup",
                  ifelse(item_stats$D_index < 0.40, "Baik", "Sangat Baik"))))

  # Keputusan akhir item
  item_stats$keputusan <- ifelse(
    item_stats$r_item_total < 0.10 | item_stats$D_index < 0.10, "Ditolak",
    ifelse(item_stats$r_item_total < 0.20 | item_stats$D_index < 0.20, "Direvisi",
           "Diterima"))

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
# 4b. ANALISIS BUTIR AITEM KOMPREHENSIF
# ==============================================================================

cat("\n>>> [2b] Analisis Butir Aitem Komprehensif...\n\n")

# --- Ringkasan kualitas item per subtes ---
item_quality_summary <- data.frame(
  Subtes = character(), Nama = character(),
  Jumlah_Item = integer(),
  Diterima = integer(), Direvisi = integer(), Ditolak = integer(),
  Mean_D_Index = numeric(), Mean_r_pbis = numeric(),
  Mean_r_it_corrected = numeric(),
  Mean_Item_Rel_Index = numeric(),
  Mean_Inter_Item_r = numeric(),
  stringsAsFactors = FALSE
)

for (s in names(subtests)) {
  cat("--- Analisis Butir:", s, "-", subtest_names[s], "---\n")

  ia <- ctt_results[[s]]$item_analysis
  items <- data_raw[, subtests[[s]]]

  # Inter-item correlation
  iic <- cor(items, use = "pairwise.complete.obs")
  diag(iic) <- NA
  mean_iic <- mean(iic, na.rm = TRUE)
  ctt_results[[s]]$inter_item_cor <- iic

  # Tabel ringkasan item
  cat("   Jumlah item:", nrow(ia), "\n")
  cat("   Item diterima :", sum(ia$keputusan == "Diterima"), "\n")
  cat("   Item perlu revisi:", sum(ia$keputusan == "Direvisi"), "\n")
  cat("   Item ditolak :", sum(ia$keputusan == "Ditolak"), "\n")
  cat("   Mean D-index    :", round(mean(ia$D_index, na.rm = TRUE), 3), "\n")
  cat("   Mean r(pbis)    :", round(mean(ia$r_pbis, na.rm = TRUE), 3), "\n")
  cat("   Mean r(it) corr :", round(mean(ia$r_item_total, na.rm = TRUE), 3), "\n")
  cat("   Mean inter-item r:", round(mean_iic, 3), "\n")

  # Tampilkan item bermasalah
  rejected <- ia$Item[ia$keputusan == "Ditolak"]
  revised <- ia$Item[ia$keputusan == "Direvisi"]
  if (length(rejected) > 0) cat("   >> DITOLAK:", paste(rejected, collapse = ", "), "\n")
  if (length(revised) > 0) cat("   >> PERLU REVISI:", paste(revised, collapse = ", "), "\n")
  cat("\n")

  # Simpan ringkasan
  item_quality_summary <- rbind(item_quality_summary, data.frame(
    Subtes = s, Nama = subtest_names[s],
    Jumlah_Item = nrow(ia),
    Diterima = sum(ia$keputusan == "Diterima"),
    Direvisi = sum(ia$keputusan == "Direvisi"),
    Ditolak = sum(ia$keputusan == "Ditolak"),
    Mean_D_Index = round(mean(ia$D_index, na.rm = TRUE), 3),
    Mean_r_pbis = round(mean(ia$r_pbis, na.rm = TRUE), 3),
    Mean_r_it_corrected = round(mean(ia$r_item_total, na.rm = TRUE), 3),
    Mean_Item_Rel_Index = round(mean(ia$item_rel_index, na.rm = TRUE), 3),
    Mean_Inter_Item_r = round(mean_iic, 3),
    stringsAsFactors = FALSE
  ))
}

cat("--- Ringkasan Kualitas Item Seluruh Subtes ---\n")
print(item_quality_summary[, c("Subtes","Jumlah_Item","Diterima","Direvisi","Ditolak",
                                "Mean_D_Index","Mean_r_pbis","Mean_r_it_corrected")])

total_items <- sum(item_quality_summary$Jumlah_Item)
total_accepted <- sum(item_quality_summary$Diterima)
total_revised <- sum(item_quality_summary$Direvisi)
total_rejected <- sum(item_quality_summary$Ditolak)
cat("\nTotal item:", total_items, "| Diterima:", total_accepted,
    "(", round(total_accepted/total_items*100, 1), "%) | Direvisi:", total_revised,
    "(", round(total_revised/total_items*100, 1), "%) | Ditolak:", total_rejected,
    "(", round(total_rejected/total_items*100, 1), "%)\n\n")

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
      print(plot(irt_results[[s]]$twopl, type = "trace",
                 main = paste("Item Characteristic Curves -", s, "(2PL)"),
                 par.settings = list(superpose.line = list(lwd = 2))))
    } else {
      print(plot(irt_results[[s]]$grm, type = "trace",
                 main = paste("Category Response Curves -", s, "(GRM)"),
                 par.settings = list(superpose.line = list(lwd = 2))))
    }
    dev.off()
  }
}

# --- 7.5 Item Information Curves (IIC) ---
for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    pdf(paste0("output_ist/grafik/05_IIF_", s, ".pdf"), width = 12, height = 8)

    mod <- if (irt_results[[s]]$type == "dikotomis") irt_results[[s]]$twopl else irt_results[[s]]$grm
    print(plot(mod, type = "infotrace",
               main = paste("Item Information Functions -", s)))
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

# --- 7.12 Scatter plot: Tingkat Kesulitan vs Daya Beda per subtes ---
pdf("output_ist/grafik/12_difficulty_vs_discrimination.pdf", width = 14, height = 10)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis
  col_pts <- ifelse(ia$keputusan == "Ditolak", "red",
             ifelse(ia$keputusan == "Direvisi", "orange", "forestgreen"))
  plot(ia$Mean, ia$D_index, pch = 19, col = col_pts, cex = 1.3,
       main = paste(s, "- Kesulitan vs D-index"),
       xlab = "Tingkat Kesulitan (Mean)", ylab = "Indeks Diskriminasi (D)",
       xlim = c(0, max(ia$Mean, na.rm = TRUE) * 1.1),
       ylim = c(min(0, min(ia$D_index, na.rm = TRUE) - 0.05), 1))
  text(ia$Mean, ia$D_index, labels = ia$Item, cex = 0.55, pos = 3)
  abline(h = c(0.20, 0.30), col = c("orange", "forestgreen"), lty = 2)
  if (all(ia$Mean <= 1)) abline(v = c(0.20, 0.80), col = "gray60", lty = 3)
}
dev.off()

# --- 7.13 Grafik perbandingan kelompok atas vs bawah per subtes ---
pdf("output_ist/grafik/13_upper_lower_comparison.pdf", width = 14, height = 18)
par(mfrow = c(5, 2), mar = c(5, 4, 3, 1))
for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis
  n_items <- nrow(ia)
  x_pos <- barplot(rbind(ia$p_upper, ia$p_lower), beside = TRUE,
                   names.arg = ia$Item,
                   col = c("steelblue", "salmon"),
                   main = paste(s, "- Kelompok Atas (27%) vs Bawah (27%)"),
                   ylab = "Proporsi Benar / Mean",
                   las = 2, cex.names = 0.7,
                   ylim = c(0, max(c(ia$p_upper, ia$p_lower), na.rm = TRUE) * 1.2))
  legend("topright", legend = c("Atas 27%", "Bawah 27%"),
         fill = c("steelblue", "salmon"), cex = 0.7, bty = "n")
}
dev.off()

# --- 7.14 Ringkasan kualitas item (keputusan) per subtes ---
pdf("output_ist/grafik/14_item_quality_summary.pdf", width = 10, height = 6)
par(mar = c(5, 4, 3, 1))
quality_matrix <- t(as.matrix(item_quality_summary[, c("Diterima", "Direvisi", "Ditolak")]))
colnames(quality_matrix) <- item_quality_summary$Subtes
barplot(quality_matrix, beside = FALSE,
        col = c("forestgreen", "orange", "red"),
        main = "Ringkasan Keputusan Kualitas Item per Subtes",
        ylab = "Jumlah Item", las = 1,
        legend.text = c("Diterima", "Direvisi", "Ditolak"),
        args.legend = list(x = "topright", bty = "n", cex = 0.9))
dev.off()

# --- 7.15 Scatter: r(pbis) vs r(it) corrected per subtes ---
pdf("output_ist/grafik/15_rpbis_vs_rit.pdf", width = 14, height = 10)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis
  plot(ia$r_pbis, ia$r_item_total, pch = 19, col = "steelblue", cex = 1.2,
       main = paste(s, "- r(pbis) vs r(it) terkoreksi"),
       xlab = "Point-biserial (r_pbis)", ylab = "Corrected r(it)")
  text(ia$r_pbis, ia$r_item_total, labels = ia$Item, cex = 0.55, pos = 3)
  abline(0, 1, col = "gray50", lty = 2)
  abline(h = 0.20, col = "red", lty = 3)
  abline(v = 0.20, col = "red", lty = 3)
}
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

## Analisis Butir Aitem per Subtes

```{r item_analysis_loop, results="asis"}
for (s in names(subtests)) {
  cat("\\n### Subtes", s, "-", subtest_names[s], "\\n\\n")

  ia <- ctt_results[[s]]$item_analysis

  # --- Tabel 1: Tingkat Kesulitan & Statistik Dasar ---
  cat("#### Statistik Dasar Item\\n\\n")
  basic_cols <- c("Item", "Mean", "SD")
  if ("p_difficulty" %in% names(ia)) {
    basic_cols <- c(basic_cols, "p_difficulty", "kategori_difficulty")
  }
  ia_basic <- ia[, basic_cols[basic_cols %in% names(ia)]]
  print(kable(ia_basic, caption = paste("Statistik Dasar Item -", s),
              booktabs = TRUE, digits = 3, row.names = FALSE) %>%
          kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))

  # --- Tabel 2: Daya Beda & Korelasi ---
  cat("\\n#### Daya Beda & Korelasi Item\\n\\n")
  disc_cols <- c("Item", "r_pbis", "r_item_total", "r_drop", "alpha_if_deleted", "kategori_daya_beda")
  ia_disc <- ia[, disc_cols[disc_cols %in% names(ia)]]
  names(ia_disc) <- c("Item", "r(pbis)", "r(it) corr", "r(drop)", "Alpha if Del", "Kategori")
  print(kable(ia_disc, caption = paste("Daya Beda Item -", s),
              booktabs = TRUE, digits = 3, row.names = FALSE) %>%
          kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))

  # --- Tabel 3: Analisis Kelompok Atas-Bawah (27%) ---
  cat("\\n#### Analisis Kelompok Atas-Bawah (27%)\\n\\n")
  ul_cols <- c("Item", "p_upper", "p_lower", "D_index", "kategori_D", "item_rel_index", "keputusan")
  ia_ul <- ia[, ul_cols[ul_cols %in% names(ia)]]
  names(ia_ul) <- c("Item", "P(Atas)", "P(Bawah)", "D-Index", "Kategori D", "Rel. Index", "Keputusan")
  print(kable(ia_ul, caption = paste("Analisis Kelompok Atas-Bawah -", s),
              booktabs = TRUE, digits = 3, row.names = FALSE) %>%
          kable_styling(latex_options = c("hold_position","scale_down"), font_size = 8))

  cat("\\n**Alpha keseluruhan:", round(ctt_results[[s]]$alpha$total$raw_alpha, 3), "**\\n\\n")

  # --- Ringkasan item bermasalah ---
  n_accepted <- sum(ia$keputusan == "Diterima")
  n_revised  <- sum(ia$keputusan == "Direvisi")
  n_rejected <- sum(ia$keputusan == "Ditolak")
  cat("**Keputusan kualitas item:**", n_accepted, "diterima,",
      n_revised, "perlu revisi,", n_rejected, "ditolak.\\n\\n")

  problem_items <- ia$Item[ia$r_item_total < 0.20]
  if (length(problem_items) > 0) {
    cat("*Item dengan daya beda rendah (r < 0.20):*", paste(problem_items, collapse = ", "), "\\n\\n")
  }
  rejected_items <- ia$Item[ia$keputusan == "Ditolak"]
  if (length(rejected_items) > 0) {
    cat("*Item ditolak (r\\\\_it < 0.10 atau D < 0.10):*", paste(rejected_items, collapse = ", "), "\\n\\n")
  }
  if ("reversed" %in% names(ia)) {
    rev_items <- ia$Item[ia$reversed == TRUE]
    if (length(rev_items) > 0) {
      cat("*Item berkorelasi negatif (di-reverse):*", paste(rev_items, collapse = ", "), "\\n\\n")
    }
  }

  # Inter-item correlation summary
  if (!is.null(ctt_results[[s]]$inter_item_cor)) {
    iic <- ctt_results[[s]]$inter_item_cor
    diag(iic) <- NA
    cat("**Korelasi inter-item:** Mean =", round(mean(iic, na.rm = TRUE), 3),
        ", Min =", round(min(iic, na.rm = TRUE), 3),
        ", Max =", round(max(iic, na.rm = TRUE), 3), "\\n\\n")
  }

  cat("\\n\\newpage\\n")
}
```

## Ringkasan Kualitas Item Seluruh Subtes

```{r ringkasan_kualitas_item}
quality_tbl <- item_quality_summary[, c("Subtes","Jumlah_Item","Diterima","Direvisi","Ditolak",
                                         "Mean_D_Index","Mean_r_pbis","Mean_r_it_corrected",
                                         "Mean_Item_Rel_Index","Mean_Inter_Item_r")]
names(quality_tbl) <- c("Subtes","n Item","Diterima","Revisi","Ditolak",
                          "Mean D","Mean r(pbis)","Mean r(it)","Mean Rel Idx","Mean IIC")
kable(quality_tbl, caption = "Ringkasan Kualitas Item per Subtes",
      booktabs = TRUE, digits = 3, row.names = FALSE) %>%
  kable_styling(latex_options = c("hold_position","scale_down"), font_size = 9)

total_items <- sum(item_quality_summary$Jumlah_Item)
total_accepted <- sum(item_quality_summary$Diterima)
total_revised <- sum(item_quality_summary$Direvisi)
total_rejected <- sum(item_quality_summary$Ditolak)
cat("\\n**Total:**", total_items, "item |",
    total_accepted, "diterima (", round(total_accepted/total_items*100, 1), "%) |",
    total_revised, "revisi (", round(total_revised/total_items*100, 1), "%) |",
    total_rejected, "ditolak (", round(total_rejected/total_items*100, 1), "%)\\n")
```

```{r plot_item_quality, fig.cap="Ringkasan Keputusan Kualitas Item per Subtes", fig.height=4, fig.width=8}
quality_long <- reshape2::melt(item_quality_summary[, c("Subtes","Diterima","Direvisi","Ditolak")],
                                id.vars = "Subtes", variable.name = "Keputusan", value.name = "Jumlah")
quality_long$Keputusan <- factor(quality_long$Keputusan, levels = c("Ditolak","Direvisi","Diterima"))

ggplot(quality_long, aes(x = Subtes, y = Jumlah, fill = Keputusan)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Diterima" = "forestgreen", "Direvisi" = "orange", "Ditolak" = "red")) +
  labs(title = "Keputusan Kualitas Item per Subtes IST",
       x = "Subtes", y = "Jumlah Item") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

```{r plot_diff_disc_scatter, results="asis", fig.height=5, fig.width=10}
cat("\\n### Scatter Plot: Tingkat Kesulitan vs Indeks Diskriminasi\\n\\n")
all_ia <- do.call(rbind, lapply(names(subtests), function(s) {
  ia <- ctt_results[[s]]$item_analysis
  ia$Subtes <- s
  ia
}))

ggplot(all_ia, aes(x = Mean, y = D_index, color = keputusan)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = c(0.20, 0.30), linetype = "dashed", color = c("orange","forestgreen")) +
  scale_color_manual(values = c("Diterima" = "forestgreen", "Direvisi" = "orange", "Ditolak" = "red")) +
  facet_wrap(~ Subtes, scales = "free_x", ncol = 3) +
  labs(title = "Tingkat Kesulitan vs Indeks Diskriminasi (D) per Subtes",
       x = "Tingkat Kesulitan (Mean)", y = "Indeks D", color = "Keputusan") +
  theme_minimal() +
  theme(legend.position = "bottom")
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
                         Keputusan = character(),
                         stringsAsFactors = FALSE)

for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis

  # Daya beda rendah (r_it corrected)
  low_disc <- ia[ia$r_item_total < 0.20, ]
  if (nrow(low_disc) > 0) {
    problem_df <- rbind(problem_df, data.frame(
      Subtes = s, Item = low_disc$Item,
      Masalah = "r(it) corrected < 0.20",
      Nilai = low_disc$r_item_total,
      Keputusan = low_disc$keputusan, stringsAsFactors = FALSE))
  }

  # D-index rendah
  low_D <- ia[ia$D_index < 0.20, ]
  if (nrow(low_D) > 0) {
    problem_df <- rbind(problem_df, data.frame(
      Subtes = s, Item = low_D$Item,
      Masalah = "D-index < 0.20",
      Nilai = low_D$D_index,
      Keputusan = low_D$keputusan, stringsAsFactors = FALSE))
  }

  # Item berkorelasi negatif (di-reverse)
  if ("reversed" %in% names(ia)) {
    rev_items <- ia[ia$reversed == TRUE, ]
    if (nrow(rev_items) > 0) {
      problem_df <- rbind(problem_df, data.frame(
        Subtes = s, Item = rev_items$Item,
        Masalah = "Korelasi negatif (reversed)",
        Nilai = rev_items$r_item_total,
        Keputusan = rev_items$keputusan, stringsAsFactors = FALSE))
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
        Nilai = too_easy$p_difficulty,
        Keputusan = too_easy$keputusan, stringsAsFactors = FALSE))
    }
    if (nrow(too_hard) > 0) {
      problem_df <- rbind(problem_df, data.frame(
        Subtes = s, Item = too_hard$Item,
        Masalah = "Terlalu sulit (p < 0.05)",
        Nilai = too_hard$p_difficulty,
        Keputusan = too_hard$keputusan, stringsAsFactors = FALSE))
    }
  }
}

# Hapus duplikat (item bisa muncul di beberapa kategori masalah)
if (nrow(problem_df) > 0) {
  problem_df$Nilai <- round(problem_df$Nilai, 3)
  kable(problem_df, caption = "Daftar Item Bermasalah (Semua Kriteria)",
        booktabs = TRUE, row.names = FALSE) %>%
    kable_styling(latex_options = c("hold_position","scale_down"), font_size = 9)
} else {
  cat("Tidak ditemukan item bermasalah berdasarkan kriteria yang ditetapkan.\\n")
}
```

## Rekomendasi

Berdasarkan hasil analisis butir aitem di atas, berikut beberapa rekomendasi:

1. **Item ditolak** (r\\_it < 0.10 atau D-index < 0.10) sebaiknya dihapus atau ditulis ulang sepenuhnya karena tidak mampu membedakan kelompok kemampuan tinggi dan rendah.
2. **Item perlu revisi** (r\\_it 0.10-0.20 atau D-index 0.10-0.20) perlu ditinjau ulang dari segi konten, kunci jawaban, dan kualitas distraktor.
3. **Item dengan daya beda rendah** (r < 0.20) perlu ditinjau ulang, baik dari segi konten maupun kunci jawaban.
4. **Item yang terlalu mudah atau terlalu sulit** (p < 0.05 atau p > 0.95) perlu direvisi agar distribusi tingkat kesulitan lebih merata.
5. **Item yang menunjukkan misfit** pada analisis IRT perlu dievaluasi apakah mengukur konstruk yang sama.
6. **Reliabilitas** subtes dengan Alpha < 0.70 perlu ditingkatkan, misalnya dengan menambahkan item berkualitas atau merevisi item yang ada.
7. **Struktur faktor** IST sebaiknya dikonfirmasi dengan sampel yang lebih besar untuk memastikan model 3-faktor (Verbal, Numerik, Figural) sesuai.

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
# 8b. LAPORAN KESELURUHAN ANALISIS (PLAIN TEXT)
# ==============================================================================

cat("\n>>> [6b] Membuat laporan keseluruhan analisis...\n")

laporan_lines <- c()
ln <- function(...) laporan_lines <<- c(laporan_lines, paste0(...))
ln_sep <- function() ln(paste(rep("=", 80), collapse = ""))
ln_sub <- function() ln(paste(rep("-", 60), collapse = ""))

ln_sep()
ln("  LAPORAN KESELURUHAN ANALISIS PSIKOMETRIK")
ln("  INTELLIGENZ STRUKTUR TEST (IST)")
ln_sep()
ln("  Tanggal analisis: ", format(Sys.Date(), "%d %B %Y"))
ln("  Jumlah responden: ", nrow(data_raw))
ln("  Jumlah item total: ", length(unlist(subtests)))
ln("  Jumlah subtes    : ", length(subtests))
ln_sep()
ln("")

# --- A. Deskripsi Sampel ---
ln("A. DESKRIPSI SAMPEL")
ln_sub()
ln("")
ln("  A.1 Jenis Kelamin:")
tbl_jk <- table(data_raw$jk)
for (nm in names(tbl_jk)) {
  ln("      ", nm, ": ", tbl_jk[nm], " (", round(tbl_jk[nm]/sum(tbl_jk)*100, 1), "%)")
}
ln("")
ln("  A.2 Kelompok Usia:")
tbl_usia <- table(data_raw$rentus)
for (nm in names(tbl_usia)) {
  ln("      ", nm, ": ", tbl_usia[nm], " (", round(tbl_usia[nm]/sum(tbl_usia)*100, 1), "%)")
}
ln("")
ln("  A.3 Pendidikan:")
tbl_pend <- table(data_raw$pendidikan)
for (nm in names(tbl_pend)) {
  ln("      ", nm, ": ", tbl_pend[nm], " (", round(tbl_pend[nm]/sum(tbl_pend)*100, 1), "%)")
}
ln("")

# --- B. Statistik Deskriptif Skor ---
ln("B. STATISTIK DESKRIPTIF SKOR PER SUBTES")
ln_sub()
ln("")
ln(sprintf("  %-6s %-40s %5s %6s %6s %6s %6s %8s %8s",
           "Subtes", "Nama", "n", "Min", "Max", "Mean", "SD", "Skewness", "Kurtosis"))
ln(paste(rep("-", 100), collapse = ""))
for (s in names(subtests)) {
  skor <- data_raw[[paste0("skor_", s)]]
  desc <- psych::describe(skor)
  ln(sprintf("  %-6s %-40s %5d %6.1f %6.1f %6.2f %6.2f %8.3f %8.3f",
             s, subtest_names[s], length(skor),
             min(skor, na.rm = TRUE), max(skor, na.rm = TRUE),
             mean(skor, na.rm = TRUE), sd(skor, na.rm = TRUE),
             desc$skew, desc$kurtosis))
}
skor_total <- data_raw$skor_total
desc_total <- psych::describe(skor_total)
ln(paste(rep("-", 100), collapse = ""))
ln(sprintf("  %-6s %-40s %5d %6.1f %6.1f %6.2f %6.2f %8.3f %8.3f",
           "TOTAL", "Skor Total IST", length(skor_total),
           min(skor_total, na.rm = TRUE), max(skor_total, na.rm = TRUE),
           mean(skor_total, na.rm = TRUE), sd(skor_total, na.rm = TRUE),
           desc_total$skew, desc_total$kurtosis))
ln("")

# --- C. Reliabilitas ---
ln("C. RELIABILITAS (CRONBACH'S ALPHA)")
ln_sub()
ln("")
ln(sprintf("  %-6s %5s %7s %9s %8s %8s %6s  %-15s",
           "Subtes", "n", "Alpha", "Alpha Std", "Mean rit", "SEM", "SH", "Interpretasi"))
ln(paste(rep("-", 85), collapse = ""))
for (i in 1:nrow(reliability_summary)) {
  r <- reliability_summary[i, ]
  interp <- ifelse(r$Alpha >= 0.9, "Sangat Baik",
            ifelse(r$Alpha >= 0.8, "Baik",
            ifelse(r$Alpha >= 0.7, "Memadai",
            ifelse(r$Alpha >= 0.6, "Cukup",
            ifelse(r$Alpha >= 0.5, "Kurang", "Rendah")))))
  sh_val <- tryCatch(round(ctt_results[[r$Subtes]]$split_half$maxrb, 3), error = function(e) NA)
  ln(sprintf("  %-6s %5d %7.3f %9.3f %8.3f %8.2f %6s  %-15s",
             r$Subtes, r$Jumlah_Item, r$Alpha, r$Alpha_Std, r$Mean_r_IT, r$SEM,
             ifelse(is.na(sh_val), "N/A", as.character(sh_val)), interp))
}
ln("")

# --- D. Analisis Butir Aitem Komprehensif ---
ln("D. ANALISIS BUTIR AITEM PER SUBTES")
ln_sub()
ln("")

for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis

  ln("  D.", match(s, names(subtests)), " Subtes ", s, " - ", subtest_names[s])
  ln("  Alpha: ", round(ctt_results[[s]]$alpha$total$raw_alpha, 3),
     " | Jumlah item: ", nrow(ia))
  ln("")

  # Header tabel
  has_p <- "p_difficulty" %in% names(ia)
  if (has_p) {
    ln(sprintf("  %-7s %6s %6s %7s %7s %7s %7s %7s %7s %7s %-12s %-10s",
               "Item", "Mean", "SD", "r(pbis)", "r(it)", "p_up", "p_low", "D-idx",
               "RelIdx", "AlpDel", "Kat.Diff", "Keputusan"))
  } else {
    ln(sprintf("  %-7s %6s %6s %7s %7s %7s %7s %7s %7s %7s %-10s",
               "Item", "Mean", "SD", "r(pbis)", "r(it)", "p_up", "p_low", "D-idx",
               "RelIdx", "AlpDel", "Keputusan"))
  }
  ln(paste("  ", paste(rep("-", 110), collapse = ""), sep = ""))

  for (j in 1:nrow(ia)) {
    row <- ia[j, ]
    rev_mark <- ifelse(row$reversed, " *", "")
    if (has_p) {
      diff_cat <- substr(row$kategori_difficulty, 1, 12)
      ln(sprintf("  %-7s %6.3f %6.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %-12s %-10s%s",
                 row$Item, row$Mean, row$SD, row$r_pbis, row$r_item_total,
                 row$p_upper, row$p_lower, row$D_index,
                 row$item_rel_index, row$alpha_if_deleted,
                 diff_cat, row$keputusan, rev_mark))
    } else {
      ln(sprintf("  %-7s %6.3f %6.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %-10s%s",
                 row$Item, row$Mean, row$SD, row$r_pbis, row$r_item_total,
                 row$p_upper, row$p_lower, row$D_index,
                 row$item_rel_index, row$alpha_if_deleted,
                 row$keputusan, rev_mark))
    }
  }

  # Ringkasan per subtes
  ln("")
  ln("  Ringkasan: Diterima=", sum(ia$keputusan == "Diterima"),
     " | Direvisi=", sum(ia$keputusan == "Direvisi"),
     " | Ditolak=", sum(ia$keputusan == "Ditolak"))
  ln("  Mean D-index=", round(mean(ia$D_index, na.rm = TRUE), 3),
     " | Mean r(pbis)=", round(mean(ia$r_pbis, na.rm = TRUE), 3),
     " | Mean r(it)=", round(mean(ia$r_item_total, na.rm = TRUE), 3))

  # Inter-item
  if (!is.null(ctt_results[[s]]$inter_item_cor)) {
    iic <- ctt_results[[s]]$inter_item_cor
    diag(iic) <- NA
    ln("  Mean inter-item r=", round(mean(iic, na.rm = TRUE), 3),
       " | Min=", round(min(iic, na.rm = TRUE), 3),
       " | Max=", round(max(iic, na.rm = TRUE), 3))
  }

  rev_items <- ia$Item[ia$reversed == TRUE]
  if (length(rev_items) > 0) {
    ln("  * Item reversed: ", paste(rev_items, collapse = ", "))
  }
  ln("")
}

# --- E. Ringkasan Kualitas Item Keseluruhan ---
ln("E. RINGKASAN KUALITAS ITEM SELURUH SUBTES")
ln_sub()
ln("")
ln(sprintf("  %-6s %5s %8s %8s %7s %8s %9s %8s %8s",
           "Subtes", "n", "Diterima", "Direvisi", "Ditolak", "Mean D", "Mean rpbis", "Mean rit", "Mean IIC"))
ln(paste(rep("-", 85), collapse = ""))
for (i in 1:nrow(item_quality_summary)) {
  q <- item_quality_summary[i, ]
  ln(sprintf("  %-6s %5d %8d %8d %7d %8.3f %9.3f %8.3f %8.3f",
             q$Subtes, q$Jumlah_Item, q$Diterima, q$Direvisi, q$Ditolak,
             q$Mean_D_Index, q$Mean_r_pbis, q$Mean_r_it_corrected, q$Mean_Inter_Item_r))
}
ln(paste(rep("-", 85), collapse = ""))
ln(sprintf("  %-6s %5d %8d %8d %7d",
           "TOTAL", total_items, total_accepted, total_revised, total_rejected))
ln(sprintf("  Persentase: Diterima %.1f%% | Direvisi %.1f%% | Ditolak %.1f%%",
           total_accepted/total_items*100, total_revised/total_items*100, total_rejected/total_items*100))
ln("")

# --- F. IRT ---
ln("F. ANALISIS ITEM RESPONSE THEORY (IRT)")
ln_sub()
ln("")

for (s in names(subtests)) {
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    ln("  F.", match(s, names(subtests)), " Subtes ", s, " - ", subtest_names[s])

    if (irt_results[[s]]$type == "dikotomis") {
      params <- as.data.frame(irt_results[[s]]$twopl_params)
      ln(sprintf("  %-7s %10s %10s", "Item", "a (Disc)", "b (Diff)"))
      ln(paste("  ", paste(rep("-", 30), collapse = "")))
      for (j in 1:nrow(params)) {
        ln(sprintf("  %-7s %10.3f %10.3f", rownames(params)[j], params$a[j], params$b[j]))
      }

      # Model comparison
      if (!is.null(irt_results[[s]]$anova)) {
        av <- irt_results[[s]]$anova
        ln("")
        ln("  Perbandingan: AIC Rasch=", round(av$AIC[1], 1),
           " | AIC 2PL=", round(av$AIC[2], 1))
        ln("                BIC Rasch=", round(av$BIC[1], 1),
           " | BIC 2PL=", round(av$BIC[2], 1))
        if (!is.na(av$p[2])) {
          ln("  LRT p-value: ", round(av$p[2], 4),
             ifelse(av$p[2] < 0.05, " => 2PL lebih baik", " => Rasch cukup memadai"))
        }
      }

      # Model fit M2
      if (!is.null(irt_results[[s]]$twopl_m2)) {
        m2 <- irt_results[[s]]$twopl_m2
        ln("  Model Fit 2PL: RMSEA=", round(m2$RMSEA, 4),
           " | SRMSR=", round(m2$SRMSR, 4),
           " | TLI=", round(m2$TLI, 4),
           " | CFI=", round(m2$CFI, 4))
      }

      # Item fit misfit
      fit_df <- irt_results[[s]]$twopl_fit
      misfit <- fit_df[fit_df$p.S_X2 < 0.05, ]
      if (nrow(misfit) > 0) {
        ln("  Item MISFIT (p < 0.05): ", paste(misfit$item, collapse = ", "))
      } else {
        ln("  Semua item FIT (p >= 0.05)")
      }

    } else {
      params <- as.data.frame(irt_results[[s]]$grm_params)
      param_names <- colnames(params)
      header <- sprintf("  %-7s", "Item")
      for (pn in param_names) header <- paste0(header, sprintf(" %8s", pn))
      ln(header)
      ln(paste("  ", paste(rep("-", 8 + 9 * length(param_names)), collapse = "")))
      for (j in 1:nrow(params)) {
        row_str <- sprintf("  %-7s", rownames(params)[j])
        for (pn in param_names) row_str <- paste0(row_str, sprintf(" %8.3f", params[j, pn]))
        ln(row_str)
      }

      fit_df <- irt_results[[s]]$grm_fit
      misfit <- fit_df[fit_df$p.S_X2 < 0.05, ]
      if (nrow(misfit) > 0) {
        ln("  Item MISFIT (p < 0.05): ", paste(misfit$item, collapse = ", "))
      } else {
        ln("  Semua item FIT (p >= 0.05)")
      }
    }
    ln("")
  } else if (!is.null(irt_results[[s]]) && !is.null(irt_results[[s]]$error)) {
    ln("  F.", match(s, names(subtests)), " Subtes ", s, " - GAGAL: ", irt_results[[s]]$error)
    ln("")
  }
}

# --- G. Analisis Struktur Faktor ---
ln("G. ANALISIS STRUKTUR FAKTOR")
ln_sub()
ln("")

# Korelasi
ln("  G.1 Korelasi Antar Subtes")
ln("")
header_cor <- sprintf("  %-6s", "")
for (s in names(subtests)) header_cor <- paste0(header_cor, sprintf(" %6s", s))
ln(header_cor)
for (s1 in names(subtests)) {
  row_str <- sprintf("  %-6s", s1)
  for (s2 in names(subtests)) row_str <- paste0(row_str, sprintf(" %6.3f", cor_matrix[s1, s2]))
  ln(row_str)
}
ln("")

# KMO
ln("  G.2 KMO = ", round(kmo_res$MSA, 3))
ln("")

# EFA
ln("  G.3 EFA (3 Faktor, Varimax)")
loadings_mat <- unclass(efa_result$loadings)
header_efa <- sprintf("  %-6s", "")
for (cn in colnames(loadings_mat)) header_efa <- paste0(header_efa, sprintf(" %8s", cn))
header_efa <- paste0(header_efa, sprintf(" %10s", "Communal."))
ln(header_efa)
for (rn in rownames(loadings_mat)) {
  row_str <- sprintf("  %-6s", rn)
  for (cn in colnames(loadings_mat)) row_str <- paste0(row_str, sprintf(" %8.3f", loadings_mat[rn, cn]))
  row_str <- paste0(row_str, sprintf(" %10.3f", efa_result$communality[rn]))
  ln(row_str)
}
ln("")

# CFA
if (!is.null(cfa_fit)) {
  ln("  G.4 CFA Model 3-Faktor (Verbal: SE+WA+AN+GE | Numerik: RA+ZR+ME | Figural: FA+WU)")
  fit_idx <- fitMeasures(cfa_fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
  ln(sprintf("  Chi-sq=%.2f (df=%d, p=%.4f) | CFI=%.4f | TLI=%.4f | RMSEA=%.4f | SRMR=%.4f",
             fit_idx["chisq"], fit_idx["df"], fit_idx["pvalue"],
             fit_idx["cfi"], fit_idx["tli"], fit_idx["rmsea"], fit_idx["srmr"]))

  std_loads <- lavaan::standardizedSolution(cfa_fit)
  loadings_cfa <- std_loads[std_loads$op == "=~", ]
  ln("")
  ln(sprintf("  %-10s %-6s %8s %8s %10s", "Faktor", "Subtes", "Loading", "SE", "p-value"))
  ln(paste("  ", paste(rep("-", 45), collapse = "")))
  for (j in 1:nrow(loadings_cfa)) {
    lc <- loadings_cfa[j, ]
    ln(sprintf("  %-10s %-6s %8.3f %8.3f %10.4f", lc$lhs, lc$rhs, lc$est.std, lc$se, lc$pvalue))
  }
} else {
  ln("  G.4 CFA tidak dapat diestimasi.")
}
ln("")

# --- H. Item Bermasalah ---
ln("H. DAFTAR ITEM BERMASALAH")
ln_sub()
ln("")

all_problems <- data.frame(Subtes = character(), Item = character(),
                           Masalah = character(), stringsAsFactors = FALSE)
for (s in names(subtests)) {
  ia <- ctt_results[[s]]$item_analysis
  rejected <- ia[ia$keputusan == "Ditolak", ]
  if (nrow(rejected) > 0) {
    all_problems <- rbind(all_problems, data.frame(
      Subtes = s, Item = rejected$Item,
      Masalah = paste0("DITOLAK (r_it=", rejected$r_item_total, ", D=", rejected$D_index, ")"),
      stringsAsFactors = FALSE))
  }
  revised <- ia[ia$keputusan == "Direvisi", ]
  if (nrow(revised) > 0) {
    all_problems <- rbind(all_problems, data.frame(
      Subtes = s, Item = revised$Item,
      Masalah = paste0("REVISI (r_it=", revised$r_item_total, ", D=", revised$D_index, ")"),
      stringsAsFactors = FALSE))
  }
  if ("p_difficulty" %in% names(ia)) {
    extreme <- ia[ia$p_difficulty > 0.95 | ia$p_difficulty < 0.05, ]
    if (nrow(extreme) > 0) {
      all_problems <- rbind(all_problems, data.frame(
        Subtes = s, Item = extreme$Item,
        Masalah = paste0("Ekstrem (p=", extreme$p_difficulty, ")"),
        stringsAsFactors = FALSE))
    }
  }
  if ("reversed" %in% names(ia)) {
    rev_i <- ia[ia$reversed == TRUE, ]
    if (nrow(rev_i) > 0) {
      all_problems <- rbind(all_problems, data.frame(
        Subtes = s, Item = rev_i$Item,
        Masalah = "Korelasi negatif (reversed)",
        stringsAsFactors = FALSE))
    }
  }
  # IRT misfit
  if (!is.null(irt_results[[s]]) && is.null(irt_results[[s]]$error)) {
    fit_df <- if (irt_results[[s]]$type == "dikotomis") irt_results[[s]]$twopl_fit else irt_results[[s]]$grm_fit
    misfit_items <- fit_df[fit_df$p.S_X2 < 0.05, ]
    if (nrow(misfit_items) > 0) {
      all_problems <- rbind(all_problems, data.frame(
        Subtes = s, Item = misfit_items$item,
        Masalah = paste0("IRT Misfit (p=", round(misfit_items$p.S_X2, 4), ")"),
        stringsAsFactors = FALSE))
    }
  }
}

if (nrow(all_problems) > 0) {
  # Deduplicate
  all_problems <- all_problems[!duplicated(paste(all_problems$Subtes, all_problems$Item, all_problems$Masalah)), ]
  ln(sprintf("  %-6s %-7s %s", "Subtes", "Item", "Masalah"))
  ln(paste("  ", paste(rep("-", 70), collapse = "")))
  for (j in 1:nrow(all_problems)) {
    ln(sprintf("  %-6s %-7s %s", all_problems$Subtes[j], all_problems$Item[j], all_problems$Masalah[j]))
  }
  ln("")
  ln("  Total item bermasalah (unik): ", length(unique(paste(all_problems$Subtes, all_problems$Item))))
} else {
  ln("  Tidak ditemukan item bermasalah.")
}
ln("")

# --- I. Rekomendasi ---
ln("I. REKOMENDASI")
ln_sub()
ln("")
ln("  1. Item DITOLAK (r_it < 0.10 atau D < 0.10): hapus atau tulis ulang.")
ln("  2. Item DIREVISI (r_it 0.10-0.20 atau D 0.10-0.20): tinjau konten & distraktor.")
ln("  3. Item terlalu mudah/sulit (p > 0.95 atau p < 0.05): revisi tingkat kesulitan.")
ln("  4. Item IRT misfit: evaluasi kesesuaian dengan konstruk yang diukur.")
ln("  5. Subtes Alpha < 0.70: tambah item berkualitas atau revisi item lemah.")
ln("  6. Konfirmasi struktur 3-faktor dengan sampel lebih besar.")
ln("")
ln_sep()
ln("  Laporan ini dihasilkan secara otomatis menggunakan R.")
ln("  ", format(Sys.Date(), "%d %B %Y"))
ln_sep()

# Tulis file
writeLines(laporan_lines, "output_ist/Laporan_Keseluruhan_Analisis_IST.txt")
cat(">>> LAPORAN KESELURUHAN BERHASIL DIBUAT: output_ist/Laporan_Keseluruhan_Analisis_IST.txt\n")

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

# Simpan ringkasan kualitas item
write.csv(item_quality_summary, "output_ist/item_quality_summary.csv", row.names = FALSE)

# Simpan inter-item correlation per subtes
for (s in names(subtests)) {
  if (!is.null(ctt_results[[s]]$inter_item_cor)) {
    write.csv(round(ctt_results[[s]]$inter_item_cor, 3),
              paste0("output_ist/inter_item_cor_", s, ".csv"))
  }
}

cat("\n")
cat("============================================================\n")
cat("  ANALISIS SELESAI!                                        \n")
cat("============================================================\n")
cat("  File output tersedia di folder: output_ist/              \n")
cat("                                                            \n")
cat("  Isi folder:                                               \n")
cat("  - Laporan_Psikometrik_IST.pdf    (Laporan PDF lengkap)    \n")
cat("  - Laporan_Keseluruhan_Analisis_IST.txt (Laporan ringkasan)\n")
cat("  - laporan_psikometrik_IST.Rmd    (File source Rmd)       \n")
cat("  - reliabilitas_summary.csv                                \n")
cat("  - item_quality_summary.csv       (ringkasan kualitas)    \n")
cat("  - item_analysis_[SUBTES].csv     (per subtes)            \n")
cat("  - inter_item_cor_[SUBTES].csv    (korelasi inter-item)   \n")
cat("  - irt_params_[MODEL]_[SUBTES].csv                        \n")
cat("  - korelasi_subtes.csv                                     \n")
cat("  - grafik/                         (Semua grafik PDF)      \n")
cat("============================================================\n")
