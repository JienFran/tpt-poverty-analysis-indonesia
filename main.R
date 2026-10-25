## =========================
## DATASET
## =========================
Provinsi <- c(
  "ACEH","SUMATERA UTARA","SUMATERA BARAT","RIAU","JAMBI","SUMATERA SELATAN",
  "BENGKULU","LAMPUNG","KEP. BANGKA BELITUNG","KEP. RIAU","DKI JAKARTA",
  "JAWA BARAT","JAWA TENGAH","DI YOGYAKARTA","JAWA TIMUR","BANTEN","BALI",
  "NUSA TENGGARA BARAT","NUSA TENGGARA TIMUR","KALIMANTAN BARAT",
  "KALIMANTAN TENGAH","KALIMANTAN SELATAN","KALIMANTAN TIMUR",
  "KALIMANTAN UTARA","SULAWESI UTARA","SULAWESI TENGAH","SULAWESI SELATAN",
  "SULAWESI TENGGARA","GORONTALO","SULAWESI BARAT","MALUKU","MALUKU UTARA",
  "PAPUA BARAT","PAPUA BARAT DAYA","PAPUA","PAPUA SELATAN","PAPUA TENGAH",
  "PAPUA PEGUNUNGAN"
)

TPT <- c(
  5.64,5.32,5.62,4.16,4.26,3.69,3.41,4.21,4.45,6.45,6.05,
  6.77,4.66,3.46,3.88,6.69,1.49,3.06,3.31,4.82,3.97,4.16,
  5.18,3.85,5.99,2.92,4.21,3.31,3.42,2.86,6.27,4.55,4.55,
  6.85,6.96,4.04,3.62,1.68
)

Kemiskinan <- c(
  12.33,7.36,5.35,6.16,7.19,10.15,12.08,10.00,5.00,4.44,4.28,
  7.02,9.48,10.23,9.50,5.63,3.72,11.78,18.60,6.16,5.19,3.84,
  5.17,5.54,6.71,10.92,7.60,10.54,13.24,10.41,15.38,5.81,
  20.66,17.95,19.16,19.71,28.90,30.03
)

data <- data.frame(Provinsi, TPT, Kemiskinan)

## =======================================================
## 1. STATISTIK DESKRIPTIF
## =======================================================
summary(data)

var(TPT); sd(TPT)
var(Kemiskinan); sd(Kemiskinan)

# Koefisien variasi
CV_TPT <- sd(TPT)/mean(TPT)
CV_Kemiskinan <- sd(Kemiskinan)/mean(Kemiskinan)

# Skewness & kurtosis manual (Base R)
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  s3 <- sd(x)^3
  m3/s3
}
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  s4 <- sd(x)^4
  m4/s4 - 3
}

skew(TPT); kurt(TPT)
skew(Kemiskinan); kurt(Kemiskinan)

# Histogram
hist(TPT, main="Histogram TPT")
hist(Kemiskinan, main="Histogram Kemiskinan")

# Scatterplot
plot(TPT, Kemiskinan, main="Scatterplot TPT vs Kemiskinan",
     xlab="TPT", ylab="Kemiskinan")

## =======================================================
## 2. UJI NORMALITAS
## =======================================================
shapiro.test(TPT)
shapiro.test(Kemiskinan)

## Residual normality (setelah model regresi)
model <- lm(Kemiskinan ~ TPT)
res <- residuals(model)
shapiro.test(res)

## =======================================================
## 3. ANALISIS KORELASI
## =======================================================
# Pearson
cor.test(TPT, Kemiskinan, method="pearson")

# Spearman
cor.test(TPT, Kemiskinan, method="spearman")

## =======================================================
## 4. UJI INFERENSI SATU POPULASI
## Contoh: apakah mean kemiskinan berbeda dari 10%?
## =======================================================
mu0 <- 10
t.test(Kemiskinan, mu = mu0)

## =======================================================
## 5. UJI INFERENSI DUA POPULASI
## Kategori berdasarkan rata-rata TPT nasional
## =======================================================
meanTPT <- mean(TPT)

Kelompok <- ifelse(TPT > meanTPT, "Tinggi", "Rendah")
data$Kelompok <- as.factor(Kelompok)

Kemiskinan_tinggi <- data$Kemiskinan[data$Kelompok=="Tinggi"]
Kemiskinan_rendah <- data$Kemiskinan[data$Kelompok=="Rendah"]

# Variansi
var.test(Kemiskinan_tinggi, Kemiskinan_rendah)

# Welch t-test
t.test(Kemiskinan_tinggi, Kemiskinan_rendah)

## =======================================================
## 6. REGRESI LINEAR + OUTPUT MODEL
## =======================================================
summary(model)
confint(model)

plot(TPT, Kemiskinan)
abline(model, lwd=2)

## =======================================================
## 7. ANALISIS RESIDU
## =======================================================
# Residual vs Fitted
plot(model$fitted.values, res,
     main="Residual vs Fitted",
     xlab="Fitted", ylab="Residuals")

# Cook’s Distance
plot(cooks.distance(model), main="Cook's Distance")

## =======================================================
## 8. UJI CHI-SQUARE (Kategori)
## =======================================================
# BUAT kategori 2 tingkat kemiskinan agar frekuensi mencukupi
KemKategori <- ifelse(Kemiskinan > median(Kemiskinan), "Tinggi", "Rendah")
data$KemKategori <- as.factor(KemKategori)

tab <- table(data$Kelompok, data$KemKategori)
tab

chisq.test(tab)