penjualan_permen = read.csv("C:/Users/LENOVO/OneDrive/Documents/Praktek/tingkat_penjualan_kota_x_dqlab.tsv", header = TRUE, sep = "\t")
kunjungan_dokter = read.csv("C:/Users/LENOVO/OneDrive/Documents/Praktek/kunjungan_dokter_gigi_kota_x_dqlab.tsv", header = TRUE, sep = "\t")
print(penjualan_permen)
print(kunjungan_dokter)

data_gabungan = merge(kunjungan_dokter, penjualan_permen, by.x = c("Tahun", "Bulan"), by.y = c("Tahun", "Bulan"), sort = FALSE)
data_gabungan
summary(data_gabungan$tingkat.kunjungan.ke.dokter.gigi)
summary(data_gabungan$penjualan.permen)
summary(data_gabungan$penjualan.sereal)
summary(data_gabungan$penjualan.buah.pisang)

#Melakukan explorasi data
plot(data_gabungan$tingkat.kunjungan.ke.dokter.gigi, data_gabungan$penjualan.permen, 
     pch = 19,
     xlab = "Kunjangan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan dokter dengan penjualan permen",
     col = 'blue')
#Melakukan explorasi data
plot(data_gabungan$tingkat.kunjungan.ke.dokter.gigi, data_gabungan$penjualan.sereal, 
     pch = 19,
     xlab = "Kunjungan Dokter", 
     ylab = "Penjualan Sereal",
     main = "Kunjungan Dokter dengan Penjualan Sereal",
     col = 'red')
#Melakukan explorasi data
plot(data_gabungan$tingkat.kunjungan.ke.dokter.gigi, data_gabungan$penjualan.buah.pisang, 
     pch = 19,
     xlab = "Kunjungan Dokter", 
     ylab = "Penjualan Buah Pisang",
     main = "Kunjungan Dokter dengan Penjualan Buah Pisang",
     col = "yellow")

#melakukan delay efect
library(dplyr)
data_delayed_effect = data.frame(bulan=data_gabungan$Bulan, tahun=data_gabungan$Tahun, kunjungan_dokter=data_gabungan$tingkat.kunjungan.ke.dokter.gigi, penjualan_permen=data_gabungan$penjualan.permen,
                                 penjualan_permen_1 = lag(data_gabungan$penjualan.permen),
                                 penjualan_permen_2 = lag(data_gabungan$penjualan.permen,2),
                                 penjualan_permen_3 = lag(data_gabungan$penjualan.permen,3),
                                 penjualan_permen_4 = lag(data_gabungan$penjualan.permen,4),
                                 penjualan_permen_5 = lag(data_gabungan$penjualan.permen,5),
                                 penjualan_permen_6 = lag(data_gabungan$penjualan.permen,6),
                                 penjualan_permen_7 = lag(data_gabungan$penjualan.permen,7),
                                 penjualan_permen_8 = lag(data_gabungan$penjualan.permen,8),
                                 penjualan_permen_9 = lag(data_gabungan$penjualan.permen,9),
                                 penjualan_permen_10 = lag(data_gabungan$penjualan.permen,10),
                                 penjualan_permen_11 = lag(data_gabungan$penjualan.permen,11),
                                 penjualan_permen_12 = lag(data_gabungan$penjualan.permen,12))
data_delayed_effect

#memvisualisasikan dengan scatterplot
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_1,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 1 bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_2,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 2 bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_3,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 3bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_4,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 4bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_5,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 5bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_6,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 6bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_7,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 7bulan",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_8,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 8bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_9,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 9bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_10,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 10bulan",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_11,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 11bulan)",
     col = 'blue')
plot(data_delayed_effect$kunjungan_dokter, data_delayed_effect$penjualan_permen_12,
     pch = 19,
     xlab = "Kunjungan Dokter",
     ylab = "Penjualan Permen",
     main = "Kunjungan Dokter dengan Penjualan Permen (delay 12)",
     col = 'blue')

#Melakukan Analisis regresi Penjualan Permen
data_regresi = data.frame(bulan = data_delayed_effect$bulan, tahun = data_delayed_effect$tahun, kunjungan_dokter = data_delayed_effect$kunjungan_dokter, penjualan_permen = data_delayed_effect$penjualan_permen_7)
data_regresi = na.omit(data_regresi)
model = lm(kunjungan_dokter~penjualan_permen, data = data_regresi)
summary(model) 



