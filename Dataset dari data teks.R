#Mengimport Data Teks
data <- read.table(
  file = "https://storage.googleapis.com/dqlab-dataset/datalahir_teks_dqlab.txt",
  header = FALSE,
  sep = "\n",
  na.strings = c("NA","N/A",""),
  col.names = 'data_list',
  skip = 1
)
data

#Memisahkan data ke kolom
data <- strsplit(data$data_list, split = "|||", fixed = TRUE)

#Mengubah data ke dataframe
df <- data.frame(matrix(unlist(data),nrow = length(data),byrow = TRUE))
#Memberikan nama pada kolom
colnames(df) <- c('Nama', 'Tempat_Lahir', 'Tanggal_Lahir', 'Provinsi')
#Menggabungkan kolom 
df$kota_provinsi <- paste(df$Tempat_Lahir,',',df$Provinsi)
df

#Menghapus angka di kolom nama
df$Nama <- gsub('[A-Za-z]','',df$Nama)
df
