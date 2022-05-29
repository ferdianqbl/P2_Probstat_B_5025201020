# P2_Probstat_B_5025201020
## Laporan Praktikum Modul 2 Probabilitas dan Statistik 2022

**Nama  : Muhammad Ferdian Iqbal**

**NRP   : 5025201020**

**Kelas : B**

***

## **Soal 1**
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

![1](screenshots/1.jpg)


``` R
x_data <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
y_data <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)
dif<- c()
for(i in 1:9)
  dif[i] <- y_data[i] - x_data[i]
```

### A.
> Standar deviasi selisih.

``` R
sd(x_data)
sd(y_data)
sd(dif)
```

![1.a](screenshots/1A.jpg)

Standar deviasi dapat dicari dengan fungsi **sd** pada R. Pada kode tersebut, terdapat standar deviasi sebelum beraktivitas dan sesudah aktivitas. Kemudian, ada juga standar deviasi dari selisih keduanya

### B.
>Carilah nilai t (p-value)

``` R
mean_dif = mean(dif)
mean_sample_data = mean(dif[1:6])
total_n = 6
total_sd = sd(dif[1:6])
t_val = (mean_sample_data - mean_dif) / (total_sd / sqrt(total_n))
p_val = pt(t_val, total_n-1, lower.tail = TRUE)
p_val


```

![1.b](screenshots/1B.jpg)

- P-value dari t score dapat dicari dengan mengetahui t score dahulu. Sebelum menghitung T score, kita harus mengetahui hasi dari pengurangan rata-rata sampel data `mean_sample_data` dengan rata-rata semua data `mean_dif`. Kemudian, kita juga harus mengetahui hasil dari standar deviasi sampel data `total_sd` dengan akar dari total sampel `total_sd`. Hasil dari keduanya dibagi dan menghasilkan t score `t_val`.
- Di sini saya menggunakan dua cara untuk mencari p-value. p-value pertama `0.4708124` saya peroleh dengan perhitungan manual dengan mencari mean dari sampel data dan standar deviasi dari sampel data dahulu. Kemudian, saya juga mencari p-value dengan menggunakan fungsi `t.test` yang menghasilkan p-value 0.9998

### C.
> tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”

``` R
var.test(x_data, y_data)
t.test(dif, alternative = 'two.sided', mu = mean_dif)
t.test(x_data, y_data, var.equal = TRUE)
```

![1.c](screenshots/1_C.jpg)
![1.c](screenshots/1__C.jpg)
![1.c](screenshots/1C.jpg)

Pada `var.test` merupakan perbandingan antara data `x_data` dan `y_-_data`. Lalu, dilakukan `t.test` saat dengan parameter `x_data` data sebelum aktivitas dan `y_data` data setelah aktivitas. Selanjutnya juga dilakukan `t.test` dengan parameter `dif` data selisih tiap data. Hasilnya adalah pada `t.test` pertama dan kedua memiliki hasil yang mirip dengan `t.test` 1B. Sehingga dapat disimpulkan tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen.

***

## **Soal 2**
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer. 

### A.
> Apakah Anda setuju dengan klaim tersebut?

### B.
> Jelaskan maksud dari output yang dihasilkan!
### C.
> Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!

***

## **Soal 3**
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya didapatkanlah data berikut dari perusahaan saham tersebut.
![3](screenshots/3.jpg)
Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)?

### A.
> H0 dan H1
### B. 
> Hitung Sampel Statistik
## C.
> Lakukan Uji Statistik (df =2)
### D.
> Nilai Kritikal
### E.
> Keputusan
### F.
> Kesimpulan

***
## **Soal 4**
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
kucing putih dengan panjangnya masing-masing.
Jika :
diketahui dataset https://intip.in/datasetprobstat1
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama

### A.
> Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.
### B.
> carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?
### C.
> Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.
### D. 
> Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
### E.
> Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain?

### F.
> Visualisasikan data dengan ggplot2

***
## **Soal 5**
Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan
dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil Eksperimen.

### A.
> Buatlah plot sederhana untuk visualisasi data
### B.
> Lakukan uji ANOVA dua arah
### C.
> Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
### D.
> Lakukan uji Tukey
### E.
> Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey