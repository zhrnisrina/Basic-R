setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/AED/Setelah UTS/Final Project")

library("readxl")
library(ggplot2)

data1kab <- read_excel("Pertumbuhan Ekonomi Menurut Kabupaten_Kota.xlsx", sheet = 1)
data1kot <- read_excel("Pertumbuhan Ekonomi Menurut Kabupaten_Kota.xlsx", sheet = 2)
data1 <- read_excel("Pertumbuhan Ekonomi Menurut Kabupaten_Kota.xlsx", sheet = 3)
data2 <- read_excel("PDRB Menurut Lapangan Usaha (17 Sektor) Triwulanan .xlsx", sheet = 2) 
data3 <- read_excel("PDRB Menurut Pengeluaran Triwulanan.xlsx", sheet = 2)
data3kom <- read_excel("PDRB Menurut Pengeluaran Triwulanan.xlsx", sheet = 3)
data3kde <- read_excel("PDRB Menurut Pengeluaran Triwulanan.xlsx", sheet = 4)

data1kab[33, ]
data1kot[7, ]

avpdrb <- data.frame(c(2019,2020,2021,2019,2020,2021), c(4.94, -2.47, 2.86, 5.91, -4.39, 4.97), c("Kabupaten", "Kabupaten","Kabupaten","Kota","Kota","Kota"))
colnames(avpdrb) <- c("Tahun", "PDRB","KabupatenKota")

ggplot(avpdrb, aes(x=Tahun, y=PDRB, group=KabupatenKota)) +
  labs(caption = "Source : https://jatim.bps.go.id/statictable/2021/05/28/2145/laju-pertumbuhan-produk-domestik-regional-bruto-\natas-dasar-harga-konstan-2010-menurut-kabupaten-kota-di-provinsi-jawa-timur-persen-2017-2020.html") +
  geom_line(aes(color=KabupatenKota), size = 2) + geom_point(aes(color=KabupatenKota), size = 3) +
  ggtitle("Rata-Rata Pertumbuhan Ekonomi") + labs(y = "Laju Pertumbuhan") + theme(
    plot.title = element_text(color = "#990000", size = 14, face = "bold"),
    axis.title.x = element_text(color="#B22727", size=12),
    axis.title.y = element_text(color="#B22727", size=12),
    plot.caption = element_text(face="italic", size=6.5),
    panel.background = element_rect(fill = "#F7E9D7") 
  ) + scale_x_continuous(limits = c(2019,2021), breaks = c(2019,2020,2021)) +
  scale_color_manual(values=c("#FF8C8C", "#A25B5B")) + theme(legend.position="bottom")

ggplot(data1, aes(reorder(x=KabupatenKota, Rata2), y=Rata2)) + xlab("Kabupaten/Kota") +
  labs(caption = "Source : https://jatim.bps.go.id/statictable/2021/05/28/2145/laju-pertumbuhan-produk-domestik-regional-bruto-\natas-dasar-harga-konstan-2010-menurut-kabupaten-kota-di-provinsi-jawa-timur-persen-2017-2020.html") +
  ggtitle("Rata-Rata Pertumbuhan Ekonomi (2019-2021)") +geom_bar(stat="identity", width=0.6, fill = "#FF8C8C", color = "#990000") + 
  coord_flip() + theme(
    plot.title = element_text(color = "#990000", size = 10, face = "bold"),
    axis.title.x = element_text(color="#B22727", size=12),
    axis.title.y = element_text(color="#B22727", size=12),
    plot.caption = element_text(face="italic", size=6.5),
    axis.text = element_text(size=5, color = "black"),
    panel.background = element_blank()
  )

ggplot(data2, aes(x= Harga, y= Total)) + geom_violin(fill="#F2D1D1", color = "#8E3200") + ylim(0,600000) +
  labs(title="PDRB Menurut Lapangan Usaha (17 Sektor)",
       subtitle="per Harga Dasar dan Harga Konstan Triwulanan",
       x="Jenis Harga",
       y="PDRB",
       caption="Source: https://jatim.bps.go.id/indicator/52/479/1/-\nseri-2010-pdrb-menurut-lapangan-usaha-17-sektor-triwulanan-.html") +
  theme(plot.title = element_text(color = "#990000", size = 13, face = "bold"),
        axis.title.x = element_text(color="#B22727", size=12),
        axis.title.y = element_text(color="#B22727", size=12),
        plot.caption = element_text(face="italic", size=8),
        panel.background = element_rect(fill = "#F2EBE9")
  ) +scale_color_manual(values=c("#FF8C8C", "#A25B5B"))

ggplot(data3kde,aes(Total, colour = Triwulan, fill = Triwulan)) + geom_density(alpha = 0.5, col="maroon") +
  labs(title="Kernel Density Estimation",
       subtitle="PDRB Menurut Pengeluaran Triwulanan",
       caption="Source: https://jatim.bps.go.id/indicator/52/505/2/-\nseri-2010-pdrb-menurut-pengeluaran-triwulanan.html") +
  theme(plot.title = element_text(color = "#990000", size = 13, face = "bold"),
        axis.title.x = element_text(color="#B22727", size=12),
        axis.title.y = element_text(color="#B22727", size=12),
        plot.caption = element_text(face="italic", size=8),
        panel.background = element_rect(fill = "#F9F9F9")
  ) + scale_fill_brewer(palette="YlOrRd")

ggplot(data3kom, aes(x=Komponen, y=Total, group = 1)) +
  geom_line(color = "#733C3C", size = 1) + geom_point(size = 3, color ="#990000") +
  labs(title="PDRB Menurut Pengeluaran Triwulanan",
       subtitle="(Triwulan l 2021 - Triwulan l 2022)",
       x="Triwulanan",
       y="PDRB Total",
       caption="Source: https://jatim.bps.go.id/indicator/52/505/2/-\nseri-2010-pdrb-menurut-pengeluaran-triwulanan.html") + theme(
    plot.title = element_text(color = "#990000", size = 13, face = "bold"),
    axis.title.x = element_text(color="#B22727", size=12),
    axis.title.y = element_text(color="#B22727", size=12),
    plot.caption = element_text(face="italic", size=8),
    axis.text = element_text(size=6.5, color = "black"),
    panel.background = element_rect(fill = "#F7E9D7")
  )

ggplot(data3, aes(x=Harga, y=PDRB)) + geom_boxplot(fill="#AC7D88", notch = TRUE) + ylim(0,390000) +
  labs(title="PDRB Menurut Pengeluaran Triwulanan",
       subtitle="per Harga Dasar dan Harga Konstan",
       x="Jenis Harga",
       y="PDRB",
       caption="Source: https://jatim.bps.go.id/indicator/52/505/2/-\nseri-2010-pdrb-menurut-pengeluaran-triwulanan.html") +
  theme(plot.title = element_text(color = "#990000", size = 13, face = "bold"),
        axis.title.x = element_text(color="#B22727", size=12),
        axis.title.y = element_text(color="#B22727", size=12),
        plot.caption = element_text(face="italic", size=8),
        panel.background = element_rect(fill = "#EDE6DB")
  ) +scale_color_manual(values=c("#FF8C8C", "#A25B5B"))

