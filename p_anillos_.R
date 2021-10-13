
library(tidyverse)
library(datapasta)
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))
library(patchwork)
library(ragg)

# disparos ----------

df <- tibble::tribble(
                  ~jugador, ~tiros, ~puerta, ~reg_exito, ~reg_fallidos,
                  "RUIBAL",     2L,      1L,         5L,            6L,
                "GUARDADO",     1L,      0L,         0L,            1L,
                "IGLESIAS",    10L,      5L,         2L,            3L,
                   "BRAVO",     0L,      0L,         0L,            0L,
                   "TELLO",     5L,      1L,         3L,            4L,
                  "LAINEZ",     0L,      0L,         0L,            0L,
                "EDGAR",     2L,      0L,         3L,            0L,
                 "DELGADO",     0L,      0L,         0L,            0L,
                "PEZZELLA",     2L,      0L,         1L,            0L,
                   "GUIDO",     4L,      3L,         2L,            0L,
                "BELLERÍN",     0L,      0L,         5L,            2L,
                 "JOAQUÍN",     3L,      2L,         2L,            2L,
                "CALDERÓN",     0L,      0L,         0L,            0L,
                 "MIRANDA",     1L,      0L,         1L,            2L,
                  "JUANMI",     8L,      5L,         6L,            1L,
                 "HERMOSO",     2L,      1L,         1L,            0L,
                   "LOREN",     NA,      NA,         NA,            NA,
                  "BARTRA",     0L,      0L,         0L,            0L,
                 "MONTOYA",     3L,      2L,         0L,            0L,
                   "FEKIR",    21L,     11L,        25L,           17L,
                    "PAUL",     1L,      0L,         1L,            0L,
        "ROBER GONZÁLEZ",     1L,      0L,         1L,            2L,
                   "RODRI",     8L,      4L,         8L,            4L,
               "RUI SILVA",     0L,      0L,         0L,            0L,
                 "CANALES",     7L,      4L,        10L,            6L,
                  "SIDNEI",     0L,      0L,         0L,            0L,
                "CAMARASA",     1L,      0L,         0L,            1L,
             "V. RUIZ",     1L,      0L,         0L,            1L,
                "CARVALHO",     5L,      2L,         8L,            4L,
               "WILLIAN J",     8L,      6L,         2L,            4L,
            "YASSIN FEKIR",     0L,      0L,         0L,            0L,
                "A. MORENO",     2L,      1L,         2L,            6L
        )

hsize = 2

df <- df %>% 
  mutate(tiros_totales = tiros+puerta,
         regates_totales = reg_exito+reg_fallidos,
         acierto_tiros = (puerta/tiros_totales)*100,
         acierto_regate = (reg_exito/regates_totales)*100) %>% 
  filter(acierto_tiros > 15) %>% 
  mutate(fallo_tiros = 100-acierto_tiros,
         fallo_regate = 100-acierto_regate) %>% 
  relocate(fallo_tiros, .after = acierto_tiros) %>% 
  #pivot_longer(10:11, names_to = "regates", values_to = "percentage_regates") %>% 
  mutate(x = hsize) %>% 
pivot_longer(8:9, names_to = "disparos", values_to = "percentage_disparos") 

df$jugador<-factor(df$jugador, levels = unique(df$jugador[order(df$tiros_totales)]))
  
  
p1 <- ggplot(df, aes(x = hsize, y = percentage_disparos, fill = as.factor(disparos))) +
  geom_col(color="white") +
  coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percentage_disparos),"%")),
            size = 1.5,
            fontface="bold",
            position = position_stack(vjust = 0.5))+
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(jugador~tiros_totales,ncol = 4) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  theme(legend.background = element_rect(fill = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(colour = "white",face="bold"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white"))+
  scale_fill_manual(values = c("#7bc043", "#f73225"),labels = c("Éxito", "Fallo"))+
  labs(title = "Nº DE DISPAROS",
       subtitle = "",
       caption="")+
  theme(plot.title = element_text(colour = "white",face="bold",size=12, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,face="italic",hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))

# REGATES -------------------------


df <- tibble::tribble(
  ~jugador, ~tiros, ~puerta, ~reg_exito, ~reg_fallidos,
  "RUIBAL",     2L,      1L,         5L,            6L,
  "GUARDADO",     1L,      0L,         0L,            1L,
  "IGLESIAS",    10L,      5L,         2L,            3L,
  "BRAVO",     0L,      0L,         0L,            0L,
  "TELLO",     5L,      1L,         3L,            4L,
  "LAINEZ",     0L,      0L,         0L,            0L,
  "EDGAR",     2L,      0L,         3L,            0L,
  "DELGADO",     0L,      0L,         0L,            0L,
  "PEZZELLA",     2L,      0L,         1L,            0L,
  "GUIDO",     4L,      3L,         2L,            0L,
  "BELLERÍN",     0L,      0L,         5L,            2L,
  "JOAQUÍN",     3L,      2L,         2L,            2L,
  "CALDERÓN",     0L,      0L,         0L,            0L,
  "MIRANDA",     1L,      0L,         1L,            2L,
  "JUANMI",     8L,      5L,         6L,            1L,
  "HERMOSO",     2L,      1L,         1L,            0L,
  "LOREN",     NA,      NA,         NA,            NA,
  "BARTRA",     0L,      0L,         0L,            0L,
  "MONTOYA",     3L,      2L,         0L,            0L,
  "FEKIR",    21L,     11L,        25L,           17L,
  "PAUL",     1L,      0L,         1L,            0L,
  "ROBER GONZÁLEZ",     1L,      0L,         1L,            2L,
  "RODRI",     8L,      4L,         8L,            4L,
  "RUI SILVA",     0L,      0L,         0L,            0L,
  "CANALES",     7L,      4L,        10L,            6L,
  "SIDNEI",     0L,      0L,         0L,            0L,
  "CAMARASA",     1L,      0L,         0L,            1L,
  "V. RUIZ",     1L,      0L,         0L,            1L,
  "CARVALHO",     5L,      2L,         8L,            4L,
  "WILLIAN J",     8L,      6L,         2L,            4L,
  "YASSIN FEKIR",     0L,      0L,         0L,            0L,
  "A. MORENO",     2L,      1L,         2L,            6L
)

hsize = 2

df <- df %>% 
  mutate(tiros_totales = tiros+puerta,
         regates_totales = reg_exito+reg_fallidos,
         acierto_tiros = (puerta/tiros_totales)*100,
         acierto_regate = (reg_exito/regates_totales)*100) %>% 
  filter(regates_totales > 4) %>% 
  mutate(fallo_tiros = 100-acierto_tiros,
         fallo_regate = 100-acierto_regate) %>% 
  relocate(fallo_tiros, .after = acierto_tiros) %>% 
  pivot_longer(10:11, names_to = "regates", values_to = "percentage_regates") %>% 
  mutate(x = hsize) 

df$jugador<-factor(df$jugador, levels = unique(df$jugador[order(df$regates_totales)]))


p2 <- ggplot(df, aes(x = hsize, y = percentage_regates, fill = as.factor(regates))) +
  geom_col(color="white") +
  coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percentage_regates),"%")),
            size = 1.5,
            fontface="bold",
            position = position_stack(vjust = 0.5))+
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(jugador~regates_totales,ncol = 4) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  theme(legend.background = element_rect(fill = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(colour = "white",face="bold"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white"))+
  scale_fill_manual(values = c("#7bc043", "#f73225"),labels = c("Éxito", "Fallo"))+
  labs(title = "Nº DE REGATES",
       subtitle = "",
       caption="")+
  theme(plot.title = element_text(colour = "white",face="bold",size=12, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,face="italic",hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))


# entradas -------------

df <- tibble::tribble(
                  ~jugador, ~entradas_exito, ~entradas_falladas, ~duelos_exito, ~duelos_fallados, ~duelos_aereos_exito, ~duelos_aereos_fallados,
                  "RUIBAL",              6L,                 2L,           20L,              34L,                   2L,                      4L,
                "GUARDADO",              0L,                 1L,            5L,              18L,                   4L,                     10L,
                "IGLESIAS",              1L,                 0L,           15L,              19L,                   4L,                      8L,
                   "BRAVO",              0L,                 0L,            0L,               0L,                   0L,                      0L,
                   "TELLO",              2L,                 2L,           10L,              15L,                   1L,                      1L,
                  "LAINEZ",              0L,                 0L,            0L,               0L,                   0L,                      0L,
                "EDGAR",              9L,                 1L,           35L,              18L,                  19L,                     10L,
                 "DELGADO",              0L,                 0L,            0L,               0L,                   0L,                      0L,
                "PEZZELLA",              2L,                 3L,           16L,              18L,                  10L,                      5L,
                   "GUIDO",             10L,                 9L,           45L,              26L,                  10L,                      2L,
                "BELLERÍN",              2L,                 5L,           22L,               9L,                   6L,                      3L,
                 "JOAQUÍN",              0L,                 0L,            4L,               7L,                   1L,                      2L,
                "CALDERÓN",              0L,                 2L,            3L,               5L,                   0L,                      2L,
                 "MIRANDA",              5L,                 3L,           18L,              13L,                   6L,                      3L,
                  "JUANMI",              7L,                 0L,           18L,              16L,                   2L,                      3L,
                 "HERMOSO",              1L,                 1L,            9L,               3L,                   6L,                      2L,
                   "LOREN",              1L,                 0L,            1L,               0L,                   0L,                      0L,
                  "BARTRA",              0L,                 0L,            0L,               0L,                   0L,                      0L,
                 "MONTOYA",              9L,                 5L,           27L,              12L,                   7L,                      3L,
                   "FEKIR",              5L,                 3L,           58L,              73L,                   5L,                     10L,
                    "PAUL",              1L,                 1L,            5L,               5L,                   2L,                      0L,
        "ROBER GONZÁLEZ",              1L,                 0L,            5L,               4L,                   1L,                      1L,
                   "RODRI",              5L,                 1L,           19L,              15L,                   0L,                      1L,
               "RUI SILVA",              0L,                 0L,            0L,               1L,                   0L,                      0L,
                 "CANALES",              2L,                 1L,           18L,              23L,                   0L,                      4L,
                  "SIDNEI",              0L,                 0L,            0L,               0L,                   0L,                      0L,
                "CAMARASA",              1L,                 1L,            6L,               4L,                   2L,                      1L,
             "V. RUIZ",              3L,                 1L,           17L,              13L,                  13L,                     10L,
                "CARVALHO",             12L,                 4L,           35L,              20L,                   7L,                      2L,
               "WILLIAN J",              2L,                 0L,           11L,              15L,                   5L,                      2L,
            "YASSIN FEKIR",              0L,                 0L,            0L,               0L,                   0L,                      0L,
                "A. MORENO",              4L,                 5L,           22L,              18L,                   6L,                      5L
        )


hsize = 2


df <- df %>% 
  mutate(entradas_totales = entradas_exito+entradas_falladas,
         acierto_entradas = (entradas_exito/entradas_totales)*100) %>% 
  filter(entradas_totales > 5) %>% 
  mutate(fallo_entradas = 100-acierto_entradas) %>% 
  #relocate(fallo_tiros, .after = acierto_tiros) %>% 
  pivot_longer(9:10, names_to = "entradas", values_to = "percentage_entradas") %>% 
  mutate(x = hsize) 


df$jugador<-factor(df$jugador, levels = unique(df$jugador[order(df$entradas_totales)]))


p3 <- ggplot(df, aes(x = hsize, y = percentage_entradas, fill = as.factor(entradas))) +
  geom_col(color="white") +
  coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percentage_entradas),"%")),
            size = 1.5,
            fontface="bold",
            position = position_stack(vjust = 0.5))+
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(jugador~entradas_totales,ncol = 4) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  theme(legend.background = element_rect(fill = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(colour = "white",face="bold"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white"))+
  scale_fill_manual(values = c("#7bc043", "#f73225"),labels = c("Éxito", "Fallo"))+
  labs(title = "Nº DE ENTRADAS",
       subtitle = "",
       caption="")+
  theme(plot.title = element_text(colour = "white",face="bold",size=12, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,face="italic",hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))


# DUELOS ---------------

df <- tibble::tribble(
  ~jugador, ~entradas_exito, ~entradas_falladas, ~duelos_exito, ~duelos_fallados, ~duelos_aereos_exito, ~duelos_aereos_fallados,
  "RUIBAL",              6L,                 2L,           20L,              34L,                   2L,                      4L,
  "GUARDADO",              0L,                 1L,            5L,              18L,                   4L,                     10L,
  "IGLESIAS",              1L,                 0L,           15L,              19L,                   4L,                      8L,
  "BRAVO",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "TELLO",              2L,                 2L,           10L,              15L,                   1L,                      1L,
  "LAINEZ",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "EDGAR",              9L,                 1L,           35L,              18L,                  19L,                     10L,
  "DELGADO",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "PEZZELLA",              2L,                 3L,           16L,              18L,                  10L,                      5L,
  "GUIDO",             10L,                 9L,           45L,              26L,                  10L,                      2L,
  "BELLERÍN",              2L,                 5L,           22L,               9L,                   6L,                      3L,
  "JOAQUÍN",              0L,                 0L,            4L,               7L,                   1L,                      2L,
  "CALDERÓN",              0L,                 2L,            3L,               5L,                   0L,                      2L,
  "MIRANDA",              5L,                 3L,           18L,              13L,                   6L,                      3L,
  "JUANMI",              7L,                 0L,           18L,              16L,                   2L,                      3L,
  "HERMOSO",              1L,                 1L,            9L,               3L,                   6L,                      2L,
  "LOREN",              1L,                 0L,            1L,               0L,                   0L,                      0L,
  "BARTRA",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "MONTOYA",              9L,                 5L,           27L,              12L,                   7L,                      3L,
  "FEKIR",              5L,                 3L,           58L,              73L,                   5L,                     10L,
  "PAUL",              1L,                 1L,            5L,               5L,                   2L,                      0L,
  "ROBER GONZÁLEZ",              1L,                 0L,            5L,               4L,                   1L,                      1L,
  "RODRI",              5L,                 1L,           19L,              15L,                   0L,                      1L,
  "RUI SILVA",              0L,                 0L,            0L,               1L,                   0L,                      0L,
  "CANALES",              2L,                 1L,           18L,              23L,                   0L,                      4L,
  "SIDNEI",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "CAMARASA",              1L,                 1L,            6L,               4L,                   2L,                      1L,
  "V. RUIZ",              3L,                 1L,           17L,              13L,                  13L,                     10L,
  "CARVALHO",             12L,                 4L,           35L,              20L,                   7L,                      2L,
  "WILLIAN J",              2L,                 0L,           11L,              15L,                   5L,                      2L,
  "YASSIN FEKIR",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "A. MORENO",              4L,                 5L,           22L,              18L,                   6L,                      5L
)


hsize = 2


df <- df %>% 
  mutate(duelos_totales = duelos_exito+duelos_fallados,
         acierto_duelos = (duelos_exito/duelos_totales)*100) %>% 
  filter(duelos_totales > 25) %>% 
  mutate(fallo_duelos = 100-acierto_duelos) %>% 
  #relocate(fallo_tiros, .after = acierto_tiros) %>% 
  pivot_longer(9:10, names_to = "duelos", values_to = "percentage_duelos") %>% 
  mutate(x = hsize) 


df$jugador<-factor(df$jugador, levels = unique(df$jugador[order(df$duelos_totales)]))


p4 <- ggplot(df, aes(x = hsize, y = percentage_duelos, fill = as.factor(duelos))) +
  geom_col(color="white") +
  coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percentage_duelos),"%")),
            size=1.5,
            fontface="bold",
            position = position_stack(vjust = 0.5))+
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(jugador~duelos_totales,ncol = 4) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  theme(legend.background = element_rect(fill = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(colour = "white",face="bold"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white"))+
  scale_fill_manual(values = c("#7bc043", "#f73225"),labels = c("Éxito", "Fallo"))+
  labs(title = "Nº DE DUELOS",
       subtitle = "",
       caption="")+
  theme(plot.title = element_text(colour = "white",face="bold",size=12, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,face="italic",hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))

# duelos aéreos ---------------------------------

df <- tibble::tribble(
  ~jugador, ~entradas_exito, ~entradas_falladas, ~duelos_exito, ~duelos_fallados, ~duelos_aereos_exito, ~duelos_aereos_fallados,
  "RUIBAL",              6L,                 2L,           20L,              34L,                   2L,                      4L,
  "GUARDADO",              0L,                 1L,            5L,              18L,                   4L,                     10L,
  "IGLESIAS",              1L,                 0L,           15L,              19L,                   4L,                      8L,
  "BRAVO",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "TELLO",              2L,                 2L,           10L,              15L,                   1L,                      1L,
  "LAINEZ",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "EDGAR",              9L,                 1L,           35L,              18L,                  19L,                     10L,
  "DELGADO",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "PEZZELLA",              2L,                 3L,           16L,              18L,                  10L,                      5L,
  "GUIDO",             10L,                 9L,           45L,              26L,                  10L,                      2L,
  "BELLERÍN",              2L,                 5L,           22L,               9L,                   6L,                      3L,
  "JOAQUÍN",              0L,                 0L,            4L,               7L,                   1L,                      2L,
  "CALDERÓN",              0L,                 2L,            3L,               5L,                   0L,                      2L,
  "MIRANDA",              5L,                 3L,           18L,              13L,                   6L,                      3L,
  "JUANMI",              7L,                 0L,           18L,              16L,                   2L,                      3L,
  "HERMOSO",              1L,                 1L,            9L,               3L,                   6L,                      2L,
  "LOREN",              1L,                 0L,            1L,               0L,                   0L,                      0L,
  "BARTRA",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "MONTOYA",              9L,                 5L,           27L,              12L,                   7L,                      3L,
  "FEKIR",              5L,                 3L,           58L,              73L,                   5L,                     10L,
  "PAUL",              1L,                 1L,            5L,               5L,                   2L,                      0L,
  "ROBER GONZÁLEZ",              1L,                 0L,            5L,               4L,                   1L,                      1L,
  "RODRI",              5L,                 1L,           19L,              15L,                   0L,                      1L,
  "RUI SILVA",              0L,                 0L,            0L,               1L,                   0L,                      0L,
  "CANALES",              2L,                 1L,           18L,              23L,                   0L,                      4L,
  "SIDNEI",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "CAMARASA",              1L,                 1L,            6L,               4L,                   2L,                      1L,
  "V. RUIZ",              3L,                 1L,           17L,              13L,                  13L,                     10L,
  "CARVALHO",             12L,                 4L,           35L,              20L,                   7L,                      2L,
  "WILLIAN J",              2L,                 0L,           11L,              15L,                   5L,                      2L,
  "YASSIN FEKIR",              0L,                 0L,            0L,               0L,                   0L,                      0L,
  "A. MORENO",              4L,                 5L,           22L,              18L,                   6L,                      5L
)


hsize = 2


df <- df %>% 
  mutate(duelos_aereos_totales = duelos_aereos_exito+duelos_aereos_fallados,
         acierto_duelos_aereos = (duelos_aereos_exito/duelos_aereos_totales)*100) %>% 
  filter(duelos_aereos_totales > 4) %>% 
  mutate(fallo_duelos_aereos = 100-acierto_duelos_aereos) %>% 
  #relocate(fallo_tiros, .after = acierto_tiros) %>% 
  pivot_longer(9:10, names_to = "duelos_aereos", values_to = "percentage_duelos_aereos") %>% 
  mutate(x = hsize) 


df$jugador<-factor(df$jugador, levels = unique(df$jugador[order(df$duelos_aereos_totales)]))


p5 <- ggplot(df, aes(x = hsize, y = percentage_duelos_aereos, fill = as.factor(duelos_aereos))) +
  geom_col(color="white") +
  coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percentage_duelos_aereos),"%")),
            size = 1.5,
            fontface="bold",
            position = position_stack(vjust = 0.5))+
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(jugador~duelos_aereos_totales,ncol = 4) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  theme(legend.background = element_rect(fill = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(colour = "white",face="bold"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white"))+
  scale_fill_manual(values = c("#7bc043", "#f73225"),labels = c("Éxito", "Fallo"))+
  labs(title = "Nº DE DUELOS AÉREOS",
       subtitle = "",
       caption="")+
  theme(plot.title = element_text(colour = "white",face="bold",size=12, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,face="italic",hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))



# unir plots ------------

p <- (p1|p2|p3|p4|p5) + plot_layout(guides = "collect") & theme(legend.position = "top") 



p <- p + plot_annotation(
  title = "Errores/Aciertos jugadores Real Betis",
  subtitle = 'Actualizado a jornada 8',
  caption = 'laliga.com | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

p <- p + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'black', color = "gray95"),
                                                   plot.title = element_text(color = "white", hjust = 0.5),
                                                   plot.subtitle = element_text(color = "white", hjust = 0.5),
                                                   panel.background = element_rect(fill = "black", color = "gray95"),
                                                   plot.caption = element_text(color = "white"))) 

p <- p + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

p <- p + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                                   plot.subtitle = element_text(size = 8, hjust = 0.5),
                                                   plot.caption = element_text(size = 9, hjust = 1)))

p

ggsave("p_anillos.png", width = 15.5, height = 12, device = agg_png, dpi = 500)