# =========================================================
# HEATMAP DE ABUNDANCIA RELATIVA DE HMA - SOLO CT
# =========================================================

# 1. CARGAR LIBRERÍAS

library(tidyverse)
library(readxl)
library(pheatmap)
library(vegan)


# 2. CARGAR BASE DE DATOS

datos <- read_excel(
  "C:/Users/fvygh/Documents/TESIS_PREGRADO/base_de_datos_HMA.xlsx",
  sheet = "fb"
)


# 3. FILTRAR SOLO CT

datos_CT <- datos %>%
  filter(grupo == "CT")


# 4. CREAR NOMBRE COMPLETO DEL TAXÓN

datos_CT <- datos_CT %>%
  mutate(
    taxon = paste(genero, especie)
  )


# 5. SUMAR ABUNDANCIA POR ACCESIÓN Y TAXÓN

tabla_CT <- datos_CT %>%
  group_by(accesion, taxon) %>%
  summarise(
    abundancia = sum(cantidad),
    .groups = "drop"
  )


# 6. CREAR MATRIZ ACCESIÓN × TAXÓN

matriz_CT <- tabla_CT %>%
  pivot_wider(
    names_from = taxon,
    values_from = abundancia,
    values_fill = 0
  ) %>%
  column_to_rownames("accesion") %>%
  as.matrix()


# 7. CALCULAR ABUNDANCIA RELATIVA

matriz_CT_rel <- matriz_CT / rowSums(matriz_CT)


# 8. CLUSTERING DE ACCESIONES CON BRAY-CURTIS

dist_CT_accesiones <- vegdist(
  matriz_CT_rel,
  method = "bray"
)

cluster_CT_accesiones <- hclust(
  dist_CT_accesiones,
  method = "average"
)


# 9. CLUSTERING DE TAXONES CON BRAY-CURTIS

dist_CT_taxones <- vegdist(
  t(matriz_CT_rel),
  method = "bray"
)

cluster_CT_taxones <- hclust(
  dist_CT_taxones,
  method = "average"
)


# 10. FORMATEAR NOMBRES CIENTÍFICOS EN CURSIVA

formato_taxon <- function(x) {
  
  partes <- strsplit(x, " ")[[1]]
  
  genero <- partes[1]
  especie <- paste(partes[-1], collapse = " ")
  
  if (grepl("^sp", especie, ignore.case = TRUE)) {
    
    especie <- gsub(
      "^Sp",
      "sp",
      especie,
      ignore.case = TRUE
    )
    
    bquote(
      italic(.(genero)) ~ .(especie)
    )
    
  } else {
    
    bquote(
      italic(.(x))
    )
  }
}


labels_CT <- as.expression(
  lapply(
    colnames(matriz_CT_rel),
    formato_taxon
  )
)


# 11. MOSTRAR HEATMAP EN RSTUDIO

pheatmap(
  t(matriz_CT_rel),
  
  cluster_rows = cluster_CT_taxones,
  cluster_cols = cluster_CT_accesiones,
  
  scale = "none",
  
  labels_row = labels_CT,
  
  color = colorRampPalette(
    c("white", "orange", "red")
  )(100),
  
  border_color = NA,
  
  fontsize_row = 8,
  fontsize_col = 9,
  
  angle_col = 45,
  
  main = "Abundancia relativa de hongos micorrízicos arbusculares en accesiones CT"
)


# 12. EXPORTAR HEATMAP

pheatmap(
  t(matriz_CT_rel),
  
  cluster_rows = cluster_CT_taxones,
  cluster_cols = cluster_CT_accesiones,
  
  scale = "none",
  
  labels_row = labels_CT,
  
  color = colorRampPalette(
    c("white", "orange", "red")
  )(100),
  
  border_color = NA,
  
  fontsize_row = 8,
  fontsize_col = 9,
  
  angle_col = 45,
  
  main = "Abundancia relativa de hongos micorrízicos arbusculares en accesiones CT",
  
  filename = "Heatmap_CT_Bray_Curtis_final.png",
  
  width = 12,
  height = 9
)
