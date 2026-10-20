# ==============================================================================
# ARCHIVO: Integración de datos tabulados - Padrones
# DESCRIPCIÓN: Consolida capas de permisos, protección patrimonial,
#              comercio, transacciones y viviendas de interes publico
#              hacia la tabla maestra de padrones.
# AUTOR: Valentina Lagos, Agencia Ciudad
# FECHA CREACIÓN: 2026-10-08
# FECHA ACTUALIZACION: 2026-10-19
# ==============================================================================

# 1. LIBRERIAS ------------------------------------------
library(readODS)
library(sf)
library(dplyr)
library(readr)
library(data.table)
library(purrr)
library(skimr)
library(readxl)

# 2. TABLA BASE: PADRONES ======================================================

## 2.1. Importación de datos base ----------------------------------------------
# read.csv2 por punto y coma.
# archivo no abren sin fileEncoding = "UTF-8"
pad_urb <- read.csv2("data_in/padrones_urbanos_mvd.csv", fileEncoding = "UTF-8")
pad_rur <- read.csv2("data_in/padrones_rurales_mvd.csv", fileEncoding = "UTF-8")

## 2.2. Consolidación y trazabilidad -------------------------------------------
# Unificamos padrones en un solo dataframe.
# "origen" indica procedencia de cada fila.
padrones <- bind_rows( 
  pad_rur %>% mutate(origen = "rural"),
  pad_urb %>% mutate(origen = "urbano")
)

## 2.3. Control de calidad y gestión de memoria --------------------------------
# Identificamos llaves unicas padron.
# Este vector (ground truth) será vital para los reportes de QA y matches.
padrones_list <- unique(padrones$padron)

# Limpieza del entorno: liberamos RAM borrando los archivos temporales.
rm(pad_urb, pad_rur)
gc()

# 3. ADICIÓN DE CAPAS EXTERNAS =================================================

## 3.1. Permisos de Edificación ------------------------------------------------

### 3.1.1. Importación ----
# Carga la capa espacial de permisos de construcción en formato GeoJSON
permisos <- st_read("data_in/permisos_de_construccion_IM.geojson")

### 3.1.2. Cruce a tabla maestra (Join) ----
# Integramos atributos a la base 'padrones' eliminando la geometría para 
# evitar redundancia espacial y optimizar la memoria. Se agregan sufijos '_per'
# para prevenir futuros conflictos de nombres.
padrones <- padrones %>%
  left_join(
    permisos %>% 
      st_drop_geometry() %>% 
      select(
        padron, 
        area_edif_per = area_edif, 
        destino_per = dsc_destino, 
        desc_tipo_obra_per = dsc_tipo_obra, 
        fecha_aprob_per = fecha_aprob
      ),
    by = "padron"
  )

### 3.1.3. Métricas de Calidad (Matches) ----
# Evaluamos cuántos registros de permisos encuentran correspondencia en nuestro 
# universo base (ground truth) y cuántos quedan huérfanos.
perm_match <- sum(permisos$padron %in% padrones_list)
perm_unmatch <- sum(!permisos$padron %in% padrones_list)

### 3.1.4. Resumen Estadístico (Skim) ----
# Generamos el perfil estadístico de las variables originales y le inyectamos 
# la etiqueta de origen para consolidarlo luego en el reporte general de QA.
skim_perm <- permisos %>%
  select(area_edif, dsc_destino, dsc_tipo_obra, fecha_aprob) %>%
  skim() %>%
  mutate(base_origen = "permisos de edificación") %>% 
  relocate(base_origen, .before = skim_variable)


## 3.2. Protección Patrimonial -------------------------------------------------

### 3.2.1. Importación ----
# Cargamos las capas de protección patrimonial divididas por zonas
prot_patrim_centro <- st_read("data_in/norm/Grado_proteccion_patrimonial_centro.json")
prot_patrim_carr <- st_read("data_in/norm/Grado_proteccion_patrimonial_carrasco.json")

### 3.2.2. Estandarización y Unificación (Bind Rows) ----
# Preparamos Archivo 1 (Centro): eliminamos geometría, unificamos nombres 
# de variables y aseguramos tipo de dato antes de la unión vertical.
prot_1 <- prot_patrim_centro %>%
  st_drop_geometry() %>% 
  rename(grado_prot_patrm = gpp) %>%
  mutate(grado_prot_patrm = as.integer(grado_prot_patrm)) %>% 
  select(padron, grado_prot_patrm)

# Preparamos Archivo 2 (Carrasco): misma lógica y estandarización
prot_2 <- prot_patrim_carr %>%
  st_drop_geometry() %>% 
  rename(grado_prot_patrm = grado_proteccion) %>%
  mutate(grado_prot_patrm = as.integer(grado_prot_patrm)) %>% 
  select(padron, grado_prot_patrm)

# Combinamos ambas zonas en un único dataframe de protección
prot_all <- bind_rows(prot_1, prot_2)

### 3.2.3. Cruce a tabla maestra (Join) ----
# Integramos a la base principal. Se utiliza many-to-many ya que un mismo 
# padrón podría (teóricamente) abarcar o cruzar múltiples zonificaciones.
padrones <- padrones %>%
  left_join(
    prot_all,
    by = "padron",
    relationship = "many-to-many"
  )

### 3.2.4. Métricas de Calidad y Skim ----
# Evaluamos cuántos registros de la tabla consolidada cruzan con el universo
prot_match <- sum(prot_all$padron %in% padrones_list)
prot_unmatch <- sum(!prot_all$padron %in% padrones_list)

# Resumen estadístico de grados de protección para el reporte de QA
skim_prot <- prot_all %>%
  select(grado_prot_patrm) %>%
  skim() %>%
  mutate(base_origen = "proteccion patrimonial") %>% 
  relocate(base_origen, .before = skim_variable)

### 3.2.5. Limpieza de entorno ----
rm(prot_patrim_centro, prot_patrim_carr, prot_1, prot_2)
gc()

## 3.3. Habilitaciones Comerciales ---------------------------------------------

### 3.3.1. Importación de datos (.ods) ----
com22 <- read_ods("data_in/com/habilitaciones_comerciales_2022.ods")
com23 <- read_ods("data_in/com/habilitaciones_comerciales_2023.ods")
com24 <- read_ods("data_in/com/habilitaciones_comerciales_2024.ods")
com25 <- read_ods("data_in/com/habilitaciones_comerciales_2025.ods")
com26 <- read_ods("data_in/com/habilitaciones_comerciales_2026.ods")

### 3.3.2. Preparación: Archivos SIN área a habilitar (2022-2023) ----
habcom_22 <- com22 %>%
  rename(
    padron = PADRON,              # Estandarizamos la llave
    tipologia = TIPOLOGÍA,
    tenencia_propiedad = TENENCIA_PROPIEDAD,
    fecha = `FECHA DE NOTIFICACIÓN`,
    estado = ESTADO
  ) %>%
  mutate(
    padron = as.integer(padron), 
    fecha = as.character(fecha)
  ) %>%
  select(padron, tipologia, tenencia_propiedad, fecha, estado)

habcom_23 <- com23 %>%
  rename(
    padron = PADRON,
    tipologia = TIPOLOGÍA,
    tenencia_propiedad = TENENCIA_PROPIEDAD,
    fecha = `FECHA DE NOTIFICACIÓN`,
    estado = ESTADO
  ) %>%
  mutate(
    padron = as.integer(padron), 
    fecha = as.character(fecha)
  ) %>%
  select(padron, tipologia, tenencia_propiedad, fecha, estado)

### 3.3.3. Preparación: Archivos CON área a habilitar (2024-2026) ----
habcom_24 <- com24 %>%
  rename(
    padron = PADRON,
    tipologia = TIPOLOGÍA,
    tenencia_propiedad = TENENCIA_PROPIEDAD,
    fecha = `FECHA DE NOTIF.`,
    area_a_habilitar = `ÁREA A HABILITAR`, # Aquí sí existe el área
    estado = ESTADO      
  ) %>%
   mutate(
    padron = as.integer(padron), 
    fecha = as.character(fecha),
    area_a_habilitar = as.numeric(area_a_habilitar)
  ) %>%
  select(padron, tipologia, tenencia_propiedad, fecha, area_a_habilitar, estado)

habcom_25 <- com25 %>%
  rename(
    padron = PADRON,
    tipologia = TIPOLOGÍA,
    tenencia_propiedad = TENENCIA_PROPIEDAD,
    fecha = `FECHA DE NOTIF.`,
    area_a_habilitar = `ÁREA A HABILITAR`,
    estado = ESTADO      
  ) %>%
   mutate(
    padron = as.integer(padron), 
    fecha = as.character(fecha),
    area_a_habilitar = as.numeric(area_a_habilitar)
  ) %>%
  select(padron, tipologia, tenencia_propiedad, fecha, area_a_habilitar, estado)

habcom_26 <- com26 %>%
  rename(
    padron = PADRON,
    tipologia = TIPOLOGÍA,
    tenencia_propiedad = TENENCIA_PROPIEDAD,
    fecha = `FECHA DE NOTIF.`,
    area_a_habilitar = `ÁREA A HABILITAR`,
    estado = ESTADO      
  ) %>%
   mutate(
    padron = as.integer(padron), 
    fecha = as.character(fecha),
    area_a_habilitar = as.numeric(area_a_habilitar)
  ) %>%
  select(padron, tipologia, tenencia_propiedad, fecha, area_a_habilitar, estado)

### 3.3.4. Unificación y Limpieza de entorno ----
# Liberamos la memoria de las cargas iniciales
rm(com22, com23, com24, com25, com26)

# Unimos los 5 años. Las filas de 2022 y 2023 asumirán NA en 'area_a_habilitar'
hab_all <- bind_rows(habcom_22, habcom_23, habcom_24, habcom_25, habcom_26)

# Borramos los objetos intermedios
rm(habcom_22, habcom_23, habcom_24, habcom_25, habcom_26)

### 3.3.5. Métricas de Calidad y Skim ----
hab_match <- sum(hab_all$padron %in% padrones_list)
hab_unmatch <- sum(!hab_all$padron %in% padrones_list)

skim_hab <- hab_all %>%
  # Ya no es necesario el select() explícito si queremos todas las columnas,
  # pero dejarlo es una buena práctica de control.
  select(padron, tipologia, tenencia_propiedad, fecha, area_a_habilitar, estado) %>%
  skim() %>%
  mutate(base_origen = "habilitaciones comerciales") %>% 
  relocate(base_origen, .before = skim_variable)

### 3.3.6. Cruce a tabla maestra (Join) ----
padrones <- padrones %>%
  left_join(
    hab_all %>%
      # Renombramos inyectando el sufijo _hab para proteger el diccionario
      rename(
        tipologia_hab = tipologia,
        tenencia_propiedad_hab = tenencia_propiedad,
        fecha_hab = fecha,
        area_a_habilitar_hab = area_a_habilitar,
        estado_hab = estado # <-- Añadido para estandarizar
      ),
    by = "padron",
    relationship = "many-to-many"
  )


## 3.4. Transacciones DGR (2013-2026) ------------------------------------------

### 3.4.1. Importación explícita ----
# Se cargan los archivos individualmente usando read.csv2 debido a que el 
# delimitador original es punto y coma (;). El encoding UTF-8 es fundamental.
trans_2013 <- read.csv2("data_in/trans/2013DGR.csv", fileEncoding = "UTF-8")
trans_2014 <- read.csv2("data_in/trans/2014DGR.csv", fileEncoding = "UTF-8")
trans_2015 <- read.csv2("data_in/trans/2015DGR.csv", fileEncoding = "UTF-8")
trans_2016 <- read.csv2("data_in/trans/2016DGR.csv", fileEncoding = "UTF-8")
trans_2017 <- read.csv2("data_in/trans/2017DGR.csv", fileEncoding = "UTF-8")
trans_2018 <- read.csv2("data_in/trans/2018DGR.csv", fileEncoding = "UTF-8")
trans_2019 <- read.csv2("data_in/trans/2019DGR.csv", fileEncoding = "UTF-8")
trans_2020 <- read.csv2("data_in/trans/2020DGR.csv", fileEncoding = "UTF-8")
trans_2021 <- read.csv2("data_in/trans/2021DGR.csv", fileEncoding = "UTF-8")
trans_2022 <- read.csv2("data_in/trans/2022DGR.csv", fileEncoding = "UTF-8")
trans_2023 <- read.csv2("data_in/trans/2023DGR.csv", fileEncoding = "UTF-8")
trans_2024 <- read.csv2("data_in/trans/2024DGR.csv", fileEncoding = "UTF-8")
trans_2025 <- read.csv2("data_in/trans/2025DGR.csv", fileEncoding = "UTF-8")
trans_2026 <- read.csv2("data_in/trans/2026DGR.csv", fileEncoding = "UTF-8")

### 3.4.2. Estandarización y Unificación ----
# Agrupamos los 14 dataframes en una lista para procesarlos en lote
lista_trans <- mget(paste0("trans_", 2013:2026))

# (Diagnóstico opcional: Verificación de tipos de datos y columnas por año)
# lapply(lista_trans, function(df) sapply(df, class))

# Filtramos las columnas antes de unir para evitar conflictos de tipos de datos 
# (ej. variable UNIDAD que cambiaba entre integer/character según el año).
trans_all <- lista_trans %>%
  map(function(df) {
    df %>%
      # 1. Filtramos solo las variables de interés (ahorra memoria)
      select(FECHA, PADRON, AREA, EXTENSION, MONEDA, MONTO) %>%
      # 2. Estandarización a minúsculas
      rename_with(tolower) %>%
      # 3. Forzamos padron a numérico (Regla de oro de la BBDD)
      mutate(padron = as.numeric(padron)) %>%
      # 4. Agregamos sufijo "_trans" para evitar colisiones en la tabla maestra
      rename_with(~ paste0(., "_trans"), -padron)
  }) %>%
  # 5. Colapsamos la lista en un único dataframe
  bind_rows()

### 3.4.3. Limpieza de Entorno ----
# Eliminamos los 14 dataframes individuales y forzamos la liberación de memoria RAM (gc)
rm(list = paste0("trans_", 2013:2026))
gc()

### 3.4.4. Métricas de Calidad y Skim ----
trans_match <- sum(trans_all$padron %in% padrones_list)
trans_unmatch <- sum(!trans_all$padron %in% padrones_list)

skim_trans <- trans_all %>%
  select(fecha_trans, padron, area_trans, extension_trans, moneda_trans, monto_trans) %>%
  skim() %>%
  mutate(base_origen = "transacciones") %>% 
  relocate(base_origen, .before = skim_variable)

### 3.4.5. Cruce a tabla maestra (Join) ----
padrones <- padrones %>%
  left_join(
    trans_all,
    by = "padron",
    # Fundamental: un padrón puede registrar múltiples transacciones entre 2013-2026
    relationship = "many-to-many" 
  )


## 3.5. Viviendas de Interés Social (VIS) --------------------------------------

### 3.5.1. Importación y Preparación ----
# Leemos directamente desde la pestaña 'SUBPROYECTO' del archivo Excel.
# Filtramos únicamente los proyectos de Montevideo para mantener la 
# correspondencia geográfica con nuestra base de padrones.
vis <- read_excel("data_in/vis/PROYECTOS_PROMOVIDOS_AL_20260831.xlsx", sheet = "SUBPROYECTO") %>%
  filter(DEPARTAMENTO == "MONTEVIDEO") %>%
  select(PADRON, PROMOVIDO, TIPO, TOTAL_VP) %>%
  rename_with(tolower) %>%             
  mutate(padron = as.numeric(padron)) # Regla de oro: llave primaria como numérico

### 3.5.2. Métricas de Calidad y Skim ----
# Evaluamos correspondencia directa (Matches) contra el universo base
vis_match <- sum(vis$padron %in% padrones_list)
vis_unmatch <- sum(!vis$padron %in% padrones_list)

# Resumen estadístico (Skim) de la data VIS
skim_vis <- vis %>%
  skim() %>%
  mutate(capa_origen = "vis_subproyecto") %>%
  relocate(capa_origen, .before = skim_variable)

### 3.5.3. Cruce a tabla maestra (Join) ----
# Integramos a la tabla principal aplicando un sufijo preventivo (_vis) 
# a todas las columnas excepto a la llave primaria 'padron'.
padrones <- padrones %>%
  left_join(
    vis %>% 
      rename_with(~ paste0(., "_vis"), -padron), 
    by = "padron",
    relationship = "many-to-many"
  )


# 4. RESPALDO Y REPORTES DE CALIDAD ============================================

## 4.1. Exportación de Base Maestra (.rds) -------------------------------------
# Guardamos el objeto consolidado en formato nativo de R (.rds) para 
# preservar los tipos de datos exactos (numeric, character) de cara al análisis.
saveRDS(padrones, file = "data_out/padrones_consolidado_0910.rds")

# (Opcional) Código para recargar la base maestra en futuras sesiones:
# padrones <- readRDS("data_out/padrones_consolidado_0910.rds")

## 4.2. Consolidación de Reportes Estadísticos (Skim) --------------------------
# Unimos los resúmenes de todas las variables originales procesadas.
skim_rep <- bind_rows(skim_hab, skim_perm, skim_prot, skim_trans, skim_vis)

## 4.3. Consolidación de Reporte de Matches ------------------------------------
# Creamos la tabla maestra de integridad relacional (QA) para evaluar 
# la tasa de éxito de los cruces (joins) hacia la base principal.
tabla_matches <- data.frame(
  base_origen = c(
    "habilitaciones comerciales", 
    "permisos edificacion", 
    "proteccion patrimonial", 
    "transacciones",
    "viviendas de interes social"
  ),
  matches = c(hab_match, perm_match, prot_match, trans_match, vis_match),
  no_matches = c(hab_unmatch, perm_unmatch, prot_unmatch, trans_unmatch, vis_unmatch)
) %>%
  mutate(
    total_registros = matches + no_matches,
    porcentaje_match = round((matches / total_registros) * 100, 2)
  )

## 4.4. Exportación de Reportes de Calidad (CSV) -------------------------------
# Guardamos los reportes de calidad para revisión en Excel o tableros de BI.
write.csv(tabla_matches, "data_out/tabla_matches2.csv", row.names = FALSE)
write.csv(skim_rep, "data_out/skim_rep2.csv", row.names = FALSE)
