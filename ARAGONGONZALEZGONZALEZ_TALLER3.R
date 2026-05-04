# =============================================================================
# Taller 3. Taller de Programación en R
# Facultad de Economía
# Universidad de los Andes
# Profesor : Santiago Neira 
# Estudiantes:
# - Mauricio Aragón - 201729052
# - Alejandra González - 202111607
# - Valentina González - 202111608
# =============================================================================

# -----------------------------------------------------------------------------
# Paquetes necesarios y preparación del ambiente de trabajo
# -----------------------------------------------------------------------------

# Limpiamos el espacio de trabajo
rm(list = ls())

# Instalación y carga de paquetes necesarios
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "ggplot2", 
                       "dplyr", "gapminder", "gganimate", "gifski", "viridis",
                       "openxlsx", "leaflet", "sf", "tigris", "remotes", "lubridate", "readr",
                       "writexl", "tidyr", "ggrepel",
                       "sf", "patchwork","readxl","scales", "plotly","rmapshaper","leaflet","htmltools","sf","magrittr","modelsummary", 
                       "rlang","polite", "rvest", "chromote", "httr", "jsonlite", "stringr")

# Función para instalar paquetes si no están instalados
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

# Instalar paquetes faltantes
install_if_missing(required_packages)

# Cargar librerías
lapply(required_packages, library, character.only = TRUE)


#  Configuración del directorio de trabajo ----------------------------------
# Obtener usuario del sistema para crear ruta dinámica
username <- Sys.getenv("USERNAME")

# Construir ruta hacia los datos usando el usuario actual
path_taller <- paste0("C:/Users/", username, "/OneDrive/Documents/GitHub/MEcA/Taller R/Taller3")

# Establecer directorio de trabajo
setwd(path_taller)

# -----------------------------------------------------------------------------
# Punto 3. Aplicativo Shiny Interactivo
# -----------------------------------------------------------------------------

# Vamos a crear un aplicativo shiny con tres pestañas: la primera mostrará los 
# datos geoespaciales interactivos del índice de desempeño fiscal en los
# departamentos de Colombia. La segunda, la visualización de las series 
# financieras IBR (Colombia) y SOFR (EE.UU.). La tercera, información sobre
# tiroteos en EE.UU desde 1996 a 2023. 

# Para la última pestaña se usará la información recopilada por el proyecto
# the Violence Proyect 

# PARA EL LITERAL A:
#Primero cargamos la del IDF
#Usar el nombre con el que tengan disponible la consulta del DNP
df_idf <- read_excel(
  "ConsultaIDF_2024.xlsx",
  skip = 16
) %>% rename(MPIO = `Código Entidad`)

#Segundo cargamos la de población para 2024 y solo los totales
df_pop <- read_excel(
  "PPED-AreaMun-2018-2042_VP.xlsx",
  sheet = "PobMunicipalxÁrea",
  skip = 7
) %>% filter(AÑO==2024) %>% filter(`ÁREA GEOGRÁFICA` == "Total")

#Tercero cargamos el valor agregado municipal (pesos corrientes)
df_va <- read_excel(
  "anex-PIBDep-ValorAgreMuni-2011-2023p.xlsx",
  sheet = "Cuadro 14",
  skip = 10
) %>% filter(!is.na(Municipio)) %>% rename(MPIO = `Código Municipio`)

#Cuarto hacemos el junte de las bases de datos

df <- df_idf %>% left_join(df_pop, by = "MPIO") %>% rename(Poblacion = TOTAL) %>% 
  left_join(df_va, by = "MPIO")

base_mun <- df %>% select(DP, DPNOM, MPIO, DPMP, IDF,
                          Poblacion, `Valor agregado\r\n`, Rango) %>%
  rename(VA = `Valor agregado\r\n`) 

# Ajustamos el VA a 2024
base_mun_ind <- base_mun %>% mutate(VA24 = VA * 1.0795)

# Comenzamos por adicionar la variable L x VA
base_mun_ind <- base_mun_ind %>% mutate(PobxVA = Poblacion * VA24)

# La agregamos a nivel departamental
base_d <- base_mun_ind %>% group_by(DP) %>% summarise(
  sum(PobxVA, na.rm = TRUE))

# La añadimos a la base municipal
base_mun_ind <- base_mun_ind %>% left_join(base_d, by="DP") %>%
  rename(PobxVA_Dep = `sum(PobxVA, na.rm = TRUE)`) %>% 
  # Creamos la variable departamental que se debe agregar
  mutate(ind_dep = IDF * PobxVA / PobxVA_Dep)

# Armamos la base departamental
base_dep <- base_mun_ind %>% group_by(DP, DPNOM) %>% summarise(IDF_dep =
                                                                 sum(ind_dep, na.rm = TRUE), .groups = "drop")

# Añadimos el factor de rango para los mapas
base_dep <- base_dep %>%
  mutate(Rango = case_when(
    IDF_dep < 40 ~ 1,
    IDF_dep >= 40 & IDF_dep < 60 ~ 2,
    IDF_dep >= 60 & IDF_dep < 70 ~ 3,
    IDF_dep >= 70 & IDF_dep < 80 ~ 4,
    IDF_dep >= 80 ~ 5
  )
  ) %>% mutate(Rango_Cat = factor(
    Rango,
    levels = c(1,2,3,4,5),
    labels = c("Deterioro", "Riesgo", "Vulnerable", "Solvente", "Sostenible")
  ))

# Cargamos los datos geoespaciales
shapefile_path <- "MGN2025_DPTO_POLITICO"
crs <- "+proj=longlat +datum=WGS84"  # Proyección WGS84
sf_data <- st_read(dsn = shapefile_path) %>% rename(DP = dpto_ccdgo) %>% 
  left_join(base_dep, by = "DP") %>%
  ms_simplify(keep=0.05, keep_shapes = TRUE)

# Cargamos los datos geoespaciales municipales
base_mun <- base_mun%>%
  mutate(Rango = case_when(
    IDF < 40 ~ 1,
    IDF >= 40 & IDF < 60 ~ 2,
    IDF >= 60 & IDF < 70 ~ 3,
    IDF >= 70 & IDF < 80 ~ 4,
    IDF >= 80 ~ 5
  )
  ) %>% mutate(Rango_Cat = factor(
    Rango,
    levels = c(1,2,3,4,5),
    labels = c("Deterioro", "Riesgo", "Vulnerable", "Solvente", "Sostenible")
  ))

shapefile_pathM <- "MGN2025_MPIO_GRAFICO"
sf_dataM <- st_read(dsn = shapefile_pathM) %>% rename(MPIO = mpio_cdpmp) %>% 
  left_join(base_mun, by = "MPIO")%>%
  st_make_valid() %>% st_cast("MULTIPOLYGON") %>%
  ms_simplify(keep=0.05, keep_shapes = TRUE)

# PARA EL LITERAL B:
# Cargamos los datos que obtuvimos para facilitar la renderización del
# aplicativo
df_ibr <- read.csv("df_ibr.csv")
df_sofr <- read.csv("df_sofr.csv")
df_ibr$Fecha <- as.Date(df_ibr$Fecha)
df_sofr$Fecha <- as.Date(df_sofr$Fecha)
df_merge <- df_sofr %>%
  inner_join(df_ibr, by = "Fecha")

# PARA EL LITERAL C:
# Antes de empezar, definimos una sección que nos ayude a traducir las 
# abreviaturas de los nombres de estados a sus nombre completos. Estos son 
# vectores que ya vienen incluidos en R base, sólo los llamamos
state_lookup <- data.frame(
  abb = state.abb, 
  name = state.name
)

# Asimismo, leemos la base de datos para tenerla ya cargada en la sesión:
df<- read.xlsx("Violence Project Mass Shooter 7.0.xlsx",
               sheet = "Full Database")

# Definimos una función para recodificar y etiquetar las variables:
recodificar <- function(df){
  df %>%
    mutate(Gender = recode(as.character(Gender),
                      "0" = "Hombre",
                      "1" = "Mujer",
                      "3" = "No binario",
                      "4" = "Transgénero"),
      
      Race = recode(as.character(Race),
                    "0" = "Blanco",
                    "1" = "Negro",
                    "2" = "Latino",
                    "3" = "Asiático",
                    "4" = "Medio Oriente",
                    "5" = "Nativo americano"),
      
      Immigrant = recode(as.character(Immigrant),
                         "0" = "No inmigrante",
                         "1" = "Inmigrante"),
      
      Employment.Status = recode(as.character(Employment.Status),
                                  "0" = "Desempleado",
                                  "1" = "Trabajando"),
      
      `On-Scene.Outcome` = recode(as.character(`On-Scene.Outcome`),
                                "0" = "Se suicidó",
                                "1" = "Abatido en la escena",
                                "2" = "Aprehendido",
                                "3" = "Aprehendido, luego suicidio",
                                "4" = "Huyó"),
      
      Location = recode(as.character(Location),
                                "0"  = "Escuela K-12",
                                "1"  = "Universidad / College",
                                "2"  = "Edificio gubernamental / lugar cívico",
                                "3"  = "Lugar de culto",
                                "4"  = "Comercio / tienda",
                                "5"  = "Restaurante / bar / discoteca",
                                "6"  = "Oficina",
                                "7"  = "Residencia",
                                "8"  = "Exterior / espacio abierto",
                                "9"  = "Bodega / fábrica",
                                "10" = "Oficina postal"),
      
      Religion =  recode(as.character(Religion), 
                                "0" = "Ninguna",
                                "1" = "Cristiana",
                                "2" = "Musulmana",
                                "3" = "Budista",
                                "4" = "Espiritualidad cultural / otra",
                                "5" = "Judía"),
      
      Sexual.Orientation = recode(as.character(Sexual.Orientation),
                                 "0" = "Heterosexual",
                                 "1" = "No heterosexual"),
      
      Education = recode(as.character(Education),
                         "0" = "Menos que secundaria",
                         "1" = "Secundaria / GED",
                         "2" = "Algo de universidad / técnico",
                         "3" = "Pregrado",
                         "4" = "Posgrado / estudios avanzados"),
      
      School.Performance = recode(as.character(School.Performance),
                                  "0" = "Bajo",
                                  "1" = "Promedio",
                                  "2" = "Alto"),
      
      Birth.Order = recode(as.character(Birth.Order),
                           "0" = "Hijo único",
                           "1" = "Mayor",
                           "2" = "Intermedio",
                           "3" = "Menor",
                           "4" = "Gemelo/a"),
      
      Relationship.Status = recode(as.character(Relationship.Status),
                                   "0" = "Soltero/a",
                                   "1" = "Con pareja",
                                   "2" = "Casado/a",
                                   "3" = "Divorciado/a / separado/a / viudo/a"),
      
      Children = recode(as.character(Children),
                        "0" = "Sin evidencia",
                        "1" = "Sí"),
                                
      Attempt.to.Flee = recode(as.character(Attempt.to.Flee),  
                               "0" = "No",
                               "1" = "Si",
                               "2" = "Escapó y se suicidó"),
                               
      Military.Service = recode(as.character(Military.Service),
                                "0" = "No",
                                "1" = "Sí",
                                "2" = "Ingresó pero no completó" ))}

# Y definimos una paleta para usar en las gráficas:
colores <- list(
  crema = "#FDF8F2",
  arena = "#F4E3C1",
  oro = "#D9A441",
  mostaza = "#C98C10",
  terracota = "#C96A3D",
  coral = "#B94E48",
  vino = "#8C2D2D",
  borgona = "#6E1F1F",
  carbon = "#2F2F2F",
  gris = "#666666")

# Ahora si, creamos nuestro aplicativo shiny:

# PASO 1: Crear la interfaz de usuario (ui)
ui <- dashboardPage(
  
  skin = "black",
  dashboardHeader(title = "Aplicativo Shiny Interactivo - Grupo 5"),
  
  dashboardSidebar(
    
    collapsed = FALSE,
    
    width = 280,
    
    sidebarMenu(
      
      id = "tabs", 
      
      menuItem("Indice de Desempeño Fiscal", tabName = "tab1", icon = icon("building-columns")),
      menuItem("IBR y SOFR", tabName = "tab2", icon = icon("chart-line")),
      menuItem("Tiroteos en EE.UU.", tabName = "mapa", icon = icon("crosshairs"))
    ),
    
    br(),
    
    conditionalPanel(
      condition = "input.tabs == 'tab1'",
      actionButton("reset_depto", "Ver todo Colombia", icon = icon("arrows-up-down-left-right"))),
                   
        conditionalPanel(
      condition = "input.tabs == 'mapa'",
      
      h4("  Filtros"),
      
      sliderInput("year_range", "Rango de años:",
        min = 1966, max = 2023, value = c(1966, 2023), sep = ""),
      
      selectInput("tipo_mapa", "Vista del mapa:", 
        choices = c("Total tiradores", "Tiradores Hombres", "Tiradores Mujeres")),
      
      actionButton("reset_map", "Ver todo EE.UU.", icon = icon("globe"))
  )),
  
  dashboardBody(
    
    tags$head(tags$style(HTML(".content-wrapper, .right-side {background-color: #f4f6f9;}
    .box {border-radius: 10px;}.small-box {border-radius: 10px;}
    h2, h3, h4 {font-weight: 600;}"))),
    
    tabItems(
      
      tabItem(tabName = "tab1",
              
              fluidRow(
                box(width = 12, title = "Descripción",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  HTML(
                    paste0(
                      "<b>¿Qué muestra este panel?</b><br>",
                      "Este módulo permite explorar el Índice de Desempeño Fiscal (IDF) de los departamentos de Colombia para 2024, ",
                      "construido a partir de información del DNP y el DANE, ponderando por valor agregado per cápita.<br><br>",
                      
                      "<b>¿Cómo interactuar?</b><br>",
                      "• Pasa el cursor sobre el mapa para ver detalles por departamento.<br>",
                      "• Haz clic sobre un departamento para activarlo.<br>",
                      "• Consulta su IDF, ranking nacional y compáralo con el promedio país.<br>",
                      "• Observa los 3 municipios con mejor desempeño dentro del departamento.<br>",
                      "• Analiza la distribución del IDF municipal para entender la heterogeneidad interna.<br><br>",
                      
                      "<b>Interpretación:</b><br>",
                      "Valores más altos del IDF indican mejor desempeño fiscal. ",
                      "Sin embargo, diferencias dentro de cada departamento pueden revelar desigualdades territoriales importantes.")))),
              
              fluidRow(valueBoxOutput("idf_depto"),
                       valueBoxOutput("idf_pais"),
                       valueBoxOutput("ranking_depto")),
              
              fluidRow(box(width = 12, title = "Mapa IDF por Departamento",
                      status = "primary",
                      solidHeader = TRUE,
                      leafletOutput("mapa_idf", height = 500))),
                      
                      box(width = 6,title = "Top 3 municipios",
                          status = "info",
                          solidHeader = TRUE,
                          uiOutput("top_municipios")),
                      
                      box(width = 6, title = "Distribución del IDF municipal",
                          status = "warning",
                          solidHeader = TRUE,
                          plotlyOutput("hist_municipios"))),

  
      tabItem(tabName = "tab2",
              
              fluidRow(
                box(
                  width = 12,
                  title = "Descripción",
                  status = "info",
                  solidHeader = TRUE,
                  
                  HTML(
                    paste0(
                      "<b>¿Qué muestra este módulo?</b><br>",
                      "Este panel permite analizar y comparar la evolución de las tasas overnight de ",
                      "Colombia y Estados Unidos entre 2020 y 2025, utilizando series oficiales ",
                      "de referencia monetaria.<br><br>",
                      
                      "<b>Indicadores incluidos:</b><br>",
                      "• <b>IBR ON:</b> tasa de referencia del mercado interbancario colombiano.<br>",
                      "• <b>SOFR ON:</b> tasa overnight garantizada basada en operaciones repo en EE.UU.<br><br>",
                      
                      "<b>Herramientas interactivas:</b><br>",
                      "• Explora ambas series en el gráfico comparativo.<br>",
                      "• Selecciona una fecha específica para consultar tasas históricas.<br>",
                      "• Calcula automáticamente la <b>devaluación implícita</b> mediante la paridad de tasas de interés.<br><br>",
                      
                      "<b>Fórmula utilizada:</b><br>",
                      "&pi; = ((1 + i<sub>d</sub>) / (1 + i<sub>e</sub>)) - 1<br>",
                      "donde:<br>",
                      "• i<sub>d</sub> = tasa doméstica (IBR)<br>",
                      "• i<sub>e</sub> = tasa externa (SOFR)<br><br>",
                      
                      "<b>Interpretación:</b><br>",
                      "Cuando la tasa doméstica supera la externa, el mercado suele incorporar ",
                      "expectativas de depreciación de la moneda local.")))),
              
              fluidRow(box(width = 12, title = "IBR vs SOFR",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput('grafico_tasas', height = 500))),
              
              fluidRow(box(width = 4, title = "Calculadora de Devaluación Implícita",
                  status = "warning",
                  solidHeader = TRUE,
                  dateInput(
                    "fecha_tasa",
                    "Selecciona fecha:",
                    value = max(df_merge$Fecha),
                    min = min(df_merge$Fecha),
                    max = max(df_merge$Fecha),
                    datesdisabled = NULL),
                  
                  actionButton("buscar_tasa",
                    "Calcular",
                    icon = icon("calculator"),
                    width = "100%")),
                
                box(width = 8, title = "Resultado",
                  status = "success",
                  solidHeader = TRUE,
                  
                  valueBoxOutput("valor_ibr"),
                  valueBoxOutput("valor_sofr"),
                  valueBoxOutput("valor_pi")))),
      
      tabItem(tabName = "mapa",
              
              fluidRow(
                box(width = 12,
                    title = "Descripción",
                    status = "info",
                    solidHeader = TRUE,
                    uiOutput("descripcion"))),
              
              fluidRow(
                valueBoxOutput("total_eventos"),
                valueBoxOutput("total_muertes"),
                valueBoxOutput("total_heridos")
              ),
            
              fluidRow(box(width = 12, actionButton(
                "ver_tabla",
                "Ver detalle de eventos filtrados", icon = icon("table")))),
              
              fluidRow( box(width = 8, title = "Mapa de Tiroteos", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    leafletOutput("mapa", height = 500)), 

                    box(width = 4,title = "Tendencia histórica",
                        status = "info",
                        solidHeader = TRUE,
                        plotlyOutput("grafico_anual", height = 250),
                        br(),
                        plotlyOutput("grafico_letalidad", height = 250))),
              
              fluidRow(box(width = 4, title = "Perfil del agresor",
                  status = "info",
                  solidHeader = TRUE,
                  selectInput(
                    "variable_agresor",
                    "Selecciona variable:",
                    choices = c(
                      "Género" = "Gender",
                      "Etnia" = "Race",
                      "Edad" = "Age",
                      "Status migratorio" = "Immigrant",
                      "Religión" = "Religion",
                      "Orientación sexual" = "Sexual.Orientation",
                      "Desempeño escolar" = "School.Performance",
                      "Educación" = "Education",
                      "Orden de nacimiento" = "Birth.Order",
                      "Estatus laboral" = "Employment.Status",
                      "Servicio militar" = "Military.Service",
                      "Estado sentimental" = "Relationship.Status",
                      "Hijos" = "Children"))),
 
                  box(width = 8, title = "Distribución - Perfil del agresor",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("grafico_agresor", height = 350))),
              
              fluidRow(box(width = 12, title = "Principales motivaciones registradas",
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_motivos", height = 420))),
              
              fluidRow(box(width = 6, title = "Resultado en escena",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_outcome", height = 350)),
                
                box(
                  width = 6,
                  title = "Intentó huir",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("grafico_flee", height = 350))),
              
              br(),
              
              fluidRow(
                box(width = 12,title = "Guía de uso: Risk Factor Builder",
                  status = "warning",
                  solidHeader = TRUE,
                  
                  HTML(
                    paste0(
                      "<b>¿Qué hace esta herramienta?</b><br>",
                      "Permite seleccionar factores históricos asociados a antecedentes de violencia, ",
                      "conductas problemáticas y experiencias traumáticas registradas en la base de datos.<br><br>",
                      
                      "<b>¿Cómo usarla?</b><br>",
                      "1. Marca uno o varios factores en la lista.<br>",
                      "2. Haz clic en <i>Construir Perfil</i>.<br>",
                      "3. El sistema mostrará cuántos casos históricos coincidieron con esa combinación.<br>",
                      "4. También verás su frecuencia y un perfil promedio de edad, víctimas y heridos.<br><br>",
                      
                      "<b>Importante:</b><br>",
                      "Esta herramienta es únicamente descriptiva y académica. ",
                      "No predice conductas individuales ni reemplaza evaluaciones profesionales.")))),
              
              fluidRow(box(width = 4, title = "Risk Factor Builder",
                           status = "danger",
                           solidHeader = TRUE,
                           checkboxGroupInput(
                             "risk_vars",
                             "Selecciona factores de riesgo:",
                             choices = c(
                               # Variables de CRIME & VIOLENCE
                               "Conocido por Policía/FBI" = "Known.to.Police.or.FBI",
                               "Antecedentes criminales" = "Criminal.Record",
                               "Abuso animal" = "Animal.Abuse",
                               "Violencia doméstica" = "History.of.domestic.abuse",
                               "Altercados físicos" = "History.of.Physical.Altercations",
                               "Ofensas sexuales" = "History.of.sexual.offenses",
                               "Pandillas" = "Gang.association",
                               
                               # Variables TRAUMA
                               "Bullying" = "Bullied",
                               "Crianza monoparental" = "Raised.by.single.parent",
                               "Divorcio parental" = "Parental.separation.or.divorce",
                               "Trauma infantil" = "Childhood.trauma",
                               "Abuso físico" = "Physical.Abuse",
                               "Abuso sexual" = "Sexual.Abuse",
                               "Abuso emocional" = "Emotional.Abuse",
                               "Negligencia" = "Neglect",
                               "Adult trauma" = "Adult.trauma"),
                             
                             selected = NULL),
                           
                           actionButton("run_risk","Construir Perfil",
                                        icon = icon("triangle-exclamation"),
                                        width = "100%",
                                        color = "red")),
                       
                       box(width = 8,title = "Coincidencias históricas",status = "warning",
                           solidHeader = TRUE,
                           valueBoxOutput("risk_cases"),
                           valueBoxOutput("risk_freq"),
                           plotlyOutput("risk_profile", height = 330),
                           br(),
                           DT::dataTableOutput("risk_table"),size="xl")),
              
              ))))



# PASO 2: Crear la lógica del servidor (server)
server <- function(input, output) {
  
  #----------------------------------------------------------------------------
  # PARA LA PESTAÑA 1:
  
  # Vamos a rearmar el gráfico desde aquí para asegurar que no hay problemas
  # de lectura desde rmarkdown al script 
  
  # Guardamos la selección del departamento
  departamento_sel <- reactiveVal(NULL)
  
  # Y con eso el botón para resetearlo
  observeEvent(input$reset_depto, {
    departamento_sel(NULL)
  })
  
  # Ahora si el mapa
  output$mapa_idf <- renderLeaflet({
    
    pal_dep <- colorNumeric(
      palette = c("red", "#339933"),
      domain = sf_data$IDF_dep,
      na.color = "#f0f0f0")
    
    labels_dep <- sprintf(
      "<strong>Departamento:</strong> %s<br/>
     <strong>IDF:</strong> %g<br/>
     <strong>Rango:</strong> %s",
      sf_data$DPNOM,
      sf_data$IDF_dep,
      sf_data$Rango_Cat) %>% lapply(htmltools::HTML)
    
    leaflet(sf_data,
            options = leafletOptions(
              zoomControl = TRUE,
              dragging = TRUE,
              scrollWheelZoom = FALSE)) %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      setView(lng = -74, lat = 4.5, zoom = 5) %>%  # Centramos en Colombia
      
        addPolygons(
          fillColor = ~pal_dep(IDF_dep),
          weight = 1,
          opacity = 1,
          color = "lightgray",
          dashArray = "1",
          fillOpacity = 0.7,
          
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#333",
            fillOpacity = 0.9,
            bringToFront = TRUE),
        
        label = labels_dep,
        
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"),
        
        layerId = ~DPNOM) %>%
      
      addLegend(pal = pal_dep,
        values = ~IDF_dep,
        opacity = 0.7,
        title = "IDF",
        position = "bottomright") %>%
      
      addControl(html = "
      <div style='font-size:15px;
                  font-weight:bold;
                  background:white;
                  padding:5px;
                  border-radius:5px;'>
      Índice de Desempeño Fiscal por Departamento - 2024
      </div>",
        position = "topright") %>%
      
      addControl(html = "
      <div style='font-size:10px;
                  color:gray;
                  background:rgba(255,255,255,0.5);
                  padding:2px;'>
      Fuente: DNP
      </div>",
        position = "bottomleft")})
  
  # Capturar click sobre el departamento
  observeEvent(input$mapa_idf_shape_click, {
    departamento_sel(input$mapa_idf_shape_click$id)})
  
  # Ahora, para una visualización extra creamos el promedio nacional
  idf_promedio_pais <- reactive({
  mean(base_dep$IDF_dep, na.rm = TRUE)})
  
  # El ranking de departamento
  ranking_dep <- reactive({
    base_dep %>%
      arrange(desc(IDF_dep)) %>%
      mutate(rank = row_number())
  })
  
  # Guardamos el IDF del depto seleccionado
  idf_depto_val <- reactive({
    if (is.null(departamento_sel())) return(NULL)
    
    base_dep %>%
      filter(DPNOM == departamento_sel())
  })
  
  # Y ponemos los text holders para esa información
  output$idf_depto <- renderValueBox({
    if (is.null(departamento_sel())) {
      return(valueBox(
        value = "-",
        subtitle = "Selecciona un departamento",
        color = "blue"
      ))}
    
    valueBox(
      value = round(idf_depto_val()$IDF_dep,2),
      subtitle = paste("IDF -", departamento_sel()),
      color = "blue")})
  
  output$idf_pais <- renderValueBox({
    valueBox(
      value = round(idf_promedio_pais(),2),
      subtitle = "Promedio nacional",
      color = "green")})
  
  output$ranking_depto <- renderValueBox({
    req(idf_depto_val())
    
    rank <- ranking_dep() %>%
      filter(DPNOM == departamento_sel()) %>%
      pull(rank)
    
    valueBox(
      value = paste("#", rank),
      subtitle = paste("Ranking nacional -", departamento_sel()),
      color = "purple")})
  
  # Ahora, creamos el top 3 municipal
  top_municipios_data <- reactive({
    
    if (is.null(departamento_sel())) {
      return(
        base_mun %>%
          arrange(desc(IDF)) %>%
          slice(1:3))}
    
    base_mun %>%
      filter(DPNOM == departamento_sel()) %>%
      arrange(desc(IDF)) %>%
      slice(1:3)})
  
  output$top_municipios <- renderUI({
    req(top_municipios_data())
    
    df <- top_municipios_data()
    
    if (is.null(departamento_sel())) {
      return(
        tagList(
          h4("Top 3 municipios a nivel nacional"),
          tags$ol(
            lapply(1:nrow(df), function(i) {
              tags$li(
                paste0(df$DPMP[i], " — IDF: ", round(df$IDF[i], 2))
              )}))))}
    
    tagList(
      h4(paste("Departamento:", departamento_sel())),
      
      tags$ol(
        lapply(1:nrow(df), function(i) {
          tags$li(
            paste0(df$DPMP[i], " — IDF: ", round(df$IDF[i], 2))
          )})))})
  
  # Y finalmente un histograma que muestre la distribución municipal del IDF
  output$hist_municipios <- renderPlotly({
    req(departamento_sel())
    
    df <- if (is.null(departamento_sel())) {
      base_mun
    } else {
      base_mun %>% filter(DPNOM == departamento_sel())
    }
    
    p <- ggplot(df, aes(x = IDF)) +
      geom_histogram(bins = 15, fill = "#C96A3D") +
      geom_vline(xintercept = mean(df$IDF, na.rm = TRUE),
        linetype = "dashed") +
      labs(x = "IDF",
        y = "Número de municipios") +
      theme_minimal()
    ggplotly(p)})
  
  
  #----------------------------------------------------------------------------
  # PARA LA PESTAÑA 2:
  
  # Creamos de nuevo el gráfico a doble eje interactivo
  output$grafico_tasas <- renderPlotly({
    
    df <- df_merge
    
    # Reescalar IBR al rango SOFR
    ibr_scaled <- (df$IBR - min(df$IBR)) /
      (max(df$IBR) - min(df$IBR)) *
      (max(df$SOFR) - min(df$SOFR)) +
      min(df$SOFR)
    
    plot_ly(df, x = ~Fecha) %>%
      
      add_lines(
        y = ~SOFR,
        name = "SOFR",
        line = list(width = 2),
        hovertemplate =
          paste(
            "Fecha: %{x|%d-%m-%Y}<br>",
            "SOFR: %{y:.2f}%<extra></extra>"
          )) %>%
      
      add_lines(
        y = ibr_scaled,
        name = "IBR",
        line = list(width = 2, color = "red"),
        hovertemplate =
          paste(
            "Fecha: %{x|%d-%m-%Y}<br>",
            "IBR: %{customdata:.2f}%<extra></extra>"
          ),
        customdata = ~IBR) %>%
      
      layout(
        title = "Evolución de las tasas SOFR e IBR (2020–2025)",
        
        hovermode = "x unified",
        
        xaxis = list(
          title = "Fecha",
          showspikes = TRUE,
          spikemode = "across",
          spikesnap = "cursor",
          spikedash = "dot",
          spikethickness = 1),
        
        yaxis = list(title = "SOFR (%)"),
        
        yaxis2 = list(
          title = "IBR (%)",
          overlaying = "y",
          side = "right"),

      legend = list(
          orientation = "h",
          x = 0.35,
          y = -0.15))})
  
  
  # Ahora, la calculadora de tasa de devaluación implícita:
  
  # Primero, buscará la fecha en el database
  
  # Establecemos las fechas para las que tenemos información
  fechas_validas <- sort(unique(df_merge$Fecha))
  
  tasas_fecha <- eventReactive(input$buscar_tasa, {
    
    req(input$fecha_tasa)
    
    fecha_buscada <- as.Date(input$fecha_tasa)
    
    resultado <- df_merge %>%
      filter(Fecha == fecha_buscada)

    resultado})

  # Luego, guardará la información y la mostrará en un text holder
  output$valor_ibr <- renderValueBox({
    
    df <- tasas_fecha()
    
    print("---- DEBUG ----")
    print(df)
    print(str(df))
    print(nrow(df))
    
    valor <- df$IBR[1]
    
    print(valor)
    print(typeof(valor))
    print(length(valor))
    
    valueBox(
      value = paste0(round(as.numeric(valor), 2), "%"),
      subtitle = "IBR",
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  output$valor_sofr <- renderValueBox({
    
    df <- tasas_fecha()
    req(df)
    req(nrow(df) > 0)
    
    valor <- df$SOFR[1]
    
    valueBox(
      value = paste0(round(as.numeric(valor), 2), "%"),
      subtitle = "SOFR",
      icon = icon("dollar-sign"),
      color = "blue")})
  
  # Finalmente, calcula la tasa
  output$valor_pi <- renderValueBox({
    
    df <- tasas_fecha()
    req(df)
    req(nrow(df) > 0)
    
    id <- as.numeric(df$IBR[1]) / 100
    ie <- as.numeric(df$SOFR[1]) / 100
    
    pi <- ((1 + id) / (1 + ie)) - 1
    
    valueBox(
      value = paste0(round(pi * 100, 2), "%"),
      subtitle = "Devaluación implícita",
      icon = icon("calculator"),
      color = "green")})


  #----------------------------------------------------------------------------
  # PARA LA PESTAÑA 3:
    observeEvent(input$mapa_shape_click, {
    print(input$mapa_shape_click)
  })
  
  # Estado seleccionado
    estado_sel <- reactiveVal(NULL)
    
    observeEvent(input$mapa_shape_click, {
      estado_sel(input$mapa_shape_click$id)
    })
  
  # Datos reactivos
  datos <- reactive({
      df %>%
      filter(Year >= input$year_range[1],
             Year <= input$year_range[2])})
  
  # Número de tiroteos por estado y por género
  datos_estado <- reactive({
    df <- recodificar(datos())
    
    # Filtramos según vista seleccionada (Hombres o Mujeres)
    # Importante saber que hay 2 tiroteos cuyos atacantes se identifican como
    # no binarios o transgéneros. Por facilidad los ignoramos.
    if (input$tipo_mapa == "Tiradores Hombres") {
      df <- df %>%
        filter(Gender == "Hombre")}
    if (input$tipo_mapa == "Tiradores Mujeres") {
      df <- df %>%
        filter(Gender == "Mujer")}
    
    # Unimos la información de la tabla con nombres completos por estado
    df %>%
      left_join(state_lookup, by = c("State" = "abb")) %>%
    # Reemplazamos la abreviatura por el nombre completo e imputamos el nombre 
    # de DC porque no está en state_lookup
      mutate(State = ifelse(is.na(name), "District of Columbia", name)) %>%
    # Eliminamos la columna extra 
      select(-name) %>%
    # Y agrupamos el número de tiroteo por estado
      group_by(State) %>%
      summarise(tiroteos = n(),.groups = "drop")})
  
  # Cargamos el mapa de EE.UU
  estados_sf <- reactive({
    options(tigris_use_cache = TRUE)
    states(cb = TRUE, class = "sf") %>%
      st_transform(4326)}) # Transformamos porque tigris y leaflet trabajan con
                          # geometrías distintas :v
  
  # Unimos la información de tiroteos con el mapa de estados
  mapa_data <- reactive({
    estados_sf() %>%
      left_join(datos_estado(), by = c("NAME" = "State"))})

  # Cargamos la paleta de colores
  pal <- reactive({
    colorNumeric(
      "YlOrRd",
      domain = mapa_data()$tiroteos,
      na.color = "#eeeeee"
    )
  })
  
  # Graficamos el mapa
  output$mapa <- renderLeaflet({
    
    #pal <- colorNumeric("Reds", domain = mapa_data()$tiroteos, na.color = "#cccccc")
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = mapa_data()$tiroteos,
      na.color = "#f0f0f0")
    
    leaflet(mapa_data(),
            options = leafletOptions(
              zoomControl = TRUE,     # dejamos botones + - por si acaso
              dragging = TRUE,         # permitimos que se pueda mover el mapa (para ver Alaska)
              scrollWheelZoom = FALSE, # quitamos el zoom con scroll
              doubleClickZoom = FALSE, # quitamos el doble click
              boxZoom = FALSE          # quitamos el zoom con drag
            )) %>%
      
      addProviderTiles("CartoDB.Positron") %>%
      
      # Centramos la vista en USA
      setView(lng = -98, lat = 39, zoom = 4) %>%
      
      addPolygons(
        fillColor = ~pal(tiroteos),
        fillOpacity = 0.85,
        color = "white",
        weight = 1.2,
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#333333",
          fillOpacity = 1,
          bringToFront = TRUE),
        layerId = ~as.character(NAME),
        label = ~lapply(paste0(
            "<strong>", NAME, "</strong><br>",
            "Tiroteos: ",
            ifelse(is.na(tiroteos), 0, tiroteos)), htmltools::HTML)) %>%
      addLegend(
        pal = pal,
        values = ~tiroteos,
        opacity = 0.9,
        title = "Número de eventos",
        position = "bottomright")})
 
  
  # Luego ponemos un botón que nos permita resetear los filtros
  observeEvent(input$reset_map, {
    leafletProxy("mapa") %>%
      fitBounds(-125, 24, -66, 49)
  })
  
  observeEvent(input$reset_map, {
    estado_sel(NULL)
  })
  
  # Y un botón que nos deje ver toda la información de cada tiroteo
  observeEvent(input$ver_tabla, {
    showModal(modalDialog(title = "Detalle de tiroteos",
        DT::dataTableOutput("tabla_modal"),
        size = "l",easyClose = TRUE))})
  
  # Creamos un data frame separado para las gráficas y los text holders
  
  datos_filtrados_estado <- reactive({
    df <- recodificar(datos())
    
    if (input$tipo_mapa == "Tiradores Hombres") {
      df <- df %>% filter(Gender == "Hombre")}
    if (input$tipo_mapa == "Tiradores Mujeres") {
      df <- df %>% filter(Gender == "Mujer")}
    
    if (is.null(estado_sel())) return(df)
    
    df %>%
      left_join(state_lookup, by = c("State" = "abb")) %>%
      mutate(State_full = ifelse(is.na(name), "District of Columbia", name)) %>%
      filter(State_full == estado_sel())
  })
  
 
  # Ahora, la tabla de información de tiroteos
  output$tabla_modal <- DT::renderDataTable({
    datos_filtrados_estado() %>%
      select(
        Full.Date,
        Shooter.Last.Name,
        Shooter.First.Name,
        Age,
        City,
        State,
        Location,
        Number.Killed,
        Number.Injured)},
    options = list(
      scrollX = TRUE,
      pageLength = 10,
      autoWidth = TRUE))
  
  
  # Ahora una visualización de la información del agresor (hacemos barras 
  # horizontales para visualizar las variables, excepto la edad)
  output$grafico_agresor <- renderPlotly({
    
    req(input$variable_agresor)
    var <- input$variable_agresor
    df <- datos_filtrados_estado()
    
    # Caso especial edad numérica: si elegimos Age hace un histograma
    if (var == "Age") {
      p<- ggplot(df, aes(x = Age)) +
        geom_histogram(bins = 15, fill = "#C96A3D") +
        labs(
          title = "Distribución de edad",
          x = "Edad",
          y = "Cantidad") +
        theme_minimal(base_size = 14)
      
    } else {
      
      tabla <- df %>%
        count(.data[[var]]) %>%
        filter(!is.na(.data[[var]])) %>%
        arrange(desc(n)) %>%
        head(10)
        
       p <- ggplot(
         tabla,
          aes(
            x = reorder(.data[[var]], n),
            y = n,
            text = paste0(.data[[var]],"<br>Total: ", n))) +
        geom_col(fill = "#C96A3D") +
        coord_flip() +
        labs(
          x = "",
          y = "Cantidad",) +
        theme_minimal(base_size = 14)
      ggplotly(p, tooltip = "text")}})

  
  # También una gráfica que muestre el top de motivadores del ataque
  output$grafico_motivos <- renderPlotly({
    df <- datos_filtrados_estado()
    motivos <- data.frame(
      Motivo = c(
        "Racismo",
        "Odio religioso",
        "Misoginia",
        "Homofobia",
        "Problema laboral",
        "Problema económico",
        "Problema legal",
        "Problemas sentimentales",
        "Conflicto interpersonal",
        "Búsqueda de fama",
        "Otro",
        "Desconocido"
      ),
      
      Variable = c(
        "Motive:.Racism/Xenophobia",
        "Motive:.Religious.Hate",
        "Motive:.Misogyny",
        "Motive:.Homophobia",
        "Motive:.Employment.Issue",
        "Motive:.Economic.Issue",
        "Motive:.Legal.Issue",
        "Motive:.Relationship.Issue",
        "Motive:.Interpersonal.Conflict ",
        "Motive:.Fame-Seeking",
        "Motive:.Other ",
        "Motive:.Unknown"))
    
    motivos$Total <- sapply(
      motivos$Variable,
      function(v) sum(df[[v]] == 1, na.rm = TRUE))
    
    p <- ggplot(
      motivos,
      aes(
        x = reorder(Motivo, Total),
        y = Total,
        text = paste0(Motivo, "<br>Total: ", Total))) +
      geom_col(fill = "#8C2D2D") +
      coord_flip() +
      labs(
        title = "Frecuencia de motivos registrados",
        x = "",
        y = "Cantidad de eventos") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")})
  
  # Una tabla que muestre la frecuencia de ciertos factores de riesgo en
  # los tiradores (Risk Factor Builder)
  
  # Creamos una base separada con las variables de interés 
  risk_data <- eventReactive(input$run_risk, {
    df <- datos_filtrados_estado()
    req(input$risk_vars)
    for(v in input$risk_vars){
      if(v %in% names(df)){
        df <- df %>% filter(.data[[v]] == 1)}}
   df})
  
  # Ahora los value boxes
  output$risk_cases <- renderValueBox({
    req(risk_data())
    valueBox(
      value = nrow(risk_data()),
      subtitle = "Casos históricos con esta combinación",
      icon = icon("users"),
      color = "purple")})
  
  output$risk_freq <- renderValueBox({
    req(risk_data())
    total <- nrow(datos_filtrados_estado())
    freq <- round(100*nrow(risk_data())/total,2)
    valueBox(
      value = paste0(freq,"%"),
      subtitle = "Frecuencia dentro del filtro actual",
      icon = icon("percent"),
      color = "blue")})
  
  # Definimos el perfil promedio
  output$risk_profile <- renderPlotly({
    df <- risk_data()
    req(nrow(df) > 0)
    perfil <- data.frame(
      Variable = c(
        "Edad promedio",
        "Víctimas promedio",
        "Heridos promedio"),
      Valor = c(
        mean(df$Age, na.rm=TRUE),
        mean(df$Number.Killed, na.rm=TRUE),
        mean(df$Number.Injured, na.rm=TRUE)))
    
    p <- ggplot(
      perfil,
      aes(x = Variable,
        y = Valor,
        text = paste0(Variable,": ", round(Valor,2))))+
      geom_col(fill="#8C2D2D")+
      theme_minimal(base_size = 14)+
      labs(x="",
        y="Valor promedio")
    ggplotly(p, tooltip="text")})
  
  # Y por último en esta sección: construimos la tabla de casos
  
  output$risk_table <- DT::renderDataTable({
    df <- risk_data()
    
    req(nrow(df) > 0)
    
    df %>%
      select(
        Full.Date,
        Shooter.First.Name,
        Shooter.Last.Name,
        Age,
        City,
        State,
        Location,
        Number.Killed,
        Number.Injured)},
    options = list(
      scrollX = TRUE,
      pageLength = 10,
      autoWidth = TRUE))
  
  
  # Ahora, una gráfica que muestre qué pasó en la escena y si hubo intento de huída:
  output$grafico_outcome <- renderPlotly({
    df <- datos_filtrados_estado()
    tabla <- df %>%
      count(`On-Scene.Outcome`) %>%
      filter(!is.na(`On-Scene.Outcome`))
    
      p <- ggplot(tabla,
        aes(
          x = reorder(`On-Scene.Outcome`, n),
          y = n,
          text = paste0(`On-Scene.Outcome`, "<br>Total: ", n))) +
      geom_col(fill = "#B94E48") +
      coord_flip() +
      labs(
        title = "¿Qué ocurrió?",
        x = "",
        y = "Cantidad") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")})
  
  output$grafico_flee <- renderPlotly({
    df <- datos_filtrados_estado()
    tabla <- df %>%
      count(Attempt.to.Flee) %>%
      filter(!is.na(Attempt.to.Flee))
    
      p <- ggplot( tabla,
        aes(x = Attempt.to.Flee, y = n, text = paste0(Attempt.to.Flee, "<br>Total: ", n))) +
      geom_col(fill = "#D9A441") +
      labs(
        title = "¿Intentó huir?",
        x = "",
        y = "Cantidad") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")})
  
  # Luego, gráficas de tendencia de los eventos :)
  output$grafico_anual <- renderPlotly({
    p <- datos_filtrados_estado() %>%
      count(Year) %>%
      ggplot(aes(x = Year, y = n, group = 1, text = paste("Año:", Year,"<br>Eventos:", n))) +
      geom_line(color = colores$gris, linewidth = 0.5) +
      geom_point(color = colores$mostaza,size = 1) +
      labs(x = "Año", y = "Cantidad", title="Número de eventos") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")})
  
  output$grafico_letalidad <- renderPlotly({
    p <- datos_filtrados_estado() %>%
      group_by(Year) %>%
      summarise(promedio = mean(Number.Killed, na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = promedio, group = 1, text = paste( "Año:", Year,
            "<br>Promedio:", round(promedio,2)))) +
      geom_line(color = colores$gris, linewidth = 0.5) +
      geom_point(color = colores$borgona,size = 1) +
      labs(
        x = "Año",
        y = "Promedio",
        title= "Promedio de víctimas") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")})
  
  # Finalmente ponemos la información para los text holders
  output$total_eventos <- renderValueBox({
    valueBox(
      value = nrow(datos_filtrados_estado()),
      subtitle = ifelse(is.null(estado_sel()),
                        paste("Total tiroteos -", input$tipo_mapa),
                        paste("Tiroteos en", estado_sel(), "-", input$tipo_mapa )),
      icon = icon("exclamation-triangle"),
      color = "red")})
  
  output$total_muertes <- renderValueBox({
    valueBox(
      value = sum(datos_filtrados_estado()$Number.Killed, na.rm = TRUE),
      subtitle = paste("Víctimas fatales -", input$tipo_mapa),
      icon = icon("skull", style = "color: grey;"),
      color = "black"
    )
  })
  
  output$total_heridos <- renderValueBox({
    valueBox(
      value = sum(datos_filtrados_estado()$Number.Injured, na.rm = TRUE),
      subtitle = paste("Heridos -", input$tipo_mapa),
      icon = icon("hospital"),
      color = "yellow"
    )
  })
  
  # Y ahora para el título cambien según la vista del mapa:
  output$titulo_mapa <- renderText({
    if(input$tipo_mapa == "Total tiradores"){
      return("Mapa de tiroteos en EE.UU.")}
    if(input$tipo_mapa == "Tiradores Hombres"){
      return("Mapa de tiroteos cometidos por hombres")}
    if(input$tipo_mapa == "Tiradores Mujeres"){
      return("Mapa de tiroteos cometidos por mujeres")}})
  

  # Finalmente, la descripción del dashboard
      output$descripcion <- renderUI({
        HTML(
          paste0(
            "Este panel interactivo analiza los tiroteos masivos ocurridos en Estados Unidos entre 1966 y 2023 ",
            "utilizando datos de <i>Violence Prevention Project (Peterson, J., & Densley, J. (n.d.).)</i>, antes conocido como <i>The Violence Project</i> .<br><br>",
            "Permite explorar patrones geográficos, tendencias temporales, perfiles demográficos ",
            "de los agresores, motivaciones registradas, señales previas de crisis y desenlaces posteriores.<br><br>",
            
            "Además, incorpora un <i>Risk Factor Builder</i>, herramienta interactiva que permite ",
            "seleccionar combinaciones de antecedentes de violencia, trauma y experiencias adversas ",
            "para identificar cuántos casos históricos compartieron dichos factores y cuál fue su perfil promedio.<br><br>",
  
            "<b>Cómo usar el panel:</b><br>",
            "• Selecciona rango de años.<br>",
            "• Cambia la vista por género.<br>",
            "• Haz clic sobre un estado para filtrar información.<br>",
            "• Construye combinaciones de factores de riesgo con el módulo inferior.<br>",
            "• Explora los gráficos interactivos."))})
}

# PASO 3: Crear y ejecutar la aplicación
shinyApp(ui, server)

# Gracias!!