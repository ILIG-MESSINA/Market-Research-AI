#
library(shiny)
library(shinyauthr)
library(shinydashboard)
library(shinydashboardPlus)
library(bslib)
library(shinythemes)
library(httr)       
library(jsonlite)   
library(pdftools)   
library(officer)    
library(readxl)     
library(tools)      

# 📌 Chargement du thème personnalisé
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "cerulean",
  bg = "#FFD005",
  fg = "#000000",
  primary = "#000000",
  secondary= "#FFFFFF",
  base_font = "Open Sans"
)

gemini <- function(prompt, 
                   temperature = 1,          # Contrôle la créativité (plus élevé = plus aléatoire)
                   max_output_tokens = 1024, # Nombre maximum de tokens dans la réponse
                   api_key = Sys.getenv("GEMINI_API_KEY"), # Récupération de la clé API depuis l’environnement
                   model = "gemini-2.0-flash") { # Modèle utilisé (Gemini 2.0 Flash)
  
  # Vérification si la clé API est vide
  if (nchar(api_key) < 1) { 
    api_key <- readline("Paste your API key here: ") # Demande la clé API à l'utilisateur
    Sys.setenv(GEMINI_API_KEY = api_key) # Stocke la clé API dans les variables d'environnement
  }
  
  # Création du chemin du modèle à interroger
  model_query <- paste0(model, ":generateContent")
  
  # Envoi de la requête POST à l'API Gemini
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query), # URL de l'API
    query = list(key = api_key), # Ajout de la clé API à la requête
    content_type_json(), # Définit le format de la requête en JSON
    encode = "json", # Encode les données en JSON
    body = list( # Contenu du corps de la requête
      contents = list( # Contenu envoyé à l'IA
        parts = list( # Partie contenant le texte du prompt
          list(text = prompt) # Le prompt fourni par l'utilisateur
        )),
      generationConfig = list( # Configuration de la génération de texte
        temperature = temperature, # Niveau de créativité du modèle
        maxOutputTokens = max_output_tokens # Limite du nombre de tokens
      )
    )
  )
  
  # Vérification de la réponse de l'API pour détecter d'éventuelles erreurs
  if (response$status_code > 200) {
    stop(paste("Error - ", content(response)$error$message)) # Stoppe l'exécution et affiche l'erreur
  }
  
  # Extraction du texte généré par l'IA depuis la réponse JSON
  candidates <- content(response)$candidates # Récupère la liste des réponses candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts)) # Extrait les parties du texte
  
  return(outputs) # Retourne le texte généré
}


# 📌 Dossier contenant les documents
folder_path <- "www/"

# 📌 Fonction pour extraire et fusionner le texte de plusieurs fichiers dans un dossier
extract_text_from_folder <- function(folder_path) {
  files <- list.files(folder_path, full.names = TRUE, pattern = "\\.(pdf|pptx|xlsx)$", ignore.case = TRUE)  
  if (length(files) == 0) stop("Aucun fichier PDF, PPTX ou Excel trouvé dans le dossier.")
  
  all_texts <- sapply(files, function(file) {
    ext <- tolower(file_ext(file))
    
    if (ext == "pdf") {
      return(paste(pdftools::pdf_text(file), collapse = " "))  
    } else if (ext == "pptx") {
      ppt <- officer::read_pptx(file)  
      slides_text <- sapply(seq_len(length(ppt$slides)), function(i) {
        paste(unlist(officer::slide_summary(ppt, i)$text), collapse = " ")
      })
      return(paste(slides_text, collapse = " "))  
    } else if (ext == "xlsx") {
      sheets <- excel_sheets(file)  
      excel_text <- sapply(sheets, function(sheet) {
        data <- readxl::read_excel(file, sheet = sheet)  
        paste(apply(data, 1, paste, collapse = " "), collapse = "\n")  
      })
      return(paste(excel_text, collapse = "\n"))  
    } else {
      return("")
    }
  })
  
  return(paste(all_texts, collapse = "\n\n---\n\n"))
}

# 📌 Fonction pour interroger Gemini avec les documents du dossier
gemini_multiple_documents <- function(folder_path, question,
                                      temperature = 1, 
                                      max_output_tokens = 1024,
                                      api_key = Sys.getenv("GEMINI_API_KEY"),
                                      model = "gemini-2.0-flash") {
  
  if (nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  # Extraction du texte des fichiers
  documents_text <- extract_text_from_folder(folder_path)
  
  # Création du prompt basé sur les documents
  prompt <- paste("Voici des informations extraites de plusieurs documents :\n\n", 
                  documents_text, 
                  "\n\nQuestion :", question, 
                  "\nRépondez uniquement en vous basant sur ces documents.")
  
  # Envoi de la requête à Gemini
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent"),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(parts = list(list(text = prompt))),
      generationConfig = list(temperature = temperature, maxOutputTokens = max_output_tokens)
    )
  )
  
  if (response$status_code > 200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
}

# 📌 Interface utilisateur (UI)
ui <- navbarPage(
  input_dark_mode(id = "dark_mode", mode = "light"),
  title = span("InsightsAI", style = "color: #FFD005; font-size: 28px", 
               img(src = "MTN_NEW_LOGO_1.png", style = "position: absolute; top: 10px; right: 2%;")),
  theme = custom_theme,
  
  # 🔹 Web Search Tab
  tabPanel("Web Search", icon = icon(name = "search"),
           sidebarLayout(
             sidebarPanel(width = 4,
                          textAreaInput("prompt1", "Please write your prompt here:", 
                                        placeholder = "Enter Your Query", rows = 14),
                          actionButton("submit_web", "Summit Query"),
                          actionButton("clear_text", "Clear Prompt"),  
                          actionButton("reset_web", "Clear Answers")  
             ),
             mainPanel(width = 8, 
                       verbatimTextOutput("web_result"))
           )
  ), 
  
  # 🔹 Reports Search Tab (modifié)
  tabPanel("Reports Search", icon = icon(name = "folder"),
           sidebarLayout(
             sidebarPanel(width = 4,
                          textAreaInput("prompt_reports", "Please write your prompt here:", 
                                        placeholder = "Enter Your Query", rows = 14),
                          actionButton("submit_reports", "Summit Query"),
                          actionButton("clear_text_reports", "Clear Prompt"),  
                          actionButton("reset_reports", "Clear Answers")  
             ),
             mainPanel(width = 8, 
                       verbatimTextOutput("report_result"))
           )
  )
)

# 📌 Serveur
server <- function(input, output, session) {
  
  # 🔹 Variables réactives pour stocker l'historique des résultats
  web_results <- reactiveVal("")  
  report_results <- reactiveVal("")  
  
  # 🔹 Exécuter Gemini en se basant sur les fichiers du dossier dans Reports Search
  observeEvent(input$submit_web, {
    req(input$prompt1)
    
    result1 <- gemini(input$prompt1)
    
    # Ajouter le nouveau résultat en conservant l'historique
    web_results(paste(web_results(), "\n\n🔹 **New Query to Web:**", Sys.time(), "\n", result1, collapse = "\n"))
  })
  
  # 🔹 Exécuter Gemini en se basant sur les fichiers du dossier dans Reports Search
  observeEvent(input$submit_reports, {
    req(input$prompt_reports)
    
    result <- gemini_multiple_documents(folder_path, input$prompt_reports)
    
    # Ajouter le nouveau résultat en conservant l'historique
    report_results(paste(report_results(), "\n\n🔹 **New Query to Reports:**", Sys.time(), "\n", result, collapse = "\n"))
  })
  
  # 🔹 Effacer uniquement le champ de texte
  observeEvent(input$clear_text, {
    updateTextAreaInput(session, "prompt1", value = "")
  })
  
  # 🔹 Effacer uniquement le champ de texte
  observeEvent(input$clear_text_reports, {
    updateTextAreaInput(session, "prompt_reports", value = "")
  })
  
  # 🔹 Réinitialiser l'affichage Reports Search lorsque "Effacer les réponses" est cliqué
  observeEvent(input$reset_reports, {
    report_results("")  # Réinitialise l’historique des résultats de l'onglet de recherche dans les rapports
  })
  
  # 🔹 Réinitialiser l'affichage Web Search lorsque "Effacer les réponses" est cliqué
  observeEvent(input$reset_web, {
    web_results("")  # Réinitialise l’historique des résultats de l'onglet de recherche sur le web
  })
  
  # 🔹 Met à jour l'affichage des résultats
  output$web_result <- renderText({ web_results() })
  output$report_result <- renderText({ report_results() })
}

# 📌 Lancement de l'application
shinyApp(ui = ui, server = server)


