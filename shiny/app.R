#### TEHA ####
# tee araldi tab, kus saab valida võistleja/d või võistkonna/d, kellega ennast võrrelda
# akternatiiv on võrreelda olemasolevatel graafikutel eraldi värviga märgi abil ennast konkurendiga
####

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(patchwork)
library(ggrepel)
library(hrbrthemes)
library(padr)
library(lubridate)

# Selleks, et uus äpi versioon Shiny serverisse tõsta käivita Terminalis käsk:
# cp ~/Dropbox/DataScience/R/xt_rogain/shiny/app.R ~/ShinyApps/xt_rogain/

# lae algandmed
load("~/Dropbox/DataScience/R/xt_rogain/shiny/xt_rogain.RData")

# Funktsioon, mille abil saab valitud skaala tekstid muuda boldiks
# https://stackoverflow.com/questions/39694490/highlighting-individual-axis-labels-in-bold-using-ggplot2?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
make_bold <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
  src_levels <- levels(src)                                 # retrieve the levels in their order
  brave <- boulder %in% src_levels                          # make sure everything we want to make bold is actually in the factor levels
  if (all(brave)) {                                         # if so
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) # then find out where they are
    b_vec <- rep("plain", length(src_levels))               # make'm all plain first
    b_vec[b_pos] <- "bold"                                  # make our targets bold
    b_vec                                                   # return the new vector
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}

# Kuna vaikimisi on ggplot graafikute tekstid liiga väikesed, siis tekita eraldi element, 
# mis tuleb igale graafikule juurde liita
theme_text_increase <- theme(
  axis.text = element_text(size = rel(1.3)),
  axis.title.x = element_text(size = rel(1.3)),
  axis.title.y = element_text(size = rel(1.3)),
  plot.subtitle = element_text(size = rel(1.3)),
  plot.title = element_text(size = 20),
  legend.text = element_text(size = rel(1.3)),
)


ui <- fluidPage(
  
  useShinyjs(),  # vajalik lisafunktsioonide (reset) jaoks
  
  theme = shinytheme("flatly"),  # muuda üldine kujundus

  titlePanel("XT rogaini tulemuste analüüs",
             windowTitle = "XT rogain"),
   
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Vali Võistleja nimi või võistkond"),
        br(),
        uiOutput("voistleja"),
        uiOutput("voistkond")
      ),
      
      wellPanel(
        uiOutput("voistlus")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Tulemus kokku",
                 plotOutput("punktid_kokku"),
                 plotOutput("koht_osakaal")
        ),
        tabPanel("Punktide võtmine cum",
                 plotOutput("punktid_cum", height = 600)
                 # plotOutput("punktid_30")
        ),
        tabPanel("Punktide võtmine 30 min",
                 # plotOutput("punktid_cum"),
                 plotOutput("punktid_30", height = 600)
        )
      )
    )
  )
)


server <- function(input, output) {
  
  # unikaalne list voistkondade, voistlejate ja osavõistluste nimedega
  # selle alusel genereerin dropdown listid
  valikud <- xt_rogain_toodeldud %>% 
      mutate(voistlus = fct_rev(voistlus)) %>% 
      distinct(voistlus, voistleja, voistkond)
  
  # tekita andmetabel, kus punktid on agregeeritud 30 min bin'dena kokku
  xt_rogain_30_min <- reactive({
    req(input$voistlus)  # kui osavõistlus on valimata, siis ära kuva midagi ja edasi arvuta
    
    xt_rogain_toodeldud %>% 
      filter(voistleja == input$voistleja | voistkond == input$voistkond | koht <= 3) %>% 
      # tekita eraldi andmeveerg 30 minutise detailsusega
      # selle peale grupeerin ja summeerin punktid
      thicken(by = "vaheaja_kell_min", interval = "30 mins", rounding = "up") %>% 
      select(-aeg_stardist) %>% 
      group_by(voistkond, voistlus, voistleja, vaheaja_kell_min_30_min, start, koht) %>%
      summarise_all(sum, na.rm = TRUE) %>% 
      group_by(voistlus, voistleja) %>%
      mutate(aeg_stardist = difftime(vaheaja_kell_min_30_min, start, units = "secs")) %>% 
      ungroup() %>% 
      # kuva aeg stardist kujul hh:mm:ss
      mutate(aeg_stardist = hms::as.hms(aeg_stardist))
  })
  
  # tekita unikaalsete võistlejate nimedega dropdown list
  # valida saab ühe nime
  # võimalik ka trükkides otsida
  output$voistleja <- renderUI({
    selectizeInput(inputId = "voistleja",
                   label = "Võistleja nimi:", 
                   choices = c("", levels(factor(valikud %>% 
                                                   distinct(voistleja) %>% 
                                                   pull(voistleja))))
    )
  })
  
  
  # tekita unikaalsete võistkondade nimedega dropdown list
  output$voistkond <- renderUI({
    selectizeInput(inputId = "voistkond", 
                   label = "Võistkond:", 
                   choices = c("", levels(factor(valikud %>% 
                                                   distinct(voistkond) %>% 
                                                   pull(voistkond))))
    )
  })
  
  
  
  # kui valin võistkonna nime, siis tühjenda võistleja valik
  observeEvent(input$voistkond, {
    reset("voistleja")
  })
  
  # kui valin võistleja nime, siis tühjenda võistkonna valik
  observeEvent(input$voistleja, {
    reset("voistkond")
  })
  
  
  # tekita dünaamiline droptown list kõist võistlustest, milles valitud võistleja
  # või võistkond on osalenud
  output$voistlus <- renderUI({
    # genereeri ainult siis kui võistleja või võistkond on valitud
    if (input$voistkond != "" | input$voistleja != "") {
      selectizeInput(inputId = "voistlus",
                     label = "Vali rogain:", 
                     choices = valikud %>%
                       filter(voistkond == input$voistkond | voistleja == input$voistleja) %>% 
                       distinct(voistlus) %>% 
                       mutate(voistlus = levels(factor(voistlus))) %>% 
                       pull(voistlus)
      )
    }
  })
  
  
  ## loo vahetabelid, mille abil kuvada grafaikutel valitud võistkonna tulemus
  ## vs top3 võistkonna tulemus igal osavõistlusel.
  
  # tabel valitud võistkonna ja top3 võistkonna punktide võtmistest
  xt_rogain_valikutega <- reactive({
    req(input$voistlus)
    
    xt_rogain_toodeldud %>% 
      filter((voistleja == input$voistleja | voistkond == input$voistkond) |   
             (voistlus %in% input$voistlus & 
                koht <= 3))
  })
  
  # valitud võistkonna võistluste list, et sellega piirata top3 võiskonna andmeid
  valiku_voistlused <- reactive({
    valikud %>%
      filter(voistkond == input$voistkond | voistleja == input$voistleja) %>% 
      distinct(voistlus) %>% 
      mutate(voistlus = levels(factor(voistlus))) %>% 
      pull(voistlus)
  })
  
  # top3 võistkonna koondtulemused kõigil võistlustel, kus valitud võistkond osales
  liidrid <- reactive({
    xt_rogain_toodeldud %>%
      filter(koht <= 3,
             voistlus %in% valiku_voistlused()) %>%
      mutate(tunnus = "top_3") %>% 
      distinct(voistkond, voistlus, tulemus, koht, tunnus)
  })
  
  
  # valitud võistkonna koondtulemused
  valitud_voistkond <- reactive({
    
    liidrid_keskmine <- liidrid() %>%
      group_by(voistlus) %>% 
      summarise(tulemus_mean = round(mean(tulemus, na.rm = TRUE), 0))
    
    xt_rogain_toodeldud %>%
      filter((voistleja == input$voistleja | voistkond == input$voistkond)) %>%
      mutate(tunnus = "valik") %>% 
      distinct(voistkond, voistlus, tulemus, koht, tunnus) %>% 
      left_join(liidrid_keskmine, by = "voistlus") %>% 
      mutate(protsent_esikolmiku_punktidest = tulemus / tulemus_mean)
  })
  
  
  #### Graafikud ####
  
  # loo koondgraafik valitud võistkonna kõigi osavõistluste tulemuste analüüsiks
  # võrdle tulemusi top3 võistkonnaga igal osavõistlusel
  output$punktid_kokku <- renderPlot({
    
    req(input$voistlus)  # võistleja või võistkond peab olema valitud, et graafik luua
    
    # värvid graafikule
    # see on vajalik, et tekitada custom legend
    cols <- c("valitud võistkond" = "#fc9272", "võistluse top 3" = "#377eb8")
    
    tulemus_punktid <- valitud_voistkond() %>% 
      ggplot(aes(voistlus, tulemus, group = voistkond, color = "valitud võistkond")) +
      geom_point(data = liidrid(), aes(color = "võistluse top 3"), alpha = 0.5, size = 3) +
      geom_point(data = valitud_voistkond(), aes(color = "valitud võistkond"), size = 3) +
      # valitud võistluse mull suurem
      geom_point(data = valitud_voistkond() %>% filter(voistlus == input$voistlus),
                 color = "#e41a1c", size = 4) +
      geom_text_repel(data = valitud_voistkond(), aes(label = tulemus),
                      color = "#e41a1c", size = 4.5) +
      # genereeri custom legend
      scale_colour_manual(name = "", values = cols) +
      coord_flip() +
      theme_ipsum_rc() +
      labs(title = "Punktid kokku rogainil",
           subtitle = "",
           x = "",
           y = "punktid kokku") +
      # valitud võistlus boldiks
      theme(axis.text.y = element_text(face = make_bold(unique(factor(valitud_voistkond()$voistlus)),
                                                        input$voistlus)),
            # legend üleval vasakul pealkirja all
            legend.position = c(0, 1),
            legend.justification = c(0, 0),
            legend.direction = "horizontal") +
      theme_text_increase  # muuda tekst suuremaks (vt skripti algusest)
    
    tulemus_punktid
  })
 
  
  # Koondgraafikul koht igal võistlusel ja punktide % top 3 võistkonna keskmisest punktide arvust
  output$koht_osakaal <- renderPlot({
    req(input$voistlus)
    
    tulemus_osakaal <- valitud_voistkond() %>% 
      ggplot(aes(voistlus, protsent_esikolmiku_punktidest)) +
      geom_point(color = "#fc9272", size = 3) +
      geom_point(data = valitud_voistkond() %>% 
                   filter(voistlus == input$voistlus), color = "#e41a1c",
                 size = 4) +
      geom_text_repel(data = valitud_voistkond(),
                      aes(label = str_c(round(protsent_esikolmiku_punktidest * 100, 0), "%")), 
                      color = "#e41a1c", size = 4.5) +
      coord_flip() +
      theme_ipsum_rc() +
      labs(title = "Punktide % ja koht võistlusel",
           subtitle = "Punktide osakaal top3 võistkonna keskmisest punktide arvust",
           x = "",
           y = "punktide osakaal") +
      scale_y_percent(expand = c(0.01, 0.01)) +
      theme(axis.text.y = element_text(face = make_bold(unique(factor(valitud_voistkond()$voistlus)), 
                                                        input$voistlus))) +
      theme_text_increase
    
    # Koondgraafik valitud võistkonna kõigi võistluste kohtadega
    tulemus_koht <- valitud_voistkond() %>% 
      ggplot(aes(voistlus, koht)) +
      geom_point(color = "#fc9272", size = 3) +
      geom_point(data = valitud_voistkond() %>% 
                   filter(voistlus == input$voistlus), color = "#e41a1c",
                 size = 4) +
      geom_text_repel(data = valitud_voistkond(),
                      aes(label = koht), 
                      color = "#e41a1c", size = 4.5) +
      coord_flip() +
      theme_ipsum_rc() +
      scale_y_reverse() +
      labs(title = "",
           subtitle = "Koht võistlusel",
           x = "",
           y = "mitmes koht") +
      theme(axis.text.y = element_blank(),
            plot.margin = unit(c(1, 1, 1, 1), "cm")) +  # muuda graafiku äärtes vaba ruumi
      theme_text_increase
    
    # ühel graafikul kaks eraldi graafikut koos
    wrap_plots(list(tulemus_osakaal, tulemus_koht), widths = c(2.5, 2))
    
  })
  
  # Valitud võistlusel kumulatiivne punktide võtmine ajaliselt kõikide punktide kohta
  output$punktid_cum <- renderPlot({
    req(input$voistlus)
    
    valitud_voistleja_voistlused <- xt_rogain_valikutega() %>%
      filter(voistleja == input$voistleja | voistkond == input$voistkond) %>% 
      distinct(voistkond, punktid, distants, vaheaja_kell_min, .keep_all = TRUE)

    valitud_voistleja_valitud_voistlus <- valitud_voistleja_voistlused %>%
      filter(voistlus == input$voistlus)
  
    liidrid <- xt_rogain_valikutega() %>%
      filter(voistlus == input$voistlus,
             koht <= 3) %>%
      distinct(voistkond, punktid, distants, vaheaja_kell_min, .keep_all = TRUE)
      
    valitud_voistleja_voistlused %>%
      ggplot(aes(aeg_stardist, punktid_cum, group = str_c(voistkond, voistlus))) +
      geom_step(color = "grey") +
      geom_step(data = liidrid, color = "#377eb8") +
      geom_step(data = valitud_voistleja_valitud_voistlus, color = "#e41a1c",
                size = 1.2) +
      # skaalal väärtused iga 1 H järel
      scale_x_time(breaks = seq(0, 3600 * 10, 3600),
                   limits = c(0, max(valitud_voistleja_voistlused$aeg_stardist) + 1800),
                   labels = function(x) str_c(hour(x), "H")) +
      geom_text_repel(data = valitud_voistleja_valitud_voistlus %>%
                        filter(vaheaja_kell_min == max(vaheaja_kell_min)),
                      aes(label = str_c(punktid_cum, "p (", koht, ". koht)")),
                      nudge_x = 5, color = "#e41a1c", size = 5.5) +
      geom_text_repel(data = liidrid %>%
                        filter(koht == 2) %>% 
                        filter(vaheaja_kell_min == max(vaheaja_kell_min)),
                      aes(label = "Võistluse top 3"),
                      nudge_x = 5, color = "#377eb8", size = 5.5) +
      theme_ipsum_rc() +
      theme(legend.position = "none") +
      labs(title = "Kumulatiivne punktide võtmine rogainil",
           subtitle = input$voistlus,
           x = "aeg stardist",
           y = "") +
      theme_text_increase
  })
  
  # Valitud võistlusel punktide korjamine 30 minuti gruppide lõikes
  output$punktid_30 <- renderPlot({
    req(input$voistlus)
    
    # värvid graafikule
    # see on vajalik, et tekitada custom legend
    cols <- c("valitud võistkond ja võistlus" = "#e41a1c", 
              "võistluse top 3" = "#377eb8",
              "valitud võistkonna muud võistlused" = "grey")
    
    valitud_voistleja_voistlused_30 <- xt_rogain_30_min() %>% 
      filter(voistleja == input$voistleja | voistkond == input$voistkond) %>% 
      distinct(voistkond, punktid, distants, vaheaja_kell_min, .keep_all = TRUE)
    
    valitud_voistleja_valitud_voistlus_30 <- valitud_voistleja_voistlused_30 %>% 
      filter(voistlus == input$voistlus)
    
    liidrid_30 <- xt_rogain_30_min() %>% 
      filter(voistlus == input$voistlus,
             koht <= 3) %>% 
      distinct(voistkond, punktid, distants, vaheaja_kell_min, .keep_all = TRUE)
    
    valitud_voistleja_voistlused_30 %>% 
      ggplot(aes(aeg_stardist, punktid, group = str_c(voistkond, voistlus))) +
      geom_point(aes(color = "valitud võistkonna muud võistlused"), size = 1) +
      geom_line(color = "grey", size = 0.4) +
      geom_point(data = liidrid_30, aes(color = "võistluse top 3"), size = 1.5) +
      geom_line(data = liidrid_30, color = "#377eb8", size = 1, alpha = 0.7) +
      geom_point(data = valitud_voistleja_valitud_voistlus_30, 
                 aes(color = "valitud võistkond ja võistlus"),
                 size = 3) +
      geom_line(data = valitud_voistleja_valitud_voistlus_30, color = "#e41a1c",
                size = 1.5) +
      scale_x_time(breaks = seq(0, 3600 * 15, 3600),
                   limits = c(0, max(valitud_voistleja_voistlused_30$aeg_stardist) + 1800),
                   labels = function(x) str_c(hour(x), "H")) +
      # genereeri custom legend
      scale_colour_manual(name = "", values = cols) +
      theme_ipsum_rc() +
      labs(title = "Punktide võtmine rogainil 30min lõikude kaupa",
           subtitle = str_c(input$voistlus, "\n\n"),  # alapealkirja alla paar tühja rida
           x = "aeg stardist",
           y = "") +
      # legend graafiku üles pealkirja alla
      theme(legend.position = c(0, 1),
            legend.justification = c(0, 0),
            legend.direction = "horizontal") +
      theme_text_increase
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

