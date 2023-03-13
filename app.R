library(waffle)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

library(shiny)
library(shinydashboard)

src <- read.table("student_mat.csv",sep=",",header=TRUE)


#Wykres dla zarzadzania czasem

plec_czas <- src[c("sex", "traveltime", "studytime", "freetime")]
colnames(plec_czas) <- c("Plec", "Czas na podroze", "Czas na nauke", "Czas wolny")


#Wykres dla wplywu rodziny

zycie_rodzinne <- src[c("Pstatus", "famsup", "G1", "G2", "G3")]
x <- aggregate(G1 + G2 + G3 ~ ., data = zycie_rodzinne, FUN = mean)
x1 <- aggregate(cbind(G1, G2, G3) ~ ., data = zycie_rodzinne, FUN = mean)

#Dla 3 egzaminow razem
tab_rodzina = matrix(c(x$`G1 + G2 + G3`[1],
                       x$`G1 + G2 + G3`[3],
                       x$`G1 + G2 + G3`[2], 
                       x$`G1 + G2 + G3`[4]),
                     ncol=2)

rownames(tab_rodzina) = c('Nie', 'Tak')
colnames(tab_rodzina) = c('A', 'T')

tab_rodzina <- round(tab_rodzina, 3)

tab_rodzina = as.table(tab_rodzina)

dimnames(tab_rodzina)[[2]] <- c("Nie", "Tak")
names(dimnames(tab_rodzina)) <- c("Czy rodzice wspieraja nauke", "Czy mieszka z rodzina")

#Dla 1 egzaminu
tab_rodzina1 = matrix(c(x1$`G1`[1],
                       x1$`G1`[3],
                       x1$`G1`[2], 
                       x1$`G1`[4]),
                     ncol=2)

rownames(tab_rodzina1) = c('Nie', 'Tak')
colnames(tab_rodzina1) = c('A', 'T')

tab_rodzina1 <- round(tab_rodzina1, 3)

tab_rodzina1 = as.table(tab_rodzina1)

dimnames(tab_rodzina1)[[2]] <- c("Nie", "Tak")
names(dimnames(tab_rodzina1)) <- c("Czy rodzice wspieraja nauke", "Czy mieszka z rodzina")

#Dla 2 egzaminu
tab_rodzina2 = matrix(c(x1$`G2`[1],
                        x1$`G2`[3],
                        x1$`G2`[2], 
                        x1$`G2`[4]),
                      ncol=2)

rownames(tab_rodzina2) = c('Nie', 'Tak')
colnames(tab_rodzina2) = c('A', 'T')

tab_rodzina2 <- round(tab_rodzina2, 3)
tab_rodzina2 = as.table(tab_rodzina2)

dimnames(tab_rodzina2)[[2]] <- c("Nie", "Tak")
names(dimnames(tab_rodzina2)) <- c("Czy rodzice wspieraja nauke", "Czy mieszka z rodzina")

#Dla 3 egzaminu
tab_rodzina3 = matrix(c(x1$`G3`[1],
                        x1$`G3`[3],
                        x1$`G3`[2], 
                        x1$`G3`[4]),
                      ncol=2)

rownames(tab_rodzina3) = c('Nie', 'Tak')
colnames(tab_rodzina3) = c('A', 'T')

tab_rodzina3 <- round(tab_rodzina3, 3)
tab_rodzina3 = as.table(tab_rodzina3)

dimnames(tab_rodzina3)[[2]] <- c("Nie", "Tak")
names(dimnames(tab_rodzina3)) <- c("Czy rodzice wspieraja nauke", "Czy mieszka z rodzina")



#Wykresy dla spozycia alkoholu

#Spozycie w tygodniu
spozycie_tydzien <- as.data.frame(table(src$Dalc))
czestotliwosc_tydzien <- as.numeric(spozycie_tydzien$Freq)

czesci_tydzien=c("Bardzo Niskie" = czestotliwosc_tydzien[1],
                 "Niskie"= czestotliwosc_tydzien[2],
                 "Sredie" = czestotliwosc_tydzien[3],
                 "Wysokie" = czestotliwosc_tydzien[4],
                 "Bardzo wysokie"= czestotliwosc_tydzien[5])

names(czesci_tydzien) = paste0(names(czesci_tydzien))

kolor <- c("#88B840","#C3D323","#F9EC35","#ED8D1F","#E34017")

#Spozycie w weekend
spozycie_weekend <- as.data.frame(table(src$Walc))
czestotliwosc_weekend <- as.numeric(spozycie_weekend$Freq)

czesci_weekend=c("Bardzo Niskie" = czestotliwosc_weekend[1],
                 "Niskie"= czestotliwosc_weekend[2],
                 "Sredie" = czestotliwosc_weekend[3],
                 "Wysokie" = czestotliwosc_weekend[4],
                 "Bardzo wysokie"= czestotliwosc_weekend[5])

names(czesci_weekend) = paste0(names(czesci_weekend))

col <- c("#88B840","#C3D323","#F9EC35","#ED8D1F","#E34017")


#Wplyw edukacji rodzicow na srednia liczbe punktow z egzaminow

rodzice_wyniki <- src[c("Medu", "Fedu", "G1", "G2", "G3")]

srednia_matka <- aggregate(G1 + G2 + G3 ~ Medu, data = rodzice_wyniki, FUN = mean)
colnames(srednia_matka) <- c("edu", "Matka")

srednia_ojciec <- aggregate(G1 + G2 + G3 ~ Fedu, data = rodzice_wyniki, FUN = mean)
colnames(srednia_ojciec) <- c("edu", "Ojciec")

srednia_razem <- as.data.frame(cbind(srednia_matka$edu, srednia_matka$Matka, srednia_ojciec$Ojciec))
colnames(srednia_razem) <- c("edu", "Matka", "Ojciec")

#Wplyw nieobecnosci

nieobecnosci_dane <- src[c("absences", "failures", "G1", "G2", "G3")]

nieobecnosci_egz <- aggregate(G1 + G2 + G3 ~ absences, data = nieobecnosci_dane, FUN = mean)
colnames(nieobecnosci_egz) <- c("Nieobecnosci", "Srednia ilosc punktow z egzaminow")

nieobecnosci_popr <- aggregate(failures ~ absences, data = nieobecnosci_dane, FUN = sum)
colnames(nieobecnosci_popr) <- c("Nieobecnosci", "Liczba poprawek")

nieobecnosci_razem <- as.data.frame(cbind(nieobecnosci_egz$Nieobecnosci,
                            nieobecnosci_egz$`Srednia ilosc punktow z egzaminow`,
                            nieobecnosci_popr$`Liczba poprawek`))
colnames(nieobecnosci_razem) <- c("Nieobecnosci",
                                 "Srednia ilosc punktow z egzaminow",
                                 "Liczba poprawek")


#UI
ui <- dashboardPage(skin = "yellow",
    dashboardHeader(title = "Życie młodzieży"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Zarzadzanie czasem", tabName = "plot1", icon = icon("clock")),
        menuItem("Wyniki z egzaminow", tabName = "plot2", icon = icon("book")),
        menuItem("Spozycie alkoholu", tabName = "plot3", icon = icon("flask")),
        menuItem("Wyksztalcenie rodzicow", tabName = "plot4", icon = icon("school-circle-check")),
        menuItem("Nieobecnosci i poprawki", tabName = "plot5", icon = icon("user-graduate"))
      )
    ),
    dashboardBody(
      
      tabItems(
        tabItem("plot1",
                box(plotOutput("plot1"), width = 12),
                box(selectInput("cechy","Cechy:", c("Czas na podroze",
                                                    "Czas na nauke",
                                                    "Czas wolny")), width = 4)
        ),
                
        tabItem("plot2",
                titlePanel(h1("Srednia punktow z egazminow w zaleznosci od relacji z rodzina",
                              style={'background-color:lightblue;
                                      margin-left: -15px;
                                      margin-right: -15px;
                                      padding-left: 15px;
                                      font-size: 24px'})),
                box(plotOutput("plot2"), width = 18),
                radioButtons("egz", "Wybierz egzamin:",
                     c("1 Egzamin", "2 Egzamin", "3 Egzamin", "Razem"),
                     selected = "1 Egzamin",
                     inline = TRUE)
        ),
        
        tabItem("plot3",
                box(plotOutput("plot3"), width = 12),
                sliderInput("ilosc", "Liczba uczniow na jeden kwadrat:",
                            min = 1, max = 10, value = 1),
                radioButtons("opcja", "Wybierz opcje:",
                  c("W tygodniu", "W weekend", "Oba"),
                  selected = "Oba",
                  inline = TRUE
                )
        ),
        
        tabItem("plot4",
                box(plotOutput("plot4"), width = 12),
                column(5,
                  strong("Wybor rodzica:"),
                  checkboxInput("matka", "Matka", TRUE),
                  checkboxInput("ojciec", "Ojciec", TRUE),
                  sliderInput("dolny_limit", "Dolny limit osi Y:",
                              min = 0, max = 25, value = 20),
                ),
                
                column(4, offset = 2,
                  strong("Poziomy edukacji:"),
                  p("0 - Brak lub nieznane"),
                  p("1 - Wyksztalcenie podstawowe"),
                  p("2 - Wyksztalcenie srednie"),
                  p("3 - Wyksztalcenie wyzsze"),
                  p("4 - Wyksztalcenie ponad wyzsze"),
                )
        ),
        tabItem("plot5",
                box(plotOutput("plot5"), width = 12),
                box(selectInput("dane_x","Dane na osi X:", c("Nieobecnosci", 
                                                             "Srednia ilosc punktow z egzaminow",
                                                             "Liczba poprawek")), width = 6),
                
                box(selectInput("dane_y","Dane na osi Y:", c("Srednia ilosc punktow z egzaminow",
                                                             "Nieobecnosci",
                                                             "Liczba poprawek")), width = 6)
        )
      )
    ),
    
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4"),
      plotOutput("plot5")
    )
    
)

#SERVER
server <- function(input, output) {

    output$plot1 <- renderPlot({
      
      ggplot(plec_czas, aes(x = .data[[input$cechy]], y = Plec, fill = Plec)) + 
      geom_boxplot() + coord_flip() + geom_jitter(height = 0.05, inherit.aes = TRUE) +
      theme_minimal() + ggtitle("Zarzadzanie czasem") + ylab("Plec") +
      theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette="Set3")
    })
      
    output$plot2 <- renderPlot({
      
      switch(input$egz, 
             "1 Egzamin" = fourfoldplot(margin.table(tab_rodzina1, c(1, 2)),
                                         conf.level = 0,
                                         std = "ind.max",
                                         margin = 1),
             "2 Egzamin" = fourfoldplot(margin.table(tab_rodzina2, c(1, 2)),
                                        conf.level = 0,
                                        std = "ind.max",
                                        margin = 1),
             "3 Egzamin" = fourfoldplot(margin.table(tab_rodzina3, c(1, 2)),
                                        conf.level = 0,
                                        std = "ind.max",
                                        margin = 1),
             "Razem" = fourfoldplot(margin.table(tab_rodzina, c(1, 2)),
                                    conf.level = 0,
                                    std = "ind.max",
                                    margin = 1)
      )
    })
    
    output$plot3 <- renderPlot({
      
      czesci_tydzien_p <- round(czesci_tydzien/input$ilosc)
      wykres_tydzien <- waffle(czesci_tydzien_p,
                               rows = 10, 
                               size=1, 
                               glyph_size = 15,
                               title = "Spozycie alkoholu przez uczniow w tygodniu",
                               colors = kolor)
      
      czesci_weekend_p <- round(czesci_weekend/input$ilosc)
      wykres_weekend <- waffle(czesci_weekend_p,
                               rows = 10, 
                               size=1,
                               glyph_size = 15,
                               title = "Spozycie alkoholu przez uczniow w weekend",
                               colors = col)
      
      switch(input$opcja, 
             "W tygodniu" = print(wykres_tydzien),
             "W weekend" = print(wykres_weekend),
             "Oba" = grid.arrange(wykres_tydzien,wykres_weekend, nrow=2)
      )
    })
    
    output$plot4 <- renderPlot({
      if (input$matka == TRUE && input$ojciec == TRUE) {

        srednia_razem <- as.data.frame(cbind(srednia_matka$edu, srednia_matka$Matka, srednia_ojciec$Ojciec))
        colnames(srednia_razem) <- c("edu", "Matka", "Ojciec")
        edukacja_wybor <- melt(srednia_razem[,c("edu","Matka","Ojciec")],id.vars = 1)
        colnames(edukacja_wybor) <- c("PoziomEdukacjiRodzica", "Rodzic", "SumaPkt")
      }
      else if (input$matka == TRUE && input$ojciec == FALSE){
        edukacja_wybor <- melt(srednia_matka[,c("edu","Matka")],id.vars = 1)
        colnames(edukacja_wybor) <- c("PoziomEdukacjiRodzica", "Rodzic", "SumaPkt")
      }
      else if (input$matka == FALSE && input$ojciec == TRUE){
        edukacja_wybor <- melt(srednia_ojciec[,c("edu","Ojciec")],id.vars = 1)
        colnames(edukacja_wybor) <- c("PoziomEdukacjiRodzica", "Rodzic", "SumaPkt")
      }
      
      if (input$matka == TRUE || input$ojciec == TRUE){
        ggplot(data = edukacja_wybor, aes(x = PoziomEdukacjiRodzica, y = SumaPkt)) + 
          geom_bar(aes(fill = Rodzic), stat = "identity", position = "dodge") +
          coord_cartesian(ylim=c(input$dolny_limit, 40)) + ylab("Srednia suma punktow dziecka z trzech egzamnow") +
          xlab("Poziom Edukacji Rodzica") +
          ggtitle("Zaleznosc miedzy poziomem edukacji rodzicow a wynikami dzieci") + 
          theme(plot.title = element_text(hjust = 0.5))
      }
    })
    
    output$plot5 <- renderPlot({
      
      ggplot(nieobecnosci_razem, aes(x = .data[[input$dane_x]], y = .data[[input$dane_y]])) + 
        geom_point(size = 3, shape = 21, fill = "darkred") + theme_minimal() + 
        ggtitle("Zaleznosci miedzy nieobecnosciami, poprawkami i wynikami egzaminow") +
        theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(formula = y ~ x, method = "lm")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
