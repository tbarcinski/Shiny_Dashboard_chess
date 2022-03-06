library(dashboardthemes)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(plotly)
library(tidyr)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(shinydashboard)
library(shinyWidgets)
library(ggtext)
library(scales)

####################### loading data ########################
df_plot1 <- read.csv("dane_obrobione/df_plot1.csv", na.strings=c("","NA"))
df_plot2 <- read.csv("dane_obrobione/df_plot2.csv", na.strings=c("","NA"))
df_0 <- read.csv("dane_obrobione/df_0.csv", na.strings=c("","NA"))
df_ruchygracza <- read.csv("dane_obrobione/df_ruchygracza.csv", na.strings=c("","NA"))
openingi <- read.csv("dane_obrobione/openingi.csv", na.strings=c("","NA"))
df_heatmap<- read.csv("dane_obrobione/df_heatmap.csv", na.strings=c("","NA"))
df_scatter<- read.csv("dane_obrobione/df_scatter.csv", na.strings=c("","NA"))


shinyServer(function(input, output, session) {
    
    output$gif <- renderUI({
        serce <- case_when((input$dane_heatmap=="Caro Kann"|input$dane_heatmap=="Caro-Kann Defense") ~ '<img src="gify/caro_kann_white.gif"  width = "100%" height = "100%">' ,
                           input$dane_heatmap=="English Opening"   ~  '<img src="gify/english.gif"  width = "100%" height = "100%">'  ,
                           (input$dane_heatmap=="Four Knights Game"| input$dane_heatmap=="Four Knights")  ~  '<img src="gify/four_knights_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="French Defense"   ~  '<img src="gify/french_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Giuoco Piano"   ~  '<img src="gify/giuoco_piano_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Grunfeld Defense"   ~  '<img src="gify/grunfeld.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Indian Defense"   ~  '<img src="gify/indian.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Italian Game"   ~  '<img src="gify/italian_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Modern Defense"   ~  '<img src="gify/modern.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Philidor Defense"   ~  '<img src="gify/philidor_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Pirc Defense"   ~  '<img src="gify/pirc.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Queen's Gambit Declined"   ~  '<img src="gify/qgd.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Queen's Pawn Game"   ~  '<img src="gify/queens_pawn_game.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Russian Game"   ~  '<img src="gify/russian.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Ruy Lopez"   ~  '<img src="gify/ruy_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Scandinavian Defense"   ~  '<img src="gify/scandi_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Sicilian Defense"   ~  '<img src="gify/sicilian_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Three Knights Opening"   ~  '<img src="gify/three_knights_white.gif"  width = "100%" height = "100%">'  ,
                           input$dane_heatmap=="Zukertort Opening"     ~  '<img src="gify/zukerkort_white.gif"  width = "100%" height = "100%">'  ,
                           TRUE ~ '<img src="gify/zukerkort_white.gif"  width = "100%" height = "100%">')
        shiny::HTML(serce)
    })
    
    text = paste("No data")
    emptyPlot <- ggplot() + 
        annotate(geom = "text",
                 x = 4,
                 y = 25,
                 size=8,
                 label = text,
        ) + 
        theme_void()
    emptyPlot <- ggplotly(emptyPlot)
    
    output$plot1_k <- renderPlotly({
        
        df <- df_plot1%>%
            mutate(date= as.Date(date))%>%
            filter(rodzaj==input$rodzaj) %>%
            filter(date >= input$dates[1] & date <= input$dates[2])
        p <- ggplot(df, aes(x = date)) +
            geom_line(aes(y = grooney), color = "#3f9428") +
            geom_line(aes(y = Mlodziak77), color = "#283f94") +
            geom_line(aes(y = tymekkk), color = "#94283f") +
            labs(y = "Player rating", x = element_blank(),
                 title = "Players rating across time")  + theme_bw()
        ggplotly(p)
    })
    
    output$heatmap_time <- renderPlotly({
      
      if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
        fig <- emptyPlot
      }
      else{
        df_heatmap_wynik <- df_heatmap %>%
          filter(color==input$color, gracz==input$gracz, rodzaj==input$rodzaj_2) %>%
          select(day, hour, input$category) %>%
          pivot_wider(names_from = hour, values_from = input$category) %>%
          ungroup() %>%
          arrange(day) %>%
          select(-day)
        df_heatmap_wynik <- df_heatmap_wynik[, as.character(c(0:23))]
        
        fig <- plot_ly(
          x = c(0:23),
          y = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"),
          z = as.matrix(df_heatmap_wynik),
          type = "heatmap",
          colors = colorRamp(c("#543005", "#f6e8c3"))) %>%
          layout(
            title = "When do we play most and, more importantly, when do we play best?",
            xaxis = list(
              dtick = 1,
              tick0 = 0,
              tickmode = "linear"
            ))%>% config(displayModeBar = FALSE,
                         responsive = FALSE,
                         scrollZoom=FALSE,
                         showAxisDragHandles=FALSE,
                         showAxisRangeEntryBoxes=FALSE) %>% 
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        
      }
      
      fig
      
    })
    
    output$plot2_K_Pieces <- renderPlotly({
      
      
      if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
        fig <- emptyPlot
      }
      else{
        df<-df_plot2 %>% 
          filter(rodzaj==input$rodzaj_2, gracz==input$gracz, typ==input$typ2, 
                 color==input$color)
        
        df$nazwa <- factor(df$nazwa, levels = c("R", "N", "B", "K", "Q", "B ", "N ", "R ")) 
        
        
        fig <- ggplot(df, aes(x = nazwa, y = n)) + geom_col(fill = "#b48864ff") +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          labs(title = "Pieces", x = element_blank(), y = "Total number")  +
          theme_bw()
        
        fig <- ggplotly(fig)%>% config(displayModeBar = FALSE,
                                       responsive = FALSE,
                                       scrollZoom=FALSE,
                                       showAxisDragHandles=FALSE,
                                       showAxisRangeEntryBoxes=FALSE) %>% 
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      }
      fig
    })
    
    output$plot2_K_Pawns <- renderPlotly({
      
      if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
        fig <- emptyPlot
      }
      else{
        df<-df_plot2 %>% 
          filter(rodzaj==input$rodzaj_2, gracz==input$gracz,
                 typ==input$typ1,color==input$color)
        
        
        fig <- ggplot(df, aes(x = nazwa, y = n)) + geom_col(fill = "#b48864ff") +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          labs(title = "Pawns from different lines", x = element_blank(), y = "Total number") +
          theme_bw()
        
        fig <- ggplotly(fig)%>% config(displayModeBar = FALSE,
                                       responsive = FALSE,
                                       scrollZoom=FALSE,
                                       showAxisDragHandles=FALSE,
                                       showAxisRangeEntryBoxes=FALSE) %>% 
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      }
      fig
      
    })
    
    output$scatter_openings <- renderPlotly({
        
        df_0 <- df_0 %>% 
            filter(rodzaj==input$rodzaj, color == input$color_3, n >=2)
        
        colnames(df_0)[c(3)] <- "number"
        
        p_0<-ggplot(df_0,mapping = aes(x=number,y=mean,color=gracz, opening = opening_name))+
            geom_jitter(width = 0.05, height = 0.01)+ 
            scale_x_continuous(trans = 'log2',breaks = c(2,4,8,16,32,62,128, 256,512,1024)) +
            labs(x = "Number of games (logaritmic scale)", y = "Average of points",
                 title = "Average score vs. number of games across openings") +
            scale_color_manual(values = c("#3f9428", "#283f94", "#94283f")) + theme_bw()
        
        ggplotly(p_0, tooltip = c("number", "mean", "color", "opening")) %>% 
            layout(legend=list(title=list(text='Player')))
    })
    
    output$scatter_plot <- renderPlotly({
        
        if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
            fig <- emptyPlot
        }
        else{
            df_here <- df_scatter %>%
                mutate(date = as.Date(data, format = "%Y.%m.%d")) %>%  
                filter(color==input$color_2, gracz==input$gracz, rodzaj == input$rodzaj_2) %>% 
                filter(date >= input$dates_2[1] & date <= input$dates_2[2])
            
            fig <- ggplot(df_here, aes(x = ranking_przeciwnik, y = ranking_gracz,
                                       color = as.character(wynik))) +
                geom_abline(intercept = 0, slope = 1) +
                geom_point(alpha = 0.4, size = 0.7) +
                geom_smooth(method = lm, formula = y ~ x, se=FALSE, size = 0.5)  +
                theme_bw()
            
            fig <- ggplotly(fig) %>% 
                layout(title = "Opponents' rating vs our rating",
                       legend =list(title=list(text='Score')),
                       xaxis = list(title = "Opponents' rating"),
                       yaxis = list(title = "Our rating"))
        }
        fig
        
    })
    
    observe(updateSelectInput(session,inputId ="dane_heatmap" ,
                              choices = head(pull(filter(df_0,color==input$color_2,
                                                         gracz==input$gracz, rodzaj==input$rodzaj_2),opening_name),5)))
    
    output$heatmap_openings <- renderPlotly({
      
      if(input$gracz == "tymekkk" & input$rodzaj_2 != "blitz"){
        p <- emptyPlot
      }
      else{
        df_heatmap<-df_ruchygracza %>%
          filter(color == input$color_2,
                 opening_name==input$dane_heatmap,
                 gracz==input$gracz,
                 rodzaj==input$rodzaj_2)
        
        p <- ggplot(df_heatmap, aes(X, Y, fill= count)) +
          geom_tile()+
          scale_fill_gradient(high = "#f6e8c3",
                              low = "#543005")+
          scale_y_continuous(breaks = seq(1, 8, len = 8))+
          theme(panel.background = element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank())+
          ggtitle("Where do we move pices across different opennings?")+
          coord_fixed()
      }
      ggplotly(p)%>% config(displayModeBar = FALSE,
                            responsive = FALSE,
                            scrollZoom=FALSE,
                            showAxisDragHandles=FALSE,
                            showAxisRangeEntryBoxes=FALSE) %>% 
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      
    })
})