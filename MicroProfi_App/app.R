## Load required packages:
library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(wesanderson)
library(RColorBrewer)

### Ui code begins below:
ui <- navbarPage(#shinythemes::themeSelector(),
    theme = shinytheme("yeti"), 
    title = h3("Demo App: Kshitij Kolas"),
    position = "static-top",
    
    ## First Tab:
    tabPanel(h4("Data of free standing component"),
             sidebarLayout(
                 ## Upload the data for free standing component
                 sidebarPanel(h3("Upload File"),
                              fileInput("file1", "Upload .txt/.csv File/s for the data of Free Standing Component", multiple = TRUE,
                                        accept = c("text/csv","text/comma-separated-values, text/plain",".csv")),
                              checkboxInput(inputId = 'header1', label = 'Header', value =  T),
                              checkboxInput(inputId = "stringAsFactors1", "stringAsFactors", FALSE),
                              selectInput(inputId = 'sep1', label =  'Separator', choices =   c(Comma =',',Semicolon =';', Tab='\t', Space=' ', Period ='.'), selected  =  ';'),
                              wellPanel(uiOutput("selectfile1")),
                              wellPanel(h3("Select The X and Y Axes Data"),
                                        selectInput('xcol1', 'X Variable', "", selected = ""),
                                        selectInput('ycol1', 'Y Variable', "", selected = "")),
                              wellPanel(h3("Polynomial Adjustment"),
                                        actionButton('poly', 'Do Polynomial Adjustment'),
                                        sliderInput('degree', 'Degree of the Polynomial:', min = 2, max = 10, value = 2, step = 1)),
                              wellPanel(h3("Degree of Rotation"),
                                        sliderInput('angle', 'Angle of the rotation:', min = 0, max = 360, value = 0, step = 1)),
                              #wellPanel(h3("Type of angle"),
                              #selectInput("angle2", "Type of an angle:", choices = c(tan, atan), selected = 'tan')),
                              width = 3),
                 
                 mainPanel(
                     ## Original Data Sets:
                     wellPanel(h3("Raw Data"), uiOutput("tb1"),
                               actionButton("view_data", "View Data"),
                               actionButton("hide_data", "Hide Data")),
                     wellPanel(h3("Plot Output"),
                               ## Plotly plot display:
                               plotlyOutput("plot1"),
                               style = "padding: 45px;", #click = "plot_click")), #, brush = "User_brush_1")),
                               actionButton("view_plot1", "Raw Data Plot"),
                               actionButton("view_plot2", "Rotate Plot"),
                               actionButton("view_plot3", "Normalize Plot"),
                               actionButton("view_plot4", "View Only Normalized Plot"),
                               actionButton("Line", "Draw Reference Line"),
                               actionButton("remove", "Remove Outliers"),
                               actionButton("separate", "Separate PCB and component"),
                               actionButton("view_plot5", "Draw Line")),
                     wellPanel(h3("Select 5 Points for Polynomial Adjustment"),
                               selectInput("points", "Select N points on PCB Base Line", choices = c("x1","x2","x3","x4","x5")),
                               DT::dataTableOutput("dataTable"),
                               actionButton("rem_point", "Remove Last Point"),
                               actionButton("plot_selected", "Plot Selected Points")),
                     ## Updated Data Sets:
                     wellPanel(h3("Filtered Data"),
                               DT::dataTableOutput("brush"),
                               downloadButton(outputId = "Filtered_Data_Default_000", label = "Download Table")),
                     width = 9)
             )),
    ## Second Tab
    tabPanel(h4("Data of Pure PCB without component"),
             sidebarLayout(
                 ## Upload the Data of Pure PCB without component
                 sidebarPanel(h3("Upload File"),
                              fileInput("file2", "Upload .txt/.csv File/s for the data of Pure PCB without component", multiple = T,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values, text/plain",
                                                   ".csv")),
                              checkboxInput(inputId = 'header2', label = 'Header', value =  T),
                              checkboxInput(inputId = "stringAsFactors2", "stringAsFactors", FALSE),
                              selectInput(inputId = 'sep2', label =  'Separator', choices =   c(Comma =',',Semicolon =';', Tab='\t', Space=' ', Period ='.'), selected  =  ';'),
                              wellPanel(uiOutput("selectfile2")),
                              wellPanel(h3("Select The X and Y Axes Data"),
                                        selectInput('xcol2', 'X Variable', "", selected = ""),
                                        selectInput('ycol2', 'Y Variable', "", selected = "")),
                              width = 3),
                 mainPanel(
                     ## Original Data Sets:
                     wellPanel(h3("Raw Data"), uiOutput("tb2")),
                     wellPanel(h3("Plot Output"),
                               ## Plotly plot display:
                               plotOutput("plot2", brush = "User_brush_2")),
                     ## Updated Data Sets:
                     wellPanel(h3("Filtered Data"),
                               DT::dataTableOutput("table_2"),
                               downloadButton(outputId = "mydownload2", label = "Download Table"))
                 ))),
    
    ## Third Tab:
    tabPanel(title = h4("Unmounted State")),
    
    ## Fourth Tab:
    tabPanel(title = h4("Mounted State")),
    
    ## Fifth Tab:
    tabPanel(title = h4("Interaction"))
    
)


### Server side code begins below:
server <- function(input, output, session){
    
    ### Tab 1:
    output$filedf1 <- renderTable({
        if(is.null(input$file1)){return ()}
        input$file1 
    })
    
    # Extract the file path for file
    output$filedf2 <- renderTable({
        if(is.null(input$file1)){return ()}
        input$file1$datapath
    })
    
    ## Below code to display the structure of the input file object
    output$fileob1 <- renderPrint({
        if(is.null(input$file1)){return ()}
        str(input$file1)
    })
    
    # Following code displays the select input widget with the list of file loaded by the user
    output$selectfile1 <- renderUI({
        if(is.null(input$file1)) {return()}
        list(hr(), 
             helpText("Select the files for which you need to see data and summary stats"),
             selectInput("Select1", "Select", choices=input$file1$name))
    })
    
    ## Summary Stats code
    output$summ1 <- renderPrint({
        if(is.null(input$file1)){return()}
        summary(read.table(file=input$file1$datapath[input$file1$name==input$Select1], 
                           sep=input$sep1, 
                           header = input$header1, 
                           stringsAsFactors = input$stringAsFactors1))})
    
    ## Reading the uploaded .CSV file: 
    data1 <- reactive({
        req(input$file1)
        df1 <- read.table(file=input$file1$datapath[input$file1$name==input$Select1], sep=input$sep1, header = input$header1, stringsAsFactors = input$stringAsFactors1)
        updateSelectInput(session, inputId = 'xcol1', label = 'X Variable',
                          choices = names(df1), selected = names(df1)[1])
        updateSelectInput(session, inputId = 'ycol1', label = 'Y Variable',
                          choices = names(df1), selected = names(df1))
        return(df1)
    })
    
    ## Data Table 
    output$table1 <- DT::renderDataTable({
        if(is.null(input$file1)){return()}
        else
            data1()
    })
    
    d_save <- vector()
    X <- vector()
    Y <- vector()
    
    
    ## Plot Raw Data:
    observeEvent(input$view_plot1, {
        output$plot1 <- renderPlotly({
            x1 <- data1()[, c(input$xcol1, input$ycol1)]
            M <- x1
            #plot
            g <- ggplot(data =  M) + 
                geom_point(aes_string(x= M[,1], y= M[,2]), colour= "red", size = 0.1)
            ggplotly(g)
        })
    })
    
    
    ## Rotate Plot:
    observeEvent(input$view_plot2, {
        output$plot1 <- renderPlotly({
            x1 <- data1()[, c(input$xcol1, input$ycol1)]
            M <- x1
            #calculate rotation angle
            alpha <- -atan(M[1,2]-tail(M,1)[,2])/(M[1,1]-tail(M,1)[,1])
            #rotation matrix
            rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
            #shift, rotate, shift back
            M2 <- t(rotm %*% (t(M)-c(M[1,1],M[1,2]))+c(M[1,1],M[1,2]))
            #plot
            g <- ggplot(data =  M) + 
                geom_point(aes_string(x= M[,1], y= M[,2]), colour= "red", size = 0.1)+
                geom_point(aes_string(x= M2[,1], y= M2[,2]),colour="green",size =0.1)
            ggplotly(g)
        })
    })
    
    ## Normalize Plot:
    observeEvent(input$view_plot3, {
        output$plot1 <- renderPlotly({
            x1 <- data1()[, c(input$xcol1, input$ycol1)]
            M <- x1
            #calculate rotation angle
            alpha <- -atan(M[1,2]-tail(M,1)[,2])/(M[1,1]-tail(M,1)[,1])
            #rotation matrix
            rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
            #shift, rotate, shift back
            M2 <- t(rotm %*% (t(M)-c(M[1,1],M[1,2]))+c(M[1,1],M[1,2]))
            M2[nrow(M2),2] <- M2[1,2]
            M2
            #plot
            g <- ggplot(data =  M) + 
                geom_point(aes_string(x= M[,1], y= M[,2]), colour= "red", size = 0.1)+
                geom_point(aes_string(x= M2[,1], y= M2[,2]),colour="green",size =0.1)+
                geom_point(aes_string(x= M2[,1], y= (M2[,2]-min(M2[1,2]))),colour = "blue", size =0.1)
            ggplotly(g)
        })
    })
    
    
    ## Show only Normalized Plot:  
    observeEvent(input$view_plot4, {
        output$plot1 <- renderPlotly({
            x1 <- data1()[, c(input$xcol1, input$ycol1)]
            M <- x1
            #calculate rotation angle
            alpha <- -atan(M[1,2]-tail(M,1)[,2])/(M[1,1]-tail(M,1)[,1])
            #rotation matrix
            rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
            #shift, rotate, shift back
            M2 <- t(rotm %*% (t(M)-c(M[1,1],M[1,2]))+c(M[1,1],M[1,2]))
            M2[nrow(M2),2] <- M2[1,2]
            M2
            #plot
            g <- ggplot(data =  M) + 
                geom_point(aes_string(x= M2[,1], y= (M2[,2]-min(M2[1,2]))),colour = "blue", size =0.1)
            ggplotly(g)
        })
    })
    
    ## Draw Reference Line:  
    observeEvent(input$Line, {
        output$plot1 <- renderPlotly({
            x1 <- data1()[, c(input$xcol1, input$ycol1)]
            M <- x1
            #calculate rotation angle
            alpha <- -atan(M[1,2]-tail(M,1)[,2])/(M[1,1]-tail(M,1)[,1])
            #rotation matrix
            rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
            #shift, rotate, shift back
            M2 <- t(rotm %*% (t(M)-c(M[1,1],M[1,2]))+c(M[1,1],M[1,2]))
            M2[nrow(M2),2] <- M2[1,2]
            M2
            #plot
            g <- ggplot(data =  M) + 
                geom_point(aes_string(x= M2[,1], y= (M2[,2]-min(M2[1,2]))),colour = "blue", size =0.1)+
                geom_segment(aes_string(x = M2[1,1], y = 0, xend = M2[nrow(M2),1] , yend = 0))
            ggplotly(g)
        })
    })
    
    ## Remove Outliers:
    
    
    
    ## Polynomial Adjustment:
    observeEvent(input$poly,{
        observeEvent(input$degree, {
            output$plot1 <- renderPlotly({
                x1 <- data1()[, c(input$xcol1, input$ycol1)]
                M <- x1
                #calculate rotation angle
                alpha <- -atan(M[1,2]-tail(M,1)[,2])/(M[1,1]-tail(M,1)[,1])
                #rotation matrix
                rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
                #shift, rotate, shift back
                M2 <- t(rotm %*% (t(M)-c(M[1,1],M[1,2]))+c(M[1,1],M[1,2]))
                #plot
                g <- ggplot(data =  M) + 
                    geom_point(aes_string(x= M2[,1], y= (M2[,2]-min(M2[1,2]))),colour = "blue", size =0.1)+
                    #geom_point(aes_string(x= M2[,1], y= ((M2[,2])-(M2[,1]))))+
                    #geom_point(aes_string(x = input$xcol1, y = input$ycol1, color = input$ycol1), size = 0.5) +
                    #scale_colour_gradientn(colours=rainbow(6))
                    stat_smooth(method = "lm", aes_string(x= M2[,1], y= (M2[,2]-min(M2[1,2]))), formula = y ~ poly(x,input$degree, raw = TRUE), size = 0.5)
                ggplotly(g)
            })
        })
    })
    
    
    
    nms <- row.names(data1)
    
    output$dataTable <- DT::renderDataTable({
        d <- event_data("plotly_click")
        d_save <<- c(d_save, d$pointNumber[1]+1)
        X <<- c(d$x[1], d$x[1]+1)
        Y <<- c(d$y[1], d$y[1]+1)
        dt2 <- data.frame(Point_Number = d_save,
                          X = X,
                          Y = Y)
    })
    
    
    observeEvent(input$view_plot5, {
        d <- event_data("plotly_click")
        output$plot1 <- renderPlotly({
            x1 <- data1()[, c(input$xcol1, input$ycol1)]
            M <- x1
            #calculate rotation angle
            alpha <- -atan(M[1,2]-tail(M,1)[,2])/(M[1,1]-tail(M,1)[,1])
            #rotation matrix
            rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
            #shift, rotate, shift back
            M2 <- t(rotm %*% (t(M)-c(M[1,1],M[1,2]))+c(M[1,1],M[1,2]))
            dt2 <- data.frame(Point_Number = d_save,
                              X = X,
                              Y = Y)
            #plot
            g <- ggplot(data =  M) + 
                geom_point(aes_string(x= M2[,1], y= (M2[,2]-min(M2[1,2]))),colour = "blue", size =0.1)+
                geom_line(aes(x= dt2[,2], y= dt2[,3]))
            ggplotly(g)
            
        })
    })
    
    
    observeEvent(input$separate, {
        output$brush <- DT::renderDataTable({
            d <- event_data("plotly_selected")
            if (!is.null(d)) d
        })
        
        output$plot1 <- renderPlotly({
            g <- ggplot(data = d)+
                geom_point(aes_string(x= d[,3], y= d[,4]), colour= "red", size = 0.1)
            ggplotly(g)
        })
    })
    
    ## Download the filtered data:
    output$Filtered_Data_Default_000 <- downloadHandler(
        filename = "Filtered_Data_Default_000.csv",
        content = function(file){
            write.csv(brush(), file)})
    
    ## MainPanel tabset renderUI code
    observeEvent(input$view_data, {
        output$tb1 <- renderUI({
            if(is.null(input$file1)) {return()}
            else
                tabsetPanel(
                    tabPanel("Dataset", DT::dataTableOutput("table1")),
                    tabPanel("Input File Object DF ", tableOutput("filedf1"), tableOutput("filedf2")),
                    tabPanel("Input File Object Structure", verbatimTextOutput("fileob1")),
                    tabPanel("Summary Stats", verbatimTextOutput("summ1")))
        })
    })
    
    observeEvent(input$hide_data, {
        output$tb1 <- renderUI({
            {return()}
        })
    })
    
    ### Tab 2:
    
    output$filedf3 <- renderTable({
        if(is.null(input$file2)){return ()}
        input$file2 
    })
    
    # Extract the file path for file
    output$filedf4 <- renderTable({
        if(is.null(input$file2)){return ()}
        input$file2$datapath
    })
    
    ## Below code to display the structure of the input file object
    output$fileob2<- renderPrint({
        if(is.null(input$file2)){return ()}
        str(input$file2)
    })
    
    # Following code displays the select input widget with the list of file loaded by the user
    output$selectfile2 <- renderUI({
        if(is.null(input$file2)) {return()}
        list(hr(), 
             helpText("Select the files for which you need to see data and summary stats"),
             selectInput("Select2", "Select", choices=input$file2$name))
    })
    
    ## Summary Stats code
    output$summ2 <- renderPrint({
        if(is.null(input$file2)){return()}
        summary(read.table(file=input$file2$datapath[input$file2$name==input$Select2], 
                           sep=input$sep2, 
                           header = input$header2, 
                           stringsAsFactors = input$stringAsFactors2))})
    
    ## Reading the uploaded .CSV file: 
    data2 <- reactive({
        req(input$file2)
        df2 <- read.table(file=input$file2$datapath[input$file2$name==input$Select2], sep=input$sep2, header = input$header2, stringsAsFactors = input$stringAsFactors2)
        updateSelectInput(session, inputId = 'xcol2', label = 'X Variable',
                          choices = names(df2), selected = names(df2)[1])
        updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable',
                          choices = names(df2), selected = names(df2))
        return(df2)
    })
    
    ## Data Table 
    output$table2 <- DT::renderDataTable({
        if(is.null(input$file2)){return()}
        else
            data2()
    })
    
    ## Rendering ggplotly scatter plot: 
    output$plot2 <- renderPlot({
        x2 <- data2()[, c(input$xcol2, input$ycol2)]
        ggplot(data =  x2) + 
            geom_line(aes_string(x = input$xcol2, y = input$ycol2, color = input$ycol2), size = 1) + 
            # geom_point(aes_string(x = input$xcol2, y = input$ycol2, color = input$ycol2), size = 1) +
            scale_colour_gradientn(colours=rainbow(6))
        #stat_smooth(aes_string(x = input$xcol2, y = input$ycol2, color = input$ycol2))
    })
    
    ## Using Brush on the Plotted graph:
    data2.new <- reactive({
        User_brush_2 <- input$User_brush_2
        mysel2 <- brushedPoints(data2(), User_brush_2)
        return(mysel2)
    })
    
    ## Rendering the filered data:
    output$table_2 <- DT::renderDataTable(DT::datatable(data2.new()))
    
    ## Download the filtered data:
    output$mydownload2 <- downloadHandler(
        filename = "Filtered_Data_Default_000.csv",
        content = function(file){
            write.csv(data2.new(), file)})
    
    ## MainPanel tabset renderUI code
    output$tb2 <- renderUI({
        if(is.null(input$file2)) {return()}
        else
            tabsetPanel(
                tabPanel("Dataset", DT::dataTableOutput("table2")),
                tabPanel("Input File Object DF ", tableOutput("filedf3"), tableOutput("filedf4")),
                tabPanel("Input File Object Structure", verbatimTextOutput("fileob2")),
                tabPanel("Summary Stats", verbatimTextOutput("summ2")))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)