# Install library sesuai permintaan & kebutuhan soal tugas 3
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

# Panggil library
library(shiny) # Memberi kebebasan pengguna memilih plot
library(readxl) # Untuk membaca file excel
library(ggplot2) # Untuk plot
library(dplyr) # Untuk manipulasi data (tidak jadi dipakai)

# Ambil data dri excel dan set ke lembar 1
file_path <- "DataSet Tugas 3- MSIM4310.xls"
data <- read_excel(file_path, sheet = 1, skip = 2)

# Buat variabel kolom untuk menampung data
colnames(data) <- c("Bulan", "Suhu_Udara_Min", "Suhu_Udara_Max", "Suhu_Udara_Rata2", # nolint
                    "Kelembaban_Relatif", "Tekanan_Udara", "Kecepatan_Angin",
                    "Curah_Hujan", "Penyinaran_Matahari")

# convert field bulan dengan faktor
data$Bulan <- factor(data$Bulan, levels = unique(data$Bulan), ordered = TRUE)

# Atur UI dengan function bawaan shiny
ui <- fluidPage(
    titlePanel("Visualisasi Data Stasiun Meteorologi Samarinda 2015"),
    sidebarLayout(
        sidebarPanel(
            selectInput("variable", "Pilih Variabel",
                        choices = colnames(data)[2:9]),
            selectInput("plotType", "Pilih Jenis Plot",
                        choices = c("Scatter Plot", "Line Plot", "Bar Plot")),
            actionButton("update", "Update")
        ),
        mainPanel(
            fluidRow(
                column(12, plotOutput("plot"))
            ),
            fluidRow(
                column(12, tableOutput("dataTable"))
            )
        )
    )
)

# Atur logic untuk render data dari sisi server
server <- function(input, output) {
    selectedData <- reactive({
        data
    })
    
    output$plot <- renderPlot({
        req(input$variable, input$plotType)
        
        plotData <- selectedData()
        variable <- sym(input$variable)
        
        gg <- ggplot(plotData, aes(x = Bulan, y = !!variable, group = 1)) +
            labs(x = "Bulan", y = input$variable) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.margin = margin(20, 20, 20, 20))
        
        if (input$plotType == "Scatter Plot") {
            gg <- gg + geom_point()
        } else if (input$plotType == "Line Plot") {
            gg <- gg + geom_line()
        } else if (input$plotType == "Bar Plot") {
            gg <- gg + geom_bar(stat = "identity", width = 0.7)
        }
        
        gg
    })
    
    output$dataTable <- renderTable({
        selectedData()
    })
}

# Jalankan shiny gabungan antara UI & Server logic
shinyApp(ui = ui, server = server)