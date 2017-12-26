library(shiny)
source('helpers.R')

# Define UI for DietCalcs app
ui <- fluidPage(
  
  # App header info
  #theme = 'bootstrap.flatly.css',
  titlePanel('DietCalcs'),
  
  fluidRow(
    column(6,
      h3(textOutput('header_summary')),
      h5(textOutput('current_time'))
    )
  ),
  
  # Sidebar panel on the left for inputs, tabs on the right for outputs
  sidebarLayout(

    sidebarPanel(
      
      # Input: Patient info
      textInput('name', 'Name:', value = 'Patient'),
      
      # Input: Select sex
      radioButtons('sex', 'Sex:', c('Male', 'Female')),
      
      # Input: Age
      numericInput('age', 'Age (yrs): ', value = 50),
      
      # Input: Height
      numericInput('height', 'Height: ', value = 67),
      radioButtons('height_unit', 'Height units:', c('in', 'cm')),
      
      # Input: Weight
      numericInput('weight', 'Weight: ', value = 160),
      radioButtons('weight_unit', 'Weight units:', c('lb', 'kg')),
      
      # Input: Optional Info
      textInput('room', 'Room:', placeholder = 'Room (optional)'),
      textInput('level', 'Level:', placeholder = 'Level (optional)'),
      textInput('diet', 'Diet:', placeholder = 'Diet (optional)')
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Tabs for the different calculators and reports
      tabsetPanel(type = 'tabs',
        tabPanel('Energy Needs', 
          br(),
          h4('Patient Data'),
          dataTableOutput('patient_table'),
          br(),
          h4('BMI & Weights'),
          dataTableOutput('wt_table'),
          br(),
          h4('Mifflin Activity Factors'),
          dataTableOutput('msj_table'),
          br(),
          h4('Quick Method'),
          dataTableOutput('qm_table'),
          br(),
          h4('Protein Range'),
          dataTableOutput('protein_table')
        ),
        tabPanel('TPN', 
          br(),
          h4('Coming soon!')
        ),
        tabPanel('Summary Report', 
          br(),
          h4('Coming soon!')
        )
      )
    )
  )
)