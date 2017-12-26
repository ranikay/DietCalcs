library(shiny)
source('helpers.R')

# Define server logic for DietCalcs app
server <- function(input, output){
  
  # App header info
  output$header_summary <- renderText({
    paste0('Results for: ', input$name)
  })
  output$current_time <- renderText({
    paste0('Generated: ', 
      format(Sys.time(), format = '%D at %r', tz = 'MST'))
  })
  
  # --------------------------- ENERGY NEEDS TAB ----------------------------- #
  
  # Just calculate these here since they're used all over
  .height_in = reactive({
    round(ifelse(input$height_unit == 'in', input$height, 
      cm_to_in(input$height)), 1)
  })
  .height_cm = reactive({
    round(ifelse(input$height_unit == 'cm', input$height, 
                 in_to_cm(input$height)), 1)
  })
  .weight_lb = reactive({
    round(ifelse(input$weight_unit == 'lb', input$weight, 
                 kg_to_lb(input$weight)), 1)
  })
  .weight_kg = reactive({
    round(ifelse(input$weight_unit == 'kg', input$weight, 
                 lb_to_kg(input$weight)), 1)
  })
  .ibw_lb = reactive({
    ibw(input$sex, .height_in())
  })
  .abw_lb = reactive({
    abw(.weight_lb(), .ibw_lb())
  })
  
  # Current height and weight
  output$patient_table <- DT::renderDataTable({
    df = get_pt_ht_wt(.height_in(), .weight_lb())
    datatable(df, 
      colnames = c('Height (in)', 'Height (cm)', 'Weight (lb)', 'Weight (kg)'),
      rownames = F,
      options = list(dom = 't', ordering = F),
      style = 'bootstrap')
  })
  
  # Other weights
  output$wt_table <- DT::renderDataTable({
    df = data.frame(
      BMI = bmi(.height_in(), .weight_lb(), 'in', 'lb'),
      IBW = .ibw_lb(),
      IBW_p = round((.weight_lb() / .ibw_lb()) * 100, 1),
      ABW_lb = round(.abw_lb(), 1),
      ABW_kg = round(lb_to_kg(.abw_lb()), 1)
    )
    datatable(df, 
      colnames = c('BMI', 'IBW (lb)', 'IBW (%)', 'ABW (lb)', 'ABW (kg)'),
      rownames = F,
      options = list(dom = 't', ordering = F),
      style = 'bootstrap')
  })
  
  # MSJ table data
  output$msj_table <- DT::renderDataTable({
    df = get_msj(input$sex, .height_cm(), .weight_kg(), input$age)
    datatable(df, 
      colnames = c('REE', '1.1', '1.2', '1.3', '1.5', 'Sex'),
      rownames = F,
      options = list(dom = 't', ordering = F),
      style = 'bootstrap') %>%
    formatStyle(
      'Sex', fontWeight = 'bold',
      color = styleEqual(c('Male', 'Female'), c('#6CC3D5', '#e83e8c')))
  })
  
  output$qm_table <- DT::renderDataTable({
    df = get_qm(.weight_kg())
    datatable(df, 
              colnames = c('20 kcal/kg', '22', '25', '30', '32', '35'),
              rownames = F,
              options = list(dom = 't', ordering = F),
              style = 'bootstrap')
  })
  
  # ------------------------ PROTEIN / FLUID TAB ----------------------------- #
  
  output$protein_table <- DT::renderDataTable({
    df = by_cbw_abw(.weight_kg(), lb_to_kg(.abw_lb()), values = c(.8, 1, 1.2, 1.5, 2))
    datatable(df, 
              colnames = c('0.8', '1.0', '1.2', '1.5', '2'),
              rownames = c('Based on CBW', 'Based on ABW'),
              options = list(dom = 't', ordering = F),
              style = 'bootstrap')
  })
  
  output$fluid_table <- DT::renderDataTable({
    df = by_cbw_abw(.weight_kg(), lb_to_kg(.abw_lb()), values = c(35, 30, 25))
    datatable(df, 
              colnames = c('25-52 yrs', '53-90', '90+'),
              rownames = c('Based on CBW', 'Based on ABW'),
              options = list(dom = 't', ordering = F),
              style = 'bootstrap')
  })
  
  output$protein_fluid_text <- renderText(
    paste0('* Values calculated for a ', tolower(input$sex), ' patient (',
      .height_in(), ' in, ', .weight_lb(), ' lbs)')
  )
  
  # ------------------------- SUMMARY REPORT TAB ----------------------------- #
  
  output$report <- downloadHandler(
    filename = 'report.html',
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      temp_report <- file.path(tempdir(), 'report.Rmd')
      file.copy('report.Rmd', temp_report, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(sex = input$sex)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(temp_report, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}