library(shiny)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(readr)
library(plyr)

data <- read.csv(sprintf("Trade_Results_2023-csv.csv"))
note <- tempfile()
write_rds(data, "note.rds")

#rm(list = ls())
shinyServer(function(input, output, session){

  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-readRDS("note.rds")
  
  
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
      hr(),
      column(6,offset = 5,
             HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
             ### tags$head() This is to change the color of "Add a new row" button
             tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
             tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
             tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
             ### Optional: a html button 
             #HTML('<input type="submit" name="Add_row_head" value="Add">'), #general style button
             HTML('</div>') ),
      
      column(11,dataTableOutput("Main_table_trich")),
      tags$script("$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
      
    ) 
  })
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,selection = 'single',
              escape=F)
  })
  
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          textInput(paste0("month_add", input$Add_row_head), "Month"),
                          textInput(paste0("trade_add", input$Add_row_head), "Trade"),
                          textInput(paste0("stock_add", input$Add_row_head), "Stock Name"),
                          textInput(paste0("entry_add", input$Add_row_head), "Entries"),
                          textInput(paste0("entry_price_add", input$Add_row_head), "Entry Price"),
                          dateInput(paste0("entry_date_add", input$Add_row_head), "Entry Date"),
                          textInput(paste0("stoploss_add", input$Add_row_head), "Stop Loss $"),
                          textInput(paste0("exit_add", input$Add_row_head), "Exits"),  
                          textInput(paste0("exit_price_add", input$Add_row_head), "Exit Price"), 
                          dateInput(paste0("exit_date_add", input$Add_row_head), "Exit Date"),  
                          dateInput(paste0("purchase_date_add", input$Add_row_head), "Purchase Date"),  
                          dateInput(paste0("sell_date_add", input$Add_row_head), "Sell Date"),
                          textInput(paste0("hold_time_add", input$Add_row_head), "Hold Time"),
                          actionButton("go", "Add Trade"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  
  observeEvent(input$go, {
    new_row=data.frame(
      month = input[[paste0("month_add", input$Add_row_head)]],
      trade = input[[paste0("trade_add", input$Add_row_head)]],
      stock_name = input[[paste0("stock_add", input$Add_row_head)]],
      entries = input[[paste0("entry_add", input$Add_row_head)]],
      entry_price = input[[paste0("entry_price_add", input$Add_row_head)]],
      entry_date = input[[paste0("entry_date_add", input$Add_row_head)]],
      stop_loss_dollar = input[[paste0("stoploss_add", input$Add_row_head)]],
      exits = input[[paste0("exit_add", input$Add_row_head)]],
      exit_price = input[[paste0("exit_price_add", input$Add_row_head)]],
      exit_date = input[[paste0("exit_date_add", input$Add_row_head)]],
      purchase_date = input[[paste0("purchase_date_add", input$Add_row_head)]],
      sell_date = input[[paste0("sell_date_add", input$Add_row_head)]],
      hold_time =input[[paste0("hold_time_add", input$Add_row_head)]]
    )
    vals_trich$Data<-rbind.fill(vals_trich$Data,new_row) #changed from rbind to rbind.fill
    removeModal()
  })
  
  ### save to RDS 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "note.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete",length(input$Main_table_trich_rows_selected),"rows?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    vals_trich$Data=vals_trich$Data[-input$Main_table_trich_rows_selected]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            dataTableOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  
  
  #### modify part
  output$row_modif<-renderDataTable({
    selected_row=input$Main_table_trich_rows_selected
    old_row=vals_trich$Data[selected_row]
    row_change=list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals_trich$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      else if( is.Date(vals_trich$Data[[i]])){
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      }
      else 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=row_change
    DT 
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF)=colnames(vals_trich$Data)
                 vals_trich$Data[input$Main_table_trich_rows_selected]<-DF
                 
               }
  )
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trade_Results_2023-csv.csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
  
})