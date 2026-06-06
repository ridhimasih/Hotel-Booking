library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# ── Sample Data ────────────────────────────────────────────────────────────────
set.seed(42)

# Reservations
guests <- data.frame(
  id       = 1:20,
  name     = c("Amit Kumar","Priya Sharma","Rohan Verma","Sneha Gupta","Vikram Singh",
                "Anjali Rao","Deepak Patel","Meena Joshi","Rahul Nair","Kavita Mehta",
                "Suresh Iyer","Pooja Reddy","Nikhil Bose","Sonal Tiwari","Arjun Das",
                "Rekha Chandra","Manish Sinha","Divya Saxena","Karan Malhotra","Lata Pillai"),
  room     = c(204,312,108,401,215,310,102,209,315,403,
                106,211,308,404,201,313,107,212,305,402),
  type     = c("Deluxe","Suite","Standard","Family","Standard","Deluxe","Standard","Deluxe",
                "Suite","Family","Standard","Standard","Deluxe","Family","Standard",
                "Suite","Standard","Deluxe","Standard","Family"),
  checkin  = as.Date("2026-06-01") + c(5,4,6,5,3,6,4,5,6,5,3,4,5,6,4,5,6,4,5,6),
  checkout = as.Date("2026-06-01") + c(9,7,10,8,6,9,7,8,9,8,6,7,9,10,7,8,10,7,9,9),
  amount   = c(14200,28000,7800,18500,8200,15600,7500,14800,27500,19200,
                7900,8100,15200,18800,8000,26500,7600,14900,8300,19000),
  status   = c("Check-in","Check-out","Reserved","Check-in","Check-out","Reserved",
                "Check-in","Check-out","Reserved","Check-in","Check-out","Reserved",
                "Check-in","Check-out","Reserved","Check-in","Check-out","Reserved",
                "Check-in","Check-out"),
  stringsAsFactors = FALSE
)

# Weekly data
weekly <- data.frame(
  day        = factor(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                      levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
  occupancy  = c(72, 65, 70, 80, 88, 95, 78),
  revenue    = c(110000, 90000, 100000, 130000, 160000, 185000, 140000)
)

# Room status
room_data <- data.frame(
  room_no = paste0(rep(1:5, each=5), "0", rep(1:5, 5)),
  floor   = rep(1:5, each=5),
  status  = sample(c("Occupied","Available","Reserved","Maintenance"),
                   25, replace=TRUE, prob=c(0.45,0.30,0.15,0.10))
)

status_colors <- c(
  "Occupied"    = "#378ADD",
  "Available"   = "#639922",
  "Reserved"    = "#7F77DD",
  "Maintenance" = "#BA7517"
)

booking_sources <- data.frame(
  source  = c("Direct","OTA","Booking.com","Walk-in","Corporate"),
  pct     = c(34, 27, 20, 11, 8)
)

room_type_rev <- data.frame(
  type    = c("Standard","Deluxe","Suite","Family"),
  revenue = c(245000, 312000, 480000, 198000)
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Hotel Booking Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",       tabName = "overview",      icon = icon("hotel")),
      menuItem("Reservations",   tabName = "reservations",  icon = icon("calendar-check")),
      menuItem("Room Status",    tabName = "rooms",         icon = icon("door-open")),
      menuItem("Revenue",        tabName = "revenue",       icon = icon("chart-line"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .small-box .icon { font-size: 60px; top: 10px; }
      .info-box { min-height: 80px; }
      .info-box-icon { height: 80px; line-height: 80px; }
      .info-box-content { padding-top: 12px; }
      .box-title { font-weight: 600; }
      .status-badge {
        display: inline-block;
        padding: 3px 10px;
        border-radius: 12px;
        font-size: 12px;
        font-weight: 600;
      }
      .badge-checkin    { background:#d4edda; color:#155724; }
      .badge-checkout   { background:#f8d7da; color:#721c24; }
      .badge-reserved   { background:#e2d9f3; color:#4a235a; }
    "))),

    tabItems(

      # ── Overview ─────────────────────────────────────────────────────────────
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("box_occupancy", width=3),
          valueBoxOutput("box_revenue",   width=3),
          valueBoxOutput("box_bookings",  width=3),
          valueBoxOutput("box_rating",    width=3)
        ),
        fluidRow(
          box(title="Weekly Occupancy (%)", width=7, status="primary", solidHeader=TRUE,
              plotOutput("plot_weekly_occ", height="260px")),
          box(title="Booking Sources", width=5, status="info", solidHeader=TRUE,
              plotOutput("plot_sources", height="260px"))
        ),
        fluidRow(
          box(title="Revenue by Room Type (₹)", width=6, status="success", solidHeader=TRUE,
              plotOutput("plot_rev_type", height="240px")),
          box(title="Room Status Summary", width=6, status="warning", solidHeader=TRUE,
              plotOutput("plot_status_pie", height="240px"))
        )
      ),

      # ── Reservations ─────────────────────────────────────────────────────────
      tabItem(tabName = "reservations",
        fluidRow(
          box(title="Filter Reservations", width=12, status="primary", solidHeader=TRUE,
              collapsible=TRUE,
              fluidRow(
                column(4, selectInput("filter_status", "Status",
                                      choices=c("All","Check-in","Check-out","Reserved"),
                                      selected="All")),
                column(4, selectInput("filter_type", "Room Type",
                                      choices=c("All","Standard","Deluxe","Suite","Family"),
                                      selected="All")),
                column(4, br(), actionButton("btn_reset", "Reset Filters",
                                             class="btn btn-default"))
              )
          )
        ),
        fluidRow(
          box(title="Reservation List", width=12, status="success", solidHeader=TRUE,
              DT::dataTableOutput("table_reservations"))
        )
      ),

      # ── Room Status ───────────────────────────────────────────────────────────
      tabItem(tabName = "rooms",
        fluidRow(
          infoBoxOutput("ibox_occupied",    width=3),
          infoBoxOutput("ibox_available",   width=3),
          infoBoxOutput("ibox_reserved",    width=3),
          infoBoxOutput("ibox_maintenance", width=3)
        ),
        fluidRow(
          box(title="Floor Plan — Room Status", width=12, status="primary", solidHeader=TRUE,
              plotOutput("plot_floorplan", height="380px"))
        )
      ),

      # ── Revenue ───────────────────────────────────────────────────────────────
      tabItem(tabName = "revenue",
        fluidRow(
          box(title="Weekly Revenue Trend (₹)", width=8, status="primary", solidHeader=TRUE,
              plotOutput("plot_revenue_bar", height="280px")),
          box(title="Top Guests by Spend", width=4, status="info", solidHeader=TRUE,
              tableOutput("table_top_guests"))
        ),
        fluidRow(
          box(title="Revenue by Room Type", width=6, status="success", solidHeader=TRUE,
              plotOutput("plot_rev_type2", height="260px")),
          box(title="Daily Revenue Summary", width=6, status="warning", solidHeader=TRUE,
              tableOutput("table_rev_summary"))
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # KPI Boxes
  output$box_occupancy <- renderValueBox({
    valueBox("78%", "Occupancy Rate", icon=icon("hotel"), color="blue")
  })
  output$box_revenue <- renderValueBox({
    valueBox("₹14.8L", "Today's Revenue", icon=icon("indian-rupee-sign"), color="green")
  })
  output$box_bookings <- renderValueBox({
    valueBox("23", "New Bookings", icon=icon("calendar-plus"), color="yellow")
  })
  output$box_rating <- renderValueBox({
    valueBox("4.7 ★", "Avg Guest Rating", icon=icon("star"), color="purple")
  })

  # Info Boxes (room status)
  output$ibox_occupied <- renderInfoBox({
    n <- sum(room_data$status=="Occupied")
    infoBox("Occupied", n, icon=icon("bed"), color="blue", fill=TRUE)
  })
  output$ibox_available <- renderInfoBox({
    n <- sum(room_data$status=="Available")
    infoBox("Available", n, icon=icon("door-open"), color="green", fill=TRUE)
  })
  output$ibox_reserved <- renderInfoBox({
    n <- sum(room_data$status=="Reserved")
    infoBox("Reserved", n, icon=icon("bookmark"), color="purple", fill=TRUE)
  })
  output$ibox_maintenance <- renderInfoBox({
    n <- sum(room_data$status=="Maintenance")
    infoBox("Maintenance", n, icon=icon("wrench"), color="yellow", fill=TRUE)
  })

  # Weekly occupancy bar
  output$plot_weekly_occ <- renderPlot({
    ggplot(weekly, aes(x=day, y=occupancy, fill=occupancy)) +
      geom_col(width=0.6, show.legend=FALSE) +
      geom_text(aes(label=paste0(occupancy, "%")), vjust=-0.4, size=3.5, fontface="bold") +
      scale_fill_gradient(low="#90caf9", high="#1565c0") +
      scale_y_continuous(limits=c(0,110), labels=function(x) paste0(x,"%")) +
      labs(x=NULL, y="Occupancy") +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.x=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_rect(fill="white", color=NA))
  })

  # Booking sources horizontal bar
  output$plot_sources <- renderPlot({
    df <- booking_sources %>% arrange(pct)
    df$source <- factor(df$source, levels=df$source)
    ggplot(df, aes(x=source, y=pct, fill=source)) +
      geom_col(width=0.6, show.legend=FALSE) +
      geom_text(aes(label=paste0(pct,"%")), hjust=-0.2, size=3.5, fontface="bold") +
      coord_flip() +
      scale_fill_brewer(palette="Blues", direction=1) +
      scale_y_continuous(limits=c(0,45)) +
      labs(x=NULL, y="Share (%)") +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_rect(fill="white", color=NA))
  })

  # Revenue by room type
  rev_plot <- function() {
    ggplot(room_type_rev, aes(x=reorder(type, revenue), y=revenue/1000, fill=type)) +
      geom_col(width=0.6, show.legend=FALSE) +
      geom_text(aes(label=paste0("₹", round(revenue/1000),"K")),
                hjust=-0.15, size=3.5, fontface="bold") +
      coord_flip() +
      scale_fill_manual(values=c("Standard"="#42a5f5","Deluxe"="#26a69a",
                                  "Suite"="#7e57c2","Family"="#ef7c00")) +
      scale_y_continuous(limits=c(0,600), labels=function(x) paste0("₹",x,"K")) +
      labs(x=NULL, y="Revenue (₹ thousands)") +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_rect(fill="white", color=NA))
  }
  output$plot_rev_type  <- renderPlot(rev_plot())
  output$plot_rev_type2 <- renderPlot(rev_plot())

  # Room status pie
  output$plot_status_pie <- renderPlot({
    counts <- room_data %>% count(status)
    ggplot(counts, aes(x="", y=n, fill=status)) +
      geom_col(width=1, color="white", linewidth=0.8) +
      coord_polar("y") +
      scale_fill_manual(values=status_colors) +
      geom_text(aes(label=paste0(status,"\n",n," rooms")),
                position=position_stack(vjust=0.5), size=3.5, fontface="bold", color="white") +
      labs(fill=NULL) +
      theme_void(base_size=13) +
      theme(legend.position="bottom",
            plot.background=element_rect(fill="white", color=NA))
  })

  # Floor plan heatmap
  output$plot_floorplan <- renderPlot({
    ggplot(room_data, aes(x=factor(as.integer(substr(room_no,3,3))),
                          y=factor(floor), fill=status)) +
      geom_tile(color="white", linewidth=1.5, width=0.85, height=0.85) +
      geom_text(aes(label=room_no), size=3.5, fontface="bold", color="white") +
      scale_fill_manual(values=status_colors) +
      scale_y_discrete(labels=function(x) paste("Floor", x)) +
      labs(x="Room Number", y=NULL, fill="Status") +
      theme_minimal(base_size=13) +
      theme(panel.grid=element_blank(),
            axis.ticks=element_blank(),
            legend.position="bottom",
            legend.key.size=unit(0.6,"cm"),
            plot.background=element_rect(fill="white", color=NA))
  })

  # Filtered reservations table
  filtered_guests <- reactive({
    df <- guests
    if (input$filter_status != "All") df <- df[df$status==input$filter_status, ]
    if (input$filter_type   != "All") df <- df[df$type==input$filter_type, ]
    df
  })

  observeEvent(input$btn_reset, {
    updateSelectInput(session, "filter_status", selected="All")
    updateSelectInput(session, "filter_type",   selected="All")
  })

  output$table_reservations <- DT::renderDataTable({
    df <- filtered_guests() %>%
      mutate(Amount = paste0("₹", formatC(amount, format="d", big.mark=","))) %>%
      select(Name=name, Room=room, Type=type,
             `Check-in`=checkin, `Check-out`=checkout,
             Amount, Status=status)
    DT::datatable(df,
      options=list(pageLength=10, dom='frtip', scrollX=TRUE),
      rownames=FALSE,
      class='stripe hover compact'
    ) %>%
      DT::formatStyle("Status",
        backgroundColor = DT::styleEqual(
          c("Check-in","Check-out","Reserved"),
          c("#d4edda","#f8d7da","#e2d9f3")
        )
      )
  })

  # Weekly revenue bar
  output$plot_revenue_bar <- renderPlot({
    ggplot(weekly, aes(x=day, y=revenue/1000)) +
      geom_col(fill="#26a69a", width=0.6) +
      geom_text(aes(label=paste0("₹", revenue/1000,"K")),
                vjust=-0.4, size=3.5, fontface="bold") +
      scale_y_continuous(limits=c(0,220), labels=function(x) paste0("₹",x,"K")) +
      labs(x=NULL, y="Revenue") +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.x=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_rect(fill="white", color=NA))
  })

  # Top guests by spend
  output$table_top_guests <- renderTable({
    guests %>%
      arrange(desc(amount)) %>%
      head(6) %>%
      mutate(Spend = paste0("₹", formatC(amount, format="d", big.mark=","))) %>%
      select(Guest=name, Type=type, Spend)
  }, striped=TRUE, hover=TRUE, bordered=FALSE)

  # Revenue summary
  output$table_rev_summary <- renderTable({
    data.frame(
      Metric      = c("Total this week","Best day","Avg per booking","Projected monthly"),
      Value       = c("₹9,15,000","₹1,85,000 (Sat)","₹12,400","₹39,20,000")
    )
  }, striped=TRUE, hover=TRUE, bordered=FALSE)
}

shinyApp(ui, server)
