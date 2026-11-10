# ============================================================
# FOODSCOPE | FOOD DELIVERY INTELLIGENCE
# Premium Shiny dashboard - single-file App.R
# Expected files: data/orders.csv, users.csv, restaurants.csv,
#                 menu.csv, order_items.csv
# ============================================================

packages <- c("shiny", "bslib", "dplyr", "tidyr", "plotly",
              "DT", "scales", "lubridate", "fontawesome",
              "shinyWidgets", "stringr")

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

invisible(lapply(packages, library, character.only = TRUE))

# ---------- Data loading ----------
data_dir <- "data"
files <- c("orders.csv", "users.csv", "restaurants.csv",
           "menu.csv", "order_items.csv")
missing_files <- files[!file.exists(file.path(data_dir, files))]
if (length(missing_files)) {
  stop(paste0("Missing CSV file(s) in ", normalizePath(data_dir, mustWork = FALSE),
              ": ", paste(missing_files, collapse = ", "),
              ". Create a folder named 'data' beside App.R and place the CSV files inside it."))
}

read_csv_safe <- function(file) {
  read.csv(file.path(data_dir, file), stringsAsFactors = FALSE,
           check.names = FALSE, na.strings = c("", "NA", "NULL"))
}
orders <- read_csv_safe("orders.csv")
users <- read_csv_safe("users.csv")
restaurants <- read_csv_safe("restaurants.csv")
menu <- read_csv_safe("menu.csv")
order_items <- read_csv_safe("order_items.csv")

# Find columns by common alternatives, case-insensitively.
find_col <- function(df, candidates, required = TRUE) {
  nm <- names(df)
  hit <- match(tolower(candidates), tolower(nm), nomatch = 0)
  hit <- hit[hit > 0]
  if (length(hit)) return(nm[hit[1]])
  if (required) stop(paste0("Could not find column: ", paste(candidates, collapse = " / "),
                            ". Available columns: ", paste(nm, collapse = ", ")))
  NULL
}
rename_if <- function(df, new, candidates, required = TRUE) {
  old <- find_col(df, candidates, required)
  if (!is.null(old)) names(df)[match(old, names(df))] <- new
  df
}

orders <- rename_if(orders, "order_id", c("order_id", "id"))
orders <- rename_if(orders, "user_id", c("user_id", "customer_id"))
orders <- rename_if(orders, "restaurant_id", c("restaurant_id", "rest_id"))
orders <- rename_if(orders, "order_date", c("order_date", "date", "created_at"))
orders <- rename_if(orders, "total_amount", c("total_amount", "order_amount", "amount", "total", "bill_amount"))
orders <- rename_if(orders, "order_status", c("order_status", "status"))
orders <- rename_if(orders, "payment_method", c("payment_method", "payment_type"), FALSE)
orders <- rename_if(orders, "delivery_time", c("delivery_time", "delivery_time_minutes", "delivery_minutes"), FALSE)

restaurants <- rename_if(restaurants, "restaurant_id", c("restaurant_id", "rest_id", "id"))
restaurants <- rename_if(restaurants, "restaurant_name", c("restaurant_name", "name"))
restaurants <- rename_if(restaurants, "city", c("city", "location", "area"), FALSE)
restaurants <- rename_if(restaurants, "cuisine", c("cuisine", "cuisine_type", "food_type"), FALSE)
restaurants <- rename_if(restaurants, "rating", c("rating", "restaurant_rating"), FALSE)

users <- rename_if(users, "user_id", c("user_id", "customer_id", "id"))
users <- rename_if(users, "gender", c("gender", "sex"), FALSE)
users <- rename_if(users, "age", c("age"), FALSE)
users <- rename_if(users, "occupation", c("occupation", "profession"), FALSE)

menu <- rename_if(menu, "menu_id", c("menu_id", "item_id", "id"))
menu <- rename_if(menu, "item_name", c("item_name", "name", "dish_name"), FALSE)
menu <- rename_if(menu, "category", c("category", "item_category", "food_category"), FALSE)
menu <- rename_if(menu, "is_veg", c("is_veg", "veg", "vegetarian"), FALSE)
menu <- rename_if(menu, "restaurant_id", c("restaurant_id", "rest_id"), FALSE)

order_items <- rename_if(order_items, "order_id", c("order_id"))
order_items <- rename_if(order_items, "menu_id", c("menu_id", "item_id"))
order_items <- rename_if(order_items, "quantity", c("quantity", "qty"), FALSE)
order_items <- rename_if(order_items, "price", c("price", "item_price", "unit_price"), FALSE)

# Parse / normalize core fields
orders$order_date <- as.Date(orders$order_date)
if (all(is.na(orders$order_date))) {
  parsed <- suppressWarnings(lubridate::ymd_hms(as.character(orders$order_date), quiet = TRUE))
  if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(as.character(orders$order_date), quiet = TRUE))
  orders$order_date <- as.Date(parsed)
}
orders$total_amount <- suppressWarnings(as.numeric(orders$total_amount))
if (!"order_status" %in% names(orders)) orders$order_status <- "Unknown"
if (!"payment_method" %in% names(orders)) orders$payment_method <- "Not recorded"
if (!"delivery_time" %in% names(orders)) orders$delivery_time <- NA_real_
orders$delivery_time <- suppressWarnings(as.numeric(orders$delivery_time))

if (!"city" %in% names(restaurants)) restaurants$city <- "Not recorded"
if (!"cuisine" %in% names(restaurants)) restaurants$cuisine <- "Not recorded"
if (!"rating" %in% names(restaurants)) restaurants$rating <- NA_real_
restaurants$rating <- suppressWarnings(as.numeric(restaurants$rating))
if (!"restaurant_name" %in% names(restaurants)) restaurants$restaurant_name <- as.character(restaurants$restaurant_id)

if (!"item_name" %in% names(menu)) menu$item_name <- as.character(menu$menu_id)
if (!"category" %in% names(menu)) menu$category <- "Unclassified"
if (!"is_veg" %in% names(menu)) menu$is_veg <- "Unknown"
if (!"quantity" %in% names(order_items)) order_items$quantity <- 1
if (!"price" %in% names(order_items)) order_items$price <- NA_real_
order_items$quantity <- suppressWarnings(as.numeric(order_items$quantity))
order_items$price <- suppressWarnings(as.numeric(order_items$price))

order_data <- orders %>%
  left_join(restaurants %>% select(restaurant_id, restaurant_name, city, cuisine, rating),
            by = "restaurant_id")
order_data$city[is.na(order_data$city)] <- "Not recorded"
order_data$cuisine[is.na(order_data$cuisine)] <- "Not recorded"

# Attach customer data, without assuming optional demographics exist.
if (!"gender" %in% names(users)) users$gender <- "Not recorded"
if (!"age" %in% names(users)) users$age <- NA_real_
if (!"occupation" %in% names(users)) users$occupation <- "Not recorded"

# Item table. Prefer item-level price; fall back to menu price if present.
item_join <- order_items %>%
  left_join(menu %>% select(any_of(c("menu_id", "item_name", "category", "is_veg", "restaurant_id"))),
            by = "menu_id")
if (!"restaurant_id" %in% names(item_join)) item_join$restaurant_id <- NA
if (all(is.na(item_join$price)) && "price" %in% names(menu)) {
  item_join <- item_join %>% left_join(menu %>% select(menu_id, menu_price = price), by = "menu_id")
  item_join$price <- item_join$menu_price
}
item_join$item_revenue <- item_join$quantity * item_join$price
item_join <- item_join %>%
  left_join(restaurants %>% select(restaurant_id, restaurant_name, city), by = "restaurant_id")
if (!"item_name" %in% names(item_join)) item_join$item_name <- "Unspecified item"
if (!"category" %in% names(item_join)) item_join$category <- "Unclassified"
if (!"is_veg" %in% names(item_join)) item_join$is_veg <- "Unknown"
item_join$city[is.na(item_join$city)] <- "Not recorded"
item_join$restaurant_name[is.na(item_join$restaurant_name)] <- "Not recorded"

min_date <- suppressWarnings(min(order_data$order_date, na.rm = TRUE))
max_date <- suppressWarnings(max(order_data$order_date, na.rm = TRUE))
if (!is.finite(as.numeric(min_date)) || !is.finite(as.numeric(max_date))) {
  stop("The order date column could not be parsed. Check the date format in orders.csv.")
}
cities <- sort(unique(na.omit(order_data$city)))
statuses <- sort(unique(na.omit(as.character(order_data$order_status))))
payments <- sort(unique(na.omit(as.character(order_data$payment_method))))

# ---------- Theme / UI ----------
dark_theme <- FALSE
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty", primary = "#0F9D8A",
                   base_font = font_google("Inter")),
  tags$head(
    tags$title("FoodScope | Food Delivery Intelligence"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700;800&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      :root{--ink:#182230;--muted:#788493;--line:#e9edf1;--paper:#ffffff;--canvas:#f5f7f8;--teal:#0f9d8a;--orange:#f29a4a}
      body{font-family:'DM Sans',sans-serif;background:var(--canvas);color:var(--ink);font-size:13px}
      .container-fluid{padding:0;max-width:100%}
      .app-shell{display:flex;min-height:100vh}
      .sidebar{width:238px;background:#142c32;color:#dbe8e8;padding:26px 17px;flex-shrink:0;min-height:100vh;position:sticky;top:0;height:100vh}
      .brand{font-size:23px;font-weight:800;letter-spacing:-.7px;padding:0 12px 28px;color:white}
      .brand span{color:#53c7b5}
      .brand small{display:block;font-size:9px;letter-spacing:1.6px;color:#9bb4b6;font-weight:600;margin-top:4px}
      .side-label{font-size:9px;text-transform:uppercase;letter-spacing:1.5px;color:#779397;padding:15px 12px 8px;font-weight:700}
      .sidebar .nav-pills{display:block}
      .sidebar .nav-link{color:#b8cbcc;border-radius:9px;padding:11px 12px;margin:3px 0;font-size:12px;font-weight:600;display:flex;gap:12px;align-items:center}
      .sidebar .nav-link:hover{background:#20434a;color:#fff}
      .sidebar .nav-link.active{background:#0f9d8a;color:#fff;box-shadow:0 5px 15px #071c2040}
      .side-foot{position:absolute;bottom:22px;left:26px;color:#779397;font-size:10px}
      .main-area{flex:1;min-width:0;padding:25px 30px 18px}
      .topbar{display:flex;align-items:center;justify-content:space-between;margin-bottom:23px;gap:15px}
      .page-title{font-size:23px;font-weight:800;letter-spacing:-.6px;margin:0}
      .page-subtitle{font-size:11px;color:var(--muted);margin-top:5px}
      .top-right{display:flex;align-items:center;gap:12px}
      .status-pill{background:#e5f6f1;color:#168674;border-radius:30px;padding:8px 13px;font-size:10px;font-weight:700}
      .filterbar{background:#fff;border:1px solid var(--line);border-radius:13px;padding:13px 16px;margin-bottom:20px;box-shadow:0 3px 12px #263b4a08}
      .filterbar .form-group{margin-bottom:0}
      .form-label{font-size:10px;color:#697786;font-weight:700;margin-bottom:5px}
      .form-control,.selectize-input{border:1px solid #e3e9ed!important;border-radius:8px!important;background:#fff!important;color:#24323c!important;font-size:11px!important;min-height:35px}
      .nav-tabs{display:none}
      .tab-content{border:0}
      .section-title{font-size:17px;font-weight:800;margin:4px 0 3px}
      .section-desc{font-size:11px;color:var(--muted);margin-bottom:17px}
      .kpi-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;margin-bottom:18px}
      .kpi{background:#fff;border:1px solid var(--line);border-radius:13px;padding:17px 18px;box-shadow:0 4px 15px #263b4a08;min-height:112px}
      .kpi-head{display:flex;justify-content:space-between;align-items:center;color:#75818d;font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.45px}
      .kpi-icon{width:33px;height:33px;border-radius:10px;display:grid;place-items:center;font-size:14px;background:#e8f6f3;color:#0f9d8a}
      .kpi-value{font-size:25px;font-weight:800;letter-spacing:-.7px;margin-top:9px;color:#1c2935}
      .kpi-note{font-size:10px;color:#96a1ab;margin-top:2px}
      .grid-2{display:grid;grid-template-columns:1.55fr 1fr;gap:15px}
      .grid-equal{display:grid;grid-template-columns:1fr 1fr;gap:15px}
      .panel{background:#fff;border:1px solid var(--line);border-radius:13px;padding:17px 18px;margin-bottom:15px;box-shadow:0 4px 15px #263b4a08;min-width:0}
      .panel-title{font-size:12px;font-weight:800;color:#293744;margin-bottom:3px}
      .panel-sub{font-size:10px;color:#9aa4ad;margin-bottom:10px}
      .table-panel{background:#fff;border:1px solid var(--line);border-radius:13px;padding:17px;margin-bottom:15px;overflow:hidden}
      .dataTables_wrapper{font-size:11px;color:#5d6975}
      table.dataTable thead th{background:#f5f8f8!important;color:#53616c!important;border-bottom:1px solid #e7edef!important;font-size:10px}
      table.dataTable tbody td{border-top:1px solid #f0f2f4!important}
      .dataTables_filter input,.dataTables_length select{border:1px solid #e2e8eb;border-radius:6px;padding:4px}
      .footer{color:#9aa4ad;text-align:center;font-size:9px;padding:15px}
      .sidebar .nav-link .fa{width:15px}
      @media(max-width:900px){.sidebar{width:190px}.main-area{padding:20px}.kpi-grid{grid-template-columns:repeat(2,1fr)}}
      @media(max-width:650px){.app-shell{display:block}.sidebar{width:100%;height:auto;min-height:0;position:relative}.side-foot{display:none}.sidebar .nav-pills{display:flex;flex-wrap:wrap}.sidebar .nav-link{padding:8px}.main-area{padding:15px}.grid-2,.grid-equal{grid-template-columns:1fr}.kpi-grid{grid-template-columns:1fr 1fr}.topbar{align-items:flex-start;flex-direction:column}}
    "))
  ),
  div(class = "app-shell",
      div(class = "sidebar",
          div(class = "brand", HTML("food<span>scope</span><small>DELIVERY INTELLIGENCE</small>")),
          div(class = "side-label", "Workspace"),
          tabsetPanel(id = "page", type = "pills",
                      tabPanel(title = tagList(icon("gauge-high"), " Overview"), value = "overview"),
                      tabPanel(title = tagList(icon("chart-column"), " Revenue Analytics"), value = "revenue"),
                      tabPanel(title = tagList(icon("users"), " Customer Intelligence"), value = "customers"),
                      tabPanel(title = tagList(icon("store"), " Restaurant Performance"), value = "restaurants"),
                      tabPanel(title = tagList(icon("utensils"), " Menu Intelligence"), value = "menu"),
                      tabPanel(title = tagList(icon("truck-fast"), " Delivery & Operations"), value = "operations"),
                      tabPanel(title = tagList(icon("table"), " Data Explorer"), value = "explorer")
          ),
          div(class = "side-foot", "FOODSCOPE  •  R SHINY")
      ),
      div(class = "main-area",
          div(class = "topbar",
              div(div(class = "page-title", textOutput("page_heading", inline = TRUE)),
                  div(class = "page-subtitle", "Understand performance. Discover patterns. Make better decisions.")),
              div(class = "top-right", div(class = "status-pill", icon("circle"), " DATA CONNECTED"))
          ),
          div(class = "filterbar",
              fluidRow(
                column(3, dateRangeInput("date_filter", "Date range", start = min_date, end = max_date,
                                         min = min_date, max = max_date, format = "dd M yyyy", width = "100%")),
                column(3, selectInput("city_filter", "City", choices = c("All cities", cities), selected = "All cities")),
                column(3, selectInput("status_filter", "Order status", choices = c("All statuses", statuses), selected = "All statuses")),
                column(3, selectInput("payment_filter", "Payment method", choices = c("All methods", payments), selected = "All methods"))
              )
          ),
          uiOutput("page_content"),
          div(class = "footer", "FOODSCOPE  |  FOOD DELIVERY INTELLIGENCE  |  POWERED BY R SHINY & PLOTLY")
      )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  headings <- c(overview = "Executive Overview", revenue = "Revenue Analytics",
                customers = "Customer Intelligence", restaurants = "Restaurant Performance",
                menu = "Menu Intelligence", operations = "Delivery & Operations",
                explorer = "Data Explorer")
  output$page_heading <- renderText(headings[[input$page %||% "overview"]])
  
  filtered_orders <- reactive({
    req(input$date_filter)
    x <- order_data %>% filter(!is.na(order_date),
                               order_date >= input$date_filter[1], order_date <= input$date_filter[2])
    if (input$city_filter != "All cities") x <- x %>% filter(city == input$city_filter)
    if (input$status_filter != "All statuses") x <- x %>% filter(as.character(order_status) == input$status_filter)
    if (input$payment_filter != "All methods") x <- x %>% filter(as.character(payment_method) == input$payment_filter)
    x
  })
  filtered_items <- reactive({
    x <- item_join
    if (input$city_filter != "All cities") x <- x %>% filter(city == input$city_filter)
    x
  })
  customer_data <- reactive({
    ids <- unique(filtered_orders()$user_id)
    users %>% filter(user_id %in% ids)
  })
  customer_counts <- reactive({
    filtered_orders() %>% count(user_id, name = "orders") %>%
      mutate(segment = ifelse(orders > 1, "Repeat", "One-time"))
  })
  restaurant_stats <- reactive({
    filtered_orders() %>% group_by(restaurant_id, restaurant_name, city, cuisine, rating) %>%
      summarise(orders = n(), revenue = sum(total_amount, na.rm = TRUE),
                avg_delivery = mean(delivery_time, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(revenue))
  })
  fmt_money <- function(x) paste0("₹", scales::comma(round(x, 0)))
  fmt_num <- function(x) scales::comma(round(x, 0))
  kpi <- function(label, value, note, ico, color = "#0f9d8a") {
    div(class = "kpi",
        div(class = "kpi-head", label,
            div(class = "kpi-icon", style = paste0("color:", color, ";background:", color, "18;"), icon(ico))),
        div(class = "kpi-value", value), div(class = "kpi-note", note))
  }
  kpi_row <- function(...) div(class = "kpi-grid", ...)
  panel <- function(title, subtitle, plot_id, height = "290px") {
    div(class = "panel", div(class = "panel-title", title),
        div(class = "panel-sub", subtitle), plotlyOutput(plot_id, height = height))
  }
  plot_layout <- function(p, x_title = "", y_title = "") {
    p %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                 font = list(family = "DM Sans", color = "#64727d", size = 10),
                 xaxis = list(title = x_title, gridcolor = "#edf1f3", zerolinecolor = "#edf1f3"),
                 yaxis = list(title = y_title, gridcolor = "#edf1f3", zerolinecolor = "#edf1f3"),
                 margin = list(l = 55, r = 15, t = 8, b = 45))
  }
  empty <- function(msg = "No records match the selected filters") {
    plot_ly() %>% layout(annotations = list(text = msg, x = .5, y = .5, showarrow = FALSE),
                         paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  }
  output$page_content <- renderUI({
    switch(input$page,
           overview = tagList(
             div(class="section-title","Business at a glance"), div(class="section-desc","A consolidated view of orders, customers and revenue."),
             uiOutput("ov_kpis"),
             div(class="grid-2", panel("Revenue over time","Monthly revenue across the selected period","ov_revenue","310px"),
                 panel("Order status mix","Share of orders by current status","ov_status","310px")),
             div(class="grid-equal", panel("Orders by city","Where order demand is strongest","ov_city"),
                 panel("Payment methods","How customers pay for their orders","ov_payment"))
           ),
           revenue = tagList(
             div(class="section-title","Revenue analytics"), div(class="section-desc","Track revenue patterns and compare commercial performance."),
             uiOutput("rev_kpis"),
             div(class="grid-2", panel("Monthly revenue","Revenue trend over time","rev_month","320px"),
                 panel("Revenue by city","Top-performing locations","rev_city","320px")),
             div(class="grid-equal", panel("Revenue by payment method","Contribution by payment type","rev_payment"),
                 panel("Monthly order volume","Orders placed each month","rev_orders"))
           ),
           customers = tagList(
             div(class="section-title","Customer intelligence"), div(class="section-desc","Explore customer segments and available demographic attributes."),
             uiOutput("cus_kpis"),
             div(class="grid-equal", panel("Customer segments","Repeat versus one-time customers in selected period","cus_segment"),
                 panel("Gender distribution","Customer profile","cus_gender")),
             div(class="grid-equal", panel("Age distribution","Age profile where available","cus_age"),
                 panel("Occupation","Customer occupation profile","cus_occupation"))
           ),
           restaurants = tagList(
             div(class="section-title","Restaurant performance"), div(class="section-desc","Compare restaurant sales, order volume, ratings and cuisine mix."),
             uiOutput("rest_kpis"),
             div(class="grid-2", panel("Top restaurants by revenue","Top 10 restaurants in the selected period","rest_top","340px"),
                 panel("Cuisine revenue mix","Revenue grouped by cuisine","rest_cuisine","340px")),
             div(class="table-panel", div(class="panel-title","Restaurant leaderboard"),
                 div(class="panel-sub","Sortable restaurant performance details"), DTOutput("rest_table"))
           ),
           menu = tagList(
             div(class="section-title","Menu intelligence"), div(class="section-desc","Identify popular dishes and understand category and dietary mix."),
             uiOutput("menu_kpis"),
             div(class="grid-2", panel("Best-selling items","Top 10 items by units sold","menu_top","340px"),
                 panel("Sales by food category","Units sold across categories","menu_category","340px")),
             div(class="grid-equal", panel("Vegetarian classification","Based on the dataset's veg field","menu_veg"),
                 panel("Revenue by category","Item revenue where item prices are provided","menu_revenue")),
             div(class="table-panel", div(class="panel-title","Menu performance"),
                 div(class="panel-sub","Item-level details from order_items and menu data"), DTOutput("menu_table"))
           ),
           operations = tagList(
             div(class="section-title","Delivery & operations"), div(class="section-desc","Monitor order fulfilment, delivery times and cancellations."),
             uiOutput("ops_kpis"),
             div(class="grid-equal", panel("Order status breakdown","Order count by status","ops_status"),
                 panel("Delivery-time distribution","Distribution of recorded delivery times","ops_dist")),
             div(class="grid-equal", panel("Average delivery time by city","Mean delivery time in minutes","ops_city"),
                 panel("Restaurant rating distribution","Ratings for restaurants receiving orders","ops_rating"))
           ),
           explorer = tagList(
             div(class="section-title","Data explorer"), div(class="section-desc","Inspect the filtered orders and source tables used by the dashboard."),
             div(class="table-panel", div(class="panel-title","Filtered orders"), DTOutput("orders_table")),
             div(class="table-panel", div(class="panel-title","Customers in selected orders"), DTOutput("users_table"))
           )
    )
  })
  
  output$ov_kpis <- renderUI({
    x <- filtered_orders(); n <- nrow(x); rev <- sum(x$total_amount, na.rm=TRUE)
    kpi_row(kpi("TOTAL REVENUE",fmt_money(rev),"Revenue in selected period","indian-rupee-sign"),
            kpi("TOTAL ORDERS",fmt_num(n),"Orders in selected period","bag-shopping","#4e9bd5"),
            kpi("UNIQUE CUSTOMERS",fmt_num(dplyr::n_distinct(x$user_id)),"Customers with an order","users","#8b78c8"),
            kpi("AVERAGE ORDER VALUE",fmt_money(ifelse(n>0,rev/n,0)),"Revenue divided by orders","wallet","#e49a4a"))
  })
  output$ov_revenue <- renderPlotly({
    x <- filtered_orders() %>% filter(!is.na(order_date)) %>% mutate(month=floor_date(order_date,"month")) %>%
      group_by(month) %>% summarise(revenue=sum(total_amount,na.rm=TRUE),.groups="drop")
    if(!nrow(x)) return(empty())
    plot_layout(plot_ly(x,x=~month,y=~revenue,type="scatter",mode="lines+markers",fill="tozeroy",
                        line=list(color="#0f9d8a",width=3),marker=list(color="#0f9d8a",size=7),
                        hovertemplate="%{x|%b %Y}<br>₹%{y:,.0f}<extra></extra>"),"","Revenue (₹)")
  })
  output$ov_status <- renderPlotly({
    x <- filtered_orders() %>% count(order_status)
    if(!nrow(x)) return(empty())
    plot_ly(x,labels=~order_status,values=~n,type="pie",hole=.62,
            marker=list(colors=c("#0f9d8a","#f29a4a","#6c8ebf","#d66b70","#9a88c7")),
            textinfo="label+percent") %>% layout(paper_bgcolor="rgba(0,0,0,0)",font=list(color="#64727d",size=10),
                                                 showlegend=FALSE,margin=list(l=5,r=5,t=5,b=5))
  })
  output$ov_city <- renderPlotly({
    x <- filtered_orders() %>% count(city,sort=TRUE) %>% slice_head(n=10)
    if(!nrow(x)) return(empty())
    plot_layout(plot_ly(x,x=~n,y=~reorder(city,n),type="bar",orientation="h",
                        marker=list(color="#69b9aa"),hovertemplate="%{y}: %{x:,} orders<extra></extra>"),"Orders","")
  })
  output$ov_payment <- renderPlotly({
    x <- filtered_orders() %>% count(payment_method,sort=TRUE)
    if(!nrow(x)) return(empty())
    plot_layout(plot_ly(x,x=~payment_method,y=~n,type="bar",marker=list(color="#f2a15a")),"","Orders")
  })
  
  output$rev_kpis <- renderUI({
    x<-filtered_orders(); n<-nrow(x); rev<-sum(x$total_amount,na.rm=TRUE)
    cancelled<-sum(tolower(as.character(x$order_status))=="cancelled",na.rm=TRUE)
    kpi_row(kpi("TOTAL REVENUE",fmt_money(rev),"Selected period","indian-rupee-sign"),
            kpi("ORDERS",fmt_num(n),"All statuses","receipt","#4e9bd5"),
            kpi("AVERAGE ORDER VALUE",fmt_money(ifelse(n>0,rev/n,0)),"Per order","wallet","#8b78c8"),
            kpi("CANCELLED ORDERS",fmt_num(cancelled),"Status marked cancelled","ban","#d66b70"))
  })
  output$rev_month <- renderPlotly({
    x<-filtered_orders()%>%filter(!is.na(order_date))%>%mutate(month=floor_date(order_date,"month"))%>%
      group_by(month)%>%summarise(revenue=sum(total_amount,na.rm=TRUE),.groups="drop")
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~month,y=~revenue,type="scatter",mode="lines+markers",
                        line=list(color="#0f9d8a",width=3),marker=list(size=7),fill="tozeroy"),"","Revenue (₹)")
  })
  output$rev_city <- renderPlotly({
    x<-filtered_orders()%>%group_by(city)%>%summarise(revenue=sum(total_amount,na.rm=TRUE),.groups="drop")%>%slice_max(revenue,n=10)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~revenue,y=~reorder(city,revenue),type="bar",orientation="h",marker=list(color="#f2a15a")),"Revenue (₹)","")
  })
  output$rev_payment <- renderPlotly({
    x<-filtered_orders()%>%group_by(payment_method)%>%summarise(revenue=sum(total_amount,na.rm=TRUE),.groups="drop")
    if(!nrow(x))return(empty())
    plot_ly(x,labels=~payment_method,values=~revenue,type="pie",hole=.55,textinfo="label+percent")%>%
      layout(paper_bgcolor="rgba(0,0,0,0)",font=list(color="#64727d",size=10))
  })
  output$rev_orders <- renderPlotly({
    x<-filtered_orders()%>%filter(!is.na(order_date))%>%mutate(month=floor_date(order_date,"month"))%>%count(month)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~month,y=~n,type="bar",marker=list(color="#6c8ebf")),"","Orders")
  })
  
  output$cus_kpis <- renderUI({
    x<-customer_counts(); n<-nrow(x)
    kpi_row(kpi("UNIQUE CUSTOMERS",fmt_num(n),"In selected orders","users"),
            kpi("REPEAT CUSTOMERS",fmt_num(sum(x$segment=="Repeat")),"More than one order","user-check","#4e9bd5"),
            kpi("ONE-TIME CUSTOMERS",fmt_num(sum(x$segment=="One-time")),"One order in period","user","#e49a4a"),
            kpi("ORDERS PER CUSTOMER",ifelse(n>0,round(mean(x$orders),2),0),"Average frequency","arrows-rotate","#8b78c8"))
  })
  output$cus_segment <- renderPlotly({
    x<-customer_counts()%>%count(segment)
    if(!nrow(x))return(empty())
    plot_ly(x,labels=~segment,values=~n,type="pie",hole=.58,textinfo="label+percent",
            marker=list(colors=c("#0f9d8a","#f2a15a")))%>%layout(paper_bgcolor="rgba(0,0,0,0)",font=list(color="#64727d",size=10))
  })
  output$cus_gender <- renderPlotly({
    x<-customer_data()%>%count(gender,sort=TRUE)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~gender,y=~n,type="bar",marker=list(color="#8b83bd")),"","Customers")
  })
  output$cus_age <- renderPlotly({
    x<-customer_data()%>%filter(!is.na(age))
    if(!nrow(x))return(empty("No age field/data available"))
    plot_layout(plot_ly(x,x=~age,type="histogram",nbinsx=18,marker=list(color="#69b9aa")),"Age","Customers")
  })
  output$cus_occupation <- renderPlotly({
    x<-customer_data()%>%count(occupation,sort=TRUE)%>%slice_head(n=10)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~n,y=~reorder(occupation,n),type="bar",orientation="h",marker=list(color="#8b83bd")),"Customers","")
  })
  
  output$rest_kpis <- renderUI({
    x<-restaurant_stats()
    kpi_row(kpi("RESTAURANTS",fmt_num(nrow(x)),"With orders in selected period","store"),
            kpi("REVENUE",fmt_money(sum(x$revenue,na.rm=TRUE)),"Restaurant-linked revenue","indian-rupee-sign","#4e9bd5"),
            kpi("AVG. RATING",ifelse(all(is.na(x$rating)),"N/A",round(mean(x$rating,na.rm=TRUE),2)),"Where rating is available","star","#e49a4a"),
            kpi("AVG. DELIVERY",ifelse(all(is.na(x$avg_delivery)),"N/A",paste0(round(mean(x$avg_delivery,na.rm=TRUE),1)," min")),"Minutes, where recorded","clock","#8b78c8"))
  })
  output$rest_top <- renderPlotly({
    x<-restaurant_stats()%>%slice_max(revenue,n=10)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~revenue,y=~reorder(restaurant_name,revenue),type="bar",orientation="h",marker=list(color="#0f9d8a")),"Revenue (₹)","")
  })
  output$rest_cuisine <- renderPlotly({
    x<-filtered_orders()%>%group_by(cuisine)%>%summarise(revenue=sum(total_amount,na.rm=TRUE),.groups="drop")%>%slice_max(revenue,n=8)
    if(!nrow(x))return(empty())
    plot_ly(x,labels=~cuisine,values=~revenue,type="pie",hole=.55,textinfo="label+percent")%>%
      layout(paper_bgcolor="rgba(0,0,0,0)",font=list(color="#64727d",size=10))
  })
  output$rest_table <- renderDT({
    x<-restaurant_stats()%>%select(Restaurant=restaurant_name,City=city,Cuisine=cuisine,Rating=rating,Orders=orders,Revenue=revenue,Avg_Delivery_Min=avg_delivery)
    datatable(x,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE,dom="ftip"))%>%
      formatCurrency("Revenue",currency="₹",digits=0)%>%formatRound(c("Rating","Avg_Delivery_Min"),1)
  })
  
  output$menu_kpis <- renderUI({
    x<-filtered_items()
    kpi_row(kpi("ITEM LINES",fmt_num(nrow(x)),"Rows in item-level data","utensils"),
            kpi("UNITS SOLD",fmt_num(sum(x$quantity,na.rm=TRUE)),"Quantity from order items","basket-shopping","#4e9bd5"),
            kpi("ITEM REVENUE",if(all(is.na(x$item_revenue)))"N/A" else fmt_money(sum(x$item_revenue,na.rm=TRUE)),"Only where item price exists","indian-rupee-sign","#e49a4a"),
            kpi("CATEGORIES",fmt_num(dplyr::n_distinct(x$category)),"Distinct menu categories","layer-group","#8b78c8"))
  })
  output$menu_top <- renderPlotly({
    x<-filtered_items()%>%group_by(item_name)%>%summarise(units=sum(quantity,na.rm=TRUE),.groups="drop")%>%slice_max(units,n=10)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~units,y=~reorder(item_name,units),type="bar",orientation="h",marker=list(color="#0f9d8a")),"Units sold","")
  })
  output$menu_category <- renderPlotly({
    x<-filtered_items()%>%group_by(category)%>%summarise(units=sum(quantity,na.rm=TRUE),.groups="drop")%>%slice_max(units,n=10)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~units,y=~reorder(category,units),type="bar",orientation="h",marker=list(color="#f2a15a")),"Units sold","")
  })
  output$menu_veg <- renderPlotly({
    x<-filtered_items()%>%mutate(food_type=case_when(tolower(as.character(is_veg))%in%c("yes","true","1","veg","vegetarian")~"Vegetarian",
                                                     tolower(as.character(is_veg))%in%c("no","false","0","non-veg","non vegetarian")~"Non-vegetarian",TRUE~"Unknown"))%>%
      group_by(food_type)%>%summarise(units=sum(quantity,na.rm=TRUE),.groups="drop")
    if(!nrow(x))return(empty())
    plot_ly(x,labels=~food_type,values=~units,type="pie",hole=.55,textinfo="label+percent")%>%
      layout(paper_bgcolor="rgba(0,0,0,0)",font=list(color="#64727d",size=10))
  })
  output$menu_revenue <- renderPlotly({
    x<-filtered_items()%>%group_by(category)%>%summarise(revenue=sum(item_revenue,na.rm=TRUE),.groups="drop")%>%filter(is.finite(revenue),revenue>0)%>%slice_max(revenue,n=10)
    if(!nrow(x))return(empty("Item price data unavailable"))
    plot_layout(plot_ly(x,x=~revenue,y=~reorder(category,revenue),type="bar",orientation="h",marker=list(color="#6c8ebf")),"Item revenue (₹)","")
  })
  output$menu_table <- renderDT({
    x<-filtered_items()%>%select(Item=item_name,Category=category,Vegetarian=is_veg,Restaurant=restaurant_name,City=city,Quantity=quantity,Item_Price=price,Item_Revenue=item_revenue)
    datatable(x,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE,dom="ftip"))%>%
      formatCurrency(c("Item_Price","Item_Revenue"),currency="₹",digits=0)
  })
  
  output$ops_kpis <- renderUI({
    x<-filtered_orders(); n<-nrow(x); canc<-sum(tolower(as.character(x$order_status))=="cancelled",na.rm=TRUE)
    avg<-mean(x$delivery_time,na.rm=TRUE)
    kpi_row(kpi("TOTAL ORDERS",fmt_num(n),"Selected period","box"),
            kpi("CANCELLED",fmt_num(canc),"Orders marked cancelled","ban","#d66b70"),
            kpi("CANCELLATION RATE",ifelse(n>0,paste0(round(canc/n*100,1),"%"),"0%"),"Cancelled / all orders","chart-pie","#e49a4a"),
            kpi("AVG. DELIVERY TIME",ifelse(is.nan(avg),"N/A",paste0(round(avg,1)," min")),"Where delivery time is recorded","clock","#4e9bd5"))
  })
  output$ops_status <- renderPlotly({
    x<-filtered_orders()%>%count(order_status)
    if(!nrow(x))return(empty())
    plot_layout(plot_ly(x,x=~order_status,y=~n,type="bar",marker=list(color="#0f9d8a")),"","Orders")
  })
  output$ops_dist <- renderPlotly({
    x<-filtered_orders()%>%filter(is.finite(delivery_time))
    if(!nrow(x))return(empty("No delivery-time values available"))
    plot_layout(plot_ly(x,x=~delivery_time,type="histogram",nbinsx=20,marker=list(color="#6c8ebf")),"Delivery time (minutes)","Orders")
  })
  output$ops_city <- renderPlotly({
    x<-filtered_orders()%>%group_by(city)%>%summarise(avg=mean(delivery_time,na.rm=TRUE),.groups="drop")%>%filter(is.finite(avg))%>%slice_max(avg,n=10)
    if(!nrow(x))return(empty("No delivery-time values available"))
    plot_layout(plot_ly(x,x=~avg,y=~reorder(city,avg),type="bar",orientation="h",marker=list(color="#f2a15a")),"Average minutes","")
  })
  output$ops_rating <- renderPlotly({
    x<-filtered_orders()%>%filter(is.finite(rating))
    if(!nrow(x))return(empty("No restaurant ratings available"))
    plot_layout(plot_ly(x,x=~rating,type="histogram",nbinsx=12,marker=list(color="#8b83bd")),"Restaurant rating","Orders")
  })
  
  output$orders_table <- renderDT({
    datatable(filtered_orders(),rownames=FALSE,filter="top",
              options=list(pageLength=12,scrollX=TRUE,dom="lftip"))
  })
  output$users_table <- renderDT({
    datatable(customer_data(),rownames=FALSE,filter="top",
              options=list(pageLength=10,scrollX=TRUE,dom="lftip"))
  })
}

shinyApp(ui, server)
