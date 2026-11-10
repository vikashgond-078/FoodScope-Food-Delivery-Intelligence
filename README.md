# 🍽️ FoodScope – Food Delivery Intelligence Dashboard

### Interactive Business Intelligence & Data Visualization using R Programming

FoodScope is an interactive food delivery analytics dashboard developed using **R Programming, R Shiny, and Plotly**. It transforms structured food delivery datasets into meaningful business insights through interactive visualizations, analytical summaries, and data exploration.

The project focuses on analyzing revenue, customer behavior, restaurant performance, menu trends, and delivery operations through a unified dashboard.

---

## 📌 Project Overview

FoodScope provides an integrated analytical environment for exploring food delivery data across multiple business dimensions.

The dashboard combines five datasets and presents key performance indicators (KPIs), interactive charts, and analytical insights to support data-driven decision-making.

## 🚀 Key Features

### 1. Executive Overview
- Total Revenue
- Total Orders
- Total Customers
- Average Order Value
- Overall business performance

### 2. Revenue Analytics
- Monthly revenue trends
- City-wise revenue analysis
- Payment method distribution
- Revenue performance visualization

### 3. Customer Intelligence
- Customer segmentation
- Repeat-order analysis
- Customer demographic insights
- Customer behavior exploration

### 4. Restaurant Performance
- Restaurant-level performance
- City-wise restaurant analysis
- Cuisine comparisons
- Restaurant rating insights

### 5. Menu Intelligence
- Best-selling food items
- Category-wise sales analysis
- Vegetarian and non-vegetarian analysis
- Item-level revenue insights

### 6. Delivery & Operations
- Order status distribution
- Delivered versus cancelled orders
- Operational performance analysis

### 7. Data Explorer
- Interactive data tables
- Dataset exploration
- Filtering and searching capabilities

---

## 🛠️ Technology Stack

| Technology | Purpose |
|---|---|
| R Programming | Data analysis and processing |
| R Shiny | Interactive dashboard development |
| Plotly | Interactive data visualization |
| dplyr | Data manipulation |
| DT | Interactive data tables |
| bslib | Dashboard styling and UI |
| HTML & CSS | Interface customization |

---

## 📂 Project Structure

```text
FoodScope-Food-Delivery-Intelligence/
│
├── App.R
├── FoodScope.Rproj
├── README.md
├── .gitignore
│
└── data/
    ├── orders.csv
    ├── users.csv
    ├── restaurants.csv
    ├── menu.csv
    └── order_items.csv
```

---

## 📊 Dataset Information

The project uses five interconnected datasets:

| Dataset | Description |
|---|---|
| orders.csv | Order transactions and payment information |
| users.csv | Customer information |
| restaurants.csv | Restaurant details |
| menu.csv | Menu items and categories |
| order_items.csv | Individual order item details |

**Dataset Source:**  
[Swiggy Sales Analysis – GitHub Repository](https://github.com/Harsh-Belekar/Swiggy-Sales-Analysis)

The datasets are used for educational and analytical purposes.

---

## ⚙️ Installation & Setup

### Prerequisites

- R
- RStudio
- Git

### Step 1: Clone the Repository

```bash
git clone https://github.com/vikashgond-078/FoodScope-Food-Delivery-Intelligence.git
```

### Step 2: Open the Project

Open `FoodScope.Rproj` in RStudio.

### Step 3: Install Required Packages

Run the following command in the RStudio Console:

```r
install.packages(c(
  "shiny",
  "bslib",
  "plotly",
  "dplyr",
  "DT"
))
```

Install any additional packages requested by the application if they are not already available.

### Step 4: Run the Dashboard

Open `App.R` and click **Run App**.

Alternatively, execute:

```r
shiny::runApp()
```

The dashboard will open in your default web browser.

---

## 🎯 Project Objectives

- Develop an interactive dashboard using R Programming.
- Perform exploratory data analysis.
- Integrate multiple structured datasets.
- Create meaningful business performance indicators.
- Visualize revenue and customer trends.
- Analyze restaurant and menu performance.
- Present data through an interactive interface.

---

## 🔍 Learning Outcomes

Through this project, practical experience was gained in:

- R programming and data manipulation.
- Data preprocessing and transformation.
- Exploratory Data Analysis (EDA).
- Relational data integration and joins.
- KPI development.
- Interactive dashboard development.
- Data visualization using Plotly.
- Building reactive applications with R Shiny.

---

## ⚠️ Project Limitations

- The analysis depends on the structure and quality of the supplied datasets.
- Delivery-duration analysis is limited because the supplied delivery-time field is not represented as a numeric duration.
- Some dashboard filters are not applied across every analytical module.
- Results should be interpreted as insights from the supplied educational dataset, not verified real-world business performance.

---

## 👨‍💻 Developer

**Vikash Gond**

Engineering Student | Data Analytics | Data Science | R Programming

GitHub: [@vikashgond-078](https://github.com/vikashgond-078)

---

## 📜 License

This project is intended for educational and learning purposes. Dataset redistribution and reuse are subject to the original dataset's licensing terms.
