
setwd("C:/Users/lucas/Desktop/Blogs/Blog Files/Cohort Analysis Intro")
getwd()

# -------------------------------
# Simulated D2C Order Data (12 months)
# Premium long-lasting dog dental chews (single SKU)
# Operates in Germany, ships across Europe
# -------------------------------

set.seed(42)

suppressWarnings({
  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
    library(tidyr)
    library(stringr)
  })
})

# -------------------------------
# 0) Time window (last 12 months)
# -------------------------------
end_date   <- as.Date(Sys.Date())
start_date <- end_date - 364
dates      <- seq.Date(start_date, end_date, by = "day")

# -------------------------------
# 1) Europe shipping countries 
# -------------------------------
eu_countries <- c(
  "Germany","Austria","Netherlands","Belgium","France","Italy","Spain","Portugal",
  "Poland","Czechia","Slovakia","Hungary","Denmark","Sweden","Finland","Ireland",
  "Greece","Romania","Bulgaria","Croatia","Slovenia","Lithuania","Latvia","Estonia"
)

p_germany <- 0.65  # e.g., 65% of customers from Germany 
n_customers<-10000

# countries
country <- ifelse(
  runif(n_customers) < p_germany,
  "Germany",
  sample(setdiff(eu_countries, "Germany"), size = n_customers, replace = TRUE)
)

table(country) %>%
  sort(decreasing = TRUE) %>%
  head(10)

# -------------------------------
# 2) Acquisition channels 
# -------------------------------
channels <- c("Paid Social","Paid Search","Organic","Referral")
channel_probs <- c(0.35, 0.25, 0.30, 0.10)

# -------------------------------
# 3) Payment methods in Germany/EU checkout 
# -------------------------------
payments <- c("PayPal","Klarna","Credit Card","SEPA/Invoice","Digital Wallet")
payment_probs <- c(0.30, 0.15, 0.25, 0.20, 0.10)

# Stripe-style fee model approximation
# % fee varies a bit by method; plus fixed fee
fee_pct_map <- c(
  "PayPal"         = 0.030,
  "Klarna"         = 0.032,
  "Card"           = 0.018,
  "SEPA/Invoice"   = 0.012,
  "Digital Wallet" = 0.018
)
fee_fixed_map <- c(
  "PayPal"         = 0.25,
  "Klarna"         = 0.25,
  "Card"           = 0.25,
  "SEPA/Invoice"   = 0.20,
  "Digital Wallet" = 0.25
)

# -------------------------------
# 4) Price + discount assumptions
# -------------------------------
base_price <- 12.79 #based on medium sized chews

# Discount behavior: 25–40% orders get a discount 
# Discount size: 10–30% (skewed toward smaller discounts)
draw_discount_rate <- function(n){
  # Beta distribution scaled to [0.10, 0.30]
  0.10 + 0.20 * rbeta(n, shape1 = 2.2, shape2 = 4.5)
}

# -------------------------------
# 5) Seasonality (daily demand)
# -------------------------------
# Create an expected orders-per-day process:
# - baseline growth over time (mild)
# - weekly seasonality (weekends slightly higher)
# - monthly seasonality (end-of-month slightly higher)
# - Black Friday / Cyber Week spike

# baseline: start with ~18 orders/day, end ~26 orders/day
baseline_start <- 18
baseline_end   <- 26
trend <- seq(baseline_start, baseline_end, length.out = length(dates))

dow <- wday(dates, label = TRUE, week_start = 1)  # Mon=1
week_multiplier <- ifelse(dow %in% c("Sat","Sun"), 1.18, 1.00)

dom <- mday(dates)
month_multiplier <- ifelse(dom >= 25, 1.10, 1.00)

# Black Friday week (approx): last Friday of Nov + next 6 days
year_start <- year(start_date)
year_end   <- year(end_date)
years <- unique(c(year_start, year_end))

black_friday_dates <- do.call(c, lapply(years, function(y) {
  nov_days <- seq.Date(as.Date(sprintf("%d-11-01", y)),
                       as.Date(sprintf("%d-11-30", y)),
                       by = "day")
  fridays <- nov_days[lubridate::wday(nov_days) == 6]
  bf <- tail(fridays, 1)
  seq.Date(bf, bf + 6, by = "day")
}))


bf_multiplier <- ifelse(dates %in% black_friday_dates, 1.80, 1.00)

lambda <- trend * week_multiplier * month_multiplier * bf_multiplier

# -------------------------------
# 6) Generate first-time customers across days
# -------------------------------
# First-time orders each day
new_orders_per_day <- rpois(length(dates), lambda = lambda * 0.70)  # 70% new customers
new_orders_per_day[new_orders_per_day < 0] <- 0

n_new_customers <- sum(new_orders_per_day)
customer_ids <- sprintf("C%06d", seq_len(n_new_customers))
first_order_dates <- rep(dates, times = new_orders_per_day)

# --- Assign countries per customer (65% Germany, rest random EU) ---
customer_country <- ifelse(
  runif(n_new_customers) < p_germany,
  "Germany",
  sample(setdiff(eu_countries, "Germany"), size = n_new_customers, replace = TRUE)
)

# --- Sample acquisition channels per customer ---
customer_channel <- sample(
  channels,
  size = n_new_customers,
  replace = TRUE,
  prob = channel_probs
)

customers <- tibble(
  customer_id = customer_ids,
  first_order_date = first_order_dates,
  acquisition_channel = customer_channel,
  country = customer_country
)


# -------------------------------
# 7) Repeat behavior: 20–30% repeat within 12 months, timing 30–60 days
# -------------------------------
repeat_rate <- runif(1, 0.20, 0.30)

customers <- customers %>%
  mutate(will_repeat = rbinom(n(), 1, repeat_rate) == 1)

# For repeaters: 1 repeat order 
# Repeat date = first_order_date + U(30,60), if within the window
customers <- customers %>%
  mutate(repeat_lag_days = ifelse(will_repeat, sample(30:60, n(), replace = TRUE), NA_integer_),
         repeat_order_date = ifelse(will_repeat, as.character(first_order_date + days(repeat_lag_days)), NA_character_),
         repeat_order_date = as.Date(repeat_order_date)) %>%
  mutate(will_repeat = ifelse(is.na(repeat_order_date), FALSE, will_repeat))

# -------------------------------
# 8) order table (first + repeat)
# -------------------------------
orders_first <- customers %>%
  transmute(
    customer_id,
    order_date = first_order_date,
    order_type = "first",
    acquisition_channel,
    country
  )

orders_repeat <- customers %>%
  filter(will_repeat) %>%
  transmute(
    customer_id,
    order_date = repeat_order_date,
    order_type = "repeat",
    acquisition_channel,  
    country
  )

orders <- bind_rows(orders_first, orders_repeat) %>%
  arrange(order_date) %>%
  mutate(order_id = sprintf("O%08d", row_number()))

# -------------------------------
# 9) Quantity per order (mostly 1 pack, minority multi-pack)
# -------------------------------
# 75% 1 pack, 20% 2 packs, 5% 3 packs
qty_probs <- c(`1`=0.75, `2`=0.20, `3`=0.05)
orders <- orders %>%
  mutate(quantity = as.integer(sample(c(1,2,3), size = n(), replace = TRUE, prob = qty_probs)))

# -------------------------------
# 10) Discounts (time-varying usage + size)
# -------------------------------
# Discount usage increases during BF week
base_disc_prob <- 0.28  # within 25–40% target
disc_prob <- ifelse(orders$order_date %in% black_friday_dates, 0.45, base_disc_prob)
orders <- orders %>%
  mutate(has_discount = rbinom(n(), 1, disc_prob) == 1,
         discount_rate = ifelse(has_discount, draw_discount_rate(n()), 0.0))

# -------------------------------
# 11) Payments and fees
# -------------------------------
orders <- orders %>%
  mutate(payment_method = sample(payments, size = n(), replace = TRUE, prob = payment_probs),
         fee_pct = unname(fee_pct_map[payment_method]),
         fee_fixed = unname(fee_fixed_map[payment_method]))

# -------------------------------
# 12) Delivery outcomes
# Normal delivery target: 1–3 days typical
# Delay probability: ~10% (increase during BF week slightly)
# Failed delivery: low but non-zero
# -------------------------------
delay_prob <- ifelse(orders$order_date %in% black_friday_dates, 0.14, 0.10)
fail_prob  <- ifelse(orders$order_date %in% black_friday_dates, 0.012, 0.006)

orders <- orders %>%
  mutate(
    is_failed_delivery = rbinom(n(), 1, fail_prob) == 1,
    is_delayed = ifelse(is_failed_delivery, FALSE, rbinom(n(), 1, delay_prob) == 1),
    delivery_days = case_when(
      is_failed_delivery ~ NA_integer_,
      !is_delayed ~ sample(1:3, n(), replace = TRUE, prob = c(0.35,0.40,0.25)),
      is_delayed ~ sample(4:9, n(), replace = TRUE, prob = c(0.20,0.20,0.18,0.16,0.14,0.12))
    )
  )

# -------------------------------
# 13) Refund logic
# Base refund ~5% (Statista anchor), uplift for delivery issues
# If failed delivery -> near-certain refund (modelled at 95%)
# If delayed -> higher refund probability
# -------------------------------
base_refund <- 0.05
refund_prob <- case_when(
  orders$is_failed_delivery ~ 0.95,
  orders$is_delayed ~ 0.12,
  TRUE ~ base_refund
)

orders <- orders %>%
  mutate(is_refunded = rbinom(n(), 1, refund_prob) == 1)

# -------------------------------
# 14) Financials (unit economics, per order)
# - Retail price applies per unit
# - Shipping fee charged to customer externally (not included in product revenue)
# - Compute gross product revenue, discounts, net product revenue
# - Payment fees applied on net product revenue
# - Cost components per your assumptions
# -------------------------------
# Costs per order 
cogs_unit <- 4.50
pack_unit <- 0.80
fulfill_handling <- 1.50
shipping_cost <- 3.50

orders <- orders %>%
  mutate(
    gross_product_revenue = base_price * quantity,
    discount_amount = gross_product_revenue * discount_rate,
    net_product_revenue = gross_product_revenue - discount_amount,
    
    # payment fees charged on net revenue
    payment_fee_amount = net_product_revenue * fee_pct + fee_fixed,
    
    # costs
    cogs_amount = cogs_unit * quantity,
    packaging_amount = pack_unit * quantity,
    fulfillment_amount = fulfill_handling,
    shipping_cost_amount = shipping_cost,
    
    # refund impact 
    revenue_after_refund = ifelse(is_refunded, 0, net_product_revenue),
    
    # contribution margin 
    total_cost = cogs_amount + packaging_amount + fulfillment_amount + shipping_cost_amount + payment_fee_amount,
    contribution_margin = revenue_after_refund - total_cost
  )

# -------------------------------
# 15) Cohort fields 
# -------------------------------
first_dates_lookup <- customers %>%
  select(customer_id, first_order_date)

orders <- orders %>%
  left_join(first_dates_lookup, by = "customer_id") %>%
  mutate(
    cohort_month = floor_date(first_order_date, "month"),
    days_since_first = as.integer(order_date - first_order_date)
  )

# -------------------------------
# 16) Output
# ------------------------------
print(orders %>% select(order_id, customer_id, order_date, order_type, country,
                        acquisition_channel, payment_method, quantity,
                        gross_product_revenue, discount_rate, net_product_revenue,
                        is_delayed, is_failed_delivery, is_refunded,
                        total_cost, contribution_margin,
                        cohort_month, days_since_first) %>% head(10))

out_path <- "simulated_d2c_orders_12mo_europe.csv"
write.csv(orders, out_path, row.names = FALSE)
cat("\nSaved:", out_path, "\n")
