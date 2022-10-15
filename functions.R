#' <data frame> of numeric summary.
#'
#' @param df a data frame.
#' @param var a numeric variable to summarise.
#' @param include_count whether to include the count.
#' @param groups how to treat groupings if available.
#' @param pivot whether to convert the table to a long format (useful when there are no groupings) 
#'
#' @return a tibble.
#' @export
#'
#' @examples numeric_summary(data, "profit")
#' 
numeric_summary <- function(df, var, 
                            include_count = TRUE, groups = "drop_last",
                            pivot = FALSE) {
  if (!is.numeric(df[[var]])) {
    stop("arguemnt `var` only allows numeric variables")
  }
  f_tbl <- df |>
    dplyr::summarise(count   = dplyr::n(),
                     minimum = min(.data[[var]], na.rm = TRUE),
                     Q25     = quantile(.data[[var]], 0.25, na.rm = TRUE),
                     mean    = mean(.data[[var]], na.rm = TRUE),
                     median  = median(.data[[var]], na.rm = TRUE),
                     Q75     = quantile(.data[[var]], 0.75, na.rm = TRUE),
                     maximum = max(.data[[var]], na.rm = TRUE),
                     sum     = sum(.data[[var]], na.rm = TRUE), 
                     .groups = groups)
  
  if (isTRUE(include_count)) f_tbl else f_tbl <- dplyr::select(f_tbl, -count)
  
  if (isTRUE(pivot)) {
    f_tbl |>
      tidyr::pivot_longer(cols = dplyr::everything(), 
                          names_to  = "statistics", 
                          values_to = "value")
  } else {
    f_tbl
  }
}



#' <data frame> count the number of observation in each character category.
#'
#' @param df a data frame.
#' @param count_var the character variable to count.
#' @param add_percent whether to add each category proportion of the total column.
#'
#' @return a tibble
#' @export
#'
#' @examples  count_chr(data, "sales_team")
#' 
chr_count <- function(df, count_var, add_percent = TRUE) {
  
  s_tbl <- df |>
    dplyr::count(.data[[count_var]], sort = TRUE, name = "count")
  
  if (add_percent) {
    s_tbl |>
      dplyr::mutate(percentage = round(proportions(count)*100, 2))
    
  } else {
    s_tbl
  }
}




#' Clean title labels
#'
#' @param label string to clean.
#'
#' @return a character string.
#' @export
#'
#' @examples clean_label("net_revenue")
clean_label <- function(label) {
  stringr::str_replace_all(label, "_", " ") |>
    stringr::str_to_title()
}




#' Table theme and language
#'
#' @param type type of output, either "theme" or "lang".
#' @param cell_padding the amount of top, bottom, left & right padding for each cell.
#' @param info_text additional info for the table pagination.
#'
#' @return a react table output.
#' @export
#'
#' @examples reactable(theme = table_style(type = "theme"))
#' 
table_style <- function(type = "theme", cell_padding = 6, info_text = "entries") {
  if (type == "theme") {
    reactable::reactableTheme(
      backgroundColor = "#ffffff",
      color = "#787878",
      borderWidth = "1px",
      borderColor = "#EDEDED",
      stripedColor = "#FCFCFC",
      cellPadding = cell_padding,
      
      cellStyle = list(display = "flex", 
                       flexDirection = "column",
                       justifyContent = "center"
      ),
      
      tableStyle = list(fontSize = 15),
      headerStyle = list(borderWidth = "1px",
                         padding = "5px",
                         
                         background = "#FFFFFF",
                         borderColor = "#828282",
                         fontWeight = "600",
                         fontSize = 16,
                         color = "#666666"),
      
      inputStyle = NULL,
      rowSelectedStyle = NULL,
      selectStyle = NULL,
      paginationStyle = NULL,
      
      style = list(
        fontFamily = tbl_font_family
      )
    )
    
  }  else if (type == "lang") {
    reactable::reactableLang(pageInfo = stringr::str_glue("{{rows}} {info_text}"),
                             pagePrevious = "\u276e",
                             pageNext = "\u276f")
  }
}



#' Summaries revenue
#'
#' @param df cleaned hotel revenue data.
#' @param variable numeric variable to summarise.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param percent if TRUE return its proportion of revenue generated.
#' @param a_fun aggregate function to use.
#'
#' @return a character value.
#' @export
#'
#' @examples rev_variable_summary(hotel_data, "net_revenue", percent = TRUE)
#' 
rev_variable_summary <- function(df, variable, f_hotel, 
                                 percent = FALSE, a_fun = "sum") {
  fun <- rlang::as_closure(a_fun)
  
  if (percent) {
    f_val <- df |>
      dplyr::summarise(across(c(revenue, .data[[variable]]), fun)) |>
      dplyr::mutate(percent = .data[[variable]] / revenue) |>
      dplyr::pull(percent)
    
    scales::label_percent(0.01)(f_val)
    
  } else {
    if (missing(f_hotel)) {
      f_val <- df |>
        dplyr::summarise(value = fun(.data[[variable]])) |>
        dplyr::pull() 
      
    } else {
      f_val <- df |>
        dplyr::filter(hotel == f_hotel) |>
        dplyr::summarise(value = fun(.data[[variable]])) |>
        dplyr::pull() 
    }
    
    scales::label_number(accuracy = 0.1, 
                         prefix = "$",
                         scale_cut = scales::cut_short_scale())(f_val)
  }
}



#' Reservation status description
#'
#' @param df cleaned hotel revenue data.
#' @param cat a unique sub category of the reservation_status variable.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a character value.
#' @export
#'
#' @examples res_status_description(hotel_data, "Canceled")
#' 
res_status_description <- function(df, cat, f_hotel) {
  if (!missing(f_hotel)) {
    f_tbl <- dplyr::filter(df, hotel == f_hotel) 
    
  } else {
    f_tbl <- df 
  }
  
  f_val <- f_tbl |>
    chr_count("reservation_status") |>
    dplyr::filter(reservation_status == cat) |> 
    dplyr::pull(count)
  
  scales::label_number(scale_cut = scales::cut_short_scale())(f_val)
}



#' Lead time and waiting list description 
#'
#' @param df cleaned hotel revenue data.
#' @param var the type of output to return. either "lead_time" or "waiting_list".
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param a_fun aggregate function to use.
#'
#' @return a character value.
#' @export
#'
#' @examples lead_time_description(hotel_data, "lead_time", "City Hotel", a_fun = "max")
#' 
lead_time_description <- function(df, var, f_hotel, a_fun = "mean") {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  fun <- rlang::as_closure(a_fun)
  
  if (var == "lead_time") {
    f_val <- df |>
      dplyr::summarise(value = fun(lead_time)) |>
      dplyr::pull() |>
      round()
    
  } else if (var == "waiting_list") {
    f_val <- df |>
      dplyr::filter(days_in_waiting_list > 1) |>
      dplyr::summarise(total = fun(days_in_waiting_list)) |>
      dplyr::pull() |>
      round()
  }
  
  stringr::str_glue("{f_val} days")
}



#' Bookings changes description
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of output to return. any of "changed", "no_changes" or "precent".
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a character value.
#' @export
#'
#' @examples booking_changes_description(hotel_data, "changed", "Resort Hotel")
#' 
booking_changes_description <- function(df, output_type, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  if (output_type == "changed") {
    f_val <- dplyr::filter(df, booking_changes != 0) |> nrow()
    
    scales::label_number(0.01, scale_cut = scales::cut_short_scale())(f_val)
    
  } else if (output_type == "no_changes") {
    f_val <- dplyr::filter(df, booking_changes == 0) |> nrow()
    
    scales::label_number(0.01, scale_cut = scales::cut_short_scale())(f_val)
    
  } else if (output_type == "precent") {
    f_val <- dplyr::filter(df, booking_changes != 0) |> nrow()
    f_val <- f_val / nrow(df)
    
    scales::label_percent(0.01)(f_val)
  }
}



#' Number Of Days Guest stayed at the hotel.
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a character value.
#' @export
#'
#' @examples days_stay_description(hotel_data)
#' 
days_stay_description <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  df |>
    numeric_summary("stay_nights") |>
    dplyr::select(minimum, median, maximum) |>
    as.list()
}



#' Number Of Guest
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of output to return. any of "all", "adult" or "children"
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a character value.
#' @export
#'
#' @examples guest_description(hotel_data, "children", "Resort Hotel")
#' 
guest_description <- function(df, output_type, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_df <- df |>
    dplyr::mutate(children = dplyr::case_when(is.na(children) ~ 0, TRUE ~ children))
  
  if (output_type == "all") {
    f_val <- f_df |> 
      dplyr::mutate(value = adults+children+babies) |>
      dplyr::summarise(value = sum(value)) |>
      dplyr::pull()
    
  } else if (output_type == "adult") {
    f_val <- dplyr::summarise(f_df, value = sum(adults)) |> dplyr::pull()
    
  } else if (output_type == "children") {
    f_val <- f_df |>
      dplyr::mutate(value = children+babies) |>
      dplyr::summarise(value = sum(value)) |>
      dplyr::pull()
  }
  
  scales::label_number(0.01, scale_cut = scales::cut_short_scale())(f_val)
}



#' repeated guest summary.
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of output to return. any of "repeated", "no_repeat" or "percent_repeat"
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a character value.
#' @export
#'
#' @examples repeated_guest_description(hotel_data, "no_repeat")
#' 
repeated_guest_description <- function(df, output_type, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- hr |>
    dplyr::select(is_repeated_guest) |>
    chr_count("is_repeated_guest")
  
  if (output_type %in% c("repeated", "no_repeat")) {
    if (output_type == "repeated") {
      f_val <- dplyr::filter(f_tbl, is_repeated_guest == 1) |> dplyr::pull(count)
      
    } else if (output_type == "no_repeat") {
      f_val <- dplyr::filter(f_tbl, is_repeated_guest == 0) |> dplyr::pull(count)
      
    }
    
    scales::label_number(0.01, scale_cut = scales::cut_short_scale())(f_val)
    
  } else {
    if (output_type == "percent_repeat") {
      dplyr::filter(f_tbl, is_repeated_guest == 1) |> 
        dplyr::pull(percentage) |>
        round(2) |>
        paste0("%")
    }
  }
}



#' Get Continent names.
#'
#' @return a tibble/data.frame
#' @export
#'
#' @examples get_continent()
#' 
get_continent <- function() {
  gapminder::gapminder_unfiltered |> 
    dplyr::distinct(country, continent) |>
    dplyr::arrange(continent)
}


#' guest nationality summary.
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of output to return. any of "country", "continent" or "top_national".
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param conti continent table.
#'
#' @return a character value.
#' @export
#'
#' @examples nationality_description(hotel_data, "continent", "City Hotel")
#' 
nationality_description <- function(df, output_type, f_hotel, 
                                    conti = get_continent()) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  if (output_type == "country") {
    dplyr::distinct(df, nationality) |> nrow()
    
  } else if (output_type == "top_national") {
    dplyr::count(df, nationality, sort = TRUE) |> 
      head(1) |> 
      dplyr::pull(nationality)
    
  } else if (output_type == "continent") {
    df |>
      dplyr::select(nationality) |>
      dplyr::inner_join(conti, by = c("nationality" = "country")) |>
      dplyr::distinct(continent) |>
      nrow()
  }
}



#' Special request summary.
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of output to return. any "max", "percent" or "total"
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a character value.
#' @export
#'
#' @examples special_request_description(hotel_data, "total")
#' 
special_request_description <- function(df, output_type, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  if (output_type == "total") {
    f_val <- df |>
      dplyr::summarise(value = sum(total_of_special_requests)) |> 
      dplyr::pull()
    
    scales::label_number(0.01, scale_cut = scales::cut_short_scale())(f_val)
    
  } else if (output_type == "max") {
    dplyr::summarise(df, value = max(total_of_special_requests)) |> dplyr::pull()
    
  } else if (output_type == "precent") {
    f_val <- dplyr::filter(df, total_of_special_requests > 0)
    f_val <- nrow(f_val) / nrow(df)
    
    scales::label_percent(0.01)(f_val)
  }
}



#' Revenue summary by arrival date and hotel.
#'
#' @param df cleaned hotel revenue data.
#' @param sumy_var numeric variable to summarise.
#' @param f_year year of the summary any of 2018, 2019 or 2020
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param a_fun aggregate function to use.
#' @param colr a vector of two similar colors to use.
#'
#' @return a highchart line/area chart.
#' @export
#'
#' @examples arrival_date_revenue(hotel_data, "net_revenue", 2018, "City Hotel",
#'                                colr = c("blue", "lightBlue"))
#'                                
arrival_date_revenue <- function(df, sumy_var, 
                                 f_year, f_hotel, 
                                 a_fun = "sum", colr = c("blue", "blue")) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  fun <- rlang::as_closure(a_fun)
  
  f_tbl <- df |>
    dplyr::filter(arrival_date_year == f_year) |>
    dplyr::group_by(arrival_date) |>
    dplyr::summarise(value = fun(.data[[sumy_var]]))
  
  var_name <- clean_label(sumy_var)
  
  if (sumy_var == "avg_daily_rate") {
    t_t <- stringr::str_glue("Average Daily Rate During {f_year}")
    
  } else {
    t_t <- stringr::str_glue("{agg_names[[a_fun]]} {var_name} During {f_year}")
  }
  
  t_t <- ifelse(!missing(f_hotel),
                stringr::str_glue("{t_t} For {clean_label(f_hotel)}"),
                t_t)
  
  highcharter::highchart(height = "445px") |>
    highcharter::hc_add_series(f_tbl, 
                               type = "spline",
                               highcharter::hcaes(x = arrival_date, y = value),
                               name = var_name,
                               color = colr[1],
                               tooltip = list(valueDecimals = 2,
                                              valuePrefix = "$")) |>
    highcharter::hc_add_series(f_tbl, 
                               type = "areaspline",
                               highcharter::hcaes(x = arrival_date, y = value),
                               name = var_name,
                               color = colr[2],
                               tooltip = list(valueDecimals = 2,
                                              valuePrefix = "$")) |>
    
    highcharter::hc_xAxis(type = "datetime",
                          dateTimeLabelFormats = list(day = "%m %d")) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_title(text = t_t, align = "left")
}



#' number of reservations and arrival date.
#'
#' @param df cleaned hotel revenue data.
#' @param f_year year of the summary any of 2018, 2019 or 2020
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param colr a vector of two similar colors to use.
#'
#' @return a highchart line/area chart.
#' @export
#'
#' @examples arrival_count(hotel_data, 2018)
#' 
arrival_count <- function(df, f_year, f_hotel, colr = c("blue", "blue")) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- df |>
    dplyr::filter(arrival_date_year == f_year) |>
    dplyr::count(arrival_date, name = "value") 
  
  t_t <- stringr::str_glue("Number Of Bookings In {f_year}")
  t_t <- ifelse(!missing(f_hotel),
                stringr::str_glue("{t_t} For {clean_label(f_hotel)}"),
                t_t)
  
  highcharter::highchart(height = "445px") |>
    highcharter::hc_add_series(data = f_tbl,
                               type = "spline",
                               color = colr[1],
                               highcharter::hcaes(x = arrival_date, y = value),
                               name = "Arrivals") |>
    highcharter::hc_add_series(data = f_tbl,
                               type = "areaspline",
                               color = colr[2],
                               highcharter::hcaes(x = arrival_date, y = value),
                               name = "Arrivals") |>
    highcharter::hc_xAxis(type = "datetime",
                          dateTimeLabelFormats = list(day = "%m %d")) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_title(text = t_t, align = "left")
}



#' Number Of Guest
#'
#' @param df cleaned hotel revenue data.
#' @param f_year year of the summary any of 2018, 2019 or 2020
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param a_fun aggregate function to use.
#' @param colr a vector of two similar colors to use.
#'
#' @return a highchart line/area chart.
#' @export
#'
#' @examples n_guest_arrival_date(hotel_data, 2020, a_fun = "sum")
#' 
n_guest_arrival_date <- function(df, f_year, f_hotel, 
                                 a_fun, colr = c("blue", "blue")) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  fun <- rlang::as_closure(a_fun)
  f_tbl <- hr |>
    dplyr::filter(arrival_date_year == 2018) |>
    dplyr::mutate(n_guest = adults+children+babies) |>
    dplyr::group_by(arrival_date) |>
    dplyr::summarise(value = fun(n_guest))
  
  t_t <- stringr::str_glue("{agg_names[[a_fun]]} Guest During The Year {f_year}")
  t_t <- ifelse(!missing(f_hotel),
                stringr::str_glue("{t_t} For {clean_label(f_hotel)}"),
                t_t)
  
  highcharter::highchart(height = "445px") |>
    highcharter::hc_add_series(data = f_tbl,
                               type = "spline",
                               color = colr[1],
                               highcharter::hcaes(x = arrival_date, y = value),
                               name = "Arrivals") |>
    highcharter::hc_add_series(data = f_tbl,
                               type = "areaspline",
                               color = colr[2],
                               highcharter::hcaes(x = arrival_date, y = value),
                               name = "Arrivals") |>
    highcharter::hc_xAxis(type = "datetime",
                          dateTimeLabelFormats = list(day = "%m %d")) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_title(text = t_t, align = "left")
}



#' Arrival date summary by customer type and market segment.
#'
#' @param df cleaned hotel revenue data.
#' @param var variable to group by. either "customer_type" or "market_segment".
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param a_fun aggregate function to use.
#'
#' @return a react table output.
#' @export
#'
#' @examples avg_daily_rate_cmg(hotel_data, "market_segment", "City Hotel")
#' 
avg_daily_rate_cmg <- function(df, var, f_hotel, a_fun = "sum") {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  fun <- rlang::as_closure(a_fun)
  
  f_tbl <- df |>
    dplyr::group_by(.data[[var]]) |>
    dplyr::summarise(value = fun(avg_daily_rate)) |>
    dplyr::arrange(dplyr::desc(value)) |>
    dplyr::rename(variable = 1)
  
  if (var == "customer_type") {
    cell_pad <- 18
    ele_height <- 20
    
  } else {
    cell_pad <- 8
    ele_height <- NULL
  }
  
  col_name <- clean_label(var)
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = cell_pad),
    showSortable = TRUE,
    
    columns = list(
      variable = reactable::colDef(
        name = col_name,
        sortable = FALSE,
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = rev_colr$colr1,
                                           text_size = ele_height)
      ),
      value = reactable::colDef(
        name = "Average Daily Rate",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color	= rev_colr$colr1,
                                        background = rev_colr$colr10,
                                        number_fmt = scales::label_number(
                                          accuracy = 0.01, 
                                          prefix = "$",
                                          scale_cut = scales::cut_short_scale()
                                        ),
                                        bar_height	= 25)
      )
    )
  )
}



#' Net revenue summary 
#'
#' @param df cleaned hotel revenue data.
#' @param var a character variable to group by. either
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param a_fun aggregate function to use.
#'
#' @return a react table output.
#' @export
#'
#' @examples net_revenue_dtms(hotel_data, "deposit_type")
#' 
net_revenue_dtms <- function(df, var, f_hotel, a_fun = "sum") {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  fun <- rlang::as_closure(a_fun)
  
  f_tbl <- df |>
    dplyr::group_by(.data[[var]]) |>
    dplyr::summarise(value = fun(revenue),
                     net_value = fun(net_revenue)) |>
    dplyr::arrange(dplyr::desc(value)) |>
    dplyr::rename(variable = 1)
  
  if (var == "deposit_type") {
    cell_pad <- 20
    ele_height <- 21
    
  } else {
    cell_pad <- 6
    ele_height <- NULL
  }
  col_name <- clean_label(var)
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = cell_pad),
    showSortable = TRUE,
    
    columns = list(
      variable = reactable::colDef(
        name = col_name,
        sortable = FALSE,
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = rev_colr$colr1,
                                           text_size = ele_height)
      ),
      value = reactable::colDef(
        name = "Revenue",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color	= rev_colr$colr1,
                                        background = rev_colr$colr10,
                                        number_fmt = scales::label_number(
                                          accuracy = 0.01, 
                                          prefix = "$",
                                          scale_cut = scales::cut_short_scale()
                                        ),
                                        bar_height	= 27)
      ),
      net_value = reactable::colDef(
        name = "Net Revenue",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color	= rev_colr$colr1,
                                        background = rev_colr$colr10,
                                        number_fmt = scales::label_number(
                                          accuracy = 0.01, 
                                          prefix = "$",
                                          scale_cut = scales::cut_short_scale()
                                        ),
                                        bar_height	= 25)
      )
    )
  )
}



#' Meal Revenue summary & customer type.
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of graph to return. either 'meal' or 'customer_type'
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return an echarts bar/doughnut plot
#' @export
#'
#' @examples meal_revenue(hotel_data, "meal", "Resort Hotel")
#' 
meal_revenue <- function(df, output_type, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  if (output_type == "meal") {
    df |> 
      dplyr::filter(meal != "Undefined") |>
      dplyr::group_by(meal) |>
      dplyr::summarise(value = sum(meal_cost)) |>
      dplyr::arrange(dplyr::desc(value)) |>
      
      echarts4r::e_charts(x = meal) |>
      echarts4r::e_pie(serie = value,
                       name = "Meal", 
                       legend = FALSE, 
                       radius = c("40%", "80%"),
                       itemStyle = list(borderRadius = 3,
                                        borderColor = '#fff',
                                        borderWidth = 2)) |>
      echarts4r::e_color(color = rev_multi) |>
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS(
        "
            function(params)
            {
                return `<strong>${params.name}</strong>
                        <br/>Total: $${echarts.format.addCommas(params.value)}
                        <br/>Percent: ${params.percent}%`
            }  ")) |>
      echarts4r::e_title(text = "Total Amount Spent On Each Meal Type By Guest",
                         textStyle = list(fontWeight = "lighter"))
    
  } else if (output_type == "customer_type") {
    
    df |>
      dplyr::group_by(customer_type) |>
      dplyr::summarise(value = sum(meal_cost)) |>
      dplyr::arrange(dplyr::desc(value)) |>
      
      echarts4r::e_charts(x = customer_type) |>
      echarts4r::e_bar(serie = value, name = "Meal Cost", legend = FALSE) |>
      echarts4r::e_color(color = rev_colr$colr1) |>
      echarts4r::e_title(text = "Total Amount Spent On Meals By Customer Segment",
                         textStyle = list(fontWeight = "lighter")) |>
      echarts4r::e_y_axis(show = FALSE) |>
      echarts4r::e_x_axis(axisTick = list(show = FALSE), 
                          axisLabel = list(color = "#1F1F1F",
                                           fontSize = 13),
                          type = "category") |>
      echarts4r::e_tooltip(
        formatter  = echarts4r::e_tooltip_item_formatter(style = "currency", digits = 1)
      )
  }
}



#' Discount with agent & distribution channel.
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of output. either "agent" or "distribution channel"
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return an echart bar chart if output_type is "agent" else a highchart tree map.
#' @export
#'
#' @examples discount_ad(hotel_data, "agent")
#' 
discount_ad <- function(df, output_type, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  if (output_type == "distribution_channel") {
    df |>
      dplyr::group_by(distribution_channel) |>
      dplyr::summarise(value = round(mean(discount), 2)) |>
      dplyr::arrange(desc(value)) |>
      
      echarts4r::e_charts(x = distribution_channel) |>
      echarts4r::e_bar(serie = value, legend = FALSE) |>
      echarts4r::e_color(color = rev_colr$colr1) |>
      echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter(style = "currency", digits = 1)) |>
      echarts4r::e_y_axis(show = FALSE) |>
      echarts4r::e_x_axis(axisTick = list(show = FALSE), 
                          axisLabel = list(color = "#1F1F1F",
                                           fontSize = 14),
                          type = "category") |>
      echarts4r::e_title(text = "Average Discount On Bookings By Distribution Channel",
                         textStyle = list(fontWeight = "lighter"))
    
  } else if (output_type == "agent") {
    f_tbl <- df |>
      dplyr::mutate(agent = dplyr::case_when(agent == "NULL" ~ "Undefined",
                                             TRUE ~ agent)) |>
      dplyr::group_by(agent) |>
      dplyr::summarise(value = round(sum(discount), 2),
                       count = dplyr::n()) |>
      dplyr::filter(count > 30) |>
      dplyr::arrange(dplyr::desc(count)) 
    
    
    highcharter::highchart() |>
      highcharter::hc_add_series(data = f_tbl,
                                 type = "treemap",
                                 highcharter::hcaes(x = agent, y = value),
                                 name = "Discount",
                                 color = "#19647E",
                                 tooltip = list(valueDecimals = FALSE)) |>
      
      highcharter::hc_plotOptions(series = list(
        borderWidth = 1,
        borderColor = "#FFFFFF",
        dataLabels = list(
          enabled = TRUE,
          align = "left",
          verticalAlign = "top",
          style = list(
            fontSize = "11px", 
            textOutline = FALSE,
            color = "#FFFFFF",
            fontWeight = "normal")
        )
      )
      ) |>
      highcharter::hc_title(text = "Total Discount On Booking By Agents ID",
                            align = "center")
  }
}



#' Calendar of total number of guest in the hotel for each day.
#'
#' @param df cleaned hotel revenue data.
#' @param f_year year of the summary any of 2018, 2019 or 2020
#'
#' @return an echart calendar chart.
#' @export
#'
#' @examples guest_in_hotel(hotel_data, 2019)
#' 
guest_in_hotel <- function(df, f_year) {
  f_tbl <- df |> 
    dplyr::mutate(n_guest = adults + children + babies) |>
    dplyr::filter(arrival_date_year == f_year) |>
    dplyr::group_by(arrival_date) |>
    dplyr::summarise(n_guest = sum(n_guest))
  
  if (f_year == 2018) {
    height <- 610
    date_range <- c("2018-07-01", "2018-12-31")
    cell_size <- c(70, 20)
    
  } else if (f_year == 2019) {
    height <- 880
    date_range <- 2019
    cell_size <- c(70, 15)
    
  } else if (f_year == 2020) {
    height <- 800
    date_range <- c("2020-01-01", "2020-08-31")
    cell_size <- c(70, 20)
  }
  
  colr <- c(res_colr$colr1, res_colr$colr2, res_colr$colr4) 
  
  f_tbl |>
    echarts4r::e_charts(x = arrival_date, height = height) |>
    echarts4r::e_calendar(range = date_range, 
                          cellSize = cell_size,
                          orient = "vertical",
                          left = "120",
                          splitLine = list(show = TRUE,
                                           lineStyle = list(color = "#FFFFFF",
                                                            width = 7)),
                          itemStyle = list(color = "#F5F5F5") ) |> 
    echarts4r::e_heatmap(n_guest, coord_system = "calendar") |> 
    echarts4r::e_visual_map(serie = n_guest, color = colr) |>
    echarts4r::e_title(text = "Number Of Guest In Hotel",
                       textStyle = list(fontWeight = "lighter"))|>
    echarts4r::e_tooltip(trigger = "item")
}



#' Number Of bookings that falls into each status.
#'
#' @param df cleaned hotel revenue data.
#' @param status reservation status
#' @param f_year year of the summary any of 2018, 2019 or 2020
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a highchart line chart.
#' @export
#'
#' @examples reservation_status_date_count(hotel_data, "Check-out", 2019 )
#' 
reservation_status_date_count <- function(df, status, f_year, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- df |>
    dplyr::filter(reservation_status == status, 
                  lubridate::year(reservation_status_date) == f_year) |>
    dplyr::group_by(reservation_status_date) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::mutate(reservation_status_date = lubridate::as_date(reservation_status_date))
  
  t_t <- stringr::str_glue("Number Of {status} Reservation In {f_year}")
  
  highcharter::highchart() |>
    highcharter::hc_add_series(data = f_tbl,
                               type = "spline",
                               color = res_colr$colr2,
                               highcharter::hcaes(x = reservation_status_date, 
                                                  y = count),
                               name = "Canceled") |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_xAxis(type = "datetime",
                          dateTimeLabelFormats = list(day = "%m %d")) |>
    highcharter::hc_title(text = t_t)
}



#' Deposit Type and customer type.
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a react table output.
#' @export
#'
#' @examples deposit_customer_type(hotel_date, 2019)
#' 
deposit_customer_type <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- dplyr::count(df, deposit_type, customer_type) 
  
  colr <- res_colr |> unlist() |> as.vector() |> rev()
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = 7),
    # height = 500,
    showSortable = TRUE,
    
    columns = list(
      deposit_type = reactable::colDef(
        name = "Deposit Type",
        style = reactablefmtr::group_merge_sort("deposit_type"),
        sortable = FALSE
      ),
      customer_type = reactable::colDef(
        name = "Type of Customer"
      ),
      n = reactable::colDef(
        name = "Count",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color = colr,
                                        fill_gradient = TRUE,
                                        background = 'transparent',
                                        number_fmt = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
    ),
    language = table_style(type = "lang")
  )
}



#' distribution channel
#'
#' @param df  cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a react table output.
#' @export
#'
#' @examples distribution_channel_count(hotel_date, 2018)
#' 
distribution_channel_count <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- chr_count(df, "distribution_channel") 
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = 10),
    
    columns = list(
      distribution_channel = reactable::colDef(
        name = "Distribution Channel",
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = res_colr$colr1)
      ),
      count = reactable::colDef(
        name = "Count",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color = res_colr$colr1,
                                        number_fmt = scales::label_number(0.01, scale_cut = scales::cut_short_scale()),
                                        bar_height = 30)
      ),
      percentage = reactable::colDef(
        name = " ",
        format = reactable::colFormat(suffix = "%", digits = 2)
      )
    )
  )
}



#' distribution channel and  reservation status.
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a react table output.
#' @export
#'
#' @examples distribution_channel_res(hotel_date, 2020, "City Hotel")
#' 
distribution_channel_res <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- df |>
    dplyr::group_by(distribution_channel, reservation_status) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::arrange(distribution_channel, dplyr::desc(n))
  
  colr <- res_colr |> unlist() |> as.vector() |> rev()
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = 7),
    showSortable = TRUE,
    
    columns = list(
      distribution_channel = reactable::colDef(
        name = "Distribution Channel",
        style = reactablefmtr::group_merge_sort("distribution_channel"),
        sortable = FALSE
      ),
      reservation_status = reactable::colDef(
        name = "Reservation Status"
      ),
      n = reactable::colDef(
        name = "Count",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color = colr,
                                        fill_gradient = TRUE,
                                        background = 'transparent',
                                        number_fmt = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
    ),
    language = table_style(type = "lang")
  )
}



#' Distribution & lead time summary
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return react table output.
#' @export
#'
#' @examples distribution_channel_lead_time(hotel_data, "Resort Hotel")
#' 
distribution_channel_lead_time <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- df |>
    dplyr::group_by(distribution_channel) |>
    dplyr::summarise(Minimum = min(lead_time),
                     Average = median(lead_time),
                     Maximum = max(lead_time)) |>
    dplyr::rename(`Distribution Channel` = distribution_channel)
  
  colr <- res_colr |> unlist() |> as.vector() |> rev()
  colr <- colr[2:4]
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = 10),
    showSortable = TRUE,
    
    defaultColDef = reactable::colDef(
      style = reactablefmtr::color_scales(data = f_tbl,
                                          colors = colr)
    ),
    columns = list(
      `Distribution Channel` = reactable::colDef(
        minWidth = 180,
        sortable = FALSE
      )
    ),
    columnGroups = list(
      reactable::colGroup(name = "Lead Time", 
                          columns = c("Minimum", "Average", "Maximum")
      ))
  )
}



#' Guest reserved room & hotel assigned room.
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a react table output.
#' @export
#'
#' @examples reserved_assigned_room(hotel_data, "Resort Hotel)
#' 
reserved_assigned_room <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- df |>
    dplyr::count(reserved_room_type, name = "reserved_room") |>
    dplyr::add_row(dplyr::tibble(reserved_room_type = c("I", "K"), 
                                 reserved_room = c(0, 0))) |>
    
    dplyr::inner_join(dplyr::count(hr, assigned_room_type, name = "assigned_room"), 
                      by = c("reserved_room_type" = "assigned_room_type")) |>
    
    dplyr::inner_join(dplyr::filter(df, reserved_room_type != assigned_room_type) |>
                        dplyr::count(assigned_room_type, name = "assigned_different_room") |> 
                        dplyr::add_row(dplyr::tibble(assigned_room_type = c("P", "L"), 
                                                     assigned_different_room = c(0, 0))),
                      by = c("reserved_room_type" = "assigned_room_type")) |>
    
    dplyr::rename(room_type = reserved_room_type) |>
    dplyr::rename_with(clean_label) |>
    dplyr::arrange(dplyr::desc(`Assigned Room`))
  
  
  reassigned_percent <- df |>
    dplyr::filter(reserved_room_type != assigned_room_type) |>
    dplyr::count() |>
    dplyr::summarise(percent_reassigned = (n / nrow(hr))*100) |>
    dplyr::pull() |> round(2)
  
  s_t <- stringr::str_glue("{reassigned_percent}% Of Reserved Rooms Where Reassigned")
  
  colr <- res_colr |> unlist() |> as.vector() |> rev()
  colr <- colr[2:4]
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme"),
    showSortable = TRUE, 
    
    columns = list(
      `Room Type` = reactable::colDef(
        cell = reactablefmtr::pill_buttons(data = f_tbl,
                                           colors = res_colr$colr1)
      ),
      `Reserved Room` = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color = res_colr$colr1,
                                        number_fmt = scales::label_number(scale_cut = scales::cut_short_scale()))
      ),
      `Assigned Room` = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color = res_colr$colr1,
                                        number_fmt = scales::label_number(scale_cut = scales::cut_short_scale()))
      ),
      `Assigned Different Room` = reactable::colDef(
        cell = reactablefmtr::color_tiles(data = f_tbl,
                                          colors = colr,
                                          number_fmt = scales::label_number(scale_cut = scales::cut_short_scale())))
    ),
    columnGroups = list(
      reactable::colGroup(name = "Count", columns = c("Reserved Room", "Assigned Room",
                                                      "Assigned Different Room"))
    ),
    language = table_style(type = "lang", info_text = "Types Of Room")
  ) |>
    reactablefmtr::add_title(title = "Reserved vs Assigned Room Type", 
                             font_size = 20, font_color = "#737373") |>
    reactablefmtr::add_subtitle(subtitle = s_t,
                                font_size = 11, font_color = "#ADADAD")
}



#' Guest lead channels
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a reactable output
#' @export
#'
#' @examples channels(hotel_data, "City Hotel")
#' 
channels <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_lst <- lapply(c("distribution_channel", "agent", "company"),
                  \(.x) dplyr::n_distinct(hr[[.x]])) 
  
  names(f_lst) <- c("distribution_channel", "agent", "company")
  
  f_tbl <- dplyr::as_tibble(f_lst) |> 
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::mutate(
      label = dplyr::case_when(name == "distribution_channel" ~ "Distribution Channels.",
                               name == "agent" ~ "Unique Agencies.",
                               name == "company" ~ "Partner Companies.")
    ) |>
    dplyr::select(value, label) 
  
  reactable::reactable(
    data = f_tbl,
    theme = reactablefmtr::void(font_size = 20, 
                                font_color = "#ADADAD", 
                                cell_padding = 16),
    
    columns = list(
      value = reactable::colDef(
        cell = reactablefmtr::pill_buttons(data = f_tbl, colors = "#F7F7F7")
      ),
      label = reactable::colDef(
        cell = reactablefmtr::pill_buttons(data = f_tbl, colors = "#F7F7F7")
      )
    )
  )
}



#' Changed Booking
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return an echart doughnut chart.
#' @export
#'
#' @examples changed_bookings(hotel_data, "City Hotel")
#' 
changed_bookings <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  df |>
    dplyr::mutate(book_changed = dplyr::if_else(booking_changes > 0, "Changed", "No Changes")) |>
    dplyr::count(book_changed, name = "count") |>
    
    echarts4r::e_charts(x = book_changed) |>
    echarts4r::e_pie(serie = count, 
                     legend = FALSE, 
                     radius = c("40%", "80%"),
                     itemStyle = list(borderRadius = 3,
                                      borderColor = '#fff',
                                      borderWidth = 2)) |>
    echarts4r::e_color(color = c("#00F2F2", "#096B72")) |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS(
      "
            function(params)
            {
                return `<strong>${params.name}</strong>
                        <br/>Total: ${echarts.format.addCommas(params.value)}
                        <br/>Percent: ${params.percent}%`
            }  ")) |>
    echarts4r::e_title(text = "Percentage Of Bookings With At Least 1 Changes",
                       textStyle = list(color = "#7F7F7F",
                                        fontWeight = "lighter"))
}



#' Agency performance by previous booking cancellation.
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param n number of agents to show on the chart.
#'
#' @return an echart stacked bar chart.
#' @export
#'
#' @examples agent_previous_cancellation(hotel_data, "City Hotel" n = 5)
#' 
agent_previous_cancellation <- function(df, f_hotel, n = 10) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  us_agent <- df |>
    dplyr::count(agent) |>
    dplyr::filter(n > 30) |> 
    dplyr::pull(agent)
  
  s_t <- stringr::str_glue("By Top {n} Agents")
  
  df |>
    dplyr::filter(agent %in% us_agent) |>
    dplyr::group_by(agent) |>
    dplyr::summarise(dplyr::across(c(previous_cancellations, previous_bookings_not_canceled), 
                                   sum)) |>
    dplyr::rowwise() |>
    dplyr::mutate(total_bk = sum(dplyr::c_across(c(previous_cancellations, previous_bookings_not_canceled)))) |>
    dplyr::mutate(previous_cancellations_per = div_cancellations(previous_cancellations, total_bk),
                  previous_booking_not_canceled_per = div_cancellations(previous_bookings_not_canceled, total_bk),
                  agent = dplyr::if_else(agent == "NULL", "Undefined", agent)) |>
    dplyr::select(agent, 
                  canceled = previous_cancellations_per, 
                  not_canceled = previous_booking_not_canceled_per) |>
    dplyr::filter(canceled != 0 & not_canceled != 0) |>
    dplyr::arrange(dplyr::desc(not_canceled)) |>
    
    head(n) |>
    
    echarts4r::e_charts(x = agent) |>
    echarts4r::e_bar(serie = not_canceled, name = "Not Canceled", stack = "group") |>
    echarts4r::e_bar(serie = canceled, name = "Canceled", stack = "group") |>
    echarts4r::e_tooltip(trigger = "axis") |>
    echarts4r::e_legend(right = 4) |>
    echarts4r::e_color(color = c(res_colr$colr2, "#D7C9AA")) |>
    echarts4r::e_title(text = "Percentage Of Previous Bookings",
                       subtext = s_t,
                       textStyle = list(color = "#7F7F7F",
                                        fontWeight = "normal"),
                       subTextStyle = list(color = "#BDBDBD",
                                           fontWeight = "normal",
                                           fontSize = 13)) |>
    echarts4r::e_y_axis(axisLabel = list(formatter = "{value}%")) |>
    echarts4r::e_axis_labels(x = "Agent ID")
}



#' Guest nationality summary.
#'
#' @param df cleaned hotel revenue data.
#' @param output_type the type of output. either "count" or "lead_time"
#' @param f_hotel if supplied, summary will be done for only that hotel.
#' @param n number of countries to include on the charts.
#'
#' @return if output_type is "count" a highchart column chart else a react table output.
#' @export
#'
#' @examples nationality_summary(hotel_data, "count", n = 10)
#' 
nationality_summary <- function(df, output_type, f_hotel, n = 15) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  if (output_type == "count") {
    f_tbl <- dplyr::count(df, nationality, sort = TRUE, name = "count") |>
      head(n)
    
    t_t <- stringr::str_glue("Count Of Top {n} Nationals By Booking Frequency")
    
    highcharter::highchart() |>
      highcharter::hc_add_series(data = f_tbl, 
                                 type = "bar",
                                 color = gst_colr$colr1,
                                 highcharter::hcaes(x = nationality, y = count),
                                 name = "Guest") |>
      highcharter::hc_xAxis(categories = f_tbl$nationality) |>
      highcharter::hc_legend(enabled = FALSE) |>
      
      highcharter::hc_title(text = t_t, 
                            align = "left")
    
  } else if (output_type == "lead_time") {
    f_tbl <- df |>
      dplyr::group_by(nationality) |>
      dplyr::summarise(count = dplyr::n(), value = mean(lead_time)) |>
      dplyr::filter(count > 10) |>
      dplyr::arrange(dplyr::desc(value)) 
    
    reactable::reactable(
      data = f_tbl,
      theme = table_style(type = "theme"),
      showSortable = TRUE,
      
      columns = list(
        nationality = reactable::colDef(name = "Country"),
        count = reactable::colDef(show = FALSE),
        
        value = reactable::colDef(
          name = "Average Lead Time",
          cell = reactablefmtr::data_bars(data = f_tbl,
                                          text_position = "outside-base",
                                          fill_color = gst_colr$colr1,
                                          number_fmt = scales::label_number(0.01))
        )
      ),
      language = table_style(type = "lang", info_text = "Countries")
    )
  }
}



continent_summary <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- hr |>
    dplyr::select(nationality) |>
    dplyr::inner_join(get_continent(), by = c("nationality" = "country")) |>
    dplyr::count(continent, sort = TRUE, name = "count") |>
    dplyr::mutate(percentage = paste0(round(proportions(count)*100, 2),"%"))
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = 8),
    
    columns = list(
      continent = reactable::colDef(
        name = "Continent",
        cell = reactablefmtr::pill_buttons(data = f_tbl, colors = gst_colr$colr1)
      ),
      
      count = reactable::colDef(
        name = "Number Of Bookings",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color = gst_colr$colr1,
                                        number_fmt = scales::label_number(
                                          accuracy = 0.1,
                                          scale_cut = scales::cut_short_scale()
                                        ))
      ),
      percentage = reactable::colDef(
        name = "",
        style = reactablefmtr::cell_style(data = f_tbl,
                                          rows = 1,
                                          border_style = "solid",
                                          border_color = "#F7F7F7",
                                          background_color	= gst_colr$colr4)
      )
    )
  )
}



#' The total cost of each meal 
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return an echart doughnut chart.
#' @export
#'
#' @examples meal_count(hotel_data, "Resort Hotel")
#' 
meal_count <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  dplyr::count(df, meal, name = "count", sort = TRUE) |>
    
    echarts4r::e_charts(x = meal) |>
    echarts4r::e_pie(serie = count, "Purchased",
                     legend = FALSE, 
                     radius = c("40%", "75%"),
                     itemStyle = list(borderRadius = 3,
                                      borderColor = '#fff',
                                      borderWidth = 2)) |>
    echarts4r::e_color(color = gst_multi) |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS(
      "
            function(params)
            {
                return `<strong>${params.name}</strong>
                        <br/>Total: ${echarts.format.addCommas(params.value)}
                        <br/>Percent: ${params.percent}%`
            }  ")) |>
    echarts4r::e_title(text = "Number Of Times Each Meal Was Purchased",
                       textStyle = list(fontWeight = "lighter"))
}



#' Market segment and reserved rooms
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a highchart tree map.
#' @export
#'
#' @examples market_segment_reserved_room_type(hotel_data, "City Hotel")
#' 
market_segment_reserved_room_type <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- dplyr::count(df, market_segment, reserved_room_type) |>
    dplyr::mutate(reserved_room_type = paste(market_segment, 
                                             "-", 
                                             reserved_room_type))
  
  lvl_opts <-  list(
    list(level = 1,
         borderWidth = 0,
         borderColor = "transparent",
         dataLabels = list(enabled = TRUE,
                           align = "left",
                           verticalAlign = "top",
                           style = list(fontSize = "12px", 
                                        textOutline = FALSE,
                                        color = "white",
                                        fontWeight = "normal")
         )
    ),
    list(level = 2,
         borderWidth = 0,
         borderColor = "transparent",
         colorVariation = list(key = "brightness", to = 0.250),
         dataLabels = list(enabled = FALSE),
         style = list(fontSize = "8px",
                      textOutline = FALSE, 
                      color = "white", 
                      fontWeight = "normal")
    )
  )
  
  colr <- c(gst_multi, "#461E5C", "#F56A00")
  
  highcharter::hchart(
    highcharter::data_to_hierarchical(data = f_tbl, 
                                      group_vars = c(market_segment, reserved_room_type),
                                      size_var = n,
                                      colors = colr),
    type = "treemap",
    allowDrillToNode = TRUE,
    levels = lvl_opts,
    tooltip = list(valueDecimals = FALSE)
  ) |>
    highcharter::hc_title(text = "Reserved Room Type By Guest In Each Market Segment")
}



#' Type of customer and reservation status.
#'
#' @param df cleaned hotel revenue data.
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a react table output.
#' @export
#'
#' @examples customer_type_reservation_status_count(hotel_data, "City Hotel")
#' 
customer_type_reservation_status_count <- function(df, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- df |>
    dplyr::group_by(customer_type, reservation_status) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::arrange(customer_type, dplyr::desc(count))
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme"),
    showSortable = TRUE,
    
    columns = list(
      customer_type = reactable::colDef(
        name = "Customer Type",
        style = reactablefmtr::group_merge_sort("customer_type"),
        sortable = FALSE
      ),
      reservation_status = reactable::colDef(name = "Reservation Status"),
      count = reactable::colDef(
        name = "Number Of Bookings",
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "outside-end",
                                        fill_color = gst_colr$colr1,
                                        number_fmt = scales::label_number(scale_cut = scales::cut_short_scale()))
      )
    ),
    language = table_style(type = "lang", info_text = "Type of Customer")
  )
  
}



#' Guest stay during a particular period.
#'
#' @param df cleaned hotel revenue data.
#' @param f_year year of the summary any of 2018, 2019 or 2020
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return a highchart multiple line chart.
#' @export
#'
#' @examples stays_arrival_date(hotel_data, 2018)
#' 
stays_arrival_date <- function(df, f_year, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  t_t <- stringr::str_glue("Total Guest Room Stay In {f_year}")
  
  f_tbl <- df |>
    dplyr::filter(arrival_date_year == f_year) |>
    dplyr::group_by(arrival_date) |>
    dplyr::summarise(across(c(stays_in_week_nights, stays_in_weekend_nights), sum)) 
  
  
  highcharter::highchart() |>
    highcharter::hc_add_series(data = f_tbl,
                               type = "spline",
                               color = gst_colr$colr2,
                               highcharter::hcaes(x = arrival_date, 
                                                  y = stays_in_week_nights),
                               name = "Weekdays") |>
    highcharter::hc_add_series(data = f_tbl,
                               type = "spline",
                               color = rev_colr$colr1,
                               highcharter::hcaes(x = arrival_date, 
                                                  y = stays_in_weekend_nights),
                               name = "Weekends") |>
    highcharter::hc_xAxis(type = "datetime",
                          dateTimeLabelFormats = list(day = "%m %d")) |>
    highcharter::hc_title(text =  t_t, align = "left")
}



#' Customer type & guest profile.
#'
#' @param df cleaned hotel revenue data.
#' @param f_year year of the summary any of 2018, 2019 or 2020
#' @param f_hotel if supplied, summary will be done for only that hotel.
#'
#' @return react table output.
#' @export
#'
#' @examples market_segment_guest(hotel_data, 2018, "Resort Hotel")
#' 
market_segment_guest <- function(df, f_year, f_hotel) {
  if (!missing(f_hotel)) {
    df <- dplyr::filter(df, hotel == f_hotel)
  }
  
  f_tbl <- hr |>
    dplyr::filter(arrival_date_year == f_year) |>
    dplyr::mutate(children = children + babies) |>
    dplyr::group_by(customer_type, arrival_date_month) |>
    dplyr::summarise(dplyr::across(c(adults, children), sum)) |>
    dplyr::select(-arrival_date_month) |>
    dplyr::summarise(dplyr::across(tidyselect:::where(is.numeric), list)) |>
    dplyr::rename_with(clean_label)  
  
  if (f_year == 2018) {
    duration <- c("July", "December")
    
  } else if (f_year == 2019) {
    duration <- c("January", "December")
    
  } else if (f_year == 2020) {
    duration <- c("January", "August")
  }
  
  t_t <- stringr::str_glue("Number Of Adults & Children By Month In {f_year}")
  s_t <- stringr::str_glue("From {duration[1]} to {duration[2]}")
  
  l_colr <- gst_colr$colr1
  a_colr <- gst_colr$colr2
  min_colr <- "#FB9649"
  max_colr <- "#9D53FF"
  
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme"),
    compact = TRUE,
    sortable = FALSE,
    
    columns = list(
      `Customer Type` = reactable::colDef(
        cell = reactablefmtr::pill_buttons(data = f_tbl, colors = gst_colr$colr1)
      ),
      Adults = reactable::colDef(
        minWidth = 380,
        cell = reactablefmtr::react_sparkline(data = f_tbl,
                                              height = 100,
                                              line_width = 1.5,
                                              show_area = TRUE,
                                              line_color = l_colr,
                                              area_color = a_colr,
                                              labels = c('min','max'),
                                              label_size = '0.9em',
                                              highlight_points = reactablefmtr::highlight_points(min = min_colr, 
                                                                                                 max = max_colr),
                                              margin = reactablefmtr::margin(t=15,r=18,b=15,l=18),
                                              tooltip_type = 2)
      ),
      Children =  reactable::colDef(
        minWidth = 380,
        cell = reactablefmtr::react_sparkline(data = f_tbl,
                                              height = 100,
                                              line_width = 1.5,
                                              show_area = TRUE,
                                              line_color = l_colr,
                                              area_color = a_colr,
                                              bandline_opacity = 0.6,
                                              labels = c('min','max'),
                                              label_size = '0.9em',
                                              highlight_points = reactablefmtr::highlight_points(min = min_colr, 
                                                                                                 max = max_colr),
                                              margin = reactablefmtr::margin(t=15,r=10,b=15,l=10),
                                              tooltip_type = 2)
      )
    )
  ) |>
    reactablefmtr::add_title(title = t_t, font_size = 20, font_color = "#888888") |>
    reactablefmtr::add_subtitle(subtitle = s_t, font_size = 14, font_color = "#A3A3A3")
}



#' division with zero
#'
#' @param var variable to divide.  
#' @param total_var variable to divided by.
#'
#' @return vector.
#' @export
#'
#' @examples div_cancellations(df, numeric_varible, total_variable)
#' 
div_cancellations <- function(var, total_var) {
  if (total_var != 0) {
    round((var / total_var) * 100, 2)
    
  } else {
    0
  }
}