#' Description Card
#'
#' @param first_text description of header text
#' @param first_value value of header
#' @param second_text description of sub text
#' @param second_value value of sub text
#' @param third_text description of sub text
#' @param third_value value of sub text
#' @param icon header icon
#' @param color background color of card
#' @param right_border change right boarder color
#' @param left_border change left boarder color
#'
#' @return an html tools div 
#' @export
#'
#' @examples
description_card <- function(first_text, first_value, 
                             second_text = NULL, second_value = NULL,
                             third_text = NULL, third_value = NULL,
                             icon = NULL, color = NULL, 
                             right_border = FALSE,
                             left_border = FALSE) {
  if (!is.null(color)) {
    colr <- paste0("colr-", color)
    
    if (color == "black") {
      class_first_value <- "first-value-w"
      
    } else {
      class_first_value <- "first-value-b"
    }
    
  } else {
    colr <- NULL
    class_first_value <- "first-value-w"
  } 
  
  
  if (right_border) {
    class_border <- "border-r"
    
  } else if (left_border) {
    class_border <- "border-l"
    
  } else {
    class_border <- "border-full"
  }
  
  
  htmltools::tags$div(
    class = paste("description-block", class_border, colr),
    
    htmltools::tags$div(
      class = "pad",
      
      htmltools::tags$div(
        class = "first-text",
        if (!is.null(icon)) icon,
        first_text,
      ),
      htmltools::tags$div(
        class = class_first_value,
        first_value
      ),
      
      htmltools::tags$div(
        class = "two-col-div",
        
        htmltools::tags$label(
          class = "second-text",
          second_text,
          
          htmltools::tags$br(),
          
          htmltools::tags$span(
            class = "second-value",
            second_value
          )
        ),
        
        htmltools::tags$label(
          class = "second-text",
          third_text,
          
          htmltools::tags$br(),
          
          htmltools::tags$span(
            class = "second-value",
            third_value
          )
        )
      )
    )
  )
}


#' Title
#'
#' @param ... description cards.
#' @param class css class of the div.
#'
#' @return an html tools div 
#' @export
#'
#' @examples
cus_div <- function(..., class = NULL) {
  f_class <- if (is.null(class)) "" else class
  f_class <- paste("description-div", f_class)
  htmltools::tags$div(
    class = f_class,
    ...
  )
}




#' Revenue Description block values
#'
#' @return description block
#' @export
#'
#' @examples
revenue_box <- function(net_rev_first, net_rev_second, net_rev_third,
                        adr_first, adr_second, adr_third,
                        discount_first, discount_second, discount_third,
                        meal_first, meal_second, meal_third) {
  cus_div(
    description_card(
      first_text = "Net Revenue",
      first_value = net_rev_first,
      icon = fa_i(name = "fas fa-sack-dollar"),
      second_text = "% of Revenue",
      second_value = net_rev_second,
      third_text = "Average",
      third_value = net_rev_third,
      color = "black"
    ),
    description_card(
      first_text = "Avg. Daliy Rate",
      first_value = adr_first,
      icon = fa_i(name = "fas fa-hand-holding-dollar"),
      second_text = "Minimum",
      second_value = adr_second,
      third_text = "Average",
      third_value = adr_third,
      color = "black"
    ),
    description_card(
      first_text = "Discount",
      first_value = discount_first,
      icon = fa_i(name = "fas fa-file-invoice-dollar"),
      second_text = "Average",
      second_value = discount_second,
      third_text = "Maximum",
      third_value = discount_third,
      color = "black"
    ),
    description_card(
      first_text = "Meal",
      first_value = meal_first,
      icon = fa_i(name = "fas fa-receipt"),
      second_text = "% of Revenue",
      second_value = meal_second,
      third_text = "Maximum",
      third_value = meal_third,
      color = "black"
    )
  )
}



#' Reservation Description block values.
#'
#' @return description block
#' @export
#'
#' @examples
reservation_box <- function(check_out, canceled, no_show,
                            avg_lead_time, max_lead_time, max_wait_list,
                            chg_booking, no_changes, percent_change,
                            avg_stay, min_stay, max_stay) {
  cus_div(
    description_card(
      first_text = "Check-Out",
      first_value = check_out,
      icon = fa_i(name = "fas fa-calendar-check"),
      second_text = "Canceled",
      second_value = canceled,
      third_text = "No Show",
      third_value = no_show,
      color = "green",
      right_border = TRUE
    ),
    description_card(
      first_text = "Avg. Lead Time",
      first_value = avg_lead_time,
      icon = fa_i(name = "fas fa-clock-rotate-left"),
      second_text = "Max Lead Time",
      second_value = max_lead_time,
      third_text = "Max days in waiting list",
      third_value = max_wait_list,
      color = "green",
      left_border = TRUE
    ),
    description_card(
      first_text = "Change Booking",
      first_value = chg_booking,
      icon = fa_i(name = "fas fa-calendar-plus"),
      second_text = "No Changes",
      second_value = no_changes,
      third_text = "% Changed",
      third_value = percent_change,
      color = "green",
      right_border = TRUE
    ),
    description_card(
      first_text = "Average Stay",
      first_value = avg_stay,
      icon = fa_i(name = "fas fa-user-clock"),
      second_text = "Minimum",
      second_value = min_stay,
      third_text = "Maximum",
      third_value = max_stay,
      color = "green",
      left_border = TRUE
    )
  )
}



#' Guest Description block values.
#'
#' @return description block
#' @export
#'
#' @examples
guest_box <- function(total_guest, adult, children, 
                      rep_guest, no_repeat, percent_repeat,
                      total_national, continent, top_country,
                      special_req, max_req, percent_guest) {
  cus_div(
    description_card(
      first_text = "Total Guest",
      first_value = total_guest,
      icon = fa_i(name = "fas fa-users-between-lines"),
      second_text = "Adult",
      second_value = adult,
      third_text = "Children",
      third_value = children,
      color = "yellow",
      right_border = TRUE
    ),
    description_card(
      first_text = "Repeated Guest",
      first_value = rep_guest,
      icon = fa_i(name = "fas fa-arrow-rotate-right"),
      second_text = "No Repeat",
      second_value = no_repeat,
      third_text = "% Repeated",
      third_value = percent_repeat,
      color = "yellow",
      left_border = TRUE
    ),
    description_card(
      first_text = "Total Nationals",
      first_value = total_national,
      icon = fa_i(name = "far fa-flag"),
      second_text = "Continent",
      second_value = continent,
      third_text = "Top Country",
      third_value = top_country,
      color = "yellow",
      right_border = TRUE
    ),
    description_card(
      first_text = "Special Request",
      first_value = special_req,
      icon = fa_i(name = "far fa-bell"),
      second_text = "Maximum",
      second_value = max_req,
      third_text = "% Guest",
      third_value = percent_guest,
      color = "yellow",
      left_border = TRUE
    )
  )
}



#' Ui tab set panel values.
#'
#' @return graphical objects within a tab set panel.
#' @export
#'
#' @examples
tabset_panel_output <- function(graph_1, graph_2, graph_3) {
  shiny::tabsetPanel(
    shiny::tabPanel(
      "2020",
      htmltools::tags$figure(graph_1)
    ),
    shiny::tabPanel(
      "2019",
      htmltools::tags$figure(graph_2)
    ),
    shiny::tabPanel(
      "2018",
      htmltools::tags$figure(graph_3)
    )
  )
}