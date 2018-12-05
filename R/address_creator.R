
library(data.table)
library(plotly)
library(magrittr)

days_stayed <- function(adrs, ids) {
  adrs[ids, min(From_date)]:adrs[ids,max(To_date)]
}


# lived_together(adrs, 'IND0001', 'IND0007') # TRUE
lived_together <- function(adrs, list1, list2) {
  if (length(intersect(days_stayed(adrs, list1), days_stayed(adrs, list2))) > 0) {
    lived <- TRUE
  } else {
    lived <- FALSE
  }
  return(lived)
}


test <- function() {
  lived_together(adrs, 'IND0001', 'IND0007') == TRUE
  lived_together(adrs, 'IND0001', 'IND0003') == FALSE
  lived_together(adrs, 'IND0001', c('IND0091', 'IND0007')) == TRUE
}


# For data.table containing only one Address calculates the groups of individuals who lived there together (even if through proxy)
groups_at_address <- function(addresses) {
  if (length(unique(addresses[,Address_ID]))>1) {
    stop("This data.table should contain only records from one address.")
  }
  setkeyv(addresses, 'Customer_ID')
  addresses[, Group := -1]
  # we will calculate the group for each individual
  
  for (i in 1:nrow(addresses)) {
    # we will take the first one that is not included in any group
    
    to_be_included <- addresses[i, Customer_ID]
    # For first record we create the first group
    if (i == 1) {
      addresses[to_be_included, Group := 1]  
    } else {
      groups <- addresses[Group > -1, unique(Group)]
      to_be_included_into <- c()
      for (grp in groups) {
        # for every group already created, we check whether the new one is belonging to them. 
        if (lived_together(addresses, addresses[Group == grp, Customer_ID], to_be_included)) {
          to_be_included_into <- c(to_be_included_into, grp)
        } else if (grp == max(groups) & length(to_be_included_into) == 0) {
          # if he does not belong to any group, we create new one
          to_be_included_into <- grp + 1
        }
      }
      # to_be_included_into can have one or more groups.
      # if it has more, we merge the groups into one (first one)
      addresses[Customer_ID == to_be_included | Group %in% to_be_included_into, Group := to_be_included_into[1]]
    }
  }
  # re-enumerate the groups 3,6,8 -> 1,2,3
  # It holds that (i <= groups[i])
  groups <- addresses[,unique(Group)]
  for (i in 1:length(groups)) {
    addresses[Group == groups[i], Group := i]
  }
  return(addresses)
}


# Returns the data with the calculated groups
get_data_with_groups <- function(addresses) {
  groups_by_addresses_list <- lapply( addresses[, unique(Address_ID)], function(adr_id) {
    groups_at_address(addresses[Address_ID == adr_id,])[order(From_date),]
  })
  out_sorted <- rbindlist(groups_by_addresses_list)
}


# Returns the Address_ID, Group, its From and To dates
get_address_groups <- function(addresses) {
  if (!'Group' %in% names(addresses)) {
    dt <- get_data_with_groups(addresses)
  } else {
    dt <- addresses
  }
  
  dt <- dt[,list(From_date = min(From_date), To_date = max(To_date), Count = .N), 
     by = list(Address_ID, Group)][order(Group),][order(Address_ID),]
  dt[, Duration := To_date - From_date]
  return(dt)
}


main_draw <- function() {
  addr_dt <- fread('data/address_data.csv')
  
  addr_w_groups <- get_data_with_groups(addr_dt)
  
  # get the group counts; 
  groups <- get_address_groups(addr_w_groups)
  
  addr_w_groups[,.N,by=list(Address_ID,Group)]
  
  p <- plot_ly()
  inds <- addr_w_groups[,Customer_ID]
  plot_colors <- c('green', 'blue', 'red', 'yellow', 'orange', 'black', 'brown', 'grey', 'pink', 'violet')
  addr_w_groups[,.N,Group]
  legend_drawn <- c()
  for (i in 1:length(inds)) {
    to_date <- addr_w_groups[Customer_ID == inds[i],To_date]
    from_date <- addr_w_groups[Customer_ID == inds[i],From_date]
    group <- addr_w_groups[Customer_ID == inds[i],Group]
    show_legend <- !(group %in% legend_drawn)
    legend_drawn <- unique(legend_drawn, group)
    address <- addr_w_groups[Customer_ID == inds[i], Address_ID]
    p <- p %>% add_trace(addr_w_groups[Customer_ID == inds[i],], y = i, 
                         x = c(from_date, to_date),
                         type = 'scatter', mode = 'lines', name = paste(c(address, '_GRP', sprintf('%02i', group)), collapse = ''), 
                         color = plot_colors[group],
                         showlegend = FALSE)
  }
  p
  htmlwidgets::saveWidget(as_widget(p), "visualized_groups.html")
}





