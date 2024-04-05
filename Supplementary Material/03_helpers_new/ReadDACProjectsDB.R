ReadDACProjectsDB <- function(file.dacprojects,
                              dacprojects.range,
                              capacity.factor) { 
  # Column names
  columns <- c(
    "Column1" = "reference",
    "Column2" = "name",
    "Column3" = "country",
    "Column4" = "date.online",
    "Column5" = "date.decommissioned",
    "Column6" = "status",
    "Column7" = "technology",
    "Column8" = "tech.comments",
    "Column9" = "product",
    "Column10" = "size.announced",
    "Column11" = "cap.tCO2y",
    "Column12" = "cap.efftCO2y",
    "Column13" = "cap.eff_MtCO2y",
    "Column14" = "source"
  )
  # Country
  region.mapping <- c(
    "CHE" = "Switzerland",
    "ISL" = "Iceland",
    "NOR" = "Norway",
    "DEU" = "Germany",
    "GBR" = "United Kingdom",
    "NLD" = "Netherlands",
    "ITA" = "Italy",
    "ISR" = "MENA",
    "AUS" = "Australia",
    "CAN" = "Canada",  
    "CHL" = "South America",  
    "OMN" = "MENA",  
    "USA" = "United States",  
    "USA/CAN" = "United States/Canada",
    "Unspecified" = "Unspecified",
    
    NULL
  )
  
  # Read StateOfCDR file
  data.dac.projects <-
    read_excel(
      file.dacprojects,
      sheet = "Projects (optimistic)",
      range = dacprojects.range,
      col_names = columns
    ) %>%
    # Select columns
    select(c(
      reference,
      name,
      country,
      date.online,
      date.decommissioned,
      status,
      technology,
      cap.eff_MtCO2y
    )) %>%
    # Map regions
    revalue.levels(country = region.mapping) %>%
    rename(region = country) %>%
    # Correct misspelling
    revalue.levels(status = c("Decommisioned" = "Decommissioned")) %>%
    # Drop if no capacity in MtCO2 given
    filter(!is.na(cap.eff_MtCO2y))

  `%ni%` <- Negate(`%in%`)
  
  
  # Continue data processing projects
  data.dac.projects <- data.dac.projects %>%
    # Drop if no online date given
    filter(!is.na(date.online)) %>%
    # Map all NA regions to Other
    mutate(region = case_when(is.na(region) ~ "Other",
                              TRUE ~ region)) %>%
     #If status == Proof of Technology and there is no decommissioned date -> Operational
     #If status == Proof of Technology and there is a decommissioned date -> Decommissioned
    mutate(status = case_when((status == "Proof of Technology" &
                                 is.na(date.decommissioned)) ~ "Operational",
                              (status == "Proof of Technology" &
                                 !is.na(date.decommissioned)) ~ "Decommissioned",
                              TRUE ~ status
    )) %>%
    # Create two rows for each project, one for online, one for decommissioned
    pivot_longer(
      cols = c("date.online", "date.decommissioned"),
      names_to = "type",
      names_prefix = "date.",
      values_to = "year"
    ) %>%
    # Filter rows that do not have a decomissioned date
    filter(!(type == "decommissioned" & is.na(year))) %>%
    # If status == Decommissioned and type == online -> Operational
    mutate(status = case_when((status == "Decommissioned" &
                                 type == "online") ~ "Operational",
                              TRUE ~ status
    )) %>%
    
    # Negative capacity for decommissioned
    mutate(cap.eff_MtCO2y = case_when(status == "Decommissioned" ~ -cap.eff_MtCO2y,
                                TRUE ~ cap.eff_MtCO2y)) %>%
    # Select and reorder
    select(c(name, region, status, year, cap.eff_MtCO2y)) %>%
    rename(capacity = cap.eff_MtCO2y)
    
  # Calculate relevant statistics of project database
  data.dac <- data.dac.projects %>%
    # Transform to MW to Watt and MtCO2 to tCO2
    mutate(capacity = capacity * 1E6) %>%
    # Add factor for social behavior push 
    mutate(capacity_new = capacity * capacity.factor) %>%
    # Capacity, mean and count
    group_by(region, status, year) %>%
    summarise(
      cap.sum = sum(capacity_new),
      cap.mean = mean(capacity_new),
      nprojects = n()
    ) %>%
    ungroup() %>%
    # Complete dataset
    complete(region,
             status,
             year = 2005:2050,
             fill = list(
               cap.sum = 0,
               cap.mean = 0,
               nprojects = 0
             )) %>%
    # Cumulative capacity (over years)
    group_by(region, status) %>%
    arrange(year) %>%
    mutate(cumcap.sum = cumsum(cap.sum)) %>%
    ungroup() %>% 
    order.levels(
      status = c(
        "Planned/Announced",
        "Feasibility Study",
        "Proof of Technology", #will not be visible because of line 122 & 123
        "Design and Engineering Phase",
        "FID",
        "Under Construction",
        "Operational",
        "Decommissioned"
      )
    )
  
  return(data.dac)
}

