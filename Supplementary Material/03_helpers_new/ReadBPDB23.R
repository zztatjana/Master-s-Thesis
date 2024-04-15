ReadBPDB23 <- function(file.bp) {
  
  region.mapping.bp <- c(
    "Total Europe" = "Europe",
    "Total North America" = "North America",
    "Total World" = "Global",
    NULL
  )
  
  # Read in solar sheet
  data.bp.solar <-
    read_excel(
      path = file.bp,
      sheet = "Solar Capacity",
      range = "A4:AB72",
      col_names = TRUE
    ) %>%
    filter(if_any(everything(), ~ !is.na(.))) %>%
    pivot_longer(cols = `1996`:`2022`,
                 names_to = "year",
                 values_to = "capacity") %>%
    rename(region = Megawatts) %>% 
    mutate(technology = "solar") %>%
    # Delete total values except total world
    filter(!region %in% c("Total S. & Cent. America",
                          "Total CIS",
                          "Total Middle East",
                          "Total Africa",
                          "Total Asia Pacific"))
  
  # Read in wind sheet
  data.bp.wind <-
    read_excel(
      path = file.bp,
      sheet = "Wind Capacity",
      range = "A4:AA70",
      col_names = TRUE
    ) %>%
    filter(if_any(everything(), ~ !is.na(.))) %>%
    pivot_longer(cols = `1997`:`2022`,
                 names_to = "year",
                 values_to = "capacity") %>%
    rename(region = Megawatts) %>% #aka MtCO2
    mutate(technology = "wind") %>%
    # Delete total values (use regional sub-values instead)
    filter(!region %in% c("Total S. & Cent. America",
                          "Total CIS",
                          "Total Middle East",
                          "Total Africa",
                          "Total Asia Pacific"))
  
  data.bp <- bind_rows(data.bp.solar, data.bp.wind) %>%
    revalue.levels(region = region.mapping.bp) %>%
    group_by(region, year, technology) %>%
    summarise(capacity = sum(capacity)) %>%
    mutate(year = as.numeric(year))
  
  return(data.bp)
  
}
