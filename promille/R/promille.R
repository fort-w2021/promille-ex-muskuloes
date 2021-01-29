#' compute alcohol mass
#'
#' @param volume volume (of alcohol) in ml
#' @param alcohol_percentage permille amount of alcohol
#' @param alcohol_density in g/cm3
#' @return alcohol mass in grams
compute_alcohol_mass <- function(volume, alcohol_percentage,
                                 alcohol_density = 0.8) {
  volume * alcohol_percentage * alcohol_density
}

#' compute total body water
#'
#' @param sex person's sex male or female
#' @param age person's age in years
#' @param height person's height in cm
#' @param mass person's mass in kg
#' @return total body water (unitless)
compute_total_body_water <- function(sex, age, height, mass) {
  switch(sex,
    "male" = 2.447 - (0.09516 * age) + (0.1074 * height) + (0.3362 * mass),
    "female" = 0.203 - (0.07 * age) + (0.1069 * height) + (0.2466 * mass)
  )
}

#' compute distribution factor
#'
#' @param total_body_water total body water (unitless)
#' @param mass in kg
#' @param blood_density  in g/cm3
#' @return distribution factor
compute_distribution_factor <- function(total_body_water,
                                        mass, blood_density = 1.055) {
  blood_density * total_body_water / (0.8 * mass)
}

#' compute alcohol concentration
#'
#' @param alcohol_mass in g
#' @param distribution_factor whatson distribution factor
#' @param mass in kg
#' @return alcohol concentration
compute_alcohol_concentration <- function(alcohol_mass,
                                          distribution_factor, mass) {
  alcohol_mass / (distribution_factor * mass)
}

#' Tell me how drunk
#'
#' @param age person's in years
#' @param sex person's sex (male or female)
#' @param height person's height in cm
#' @param weight person's weight in Kg
#' @param drinking_time drinking time in hours
#' @param drinks drinks vector e.g., c("schnaps", "wein")
#' @return promille Per mille blood alcohol value
#' @import checkmate
tell_me_how_drunk <- function(age, sex = c("male", "female"),
                              height, weight, drinking_time, drinks) {

  # make input homogeneous and check them
  sex <- tolower(sex)
  sex <- match.arg(sex)
  drinks <- unlist(drinks)
  checkmate::assert_choice(sex, c("male", "female"))
  checkmate::assert_number(age, lower = 0, finite = TRUE)
  checkmate::assert_number(height, lower = 0, finite = TRUE)
  checkmate::assert_number(weight, lower = 0, finite = TRUE)
  checkmate::assert_posixct(drinking_time, len = 2, sorted = TRUE)
  checkmate::assert_numeric(drinks, lower = 0, finite = TRUE)
  checkmate::assert_names(names(drinks), subset.of = c(
    "massn", "hoibe",
    "wein", "schnaps"
  ))
  drinks <- tapply(drinks, names(drinks), sum)
  if (age < 16) {
    warning("illegal drinking age")
  }
  if (age < 18 & "schnaps" %in% names(drinks)) {
    warning("illegal: hard liquor ain't allowed below 18 years old")
  }

  total_alcohol_mass <- 0
  for (name in names(drinks)) {
    total_alcohol_mass <- total_alcohol_mass + switch(name,
      "massn" = drinks[[name]] * compute_alcohol_mass(1000, 0.06),
      "hoibe" = drinks[[name]] * compute_alcohol_mass(500, 0.06),
      "wein" = drinks[[name]] * compute_alcohol_mass(200, 0.11),
      "schnaps" = drinks[[name]] * compute_alcohol_mass(40, 0.40),
    )
  }
  total_body_water <- compute_total_body_water(sex, age, height, weight)
  distribution_factor <- compute_distribution_factor(total_body_water, weight)
  promille <- compute_alcohol_concentration(
    total_alcohol_mass,
    distribution_factor, weight
  )
  drinking_time <- as.numeric(difftime(drinking_time[[2]], drinking_time[[1]],
    units = "hours"
  ))
  if (drinking_time > 1) {
    promille <- max(0, promille - (drinking_time - 1) * 0.15)
  }
  promille
}

#' show me how drunk, plots per mille alcohol level at 5 mins interval
#'
#' @inheritParams tell_me_how_drunk
#' @importFrom checkmate assert_posixct
#' @importFrom ggplot2 qplot
show_me_how_drunk <- function(age, sex, height,
                              weight, drinking_time, drinks) {
  checkmate::assert_posixct(drinking_time, len = 2, sorted = TRUE)
  x <- seq(drinking_time[[1]], drinking_time[[2]], by = "5 mins")
  y <- sapply(x, function(t) {
    tell_me_how_drunk(
      age, sex, height, weight,
      c(drinking_time[[1]], t), drinks
    )
  })
  ggplot2::qplot(x, y, xlab = "time", ylab = "per mille alcohol level")
}
