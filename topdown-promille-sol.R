library(checkmate)

# compute alcohol mass
# @params: volume(of alcohol), alcohol percentage, alcohol density
# @return: alcohol mass
compute_alcohol_mass <- function(volume, alcohol_percentage,
                                 alcohol_density = 0.8) {
  volume * alcohol_percentage * alcohol_density
}

# compute total body water
# @params: sex, age, height, mass
# @return: total body water
compute_total_body_water <- function(sex, age, height, mass) {
  switch(sex,
    "male" = 2.447 - (0.09516 * age) + (0.1074 * height) + (0.3362 * mass),
    "female" = 0.203 - (0.07 * age) + (0.1069 * height) + (0.2466 * mass)
  )
}

# compute distribution factor
# @params: total_body_water, mass, blood_density
# @return: distribution factor
compute_distribution_factor <- function(total_body_water,
                                        mass, blood_density = 1.055) {
  blood_density * total_body_water / (0.8 * mass)
}

# compute alcohol concentration
# @params: alcohol_mass, distribution_factor, mass
# @return: alcohol concentration
compute_alcohol_concentration <- function(alcohol_mass,
                                          distribution_factor, mass) {
  alcohol_mass / (distribution_factor * mass)
}

# tell me how drunk
# @params: age, sex, height, weight, drinking time, and drinks
# @return: promille
tell_me_how_drunk <- function(age, sex = c("male", "female"),
                              height, weight, drinking_time, drinks) {

  # make input homogeneous and check them
  sex <- tolower(sex)
  sex <- match.arg(sex)
  drinks <- unlist(drinks)
  assert_choice(sex, c("male", "female"))
  assert_number(age, lower = 0, finite = TRUE)
  assert_number(height, lower = 0, finite = TRUE)
  assert_number(weight, lower = 0, finite = TRUE)
  assert_posixct(drinking_time, len = 2, sorted = TRUE)
  assert_numeric(drinks, lower = 0, finite = TRUE)
  assert_names(names(drinks), subset.of = c(
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
