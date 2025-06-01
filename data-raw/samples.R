library(readr)
library(dplyr)
library(stringr)

samples01 <- readr::read_tsv(file = "data-raw/s_OSD-758.txt.xz")

samples02 <-
samples01 |>
  dplyr::select(
    sample_id = `Sample Name`,
    organism = `Characteristics[Organism]`,
    genotype = `Characteristics[Genotype]`,
    animal_source = `Characteristics[Animal Source]`,
    matrix = `Characteristics[Material Type]`,
    sex = `Characteristics[Sex]`,
    spaceflight = `Factor Value[Spaceflight]`,
    gravity = `Factor Value[Altered Gravity]`,
    age_at_launch = `Characteristics[Age at Launch]`,
    date_of_birth = `Characteristics[Date of Birth]`,
    habitat = `Parameter Value[habitat]`,
    duration = `Parameter Value[duration]`,
    weight_at_launch = `Parameter Value[Body Weight at Launch]`,
    diet = `Parameter Value[Diet]`,
    feeding_schedule = `Parameter Value[Feeding Schedule]`,
    light_cycle = `Parameter Value[light cycle]`,
    enrichment_material = `Parameter Value[Enrichment material]`,
    weight_at_euthanasia = `Parameter Value[Body Weight at Euthanasia]`,
    age_at_euthanasia = `Parameter Value[Age at Euthanasia]`,
    date_of_euthanasia = `Parameter Value[Euthanasia Date]`,
    date_of_dissection = `Parameter Value[BSP Dissection Date]`
  )

samples03 <-
  samples02 |>
  dplyr::mutate(
    spaceflight = dplyr::case_when(
      spaceflight == "Ground Control" ~ "Ground",
      spaceflight == "Space Flight" ~ "ISS",
      TRUE ~ NA_character_
    ),
    acceleration_source = dplyr::case_when(
      gravity == "1G on Earth" ~ "Terrestrial gravity",
      gravity %in% c("0.33G by centrifugation", "0.66G by centrifugation", "1G by centrifugation") ~ "Centrifugal acceleration",
      gravity == "uG" ~ "Orbital free fall",
      TRUE ~ NA_character_
    ),
    gravity = dplyr::case_when(
      gravity == "1G on Earth" ~ 1.00,
      gravity == "1G by centrifugation" ~ 1.00,
      gravity == "0.66G by centrifugation" ~ 0.66,
      gravity == "0.33G by centrifugation" ~ 0.33,
      gravity == "uG" ~ 1e-6,
      TRUE ~ NA_real_
    ),
    .after = "sex"
  )

samples04 <-
  samples03 |>
  dplyr::transmute(
    sample_id,
    organism,
    sex = factor(
      x = sex,
      levels = c("Female", "Male"),
      labels = c("female", "male")
    ),
    matrix,
    spacecraft = factor(
      x = spaceflight,
      levels = c("Ground", "ISS"),
      labels = c("Earth", "ISS")
    ),
    acceleration_source = stringr::str_to_title(acceleration_source),
    acceleration_source = factor(
      acceleration_source,
      levels = c(
        "Terrestrial Gravity",
        "Centrifugal Acceleration",
        "Orbital Free Fall"
      )
    ),
    gravity,
    gravity_class = dplyr::case_when(
      gravity == 1e-6 ~ "micro_G",
      gravity == 0.33 ~ "0.33_G",
      gravity == 0.66 ~ "0.66_G",
      gravity == 1.00 ~ "1.00_G",
      TRUE ~ NA_character_
    ),
    gravity_class = factor(gravity_class, levels = c("micro_G", "0.33_G", "0.66_G", "1.00_G")),
    weight_at_launch,
    weight_at_euthanasia
  )

samples <- samples04

readr::write_rds(x = samples, file = "inst/extdata/samples.rds", compress = "xz")
