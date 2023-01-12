authors <- c(
  "Jónsdóttir, Jóna and Mustermann, Melanie and Holm, Kari and Doe, Jane",
  "Jónsdóttir, Jóna and Modaal, Jan and Mustermann, Melanie",
  "Jónsdóttir, Jóna and Modaal, Jan and Mustermann, Melanie",
  "Jónsdóttir, Jóna and Mustermann, Melanie and Holm, Kari and Doe, Jane and Doe, Mike and Musterfrau, Michael  and Banana, Joe and Citizen, Kane and Smith, Mary",
  "Jónsdóttir, Jóna and Mustermann, Melanie and Holm, Kari and Doe, Jane and Doe, Mike and Musterfrau, Michael and Average, Albert",
  "Doe, Mike", "Doe, Mike"
)

years <- c(2022, 2022, 2022, 2022, 2022, 2022, 2022)

output <- c(
  "Jónsdóttir, Mustermann, Holm & Doe (2022)",
  "Jónsdóttir, Modaal & Mustermann (2022a)",
  "Jónsdóttir, Modaal & Mustermann (2022b)",
  "Jónsdóttir ... Banana et al. (2022)",
  "Jónsdóttir ... Musterfrau  & Average (2022)",
  "M. Doe (2022a)", "M. Doe (2022b)"
)

test_that("citations are correctly formatted", {
  expect_equal(output, generate_apa_citation(authors, years))
})
