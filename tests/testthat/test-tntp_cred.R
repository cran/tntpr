test_that("Credentials can be retrieved with tntp_cred()", {

  # Test breaks on linux in CI/CD. Skipping for now.
  skip_on_os("linux")

  cred <- "tntp_cred TEST CREDENTIAL"
  pw <- "abc123%$@#"

  # set programmatically
  keyring::key_set_with_value(cred, password = pw)

  # Test that pulling works
  expect_equal(tntp_cred(cred), pw)

  # Remove test key
  keyring::key_delete(cred)

})
