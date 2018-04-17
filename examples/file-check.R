\dontrun{
# create two example files in the working directory: 
cat("example file.", file = "QWRAPS2_EXAMPLE_1.txt")
cat("Another example file.", file = "QWRAPS2_EXAMPLE_2.txt")

# Check that you have access to these files:  (Should return TRUE)
test1 <- file_check(c("QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"))
test1

# By default, when the checks return TRUE the details of the checks are not
# printed.  You can view the details of the checks as follows:
attr(test1, "checks")

# If one or more files is not accessable then return is FALSE and the meta data
# is printed by default.
test2 <- file_check(c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"))
test2

# Or have an error thrown:
file_check(c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"),
           stop = TRUE)

# Verify the md5sums as well as file access:
file_check("QWRAPS2_EXAMPLE_1.txt", "7a3409e17f9de067740e64448a86e708")

# If you only need to verify a subset of md5sums then use an NA in the md5sums
# argument:
file_check(c("QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"),
           c("7a3409e17f9de067740e64448a86e708", NA))

# Verify all the md5sums
file_check(c("QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"),
           c("7a3409e17f9de067740e64448a86e708", "798e52b92e0ae0e60f3f3db1273235d0"))


# clean up working directory
unlink("QWRAPS2_EXAMPLE_1.txt")
unlink("QWRAPS2_EXAMPLE_2.txt")

}

