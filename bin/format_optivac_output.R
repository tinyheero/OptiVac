#!/usr/bin/env Rscript
script_description <- "Formats the OptiVac output into a TSV file"

script_author <- "Fong Chun Chan <fongchun@alumni.ubc.ca>"
script_examples <- "
Examples:

    {script_name} \\
      --optivac-output-file example/out.txt \\
      --output-file example/out.tsv
"

# Warnings should be escalated to errors:
options(warn = 2)

# The libraries to load. This should be as minimal as possible.
loaded_libs <- c("magrittr")

# Required library packages
required_libs <-
  c(loaded_libs, "glue", "readr", "argparse", "stringr")

# If any packages aren't installed, list them and stop
missing_libs <-
  required_libs[!required_libs %in% rownames(installed.packages())]
if (length(missing_libs) > 0)
  stop(paste(c("Missing required libraries:", missing_libs), collapse = " "))

# Load the necessary packages (only do this in scripts, not library code)
for (lib in loaded_libs)
  suppressPackageStartupMessages(library(lib, character.only = TRUE))

#' Main function
#'
#' This function should do little more than co-ordinate the other functions.
#' That way, the logic of the script (in the other functions) can be tested
#' independently of the parameter parsing.
#' 
#' @param cli_args Input from commandArgs(TRUE)
#' @return NULL
main <- function(cli_args) {
  # Parse the CLI arguments:
  parameters <- parser_args(cli_args)

  tmg_string <- 
    scan(parameters$optivac_output_file, what = "character", quiet = TRUE)
  peptides <- unlist(stringr::str_split(tmg_string, "-"))

  peptide_labels <- c()
  for (i in seq_len(length(peptides))) {
    if (i %% 2 == 1) {
      # If it is a peptide
      if (i == 1) {
        # If this is the first peptide
        peptide_num <- 1
      } else {
        peptide_num <- peptide_num + 1
      }
      peptide_label <- glue::glue("e{peptide_num}")
      peptide_labels <- c(peptide_labels, peptide_label)
    } else if (i %% 2 == 0) {
      # If it is a spacer
      if (i == 2) {
        # If this is the first spacer
        spacer_num <- 1
      } else {
        spacer_num <- spacer_num + 1
      }
      spacer_label <- glue::glue("s{peptide_num},s{peptide_num + 1}")
      peptide_labels <- c(peptide_labels, spacer_label)
    }
  }

  output_df <- tibble::tibble(peptide_seq = peptides, label = peptide_labels)
  readr::write_tsv(output_df, parameters$output_file)
}

#' Parse command-line arguments
#'
#' @param cli_args Input from commandArgs(TRUE)
#' @return List of parameters parsed from the command-line.
parser_args <- function(cli_args) {

  # See https://stackoverflow.com/a/27492072 for details of escaping backslashes
  # To escape the \\ in the examples section
  script_description <- gsub("\\\\", "\\\\\\\\", script_description)
  script_examples <- gsub("\\\\", "\\\\\\\\", script_examples)

  # To maintain newlines following JSON serialization to python's argparse
  script_description <- gsub("\n", "\\\\n\\\\\n", script_description) # nolint
  script_examples <- gsub("\n", "\\\\n\\\\\n", script_examples) # nolint

  # Identify how the user called the script
  full_commandline_arguments <- commandArgs(trailingOnly = FALSE)
  script_name <- sub(
    "--file=", "",
    full_commandline_arguments[grep("--file=", full_commandline_arguments)]
  )

  script_examples <- glue::glue(script_examples)

  parser <- argparse::ArgumentParser( # nolint
    description = script_description,
    epilog = script_examples,
    formatter_class = "argparse.RawTextHelpFormatter"
  )

  parser$add_argument(
    "--optivac-output-file", 
    nargs = 1, 
    help = "Path to output file from OptiVac"
  )

  parser$add_argument(
    "--output-file", 
    nargs = 1,
    help = "Path to the formatted OptiVac output file"
  )

  parameters <- parser$parse_args(cli_args)

  # Validate the parameters
  if (! file.exists(parameters$optivac_output_file)) {
    stop("--optivac-output-file doesn't exist.")
  }

  return(parameters)
}

if (! exists("testing")) {
  main(commandArgs(TRUE))
}
