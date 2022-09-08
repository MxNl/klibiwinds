read_csv_and_add_path_as_column <- function (filepath) {
  filepath %>%
    data.table::fread(sep = ";") %>%
    mutate(identifier = filepath)
}