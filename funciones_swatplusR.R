SWATplusR::run_swat2012
function (project_path, output, parameter = NULL, start_date = NULL, 
          end_date = NULL, output_interval = NULL, years_skip = NULL, 
          rch_out_var = NULL, sub_out_var = NULL, hru_out_var = NULL, 
          hru_out_nr = NULL, run_index = NULL, run_path = NULL, n_thread = NULL, 
          save_path = NULL, save_file = NULL, return_output = TRUE, 
          add_parameter = TRUE, add_date = TRUE, refresh = TRUE, keep_folder = FALSE, 
          quiet = FALSE) 
{
  stopifnot(is.character(project_path))
  stopifnot(is.character(run_path) | is.null(run_path))
  stopifnot(is.numeric(n_thread) | is.null(n_thread))
  stopifnot(is.logical(add_parameter))
  stopifnot(is.logical(add_date))
  stopifnot(is.logical(return_output))
  stopifnot(is.logical(refresh))
  stopifnot(is.logical(keep_folder))
  stopifnot(is.logical(quiet))
  if (!is.null(parameter)) {
    parameter <- format_swat2012_parameter(parameter, "2012")
  }
  if (!is.null(run_index)) {
    run_index <- check_run_index(run_index, parameter$values)
  }
  else {
    run_index <- 1:max(nrow(parameter$values), 1)
  }
  output <- check_output(output, "2012")
  if (!is.null(parameter)) {
    file_meta <- read_file_meta(project_path, parameter$definition)
    swat_parameter <- read_swat2012_files(project_path, file_meta)
  }
  model_setup <- setup_swat2012(project_path, output, start_date, 
                                end_date, output_interval, years_skip, rch_out_var, sub_out_var, 
                                hru_out_var, hru_out_nr)
  if (!is.null(save_file)) {
    save_path <- set_save_path(project_path, save_path, save_file)
    check_saved_data(save_path, parameter, output, run_index, 
                     model_setup)
  }
  check_dates(project_path, model_setup)
  n_thread <- min(max(nrow(parameter$values), 1), max(n_thread, 
                                                      1), max(length(run_index), 1), detectCores())
  run_path <- ifelse(is.null(run_path), project_path, run_path) %//% 
    ".model_run"
  os <- get_os()
  swat_exe <- manage_model_run(project_path, run_path, n_thread, 
                               os, "2012", refresh, quiet)
  write_file_cio(run_path, model_setup$file.cio)
  if (!is.null(save_file)) {
    initialize_save_file(save_path, parameter, model_setup)
  }
  cl <- makeCluster(n_thread)
  worker <- tibble(worker_id = parSapply(cl, 1:n_thread, function(x) paste(Sys.info()[["nodename"]], 
                                                                           Sys.getpid(), sep = "-")), thread_id = dir(run_path) %>% 
                     .[grepl("thread_", .)])
  registerDoSNOW(cl)
  n_run <- length(run_index)
  if (!quiet) {
    cat("Performing", n_run, ifelse(n_run == 1, "simulation", 
                                    "simulations"), "on", n_thread, "cores:", "\n")
    t0 <- now()
    progress <- function(n) {
      display_progress(n, n_run, t0, "Simulation")
    }
    opts <- list(progress = progress)
  }
  else {
    opts <- list()
  }
  sim_result <- foreach(i_run = 1:n_run, .packages = c("dplyr", 
                                                       "lubridate", "stringr", "processx"), .options.snow = opts) %dopar% 
    {
      worker_id <- paste(Sys.info()[["nodename"]], Sys.getpid(), 
                         sep = "-")
      thread_id <- worker[worker$worker_id == worker_id, 
                          2][[1]]
      thread_path <- run_path %//% thread_id
      if (!is.null(parameter)) {
        thread_parameter <- swat_parameter
        thread_parameter <- modify_parameter(parameter, 
                                             thread_parameter, file_meta, run_index, i_run)
        write_parameter(file_meta, thread_parameter, 
                        thread_path)
      }
      msg <- run(run_os(swat_exe, os), wd = thread_path, 
                 error_on_status = FALSE)
      if (nchar(msg$stderr) == 0) {
        model_output <- read_swat2012_output(output, 
                                             thread_path) %>% extract_output(output, .)
        if (!is.null(save_path)) {
          save_run(save_path, model_output, parameter, 
                   run_index, i_run, thread_id)
        }
      }
      else {
        err_msg <- str_split(msg$stderr, "\r\n|\r|\n", 
                             simplify = TRUE)
        out_msg <- str_split(msg$stdout, "\r\n|\r|\n", 
                             simplify = TRUE) %>% .[max(1, length(.) - 10):length(.)]
        err_msg <- c("Last output:", out_msg, "Error:", 
                     err_msg)
        model_output <- err_msg
        if (!is.null(save_path)) {
          save_error_log(save_path, model_output, parameter, 
                         run_index, i_run)
        }
      }
      if (return_output) {
        return(model_output)
      }
    }
  stopCluster(cl)
  if (!quiet) {
    finish_progress(n_run, t0, "simulation")
    rm(t0)
  }
  if (!keep_folder) 
    unlink(run_path, recursive = TRUE)
  if (return_output) {
    date <- get_date_vector_2012(model_setup)
    sim_result <- tidy_results(sim_result, parameter, date, 
                               add_parameter, add_date, run_index)
    if ("error_report" %in% names(sim_result)) {
      warning("Some simulations runs failed! Check '.$error_report' in your", 
              " simulation results for further information.")
    }
    return(sim_result)
  }
}


read_file_meta<- function (project_path, par_constrain) 
{
  files <- list.files(project_path) %>% .[!grepl("output", 
                                                 .)]
   
    file_meta <- tibble(file = files, file_code = str_remove(file, 
                                                             "\\..*$"),
                        file_name = str_remove(file, ".*\\.")
                        ) %>% 
                        left_join(., read_hru(project_path), by = "file_code") %>% 
                              mutate(sub = str_sub(file_code, 1, 5) %>% as_num(.), 
                       sub = ifelse(sub > 0, sub, NA)
                                    )
  #                     par_constrain %>% build_expression(.) %>% 
  #                       map_df(., ~evaluate_expression(file_meta,.x)) %>% distinct(., file, .keep_all = T) 
}

read_hru<- function (project_path) 
{
  hru_files <- list.files(path = project_path, pattern = ".hru$", 
                          full.names = TRUE) %>% .[!grepl("output", .)]
  map_df(hru_files, get_hru_meta)
}

get_hru_meta <- function (hru_file_i) {
  hru_i <- read_lines(hru_file_i, lazy = FALSE)
  hru_i_head <- unlist(strsplit(hru_i[1], "\\ |\\:|\\: "))
  tibble(file_code = (basename(hru_file_i) %>% gsub(".hru$", 
                                                    "", .)), 
         hru = as.numeric(hru_i_head[which(hru_i_head =="HRU")[1] + 1]),
         sub = as.numeric(hru_i_head[which(hru_i_head =="Subbasin") + 1]),
         hru_sub = as.numeric(hru_i_head[which(hru_i_head =="HRU")[2] + 1]),
         luse = hru_i_head[which(hru_i_head == "Luse") + 1], 
         soil = hru_i_head[which(hru_i_head == "Soil") +1], 
         slope = hru_i_head[which(hru_i_head == "Slope") +1])
  }


library(dplyr)
library(purrr)
library(ggplot2)

rerun(6, rnorm(100)) %>%
  map_df(
    ~ data_frame(x = .x), 
    .id = "dist"
  )

project_path <-
"C:/Users/Augusto/Desktop/SWATala_junio/Modelo_intento2000_ROTACIONES_16_grass_agrl_cambio_clima_90_20_saco_Qmin_embalse/Embalse01/Scenarios/Default/TxtInOut/"




output <- SWATplusR::define_output(file = "hru",
                          variable = "MON",
                          unit = 1)


 SWATplusR:::read_swat2012_output
 
 SWATplusR:::read_swat2012_output<-function (output, thread_path) 
 {output_files <- unique(output$file) #output.hru
   frst_pos <- find_first_line(output_files, thread_path)  
   file_header <- map2(output_files, frst_pos, ~get_file_header(.x, 
                                                                .y, thread_path))
   fwf_pos <- map2(output_files, frst_pos, ~get_fwf_positions(.x, 
                                                              thread_path, .y))
   out_tables <- pmap(list(output_files, fwf_pos, frst_pos), 
                      function(out, fwf, frst, thread_path) {
                        read_fwf(file = thread_path %//% out, col_positions = fwf_positions(fwf[[1]], 
                                                                                            fwf[[2]]), skip = frst, guess_max = 3, lazy = FALSE)
                      }, thread_path) %>% map2(., file_header, ~set_names(.x, 
                                                                          .y)) %>% set_names(., output_files)
   tables_nrow <- map(out_tables, ~nrow(.x)) %>% unlist(.)
   if (any(tables_nrow == 0)) {
     stop("\nOne of the SWAT runs was not successful!\n" %&&% 
            "The defined model parameters could be a reason.\n" %&&% 
            "Please check if any change in the model parametrization" %&&% 
            "caused a parameter to be out of bounds!")
   }
   return(out_tables)
 }
 
 SWATplusR:::get_file_header <- function (output_i, tbl_pos, thread_path) 
 {
   header <- read_lines(file = thread_path %//% output_i, skip = tbl_pos - 
                          1, n_max = 1, lazy = FALSE) %>% split_by_units(.) %>% 
     str_replace_all(., "-", "_") %>% str_replace_all(., "#", 
                                                      "_")
   if (output_i != "output.hru") {
     header <- c("FILE", header)
   }
   return(header)
 }
 
 SWATplusR:::split_by_units<-function (header) 
 {
   unit <- "ppm|mg\\/m3|m3|Mg\\/l|mg\\/L|mg\\/kg|mg|kg\\/ha|kg\\/h|kg|t\\/ha|mic\\/L|\\(mm\\)|kg|cms|tons|ton|mg|mg\\/|mm|km2|_tha|_kgha|\\_m|\\_kgN\\/ha|\\_kgP\\/ha|\\_m\\^3|ha\\-m|_k|mgps|degC|degc|dgC|ct|[:space:]|MJ/m2|m"
   header %>% str_replace_all(., "WTAB ", "WTAB_") %>% str_replace_all(., 
                                                                       "TOT ", "TOT_") %>% str_replace_all(., "LAT ", "LAT_") %>% 
     str_split(., unit) %>% unlist(.) %>% trimws() %>% gsub(" ", 
                                                            "_", .) %>% .[nchar(.) > 0]
 }

 
 SWATplusR:::
   get_fwf_positions <- function (output_i, thread_path, tbl_pos){
   header_line <- read_lines(file = thread_path %//% output_i, 
                             skip = tbl_pos - 1, n_max = 1, lazy = FALSE)
   first_line <- read_lines(file = thread_path %//% output_i, 
                            skip = tbl_pos, n_max = 1, lazy = FALSE)
   pos_mon_area <- c(str_locate(header_line, "MON")[1], str_locate(header_line, 
                                                                   "AREA")[2])
   if (any(is.na(pos_mon_area))) {
     start_pos <- c(1, str_locate_all(first_line, " +")[[1]][, 
                                                             1])
   }
   else {
     chr_mon_area <- str_sub(first_line, pos_mon_area[1], 
                             pos_mon_area[2])
     chr_split <- chr_mon_area %>% trimws(.) %>% str_detect(., 
                                                            " ") %>% ifelse(., " +", "\\.")
     pos_split <- (str_locate_all(chr_mon_area, chr_split)[[1]] + 
                     pos_mon_area[1] - 1) %>% .[nrow(.), 1] %>% unname(.)
     start_pos <- str_locate_all(first_line, " +")[[1]][, 
                                                        1] %>% .[!(. %in% pos_mon_area[1]:pos_mon_area[2])] %>% 
       c(1, pos_split, .) %>% sort(.)
   }
   if (output_i != "output.hru") {
     last_val <- (str_locate_all(first_line, "E")[[1]][, 1] + 
                    4) %>% .[length(.)]
   }
   else {
     last_val <- nchar(first_line)
   }
   start_pos <- start_pos[start_pos < last_val]
   end_pos <- c(start_pos[2:length(start_pos)], last_val) - 
     1
   return(list(start_pos, end_pos))
 }
 
 SWATplusR:::
   `%//%` <- function (a, b) {paste(a, b, sep = "/")}
 