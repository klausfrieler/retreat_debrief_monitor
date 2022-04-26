library(tidyverse)
library(tictoc)

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

likert_items <- 
  c("overall_satisfaction" = "I was satisfied with the retreat.",
    "liking_structure" = "I liked the overall program structure",
    "liking_location" = "Location, accommodation & food was good.",
    "scientific_quality" = "The scientific program was interesting and of good quality.",
    "workshops" = "The workshops I attended were interesting and stimulating.",
    "scientific_quantity" = "The amount of scientific talks was just right.",
    "joint_meeting" = "I liked the concept of meeting together with administration and services.",
    "meeting_people" = "I met many people which I haven't met before.",
    "learning" = "I learned a lot.",
    "motivation" = "I feel more inspired and motivated after the retreat.",
    "social_program" = "I was satisfied with the social program (Dahlem/museum tours).",
    "get_together" = "I was satisfied with the get-togethers and personal meetings.",
    "organization" = "The retreat was well organized.",
    "integrated" = "I feel more integreated into the institute after the retreat.",
    "covid19_measures" = "The pandemic security measures were adequate.")

free_text_items <- c("highlight" = "What was your personal highlight?",
                     "improvement" = "What could be improved in the future?")

dropbox_items <- list(
  department = list(
    prompt = "What is your department/group?", 
    items = c("Department of Music", 
              "Department of Literature & Language", 
              "Department of Cognitive Neuropsychology", 
              "Administration", 
              "Services (incl. IT)", 
              "Research groups")),
  status = list(
    prompt= "What is your professional role?", 
    items = c(
      "PhD",
      "Post-Doc",
      "Scientific staff", 
      "Administrative staff",
      "Service staff", 
      "Other")),
  attending = list(
    prompt = "How many retreats did you attend before?",
    items = c("None", 
              "Less than 4", 
              "4 to 6")
  ),
  # gender = list(
  #   prompt = "What is your gender?",
  #   items = c("Female", 
  #             "Male", 
  #             "Other",
  #             "Rather not say")),
  best_part = list(
    prompt = "Which part was the most satisfying for you?",
    items = c("Scientific program",
              "Workshops", 
              "Social program", 
              "Administrative topics",
              "Personal meetings",
              "Information on the institute")
  )
)

checkbox_items <- list(
  wishes = list(
    prompt = "What should be more in the focus for future retreats?",
    items = c("Scientific talks",
              "Social program",
              "Artistic & musical program",
              "Team-building exercises",
              "Hands-on workshops")),
  topics = list(
    prompt = "What do you want to learn more about in future retreats?",
    items = c("Scientific methods in general",
              "Statistics",
              "Scientific Writing",
              "Research ethics",
              "Data protection",
              "Open Science",
              "Software development",
              "Science communication/PR",
              "Administrative procedures",
              "Graphics",
              # "Soft skills",
              "Writing grant applications",
              "Career building",
              "Theory of (empirical) aesthetics",
              "Bridging the gap between science and humanities"))
)
expand_variables <- names(checkbox_items)

get_expanded_df <- function(data, var1, var2 = NULL){
  data %>% 
    select(all_of(c(var1, var2))) %>% 
    mutate(!!sym(var1) := str_split(!!sym(var1), ";")) %>% 
    unnest(cols = !!sym(var1)) %>% 
    filter(nchar(!!sym(var1)) > 0)
  
}

get_parameters <- function(data, input, keep_pseudo_na = T, var_data){
  
  vars <- c("x" = input$bv_variable1, "y" = input$bv_variable2)
  var_info1 <- var_data %>% filter(variable == input$bv_variable1)
  var_info2 <- var_data %>% filter(variable == input$bv_variable2)
  sub_type <- sprintf("%s-%s", substr(var_info1$type, 1, 3), substr(var_info2$type, 1, 3))  
  list(vars = vars, sub_type = sub_type)
}


split_multi_entry <- function(entry){
  if(length(entry) == 1){
    ret <- str_replace_all(entry, "',", "@") %>% 
      str_replace_all("'", "") %>% 
      str_split("@") %>% 
      unlist()        
  }    
  else{
    ret <- NULL
  }
  ret
}

join_rows <- function(data){
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  fixed_rows <- 
    map_dfr(ids, function(i){
    tmp <- data %>% filter(p_id == i)
    completed <- which(tmp$session.complete == TRUE)
    if(length(completed) == 0){
      tmp  <- tmp[nrow(tmp),]   
    }
    else{
      tmp <- tmp[max(completed), ]  
    }
    tmp
  })
  ret %>% bind_rows(fixed_rows) 
}

get_test_duration <- function(start_time, end_time, num_restarts){
  if(length(num_restarts) > 1){
    map2_dbl(start_time, end_time, function(st, et){
      get_test_duration(st, et, 0)
    }) %>% sum()
  }
  if(num_restarts > 0) {
    return(NA)
  }
  difftime(end_time, start_time, units = "mins") %>% as.numeric()
}

get_str_element <- function(str, split = "_", el = 2){
  map_chr(str_split(str, split), ~{.x[[el]]})
}

parse_debrief_results <- function(res){
  ret <- 
    map_dfc(names(res), function(test){
      #messagef("Parsing test %s", test)
      if(test == "results"){
        #print(test$results)
        ret <- 
          map(names(res$result), function(item){
            val <- res$result[[item]]
            if(str_detect(item, "^likert")){
              return(as.numeric(val))
            }
            else if (str_detect(item, "^question")){
              spec <- str_replace(item, "^question_", "")
              return(dropbox_items[[spec]]$items[as.numeric(val)])
            }
            else if (str_detect(item, "^text")){
              return(val)
            }
            else if (str_detect(item, "^checkbox")){
              spec <- str_replace(item, "^checkbox_", "")
              return(paste(checkbox_items[[spec]]$items[as.numeric(val)], collapse = ";"))
            }
          })
        names(ret) <- str_replace(names(res$results), "question_|likert_|checkbox_|text_", "")
        return(ret %>% as_tibble())
      }
      else{
        #tic("5")
        if(length(res[[test]]) != length(unique(names(res[[test]])))){
          messagef("Found duplicated names in %s", test)
          res[[test]] <- res[[test]][unique(names(res[[test]]))]
        }
        data <- 
          res[[test]][!str_detect(names(res[[test]]), "^q|^items|^points")] %>% 
          as_tibble() %>% 
          #select(-starts_with("q"), -starts_with("items"), -starts_with("points")) %>% 
          set_names(sprintf("%s.%s", test, names(.) %>% tolower() %>% str_replace_all(" ", "_")))
        data <- data[!duplicated(data), ]
        #toc()
        data
      }
    })
  if("session" %in% names(res)){
    if(length(ret$session.time_started) > 1){
      browser()
    }
    test_duration <- get_test_duration(ret$session.time_started, ret$session.current_time, ret$session.num_restarts)
    ret <- ret %>% 
      mutate(p_id = session.p_id, 
             session.test_duration_min = test_duration) %>% 
      select(-session.p_id, 
             -session.pilot, 
             -session.current_time, 
             -session.language, 
             -session.num_restarts) %>% 
      select(p_id, everything())
  }
  ret
}

cache_locked <- FALSE

read_cache <- function(cache_dir = g_cache_dir){
  cache_file <- file.path(cache_dir, "cache.rds")
  if(!file.exists(cache_file)){
    return(tibble())
  }
  readRDS(cache_file)
}

save_cache <- function(data, cache_dir = cache_dir){
  if(!cache_locked){
    cache_locked <- TRUE
    saveRDS(data, file.path(cache_dir, "cache.rds"))
    cache_locked <- FALSE
  }
}

delete_cache <- function(cache_dir = g_cache_dir){
  unlink(file.path(cache_dir, "cache.rds"), recursive = T)
}

g_bad_ids <- c()
update_cache <- function(result_dir = g_result_dir, cache_dir = g_cache_dir){
  messagef("Caching  data from <%s> to <%s>", result_dir, cache_dir)
  if(!file.exists(cache_dir)){
    dir.create(cache_dir)  
  } 
  #browser()
  cache <- read_cache(cache_dir)
  
  cache_ids_complete <- c()
  if(nrow(cache) > 0){
    messagef("Read cache with %d lines and %d distinct ids", nrow(cache), n_distinct(cache$p_id))
    cache <- cache %>% 
      group_by(p_id) %>% 
      mutate(any_complete = any(session.complete)) %>% 
      ungroup()
    cache_ids_complete <- unique(cache[cache$any_complete,]$p_id)
  }
  result_files <- list.files(result_dir, pattern = ".rds$", full.names = T)
  l <- length(result_files)
  
  messagef("Found %d data files in <%s>", l, result_dir)
  new_files <- result_files
  if(length(cache_ids_complete) > 0 ){
    #browser()
    ids <- str_extract(result_files, "p_id=[a-z0-9]+") %>% str_replace("p_id=", "")
    complete <- str_extract(result_files, "complete=[a-z]+") %>% str_replace("complete=", "") %>% 
      toupper() %>% 
      as.logical()
    #browser()
    file_stats <- tibble(fname = result_files, 
                         p_id = ids, 
                         is_complete = complete) %>% 
      group_by(p_id) %>% 
      mutate(n_files = n(), 
             is_bad = FALSE,
             in_cache = FALSE) %>% 
      ungroup()
    file_stats[file_stats$p_id %in% g_bad_ids,]$is_bad <- TRUE
    file_stats[file_stats$p_id %in% cache_ids_complete,]$in_cache <- TRUE
    new_files <- file_stats %>% 
      filter(!is_bad, !in_cache) %>% 
      pull(fname)
    messagef("Found %d new data files in <%s>", length(new_files), result_dir)
  }
  if(length(new_files) == 0){
    messagef("No new files.")
    return(cache)
  }
  tic()
  new_data <- purrr::map(new_files, ~{
    readRDS(.x) %>% as.list()
    })
  #assign("res", new_data, globalenv())
  t <- toc(quiet = T)
  messagef("Reading RDS: %.3f s elapsed.", t$toc- t$tic)
  tic()
  ret <-
    map_dfr(new_data, function(res){
      tic()
      res <- tryCatch({parse_debrief_results(res)}, 
                      error = function(e){
                        g_bad_ids <<- c(g_bad_ids, res$session$p_id)
                        messagef("#Bad ids: %d", length(g_bad_ids))
                        NULL
                        })
      t <- toc(quiet = T)
      #messagef("Parse single participants (%d entries): %.3f s elapsed.", length(res), t$toc- t$tic)
      res
    })
  t <- toc(quiet = T)
  
  messagef("Parsed %d participants: %.3f s elapsed.", nrow(ret), t$toc- t$tic)
  ret <- bind_rows(ret, cache) %>% 
    distinct(p_id, session.time_started, session.complete, .keep_all = T) 
  #browser()
  ret$any_complete <- NULL
  save_cache(ret, cache_dir)
  ret
}

read_data <- function(result_dir = g_result_dir){
  messagef("Setting up data from %s", result_dir)
  tic()
  results <- purrr::map(list.files(result_dir, pattern = ".rds$", full.names = T), ~{readRDS(.x) %>% as.list()})
  assign("res", results, globalenv())
  t <- toc(quiet = T)
  messagef("Reading RDS: %.3f s elapsed.", t$toc- t$tic)
  tic()
  
  ret <-
    map_dfr(results, function(res){
      tic()
      ret <- tryCatch({parse_debrief_results(res)}, 
                      error = function(e){})
      t <- toc(quiet = T)
      #messagef("Parse single participants (%d entries): %.3f s elapsed.", length(res), t$toc- t$tic)
      ret
    })
  t <- toc(quiet = T)
  messagef("Parsed %d participants: %.3f s elapsed.", nrow(ret), t$toc- t$tic)
  ret
}

setup_workspace <- function(result_dir = g_result_dir, cache_dir = g_cache_dir){
  #master <- read_data(result_dir)
  master <- update_cache(result_dir, cache_dir)
  if(nrow(master) == 0){
    assign("master", tibble(), globalenv())
    return()
  }
  tic()
  t <- toc(quiet = T)
  messagef("Post processing: %.3f elapsed", t$toc - t$tic)
  assign("master", master, globalenv())
  master
}

update_workspace <- function(result_dir = g_result_dir){
  messagef("Callind update work_space")
  setup_workspace(result_dir, g_cache_dir)
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())     
}

recode_na <- function(x, new_value = FALSE){
  x[is.na(x)] <- new_value
  x
}
  
na_to_false <- function(x){
  recode_na(x)
}

na_to_true <- function(x){
  recode_na(x, TRUE)
}

