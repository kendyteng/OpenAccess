
if (!require(tidyverse)) { 
  install.packages("tidyverse")
  library(tidyverse)
}

### 1) The simple life table. It can set 'top'

lifetable_0 <- 
  function(data, top = NULL) {
    
    # with no 'top' specified
    if (is.null(top)) {
      D.fraction_life <- data %>%
        #The column used to estimate life expectancy has to be called 'lifespan'
        dplyr::select(lifespan) %>%
        mutate(lifespan_int = floor(data$lifespan),
               fraction_life = lifespan - lifespan_int) %>%
        group_by(lifespan_int) %>%
        dplyr::summarise(ax = mean(fraction_life),
                         sd = sd(fraction_life)) 
      
      D.result <- data.frame(age = D.fraction_life$lifespan_int) %>%
        # dx: number of dogs die in this interval
        mutate(dx = c(table(floor(data$lifespan)))) %>% 
        arrange(-age) %>%
        # lx: number living at this age; qx: proportion of death in this period
        mutate(lx = cumsum(dx),
               qx = dx/lx) %>%
        # ax: fraction of year lived
        left_join(., D.fraction_life, by = c("age" = "lifespan_int")) %>%
        # Lx: number of year lived in the interval
        mutate(Lx = (lx-dx)+ax*dx, 
               # Tx: total number of year lived beyong age x
               Tx = cumsum(Lx),
               # Ex: expected life at age x
               Ex = Tx/lx) %>%
        arrange(age) 
    } 
    
    # With 'top' specified 
    else {
      # Deal with 'top'
      data1 <- data %>%
        mutate(lifespan_int = floor(data$lifespan),
               lifespan_int = if_else(lifespan_int > top, top, lifespan_int))
      
      D.fraction_life <- data1 %>%
        dplyr::select(lifespan, lifespan_int) %>%
        mutate(fraction_life = lifespan - lifespan_int) %>%
        group_by(lifespan_int) %>%
        dplyr::summarise(ax = mean(fraction_life),
                         sd = sd(fraction_life))
      
      D.result <- data.frame(age = D.fraction_life$lifespan_int) %>%
        # dx: number of dogs die in this interval
        mutate(dx = c(table(floor(data1$lifespan_int)))) %>% 
        arrange(-age) %>%
        # lx: number living at this age; qx: proportion of death in this period
        mutate(lx = cumsum(dx),
               qx = dx/lx) %>%
        # ax: fraction of year lived
        left_join(., D.fraction_life, by = c("age" = "lifespan_int")) %>%
        # Lx: number of year lived in the interval
        mutate(Lx = (lx-dx)+ax*dx) %>%
        # Tx: total number of year lived beyong age x
        mutate(Tx = cumsum(Lx)) %>%
        # Ex: expected life at age x
        mutate(Ex = Tx/lx) %>%
        arrange(age) 
    }
    return(D.result)
  }


### 2) The simple life table with 'top', 'cell_min' and 'top_min' being able to be specified

lifetable <- 
  function(data, top = NULL, cell_min = 3, top_min = 11) {
    
    ## Original life table
    the_lt <- lifetable_0(data, top = top)
    
    ## to make sure the the number of observation in  the last year >= 'top_min'
    while (tail(the_lt$dx, 1) < top_min) {
      the_lt <- lifetable_0(data, top = tail(the_lt$age, 1)-1)
    }
    
    ## for 'cell_min'
    if (the_lt$age[1] != 0 | sum(the_lt$dx < cell_min) != 0) {
      reject <- 
        paste0("AT lease one year had the number of obsevarions less than ", cell_min)
      stop(reject)
    }
    return(the_lt)
  }


### 3) The life table with the confidence interval

lifetable_ci <- 
  function(data, top = NULL, size, times, seed = NULL, ci = 0.95, cell_min = 3, top_min = 11) {
    
    the_lt <- lifetable(data, top = top, cell_min = cell_min, top_min =  top_min)
    the_lt1 <- the_lt %>% select(age, the_Ex = Ex) 
    
    set.seed(seed)
    
    ## Iterations
    for (i in seq(times)) {
      d1 <- data %>%
        sample_n(., size = size, replace = T) %>%
        # make sure having the same bottom
        lifetable_0(., top = tail(the_lt$age, 1)) %>%
        select(age, dx, Ex) 
      # take the life table only if all year interval have 'cell_min' or more observations
      if (d1$age[1] == 0 & sum(d1$dx < cell_min) == 0) {
        names(d1)[3] <- str_c("Ex", i)
        the_lt1 <- left_join(the_lt1, d1[, c(1, 3)], by = "age")
      }
    }
    
    ll <- (1-ci)/2
    ul <- 1-ll
    
    ## The confidence interval
    the_lt1 <- the_lt1 %>% 
      # remove columns with NA
      select_if(~ !any(is.na(.))) %>%
      mutate_at(vars(starts_with("Ex")), ~.x-the_Ex) %>%
      select(starts_with("Ex")) %>%
      mutate(lb = apply(., 1, function(x) quantile(x, ll, na.rm = T)),
             ub = apply(., 1, function(x) quantile(x, ul, na.rm = T))) %>%
      select(lb, ub) %>%
      mutate(age = the_lt1$age, ex = the_lt1$the_Ex) %>%
      mutate(lci = lb + ex, uci = ub + ex) %>%
      full_join(the_lt, ., by = c("age", "Ex" = "ex"))
    
    return(the_lt1)
  }

