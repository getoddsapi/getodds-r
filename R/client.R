# Fixed-host, bounded client. No automatic retries or pagination.
getodds_error <- function(code, status = 0L, retry_after = NULL) {
  stop(structure(list(message = code, call = NULL, code = code,
    status = status, retry_after = retry_after),
    class = c("getodds_error", "error", "condition")))
}
getodds_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
getodds_number <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
getodds_integer <- function(x) getodds_number(x) && x == floor(x)
getodds_bool <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
getodds_id <- function(x) getodds_text(x) && grepl("^[-A-Za-z0-9_:]{1,180}$",x)
getodds_timestamp <- function(x, utc_only = FALSE) {
  if (!getodds_text(x) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,3})?(Z|[+-][0-9]{2}:[0-9]{2})$",x)) return(NA_real_)
  utc <- endsWith(x,"Z"); if (utc_only && !utc) return(NA_real_)
  base <- substr(x,1,19)
  parsed <- as.POSIXct(base,format="%Y-%m-%dT%H:%M:%S",tz="UTC")
  if (is.na(parsed) || format(parsed,"%Y-%m-%dT%H:%M:%S",tz="UTC") != base) return(NA_real_)
  zone <- if (utc) "Z" else substr(x,nchar(x)-5,nchar(x))
  suffix <- substr(x,20,nchar(x)-nchar(zone))
  fraction <- if (nzchar(suffix)) as.numeric(paste0("0",suffix)) else 0
  shift <- 0
  if (!utc) {
    hours <- as.integer(substr(zone,2,3)); minutes <- as.integer(substr(zone,5,6))
    if (hours > 23 || minutes > 59) return(NA_real_)
    shift <- (hours*3600+minutes*60)*if(substr(zone,1,1)=="+") 1 else -1
  }
  as.numeric(parsed)+fraction-shift
}
getodds_date <- function(x) is.finite(getodds_timestamp(x,TRUE))
getodds_leagues <- c("NFL","NBA","NHL","NCAAF","NCAAB","MLB","EPL","MLS")
getodds_categories <- c("basketball","american-football","ice-hockey","baseball","soccer")
getodds_filters <- list(
  markets=character(), events=c("sport","event_id"), odds=c("sport","event_id","market","fresh_only"),
  history=c("sport","event_id","offer_id","market"), sports=character(), bookmakers=character(), "market-types"=character(),
  "events/search"=c("q","sport"), "events/{id}"="id", "odds/multi"=c("event_ids","sport","market","fresh_only"),
  "odds/updated"=c("since","sport","market","fresh_only"), "dropping-odds"=c("sport","market","min_drop_pct"),
  leagues=c("sport","upcoming_only"), "historical/events"=c("league","from","to"),
  "historical/events/search"=c("league","from","to","q"), "odds/summary"=c("event_id","market"),
  "odds/summary/multi"=c("event_ids","market")
)
getodds_query <- function(resource, options, limit, offset) {
  bad <- function() getodds_error("INVALID_QUERY")
  if (length(options) && (is.null(names(options)) || any(!nzchar(names(options))) || anyDuplicated(names(options)) || any(!names(options) %in% c("source",getodds_filters[[resource]])))) bad()
  source <- if(resource=="markets") "prizepicks" else "draftkings"
  if ("source" %in% names(options)) {
    if (!getodds_text(options$source) || !options$source %in% (if(resource=="markets") c("prizepicks","underdog","draftkings","fanduel") else "draftkings")) bad()
    source <- options$source;options$source <- NULL
  }
  if ("sport" %in% names(options) && (!getodds_text(options$sport) || !options$sport %in% (if(resource=="leagues") getodds_categories else getodds_leagues))) bad()
  if ("league" %in% names(options) && (!getodds_text(options$league) || !options$league %in% getodds_leagues)) bad()
  if ("market" %in% names(options) && (!getodds_text(options$market) || !options$market %in% c("moneyline","spread","total"))) bad()
  for (field in c("id","event_id","offer_id")) if (field %in% names(options) && !getodds_id(options[[field]])) bad()
  for (field in c("fresh_only","upcoming_only")) if (field %in% names(options)) {
    if (!getodds_bool(options[[field]])) bad()
    options[[field]] <- tolower(as.character(options[[field]]))
  }
  if (resource=="history" && !any(c("event_id","offer_id") %in% names(options))) getodds_error("HISTORY_ID_REQUIRED")
  if (resource %in% c("events/search","historical/events/search")) {
    if (!getodds_text(options$q) || nchar(trimws(options$q))<2 || nchar(trimws(options$q))>100) bad()
    options$q <- trimws(options$q)
  }
  if (resource %in% c("odds/multi","odds/summary/multi")) {
    ids <- options$event_ids
    if (getodds_text(ids) && grepl(",",ids,fixed=TRUE)) {
      if (!grepl("^[-A-Za-z0-9_:]{1,180}(,[-A-Za-z0-9_:]{1,180}){1,19}$",ids)) bad()
      ids <- strsplit(ids,",",fixed=TRUE)[[1]]
    }
    if (!is.character(ids) || length(ids)<1 || length(ids)>20 || !all(vapply(ids,getodds_id,logical(1))) || anyDuplicated(ids)) bad()
    options$event_ids <- paste(ids,collapse=",")
  }
  if (resource=="odds/summary" && !getodds_id(options$event_id)) bad()
  if (resource=="odds/updated") {
    since <- getodds_timestamp(options$since);now <- as.numeric(Sys.time())
    if (!is.finite(since) || since>now || since<now-7*86400) bad()
  }
  if ("min_drop_pct" %in% names(options) && (!getodds_number(options$min_drop_pct) || options$min_drop_pct<0.1 || options$min_drop_pct>=100 || !grepl("^[0-9]+(\\.[0-9]{1,3})?$",as.character(options$min_drop_pct)))) bad()
  if (startsWith(resource,"historical/")) {
    start <- getodds_timestamp(options$from,TRUE);end <- getodds_timestamp(options$to,TRUE)
    if (!getodds_text(options$league) || !options$league %in% getodds_leagues || !is.finite(start) || !is.finite(end) || start>=end || end-start>31*86400 || end>as.numeric(Sys.time())) bad()
  }
  path <- resource; event_id <- NULL
  if (resource=="events/{id}") {
    if (!getodds_id(options$id)) bad()
    event_id <- options$id;path <- paste0("events/",utils::URLencode(event_id,reserved=TRUE));options$id <- NULL
    query <- c(list(source=source),options)
  } else {
    query <- c(list(source=source,limit=limit),options)
    if (resource!="markets") query$offset <- offset
  }
  list(path=path,query=query,source=source,event_id=event_id)
}
getodds_validate_summaries <- function(body) {
  bad <- function() getodds_error("INVALID_RESPONSE")
  nullable <- function(x) is.null(x) || getodds_number(x)
  one <- function(x) getodds_integer(x) && x==1
  meta <- body$meta
  if (!one(meta$independent_bookmakers) || !identical(meta$cross_book_comparison,FALSE) || !getodds_text(meta$normalization)) bad()
  groups <- character()
  for (row in body$data) {
    fields <- c("event_id","league","market","period")
    if (!all(fields %in% names(row)) || !all(vapply(row[fields],getodds_text,logical(1))) || !row$market %in% c("moneyline","spread","total") ||
        !all(c("reference_line","line_orientation","implied_probability_sum") %in% names(row)) || !nullable(row$reference_line) ||
        !one(row$bookmaker_count) || !getodds_bool(row$expected_sides_present) || !getodds_bool(row$capture_times_coherent) ||
        !nullable(row$implied_probability_sum) || !is.list(row$outcomes) || length(row$outcomes)<1) bad()
    orientation <- switch(row$market,spread="home",total="total",NULL)
    if (!identical(row$line_orientation,orientation) || is.null(row$reference_line)!=(row$market=="moneyline")) bad()
    identity <- jsonlite::toJSON(row[c("event_id","market","period","reference_line")],auto_unbox=TRUE,null="null")
    if (identity %in% groups) bad();groups <- c(groups,identity)
    complete <- row$expected_sides_present && row$capture_times_coherent
    if (complete) {if (!getodds_number(row$implied_probability_sum) || row$implied_probability_sum<=0) bad()}
    else if (!is.null(row$implied_probability_sum)) bad()
    sides <- character();clocks <- numeric();implied <- 0
    for (outcome in row$outcomes) {
      if (!is.list(outcome) || !getodds_text(outcome$side) || outcome$side %in% sides || !"line" %in% names(outcome) ||
          !nullable(outcome$line) || !one(outcome$bookmaker_count) || !is.list(outcome$quotes) || length(outcome$quotes)!=1) bad()
      sides <- c(sides,outcome$side);quote <- outcome$quotes[[1]]
      if (!is.list(quote) || !getodds_text(quote$offer_id) || !identical(quote$bookmaker,"draftkings") || !getodds_number(quote$decimal) || quote$decimal<=1 ||
          !getodds_integer(quote$american) || abs(quote$american)<100 || !getodds_date(quote$observed_at)) bad()
      for (field in c("best_decimal","average_decimal","median_decimal","worst_decimal")) if (!getodds_number(outcome[[field]]) || outcome[[field]]!=quote$decimal) bad()
      implied <- implied+1/quote$decimal;clocks <- c(clocks,getodds_timestamp(quote$observed_at))
      line <- if(row$market=="moneyline") NULL else if(row$market=="spread" && outcome$side=="away") -row$reference_line else row$reference_line
      if (is.null(outcome$line)!=is.null(line) || (!is.null(line) && outcome$line!=line)) bad()
      if (!"normalized_price_probability" %in% names(outcome)) bad()
      probability <- outcome$normalized_price_probability
      if (complete) {if (!getodds_number(probability) || probability<0 || probability>1 || abs(probability-(1/quote$decimal)/row$implied_probability_sum)>1e-9) bad()}
      else if (!is.null(probability)) bad()
    }
    if (row$capture_times_coherent!=(max(clocks)-min(clocks)<=60) || (complete && abs(implied-row$implied_probability_sum)>1e-9)) bad()
    expected <- if(row$market=="total") c("over","under") else if(row$market=="moneyline" && row$league %in% c("EPL","MLS")) c("home","away","draw") else c("home","away")
    if (any(!sides %in% expected) || row$expected_sides_present!=setequal(sides,expected)) bad()
  }
  invisible(NULL)
}
getodds_transport <- function(url, api_key) {
  handle <- curl::new_handle(timeout = 20, connecttimeout = 10, followlocation = FALSE)
  curl::handle_setheaders(handle, Authorization = paste("Bearer", api_key), Accept = "application/json")
  chunks <- list(); size <- 0L; too_large <- FALSE
  response <- tryCatch(curl::curl_fetch_stream(url, function(chunk) {
    size <<- size + length(chunk)
    if (size > 2 * 1024 * 1024) { too_large <<- TRUE; getodds_error("RESPONSE_TOO_LARGE") }
    chunks[[length(chunks) + 1L]] <<- chunk
  }, handle = handle), error = function(e) {
    if (too_large) getodds_error("RESPONSE_TOO_LARGE")
    getodds_error("NETWORK_ERROR")
  })
  list(status = response$status_code, headers = curl::parse_headers_list(response$headers),
    body = do.call(c, c(list(raw()), chunks)))
}
getodds_validate <- function(body, resource, limit, offset, source = "prizepicks") {
  invalid <- function() getodds_error("INVALID_RESPONSE")
  if (!is.list(body) || !getodds_text(body$request_id) || !is.list(body$meta) ||
      !is.list(body$data) || length(body$data) > limit) invalid()
  rows <- body$data; meta <- body$meta
  if (resource == "markets") {
    if (!identical(meta$source, source) || !identical(meta$complete, FALSE) || !getodds_date(meta$observed_at)) invalid()
    ids <- character()
    for (row in rows) {
      if (!is.list(row) || !all(vapply(row[c("id","player","market")], getodds_text, logical(1))) ||
          !identical(row$source,source) || !getodds_number(row$line) ||
          !"odds" %in% names(row) || !is.null(row$odds) || !getodds_date(row$observed_at)) invalid()
      for (field in c("starts_at","source_updated_at"))
        if (!field %in% names(row) || (!is.null(row[[field]]) && !getodds_date(row[[field]]))) invalid()
      if (!"league" %in% names(row) || (!is.null(row$league) && !getodds_text(row$league))) invalid()
      ids <- c(ids, row$id)
    }
    if (anyDuplicated(ids)) invalid()
  } else {
    if (!identical(meta$source,"draftkings") || !identical(meta$transport,"stored_espn_draftkings") ||
        !identical(meta$snapshot_consistent,FALSE) || !getodds_date(meta$checked_at)) invalid()
    page <- body$pagination
    if (!is.list(page) || !getodds_integer(page$limit) || page$limit != limit ||
        !getodds_integer(page$offset) || page$offset != offset ||
        !is.logical(page$has_more) || length(page$has_more) != 1L || is.na(page$has_more) ||
        !"next_offset" %in% names(page)) invalid()
    if (!is.null(page$next_offset) && (!page$has_more || !getodds_integer(page$next_offset) ||
        page$next_offset != offset + limit || page$next_offset > 10000)) invalid()
    original <- resource
    if (resource %in% c("events/search","events/{id}","historical/events","historical/events/search")) resource <- "events"
    if (resource %in% c("odds/multi","odds/updated","dropping-odds")) resource <- "odds"
    fields <- switch(resource, events = c("id","sport","home","away"),
      odds = c("id","event_id","sport","market","period","side"),
      history = c("seq","offer_id","event_id","sport","market","period","side"),
      "market-types" = c("market","period"), if(startsWith(resource,"odds/summary")) character() else c("id","name"))
    for (row in rows) {
      if (!is.list(row) || !all(fields %in% names(row)) || !all(vapply(row[fields],getodds_text,logical(1)))) invalid()
      if (resource %in% c("events","odds","history") &&
          (!getodds_date(row$observed_at) || !getodds_integer(row$age_seconds) || row$age_seconds < 0 ||
           !is.logical(row$stale) || length(row$stale) != 1L || is.na(row$stale))) invalid()
      if (resource %in% c("events","odds") && !getodds_date(row$starts_at)) invalid()
      if (resource %in% c("odds","history") &&
          (!getodds_number(row$decimal) || row$decimal <= 1 || !getodds_integer(row$american) || abs(row$american) < 100 ||
           !"line" %in% names(row) || (!is.null(row$line) && !getodds_number(row$line)))) invalid()
    }
    if (original=="events/{id}" && (length(rows)!=1 || page$has_more || !is.null(page$next_offset))) invalid()
    if (startsWith(original,"historical/") && (!identical(meta$event_state,"unknown") || !identical(meta$finished_events_verified,FALSE) || !identical(meta$archive_complete,FALSE) || !getodds_date(meta$from) || !getodds_date(meta$to))) invalid()
    for (row in rows) {
      if (original=="leagues" && (!getodds_text(row$sport) || !row$sport %in% getodds_categories || !getodds_integer(row$event_count) || row$event_count<0 || !getodds_integer(row$upcoming_event_count) || row$upcoming_event_count<0 || row$upcoming_event_count>row$event_count)) invalid()
      if (original %in% c("odds/updated","dropping-odds") && !getodds_date(row$changed_at)) invalid()
      if (original=="dropping-odds" && (!getodds_date(row$previous_observed_at) || !getodds_number(row$previous_decimal) || row$previous_decimal<=1 || !getodds_number(row$drop_pct) || row$drop_pct<=0 || row$drop_pct>=100)) invalid()
    }
    if (startsWith(original,"odds/summary")) getodds_validate_summaries(body)
  }
  body
}

#' Read one page or sample from the GetOdds gateway.
getodds_read <- function(resource = "markets", ..., limit = 50L, offset = 0L,
                         api_key = Sys.getenv("GETODDS_API_KEY"), .transport = getodds_transport) {
  resources <- c(names(getodds_filters),names(getodds_new_filters))
  if (!getodds_text(resource) || !resource %in% resources) getodds_error("INVALID_QUERY")
  if (!getodds_text(api_key) || grepl("[[:space:][:cntrl:]]",api_key)) getodds_error("INVALID_API_KEY")
  if (!getodds_integer(limit) || limit < 1 || limit > 200 || !getodds_integer(offset) || offset < 0 || offset > 10000) getodds_error("INVALID_QUERY")
  if (resource=="events/{id}") {
    if (!missing(limit) || !missing(offset)) getodds_error("INVALID_QUERY")
    limit <- 1L;offset <- 0L
  }
  if (resource=="markets" && offset!=0) getodds_error("INVALID_QUERY")
  new_resource <- resource %in% names(getodds_new_filters)
  if (new_resource) {
    if (!missing(offset)) getodds_error("INVALID_QUERY")
    if (resource %in% getodds_metadata_resources) {
      if (!missing(limit)) getodds_error("INVALID_QUERY")
      limit <- if(resource=="participants") 2L else 1L
    }
    if (resource %in% getodds_fd_resources && limit>100) getodds_error("INVALID_QUERY")
  }
  input <- if(new_resource) getodds_new_query(resource,list(...),limit) else getodds_query(resource,list(...),limit,offset)
  encode <- function(x) utils::URLencode(as.character(x),reserved=TRUE)
  url <- paste0("https://getoddsapi.com/api/v1/",input$path,"?",paste(paste0(names(input$query),"=",vapply(input$query,encode,character(1))),collapse="&"))
  response <- tryCatch(.transport(url,api_key),error = function(e) {
    if (inherits(e,"getodds_error")) stop(e)
    getodds_error("NETWORK_ERROR")
  })
  if (!is.list(response) || !getodds_integer(response$status)) getodds_error("INVALID_RESPONSE")
  if (response$status < 200 || response$status >= 300) {
    retry <- response$headers[["retry-after"]]
    delay <- if(getodds_text(retry) && grepl("^[0-9]{1,6}$",retry)) as.integer(retry) else NULL
    getodds_error("HTTP_ERROR",response$status,delay)
  }
  if (!is.raw(response$body)) getodds_error("INVALID_RESPONSE")
  if (length(response$body) > 2 * 1024 * 1024) getodds_error("RESPONSE_TOO_LARGE")
  body <- tryCatch(jsonlite::fromJSON(rawToChar(response$body),simplifyVector=FALSE),error=function(e) getodds_error("INVALID_RESPONSE"))
  result <- if(new_resource) getodds_validate_new(body,resource,limit,input) else getodds_validate(body,resource,limit,offset,input$source)
  if (resource=="events/{id}" && !identical(result$data[[1]]$id,input$event_id)) getodds_error("INVALID_RESPONSE")
  result
}
