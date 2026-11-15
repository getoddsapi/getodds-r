library(getodds)
stamp <- "2026-11-07T00:00:00.000Z"
expect_error <- function(expr,code) {
  caught <- tryCatch({force(expr);NULL},getodds_error=function(e)e)
  stopifnot(inherits(caught,"getodds_error"),identical(caught$code,code))
  invisible(caught)
}
fixture <- list(request_id="fixture",data=list(),meta=list(source="draftkings",transport="stored_espn_draftkings",snapshot_consistent=FALSE,checked_at=stamp),pagination=list(limit=2,offset=4,has_more=FALSE,next_offset=NULL))
reply <- function(body,status=200,headers=list()) list(status=status,headers=headers,body=charToRaw(jsonlite::toJSON(body,auto_unbox=TRUE,null="null")))
calls <- 0
transport <- function(url,api_key) {
  calls <<- calls+1
  stopifnot(startsWith(url,"https://getoddsapi.com/api/v1/"),!grepl("fixture-key",url),api_key=="fixture-key")
  reply(fixture)
}
for (resource in c("events","odds","history","sports","bookmakers","market-types")) {
  args <- list(resource=resource,limit=2,offset=4,api_key="fixture-key",.transport=transport)
  if(resource=="history") args$offer_id <- "source:fixture"
  result <- do.call(getodds_read,args)
  stopifnot(length(result$data)==0,result$pagination$offset==4)
}
stopifnot(calls==6)
for(options in list(list(limit=TRUE),list(limit=201),list(offset=10001),list(sport="OTHER"),list(fresh_only="true"),list(event_id="x&y"),list(unsupported="value"))) {
  expect_error(do.call(getodds_read,c(list(resource="odds",api_key="fixture-key",.transport=transport),options)),"INVALID_QUERY")
}
expect_error(getodds_read("history",api_key="fixture-key",.transport=transport),"HISTORY_ID_REQUIRED")
expect_error(getodds_read("odds",api_key="bad\nkey",.transport=transport),"INVALID_API_KEY")
stopifnot(calls==6)
refused <- function(...) reply(list(internal="must not surface"),429,list("retry-after"="60"))
error <- expect_error(getodds_read("odds",api_key="fixture",.transport=refused),"HTTP_ERROR")
stopifnot(error$status==429,error$retry_after==60,!grepl("surface",conditionMessage(error)))
expect_error(getodds_read("odds",api_key="fixture",.transport=function(...) list(status=200,headers=list(),body=charToRaw("not json"))),"INVALID_RESPONSE")
expect_error(getodds_read("odds",api_key="fixture",.transport=function(...) list(status=200,headers=list(),body=raw(2*1024*1024+1))),"RESPONSE_TOO_LARGE")
row <- list(id="fixture",event_id="event",sport="NBA",market="moneyline",period="full",side="home",starts_at=stamp,observed_at=stamp,age_seconds=0,stale=FALSE,line=NULL,american=-110,decimal=1.909)
fixture$data <- list(row)
stopifnot(getodds_read("odds",limit=2,offset=4,api_key="fixture-key",.transport=transport)$data[[1]]$american==-110)
for(field in c("decimal","american","observed_at")) {
  malformed <- fixture;malformed$data[[1]][field] <- list(NULL)
  expect_error(getodds_read("odds",limit=2,offset=4,api_key="fixture",.transport=function(...)reply(malformed)),"INVALID_RESPONSE")
}
pp <- list(request_id="fixture",data=list(list(id="projection",source="prizepicks",player="Fixture",market="Points",line=22.5,odds=NULL,league="NBA",observed_at=stamp,starts_at=NULL,source_updated_at=NULL)),meta=list(source="prizepicks",complete=FALSE,observed_at=stamp))
stopifnot(getodds_read("markets",limit=1,api_key="fixture",.transport=function(...)reply(pp))$data[[1]]$line==22.5)
pp$data[[1]]$odds <- -110
expect_error(getodds_read("markets",api_key="fixture",.transport=function(...)reply(pp)),"INVALID_RESPONSE")
cat("R client: resource routing, filters, permissions transport, pagination, error sanitization, body limits and row validation passed.\n")

for (source in c("prizepicks","underdog","draftkings","fanduel")) {
  sample <- pp;sample$meta$source <- source;sample$data[[1]]$source <- source;sample$data[[1]]["odds"] <- list(NULL)
  result <- getodds_read("markets",source=source,api_key="fixture",.transport=function(url,api_key) {
    stopifnot(grepl(paste0("source=",source),url,fixed=TRUE));reply(sample)
  })
  stopifnot(identical(result$meta$source,source))
}
expect_error(getodds_read("odds",source="fanduel",api_key="fixture",.transport=transport),"INVALID_QUERY")
expect_error(getodds_read("markets",source="other",api_key="fixture",.transport=transport),"INVALID_QUERY")
sample <- pp;sample$data[[1]]["odds"] <- list(NULL)
expect_error(getodds_read("markets",source="underdog",api_key="fixture",.transport=function(...)reply(sample)),"INVALID_RESPONSE")

page <- function(data=list(),meta=list(),limit=2,offset=0) list(request_id="fixture",data=data,
  meta=modifyList(list(source="draftkings",transport="stored_espn_draftkings",snapshot_consistent=FALSE,checked_at=stamp),meta),
  pagination=list(limit=limit,offset=offset,has_more=FALSE,next_offset=NULL))
event <- list(id="event:1",sport="NBA",home="Home",away="Away",starts_at=stamp,observed_at=stamp,age_seconds=0,stale=FALSE)
historical <- list(league="NBA",from="2026-01-01T00:00:00Z",to="2026-01-02T00:00:00Z")
historical_meta <- c(historical[c("from","to")],list(event_state="unknown",finished_events_verified=FALSE,archive_complete=FALSE))
summary_meta <- list(independent_bookmakers=1,cross_book_comparison=FALSE,normalization="multiplicative recorded prices")
summary <- list(event_id="event:1",league="NBA",market="spread",period="game",reference_line=-3.5,line_orientation="home",
 expected_sides_present=TRUE,capture_times_coherent=TRUE,bookmaker_count=1,implied_probability_sum=1,
 outcomes=lapply(c("home","away"),function(side) list(side=side,line=if(side=="home") -3.5 else 3.5,
  best_decimal=2,average_decimal=2,median_decimal=2,worst_decimal=2,bookmaker_count=1,normalized_price_probability=0.5,
  quotes=list(list(offer_id=side,bookmaker="draftkings",decimal=2,american=100,observed_at=stamp)))))
cases <- list(
 list("events/search",list(q="  Home  "),page(list(event))),
 list("events/{id}",list(id="event:1"),page(list(event),limit=1)),
 list("odds/multi",list(event_ids=c("event:1","event:2")),page()),
 list("odds/updated",list(since=format(Sys.time()-60,"%Y-%m-%dT%H:%M:%OS3Z",tz="UTC")),page()),
 list("dropping-odds",list(min_drop_pct=1.5),page()),
 list("leagues",list(sport="basketball",upcoming_only=TRUE),page(list(list(id="NBA",name="NBA",sport="basketball",event_count=2,upcoming_event_count=1)))),
 list("historical/events",historical,page(list(event),historical_meta)),
 list("historical/events/search",c(historical,list(q="Home")),page(list(event),historical_meta)),
 list("odds/summary",list(event_id="event:1"),page(list(summary),summary_meta)),
 list("odds/summary/multi",list(event_ids="event:1,event:2"),page(list(summary),summary_meta))
)
advanced_calls <- 0
for (case in cases) {
 resource <- case[[1]];options <- case[[2]];body <- case[[3]]
 transport <- function(url,api_key) {
  advanced_calls <<- advanced_calls+1
  path <- if(resource=="events/{id}") "events/event:1" else resource
  decoded <- utils::URLdecode(url)
  stopifnot(startsWith(decoded,paste0("https://getoddsapi.com/api/v1/",path,"?")),api_key=="fixture",grepl("source=draftkings",decoded,fixed=TRUE))
  if(resource=="events/{id}") stopifnot(!grepl("limit=",decoded,fixed=TRUE))
  if("q" %in% names(options)) stopifnot(grepl(paste0("q=",trimws(options$q)),decoded,fixed=TRUE))
  if("event_ids" %in% names(options)) stopifnot(grepl("event_ids=event:1,event:2",decoded,fixed=TRUE))
  reply(body)
 }
 args <- c(list(resource=resource,api_key="fixture",.transport=transport),options)
 if(resource!="events/{id}") args$limit <- 2
 result <- do.call(getodds_read,args)
 stopifnot(length(result$data)==length(body$data),result$pagination$limit==body$pagination$limit)
}
stopifnot(advanced_calls==10)
refuse_network <- function(...) stop("network must not run")
invalid <- list(
 list("events/search",list(q="x")),list("events/search",list(q="ok",unknown=1)),
 list("events/{id}",list(id="a",limit=1)),list("events/{id}",list(id="../secret")),
 list("odds/multi",list(event_ids=c("a","a"))),list("odds/multi",list(event_ids=character())),
 list("odds/multi",list(event_ids=paste0("e",1:21))),list("odds/multi",list(event_ids="a,")),
 list("odds/updated",list(since="2020-01-01T00:00:00Z")),list("odds/updated",list(since=format(Sys.time()+86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))),
 list("dropping-odds",list(min_drop_pct=TRUE)),list("dropping-odds",list(min_drop_pct=100)),
 list("leagues",list(sport="NBA")),list("leagues",list(upcoming_only="true")),
 list("historical/events",modifyList(historical,list(from="2026-02-30T00:00:00Z"))),
 list("historical/events",modifyList(historical,list(to="2026-03-01T00:00:00Z"))),
 list("historical/events",list()),list("odds/summary",list()),list("odds/summary/multi",list(event_ids="a,")),
 list("leagues",list(source="draftkings",source="draftkings"))
)
for(case in invalid) expect_error(do.call(getodds_read,c(list(resource=case[[1]],api_key="fixture",.transport=refuse_network),case[[2]])),"INVALID_QUERY")
for(change in c("coverage","price","line","probability","sum","missing-side","coherence","american")) {
 body <- page(list(summary),summary_meta)
 if(change=="coverage") body$meta$cross_book_comparison <- TRUE
 if(change=="price") body$data[[1]]$outcomes[[1]]$best_decimal <- 3
 if(change=="line") body$data[[1]]$outcomes[[1]]$line <- -4.5
 if(change=="probability") body$data[[1]]$outcomes[[1]]$normalized_price_probability <- 0.9
 if(change=="sum") body$data[[1]]$implied_probability_sum <- 2
 if(change=="missing-side") body$data[[1]]$outcomes <- body$data[[1]]$outcomes[1]
 if(change=="coherence") body$data[[1]]$capture_times_coherent <- FALSE
 if(change=="american") body$data[[1]]$outcomes[[1]]$quotes[[1]]$american <- TRUE
 expect_error(getodds_read("odds/summary",event_id="event:1",limit=2,api_key="fixture",.transport=function(...)reply(body)),"INVALID_RESPONSE")
}
partial <- summary;partial$outcomes <- partial$outcomes[1];partial$expected_sides_present <- FALSE
partial["implied_probability_sum"] <- list(NULL);partial$outcomes[[1]]["normalized_price_probability"] <- list(NULL)
body <- page(list(partial),summary_meta)
stopifnot(is.null(getodds_read("odds/summary",event_id="event:1",limit=2,api_key="fixture",.transport=function(...)reply(body))$data[[1]]$implied_probability_sum))
body <- page(list(event),limit=1)
expect_error(getodds_read("events/{id}",id="wrong",api_key="fixture",.transport=function(...)reply(body)),"INVALID_RESPONSE")
cat("R advanced queries: all ten routes, bounds, source and event identity, historical coverage, summary math and incomplete outcomes passed.\n")
