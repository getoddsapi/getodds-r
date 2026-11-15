# Native metadata, string-cursor replay and FanDuel keyset reads.
getodds_new_filters <- list(
 "events/{id}/state"="id", participants="event_id", "participants/{id}"=c("id","event_id"),
 "odds/replay"=c("after","through","event_ids","sport","market"),
 "sportsbook/markets"=c("after","event_id","market_id","league","family","status"),
 outrights=c("after","event_id","market_id","league","family","status"),
 "outrights/odds"=c("after","event_id","market_id","league","family","status"),
 "events/live"=c("after","event_id","league","sport"))
getodds_metadata_resources <- c("events/{id}/state","participants","participants/{id}")
getodds_fd_resources <- c("sportsbook/markets","outrights","outrights/odds","events/live")
getodds_seq <- function(x) getodds_text(x) && grepl("^(0|[1-9][0-9]{0,18})$",x) && (nchar(x)<19 || x<="9223372036854775807")
getodds_seq_le <- function(a,b) nchar(a)<nchar(b) || (nchar(a)==nchar(b) && a<=b)
getodds_fd_id <- function(x) getodds_text(x) && grepl("^[A-Za-z0-9_.:-]{1,180}$",x)
getodds_event_id <- function(x) getodds_text(x) && grepl("^espn-draftkings:espn:(NBA|NFL|NHL|NCAAF|NCAAB|MLB|EPL|MLS):[0-9]{1,20}$",x)
getodds_new_query <- function(resource,options,limit) {
 bad <- function() getodds_error("INVALID_QUERY")
 if(length(options) && (is.null(names(options)) || any(!nzchar(names(options))) || anyDuplicated(names(options)) || any(!names(options)%in%c("source",getodds_new_filters[[resource]])))) bad()
 source <- if(resource%in%getodds_fd_resources) "fanduel" else "draftkings"
 if("source"%in%names(options) && !identical(options$source,source)) bad()
 options$source <- NULL;path <- resource;event_id <- NULL;participant_id <- NULL
 if(resource%in%getodds_metadata_resources) {
  event_id <- if(resource=="events/{id}/state") options$id else options$event_id
  if(!getodds_event_id(event_id)) bad()
  if(resource=="events/{id}/state") {path<-paste0("events/",utils::URLencode(event_id,reserved=TRUE),"/state");options$id<-NULL}
  if(resource=="participants/{id}") {
   participant_id<-options$id;league<-strsplit(event_id,":",fixed=TRUE)[[1]][3]
   if(!getodds_text(participant_id)||!grepl(paste0("^espn:",league,":team:[0-9]{1,20}$"),participant_id)) bad()
   path<-paste0("participants/",utils::URLencode(participant_id,reserved=TRUE));options$id<-NULL
  }
 } else if(resource=="odds/replay") {
  if(!"after"%in%names(options)) options$after<-"0"
  if(!getodds_seq(options$after) || ("through"%in%names(options) && (!getodds_seq(options$through)||!getodds_seq_le(options$after,options$through)))) bad()
  if("sport"%in%names(options) && (!getodds_text(options$sport)||!options$sport%in%getodds_leagues)) bad()
  if("market"%in%names(options) && (!getodds_text(options$market)||!options$market%in%c("moneyline","spread","total"))) bad()
  if("event_ids"%in%names(options)) {
   ids<-options$event_ids
   if(getodds_text(ids) && grepl(",",ids,fixed=TRUE)) {
    if(!grepl("^[-A-Za-z0-9_:]{1,180}(,[-A-Za-z0-9_:]{1,180}){1,19}$",ids)) bad()
    ids<-strsplit(ids,",",fixed=TRUE)[[1]]
   }
   if(!is.character(ids)||length(ids)<1||length(ids)>20||!all(vapply(ids,getodds_id,logical(1)))||anyDuplicated(ids)) bad()
   options$event_ids<-paste(ids,collapse=",")
  }
 } else {
  for(field in names(options)) if(!getodds_fd_id(options[[field]])) bad()
  if("status"%in%names(options) && !options$status%in%c("open","suspended","closed","removed")) bad()
 }
 query<-c(list(source=source),options)
 if(!resource%in%getodds_metadata_resources) query$limit<-limit
 list(path=path,query=query,source=source,event_id=event_id,participant_id=participant_id)
}
getodds_validate_new <- function(body,resource,limit,input) {
 bad<-function() getodds_error("INVALID_RESPONSE")
 nullable_date<-function(x) is.null(x)||getodds_date(x)
 nullable_number<-function(x) is.null(x)||getodds_number(x)
 if(!is.list(body)||!getodds_text(body$request_id)||!is.list(body$data)||length(body$data)>limit||!is.list(body$meta)||!identical(body$meta$source,input$source)) bad()
 if(resource%in%getodds_metadata_resources) {
  m<-body$meta
  if(!identical(m$metadata_provider,"espn")||!identical(m$settlement_verified,FALSE)||!identical(m$canonical_cross_provider_identity,FALSE)||!getodds_date(m$observed_at)||!is.null(m$source_updated_at)) bad()
  if(resource!="participants" && length(body$data)!=1) bad()
  ids<-character()
  for(row in body$data) {
   if(!is.list(row)||!identical(row$event_id,input$event_id)) bad()
   if(resource=="events/{id}/state") {
    if(!getodds_text(row$state)||!row$state%in%c("pre","in","post")||!getodds_bool(row$completed)||(row$completed&&row$state!="post")||!getodds_date(row$starts_at)||!getodds_text(row$source_status)) bad()
   } else {
    if(!getodds_text(row$id)||!getodds_text(row$native_id)||!getodds_text(row$name)||!getodds_text(row$league)||!identical(row$id,paste0("espn:",row$league,":team:",row$native_id))||!identical(row$identity_provider,"espn")||!getodds_text(row$side)||!row$side%in%c("home","away")) bad()
    if(resource=="participants/{id}"&&!identical(row$id,input$participant_id)) bad()
    ids<-c(ids,row$id)
   }
  }
  if(anyDuplicated(ids)) bad()
 } else {
  p<-body$pagination
  if(!is.list(p)||!getodds_integer(p$limit)||p$limit!=limit||!getodds_bool(p$has_more)||!getodds_date(body$meta$checked_at)) bad()
  if(resource=="odds/replay") {
   if(!identical(body$meta$source_capture_complete,FALSE)||!getodds_seq(p$after)||!identical(p$after,input$query$after)||!getodds_seq(p$through)||!getodds_seq(p$next_after)||!getodds_bool(p$replay_complete)||p$replay_complete==p$has_more||!getodds_seq_le(p$after,p$next_after)||!getodds_seq_le(p$next_after,p$through)) bad()
   if(!is.null(input$query$through)&&!identical(p$through,input$query$through)) bad()
   last<-p$after
   for(row in body$data) {
    if(!is.list(row)||!getodds_seq(row$seq)||getodds_seq_le(row$seq,last)||!getodds_seq_le(row$seq,p$through)||!getodds_date(row$observed_at)||!getodds_number(row$decimal)||row$decimal<=1||!getodds_integer(row$american)||abs(row$american)<100) bad()
    last<-row$seq
   }
   if(p$has_more && (length(body$data)==0||!identical(p$next_after,last))) bad()
   if(!p$has_more&&!identical(p$next_after,p$through)) bad()
  } else {
   if(!identical(body$meta$identity,"source_native")||!identical(body$meta$capture_complete,FALSE)||!identical(p$snapshot_consistent,FALSE)||!"next_after"%in%names(p)) bad()
   if(p$has_more) {if(!getodds_fd_id(p$next_after)||length(body$data)==0||!identical(p$next_after,body$data[[length(body$data)]]$id)) bad()} else if(!is.null(p$next_after)) bad()
   ids<-character()
   for(row in body$data) {
    if(!is.list(row)||!getodds_fd_id(row$id)||!getodds_date(row$starts_at)||!"event_read_at"%in%names(row)||!nullable_date(row$event_read_at)) bad()
    ids<-c(ids,row$id)
    if(resource=="events/live") {
     if(!identical(row$observed_in_play,TRUE)||!getodds_bool(row$metadata_ambiguous)||!getodds_date(row$qualifying_market_observed_at)||!getodds_date(row$qualifying_market_changed_at)||!identical(body$meta$completion_available,FALSE)) bad()
     age<-getodds_timestamp(body$meta$checked_at)-getodds_timestamp(row$qualifying_market_observed_at)
     if(age<0||age>900||any(c("selections","american","decimal","completed","score")%in%names(row))) bad()
    } else {
     if(!getodds_fd_id(row$event_id)||!getodds_bool(row$is_future)||row$is_future!=(resource!="sportsbook/markets")||!getodds_text(row$market_status)||!row$market_status%in%c("open","suspended","closed","removed")||!getodds_date(row$observed_at)||!getodds_date(row$changed_at)) bad()
     if(resource=="outrights") {if(!getodds_integer(row$selection_count)||row$selection_count<0||"selections"%in%names(row)) bad()} else {
      if(!is.list(row$selections)||length(row$selections)>1000) bad()
      for(selection in row$selections) if(!is.list(selection)||!getodds_fd_id(selection$id)||!all(c("american","decimal","line")%in%names(selection))||!all(vapply(selection[c("american","decimal","line")],nullable_number,logical(1)))||!getodds_text(selection$status)||!selection$status%in%c("active","suspended")) bad()
     }
    }
   }
   if(anyDuplicated(ids)) bad()
  }
 }
 body
}
