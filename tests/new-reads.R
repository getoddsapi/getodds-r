library(getodds)
fixtures<-jsonlite::fromJSON(system.file('extdata','read-fixtures.json',package='getodds'),simplifyVector=FALSE)
reply<-function(body) list(status=200,headers=list(),body=charToRaw(jsonlite::toJSON(body,auto_unbox=TRUE,null='null')))
expect_error<-function(expr,code) {e<-tryCatch({force(expr);NULL},getodds_error=function(e)e);stopifnot(inherits(e,'getodds_error'),identical(e$code,code))}
seen<-0
for(fixture in fixtures) {
 resource<-fixture[[1]];options<-fixture[[2]];body<-fixture[[3]]
 transport<-function(url,api_key) {
  seen<<-seen+1
  stopifnot(api_key=='test-key',!grepl('offset=',url,fixed=TRUE),!grepl('test-key',url,fixed=TRUE))
  if(resource%in%c('events/{id}/state','participants','participants/{id}')) stopifnot(!grepl('limit=',url,fixed=TRUE))
  if(resource=='odds/replay') stopifnot(grepl('after=9007199254740993',url,fixed=TRUE))
  if(resource=='sportsbook/markets') stopifnot(grepl('league=NBA',url,fixed=TRUE),grepl('source=fanduel',url,fixed=TRUE))
  reply(body)
 }
 result<-do.call(getodds_read,c(list(resource=resource,api_key='test-key',.transport=transport),options))
 stopifnot(identical(result$data,body$data))
 malformed<-body;malformed$meta$source<-'wrong'
 expect_error(do.call(getodds_read,c(list(resource=resource,api_key='test-key',.transport=function(...)reply(malformed)),options)),'INVALID_RESPONSE')
}
stopifnot(seen==8)
never<-function(...)stop('Network must not run')
for(args in list(list('odds/replay',after=9007199254740993),list('odds/replay',after='9223372036854775808'),list('odds/replay',after='2',through='1'),list('odds/replay',after='01'),list('odds/replay',offset=0),list('sportsbook/markets',limit=101),list('sportsbook/markets',source='draftkings'),list('events/live',status='open'),list('participants',event_id='invalid'),list('participants/{id}',event_id='espn-draftkings:espn:NBA:123',id='espn:NFL:team:1'))) expect_error(do.call(getodds_read,c(args,list(api_key='fixture',.transport=never))),'INVALID_QUERY')
f<-fixtures[[4]];b<-f[[3]];b$pagination$next_after<-9007199254740994
expect_error(do.call(getodds_read,c(list(resource=f[[1]],api_key='fixture',.transport=function(...)reply(b)),f[[2]])),'INVALID_RESPONSE')
f<-fixtures[[8]];b<-f[[3]];b$data[[1]]$qualifying_market_observed_at<-'2026-11-06T23:44:59.000Z'
expect_error(do.call(getodds_read,c(list(resource=f[[1]],api_key='fixture',.transport=function(...)reply(b)),f[[2]])),'INVALID_RESPONSE')
f<-fixtures[[5]];b<-f[[3]];b$pagination$has_more<-TRUE;b$pagination$next_after<-'wrong-id'
expect_error(do.call(getodds_read,c(list(resource=f[[1]],api_key='fixture',.transport=function(...)reply(b)),f[[2]])),'INVALID_RESPONSE')
cat('Eight new R reads preserve native IDs, clocks, scoped routes and exact string replay cursors.\n')
