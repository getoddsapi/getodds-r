\name{getodds_read}
\alias{getodds_read}
\title{Read One Bounded GetOdds Response}
\usage{
getodds_read(resource = "markets", ..., limit = 50L, offset = 0L,
  api_key = Sys.getenv("GETODDS_API_KEY"), .transport = getodds_transport)
}
\arguments{
\item{resource}{One of markets, events, odds, history, sports, bookmakers, market-types, events/search, events/\{id\}, odds/multi, odds/updated, dropping-odds, leagues, historical/events, historical/events/search, odds/summary, odds/summary/multi, events/\{id\}/state, participants, participants/\{id\}, odds/replay, sportsbook/markets, outrights, outrights/odds and events/live.}
\item{...}{Optional source (prizepicks, underdog, draftkings or fanduel for markets; fanduel for native market/futures/live-event reads; draftkings for other sportsbook reads). Resource-specific filters include sport, event_id, offer_id, market, fresh_only, id, q, event_ids, since, min_drop_pct, league, from, to and upcoming_only. Batch event_ids accepts a character vector or comma-separated string of up to 20 distinct source IDs.}
\item{limit}{Integer from 1 to 200. FanDuel keyset reads permit at most 100. Omit for event lookup and metadata routes.}
\item{offset}{Sportsbook offset from 0 to 10000; zero for markets. Omit for event lookup, metadata, replay and FanDuel keyset routes.}
\item{api_key}{GetOdds key with the required source access. Keep it in an environment variable.}
\item{.transport}{Transport injection for tests. Normally leave unchanged.}
}
\description{Makes one bounded authenticated request without automatic retries or pagination.
Market reads are incomplete source-specific threshold samples: PrizePicks, Underdog, DraftKings Pick6 or FanDuel props. Sportsbook resources read stored DraftKings data via ESPN.
History requires an event or offer ID. Pages are not a frozen snapshot.
Event lookup requires id. Historical queries require league, from and to as UTC ISO timestamps
ending in Z, with at most 31 days per request and no future end time. Search requires q (2 to 100 characters).
Updated odds requires since within the last seven days. Summaries use one independent bookmaker;
normalization is mathematical, not a verified win probability. Historical event completion is unknown.
Metadata state requires id; participant listing requires event_id; participant detail requires both.
Replay after and through are decimal character strings bounded by signed 64-bit integer range;
numeric cursors are rejected to prevent precision loss. Replay and FanDuel use after keyset pagination.
FanDuel filters are exact and case sensitive. Live-event observations are bounded to 15 minutes,
not guarantees that an event is currently live. Future prices are not verified settlement results.}
\value{A list preserving data, meta, pagination where supplied, and request_id. Errors inherit
getodds_error and expose code, status and retry_after without raw response bodies.}
\examples{
\dontrun{
quotes <- getodds_read("odds", sport = "NBA", fresh_only = TRUE, limit = 5)
print(quotes$meta)
leagues <- getodds_read("leagues", sport = "basketball")
results <- getodds_read("historical/events/search", league = "NBA",
  from = "2026-01-01T00:00:00Z", to = "2026-01-02T00:00:00Z", q = "Lakers")
# Use an actual source event ID returned by the events endpoint.
summary <- getodds_read("odds/summary", event_id = "SOURCE_EVENT_ID")
}
}
