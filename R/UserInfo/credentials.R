cookie_expiry <- 7

# This function must return a data.frame with columns user and sessionid.  Other columns are also okay
# and will be made available to the app after log in.

get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
    dbReadTable(conn, "sessions") %>%
        mutate(login_time = ymd_hms(login_time)) %>%
        as_tibble() %>%
        filter(login_time > now() - days(expiry))
}

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.

add_session_to_db <- function(user, sessionid, conn = db) {
    tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
        dbWriteTable(conn, "sessions", ., append = TRUE)
}

db <- dbConnect(SQLite(), ":memory:")
dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

user_base <- tibble(
    user = c("user1", "user2"),
    password = c("pass1", "pass2"),
    password_hash = sapply(c("pass1", "pass2"), sodium::password_store),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
)