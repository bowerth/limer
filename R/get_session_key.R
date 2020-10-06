#' Get a LimeSurvey API session key
#'
#' This function logs into the LimeSurvey API and provides an access session key.
#' @param username LimeSurvey username. Defaults to value set in \code{options()}.
#' @param password LimeSurvey password Defaults to value set in \code{options()}.
#' @return API token
#' @import httr
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @export
#' @examples \dontrun{
#' get_session_key()
#' }

get_session_key <- function(username = getOption('lime_username'),
                            password = getOption('lime_password')) {
  body.json = list(method = "get_session_key",
                   id = " ",
                   params = list(admin = username,
                                 password = password))

    # Need to use jsonlite::toJSON because single elements are boxed in httr, which
  # is goofy. toJSON can turn off the boxing automatically, though it's not
  # recommended. They say to use unbox on each element, like this:
  #   params = list(admin = unbox("username"), password = unbox("password"))
  # But that's a lot of extra work. So auto_unbox suffices here.
  # More details and debate: https://github.com/hadley/httr/issues/159
  r <- POST(getOption('lime_api'), content_type_json(),
            body = jsonlite::toJSON(body.json, auto_unbox = TRUE))

  ## deactivate SSL certificate validation:
  ##   httr::set_config(httr::config(ssl_verifypeer = 0L))
  ## in curl "-k" flag
  ##   curl -k -X POST -H "Content-Type: application/json" -d "{\"method\":\"get_session_key\",\"id\":\" \",\"params\":{\"admin\":\"$LIME_USER\",\"password\":\"$LIME_PASS\"}}" $LIME_API

  ## session_key <- as.character(jsonlite::fromJSON(content(r, encoding="utf-8"))$result)
  ## SAML adds html to JSON result
  session_key <- httr::content(r, encoding = "utf-8") %>%
      stringr::str_extract(pattern = "[{].+$") %>%
      jsonlite::fromJSON() %>%
      .[["result"]] %>%
      as.character()

  session_cache$session_key <- session_key
  session_key
}

# Start a new environment to hold the session key so all other functions can access it
# See http://trestletech.com/2013/04/package-wide-variablescache-in-r-package/
session_cache <- new.env(parent = emptyenv())
