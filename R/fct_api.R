#' Retrieve API key from environment variables
#'
#' This function retrieves the value of an API key from the environment variables by its name.
#' The name must be "ECOPI_API_KEY"
#'
#' @return A character string representing the value of the API key.
#' @noRd
get_ecopiapi_key <- function() {
  key <- Sys.getenv("ECOPI_API_KEY")
  if (identical(key, "")) {
    stop("No key found")
  }
  key
}


#' Extract error message from JSON body of an HTTP response
#'
#' @param resp An HTTP response object.
#' @return A named character vector representing the flattened JSON content of the HTTP response body.
#' @examples
#' \dontrun{
#' # Assuming an HTTP response object 'response' with a JSON error body
#' error_body <- ecopi_error_body(response)
#' }
#' @export

ecopi_error_body <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    unlist()
}


#' Make API requests to the Ecopi API
#'
#' This function sends an API request to the Ecopi API with the specified resource, parameters, and optional arguments.
#' @import httr2
#' @export


ecopi_api <- function(resource, ..., params = list(), new_data = list(), file_path) {
  params <- lapply(params, paste, collapse = ",")
  new_data <- lapply(new_data, function(x) if (identical(x, "")) jsonlite::unbox(NULL) else paste(x, collapse = ","))
  req <- request("https://api.ecopi.de/api/v0.1") |>
    req_headers(Authorization = paste("Token", get_ecopiapi_key())) |>
    req_user_agent("ecopiapi") |>
    req_error(body = ecopi_error_body) |>
    req_template(resource, ...) |>
    req_url_query(!!!params) |>
    req_body_json(new_data)

  if (missing(file_path)) {
    req_perform(req)
  } else {
    media <- curl::form_file(file_path)
    req <- req_body_multipart(req, image = media, media = media)
    req_perform(req)
  }
}

#' Convert API Response Body to Data Frame
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_status
#' @importFrom httr2 resp_has_body
resp_body_json_to_df <- function(api_response) {
  resp_body_not_empty <- resp_has_body(api_response)
  if (resp_body_not_empty) {
    api_response |>
      resp_body_json(simplifyVector = TRUE)
  } else {
    warning("The response does not contain a body. Status code: ", resp_status(api_response))
    NULL
  }
}


# Detections ------------------------------------------------------------------------------------------------------
get_detections <- function(...) {
  params <- list(...)
  ecopi_api("GET /detections/", params = params) |>
    resp_body_json_to_df()
}


# Summaryfiles --------------------------------------------------------------------------------------------------
get_recorderspeciescounts <- function(project_name, ...) {
  params <- list(...)
  ecopi_api("GET /meta/project/{project_name}/detections/recorderspeciescounts/", project_name = project_name, params = params) |>
    resp_body_json_to_df()
}


