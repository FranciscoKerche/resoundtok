#' Clean data from Zeeswchimer tiktok download
#'
#' @description
#' The function will clean and organize columns extracted from the the web-app zeeschwimer.
#'
#'
#' @param .x string. Path to data-set.
#'
#' @examples
#' read_tok("path/to/file")
#'
#' @export

read_tok <- function(.x){
  result <- corpus::read_ndjson(.x) |>
    dplyr::tibble() |>
    janitor::clean_names() |>
    dplyr::select(item_id, source_platform_url, description = data_desc, create_time = data_create_time, duration = data_video_duration,
           ratio = data_video_ratio, cover = data_video_cover, origin_cover = data_video_origin_cover,
           quality = data_video_format, music_id = data_music_id, music_title = data_music_title,
           music_cover = data_music_cover_medium, music_author = data_music_author_name, original_music = data_music_original,
           album_music = data_music_album, digg_count = data_stats_digg_count, share_count = data_stats_share_count,
           comment_count = data_stats_comment_count, play_count = data_stats_play_count) |>
    dplyr::mutate(create_time = purrr::invoke_map(as.numeric, create_time),
           author = stringr::str_sub(source_platform_url, nchar("https://www.tiktok.com/"), -1),
           engagement = digg_count + comment_count + share_count,
           reach = play_count) |>
    tidyr::unnest(create_time) |>
    dplyr::mutate(date = lubridate::as_datetime(create_time)) |>
    dplyr::mutate(post_url = stringr::str_c(source_platform_url, "/video/", item_id)) |>
    dplyr::rename(text = description)

  return(result)
}

