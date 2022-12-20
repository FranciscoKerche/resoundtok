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
  original_data <- corpus::read_ndjson(.x)

  text_on_screen <- purrr::map(original_data$data.stickersOnItem, list(1, "stickerText")) |>
    purrr::map_chr(paste0, collapse = " ")

  challenge_title <- purrr::map_depth(original_data$data.challenges, .depth = 2, "title") |>
    purrr::map_chr(paste0, collapse = "; ")

  sticker_name <- purrr::map_depth(original_data$data.effectStickers, .depth = 2, "name") |>
    purrr::map_chr(paste0, collapse = "; ")

  sticker_id = purrr::map_depth(original_data$data.effectStickers, .depth = 2, "ID") |>
    purrr::map_chr(paste0, collapse = "; ")

  table_read <- original_data |>
    dplyr::tibble() |>
    dplyr::select(item_id, source_url, source_platform_url, user_avatar = data.author.avatarMedium,
                  user_id = data.author.id, username = data.author.nickname, user_private = data.author.privateAccount,
                  bio = data.author.signature, user = data.author.uniqueId, verified = data.author.verified,
                  video_id = data.id, user_like_count = data.authorStats.diggCount, user_followers = data.authorStats.followerCount,
                  user_following = data.authorStats.followingCount, user_total_likes = data.authorStats.heartCount,
                  user_video_count = data.authorStats.videoCount, description = data.desc, comment_count = data.stats.commentCount,
                  like_count = data.stats.diggCount, play_count = data.stats.playCount, share_count = data.stats.shareCount,
                  created_at = data.createTime, music_title = data.music.title, music_album = data.music.album, music_author = data.music.authorName,
                  music_cover = data.music.coverLarge, music_url = data.music.playUrl, video_duration = data.video.duration,
                  video_cover = data.video.originCover, music_duration = data.music.duration)
  make_clean <- function(.x){
    if(length(.x) == 0){
      clean <- rep("", nrow(table_read))
    } else{
      clean <- .x
    }
    return(clean)
  }

  result <- table_read |>
    dplyr::mutate(video_text = make_clean(text_on_screen),
                  challenges = make_clean(challenge_title),
                  effect_name = make_clean(sticker_name),
                  effect_id = make_clean(sticker_id),
                  created_at = purrr::invoke_map_int(as.integer, created_at),
                  engagement = like_count + comment_count + share_count)|>
    tidyr::unnest(created_at) |>
    dplyr::mutate(created_at = lubridate::as_datetime(created_at))
  return(result)
}

#' Make sure not to have overlapping data from multiple tiktok imports
#'
#' @description
#' The function will get a tiktok input and update it to the highest value in "vanity metrics", such as likes
#' comments, shares and other aspects. This will allow the user to create multiple extractions
#' and simplify the process of curating which ones would overlap.
#'
#' @param .x table. A table of tiktok data structured with the read_tok function.
#'
#' @examples
#' latest_tok(dabloons)
#'
#' @export

latest_tok <- function(.x){
  strings <- .x |>
    dplyr::group_by(item_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(where(is.character), where(is.logical), created_at)

  numericals <- .x |>
    dplyr::mutate_if(is.integer, as.numeric) |>
    dplyr::group_by(item_id) |>
    dplyr::summarise_if(is.numeric, max)


  final <- numericals |>
    dplyr::left_join(strings)
  return(final)
}
