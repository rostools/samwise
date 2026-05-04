#' Create teams and assign learners to them.
#'
#' @param data Preworkshop survey data.
#' @param team_names Character vector that has the names to group into.
#'
#' @return A [tibble::tibble].
#' @export
#'
create_teams <- function(data, group_names) {
  data |>
    dplyr::select(
      tidyselect::contains("full_name"),
      tidyselect::matches("user_?name"),
      tidyselect::contains("perceive")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(
      tidyselect::contains("perceive"),
      as.numeric
    )) |>
    dplyr::mutate(
      perceived_skill_score = sum(
        dplyr::c_across(tidyselect::contains(
          "perceive"
        )),
        na.rm = TRUE
      ),
      score_cutoff = stats::median(perceived_skill_score, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      team_names = (perceived_skill_score >= score_cutoff) |>
        randomizr::block_ra(conditions = group_names) |>
        as.character()
    ) |>
    dplyr::select(
      team_names,
      tidyselect::contains("full_name"),
      username = tidyselect::matches("user_?name"),
      perceived_skill_score
    ) |>
    dplyr::arrange(team_names, perceived_skill_score)
}

create_gh_instructor_team <- function(usernames, organization) {
  ghclass::org_invite(organization, usernames)
  ghclass::team_create(organization, "Helpers")
  ghclass::team_invite(
    organization,
    usernames,
    "Helpers"
  )

  ghclass::org_repos(organization) |>
    purrr::walk(
      ~ {
        ghclass::repo_team_permission(
          repo = .x,
          team = "Helpers",
          permission = "admin"
        )
      }
    )
}
