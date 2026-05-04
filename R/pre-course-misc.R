setup_github_teams <- function(
  data,
  organization
) {
  checkmate::check_data_frame(data)
  checkmate::check_names(
    data,
    permutation.of = c("username", "team_names")
  )

  # Invite to the org
  ghclass::org_invite(organization, data$username)
  Sys.sleep(2)

  # Create the teams
  ghclass::team_create(
    organization,
    unique(data$team_names)
  )
  Sys.sleep(2)

  # Invite users to the team
  ghclass::team_invite(
    organization,
    data$username,
    data$team_names,
    team_type = "slug"
  )
}
