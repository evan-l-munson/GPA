
# Function Information ----------------------------------------------------

# Purpose - The purpose of this script is to get all the commits associated 
#   with the DevForce GitLab projects.

# Description - This script first gets all the DevForce GitLab projects.  It 
#   then runs through a 'for' loop by project id to get all the commits 
#   associated with that specific project.

# Result - A tibble with all commits associated with every DevForce Gitlab 
#   project.

# Note - This script will take anywhere from 5 to 20 minutes to run, depending 
#   on connection speed and the ability to access the DevForce GitLab API with
#   the `gitlabr` package.

# Libraries ---------------------------------------------------------------

library(gitlabr)
library(janitor)
library(dplyr)
library(tidyr)
library(tictoc)

# Connect to API ----------------------------------------------------------

tictoc::tic()

# DSD - GitLab 
dsd_url <- "https://gitlab.devforce.disa.mil/netc-dsd"

# connect as a fixed user to a GitLab instance for the session
gitlabr::set_gitlab_connection(
  gitlab_url = dsd_url,
  private_token = Sys.getenv("GITLAB_COM_TOKEN"))

# Get Projects ------------------------------------------------------------

# All DevForce GitLab Projects
gl_projects <- 
  gitlabr::gl_list_projects(
    max_page = 200, 
    owner = TRUE) %>% 
  janitor::clean_names()

# select only groups
gl_groups <- 
  gl_projects %>% 
  dplyr::filter(namespace_kind == "group")

# pull out project ids
#   filter out those groups that have not had a single commit to the 
#   default_branch
group_ids <- gl_groups %>%
  dplyr::filter(!is.na(default_branch)) %>% 
  dplyr::pull(id)


# Set up 'for' loop -------------------------------------------------------

# create empty tibble for the results to be apended to
all_commits <- tidyr::tibble(
  id = as.character(),
  short_id = as.character(),
  created_at = as.character(),
  parent_ids = as.character(),
  title = as.character(),
  message = as.character(),
  author_name = as.character(),
  author_email = as.character(),
  authored_date = as.character(),
  committer_name = as.character(),
  committer_email = as.character(),
  committed_date = as.character(),
  web_url = as.character()
)

# configure progress bar
pb <- progress::progress_bar$new(
  format = "[:bar] :current/:total (:percent)",
  total = length(group_ids))

# for loop pulling the commits associated with every project
for (i in group_ids) {
  
  commits <- gitlabr::gl_get_commits(project = i) %>% 
    dplyr::select(id,
                  short_id,
                  created_at,
                  title,
                  message,
                  author_name,
                  author_email,
                  authored_date,
                  committer_name,
                  committer_email,
                  committed_date) %>% 
    dplyr::mutate(proj_id = i)
  
  pb$tick()
  
  all_commits <- rbind(all_commits, commits)
  
}

tictoc::toc()

save(all_commits, 
     file = "data/all_gl_commits.rda")
