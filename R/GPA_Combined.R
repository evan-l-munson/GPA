

# Libraries ---------------------------------------------------------------

library(gitlabr)
library(janitor)
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)
library(waffle)

# Connect to API ----------------------------------------------------------

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

tictoc::tic()

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

# select, filter, format, clean raw data
#   select - remove the majority of the columns for faster processing
#   filter - out all user name spaces to leave only groups
#   format - create L1/2/3 levels to assist is bucketing
#   clean - general cleaning as required
prep_gpa <- gl_projects %>% 
  dplyr::select(
    id:default_branch,
    forks_count:namespace_full_path,
    packages_enabled:visibility) %>% 
  dplyr::filter(namespace_kind == "group") %>% 
  tidyr::separate(
    col = namespace_full_path, 
    sep = "/", 
    into = c("L1", "L2", "L3"), 
    remove = FALSE, 
    convert = TRUE) %>% 
  dplyr::mutate(
    L2 = dplyr::if_else(
      condition = is.na(L2), 
      true = L1, 
      false = L2),
    L3 = dplyr::if_else(
      condition = is.na(L3), 
      true = L2, 
      false = L3)) %>% 
  dplyr::mutate(
    last_activity_at = lubridate::as_date(last_activity_at),
    created_at = lubridate::as_date(created_at))

# reduce the columns from prep_gpa to prepare for joining with commit data
reduce_gpa <- prep_gpa %>% 
  dplyr::select(id, 
                name, 
                L1, 
                L2, 
                L3)

# join key information with the all_commits data
prep_commits <- dplyr::inner_join(
  x = all_commits,
  y = reduce_gpa,
  by = c("proj_id" = "id"))

# helper to to allow easy integration with seletInputs
filter_commits <- prep_commits %>% 
  dplyr::filter(
    L1 == "netc-dsd",
    L2 == "pmo") %>%
  dplyr::group_by(committer_name,
                  name) %>% 
  dplyr::count()

# Commits - Person - Project ----------------------------------------------

# initiate and build plot 
#   display the number of commits by project by committer
project_committer <- filter_commits %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = committer_name, 
    y = name, 
    size = n))

# dig through the "guts" of the ggplot2 object to extract the row number 
#   associated with the ggplot_build --> project --> panel --> range 
#   this is to enable a user to select of the project so it can be referenced 
#   and plotted to easily see who committed to a project.
gg_guts <- as.data.frame(ggplot_build(project_committer)$layout$panel_scales_y[[1]]$range$range) %>% 
  dplyr::mutate(id = rownames(.),
                id = as.numeric(id)) %>% 
  dplyr::rename("project_name" = 1) %>% 
  dplyr::filter(project_name == "SUIT")

# add all the details to the plot
project_commiter_plot <- project_committer +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = gg_guts$id), 
    color = "#e24329",
    alpha = 0.5) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = element_text(
      angle = 300, 
      hjust = 1)) +
  ggplot2::labs(
    title = "Number of Commits by Person by Project", 
    x = "Committer Name", 
    y = "Project Name")

# plotlyify the ggplot
plotly::ggplotly(project_commiter_plot)

# High Roller -------------------------------------------------------------

# number of commits to a DSD project by person
high_roller <- filter_commits %>% 
  dplyr::group_by(committer_name) %>% 
  dplyr::summarise(total_commits = sum(n)) %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = total_commits, 
    y = reorder(stringr::str_wrap(committer_name, 30), 
                total_commits))) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = paste("Top Committers to DSD Projects aka 'High Roller'"), 
    x = "Number of Commits", 
    y = "Committer Name")

plotly::ggplotly(high_roller)


# Dot Plot ----------------------------------------------------------------

# run lines 101 to 138

# helper to to allow easy integration with seletInputs
filter_dot <- prep_commits %>% 
  dplyr::filter(
    L1 == "netc-dsd",
    L2 == "netc-dsd") 

# initiate and build plot 
#   display the number of commits by project by committer
project_committer <- filter_dot %>% 
  dplyr::mutate(
    committed_date = lubridate::as_date(committed_date),
    day = lubridate::round_date(
      x = committed_date,
      unit =  "day")) %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = day, 
    y = name)) +
  ggplot2::geom_point()
