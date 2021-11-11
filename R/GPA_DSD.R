
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


# select, filter, format, clean raw data
#   select - remove the mojority of the columns for faster processing
#   filter - out all user namespaces to leave only groups
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

names <- prep_gpa %>% 
  dplyr::group_by(namespace_name) %>% 
  dplyr::count()

names2 <- prep_gpa %>% 
  dplyr::group_by(namespace_path) %>% 
  dplyr::count()

# Overview ----------------------------------------------------------------

# develop metrics data frame for overview
#   total projects
#   number of internal projects
#   number of private projects
#   average time a project is active
tot_projects <- nrow(prep_gpa)
num_internal <- prep_gpa %>% 
  dplyr::count(visibility) %>% 
  dplyr::filter(visibility == "internal") %>% 
  dplyr::pull(n)
num_private <- prep_gpa %>% 
  dplyr::count(visibility) %>% 
  dplyr::filter(visibility == "private") %>% 
  dplyr::pull(n)
ave_life <- prep_gpa %>% 
  dplyr::mutate(project_length = last_activity_at - created_at) %>% 
  dplyr::summarise(ave_life = mean(project_length)) %>% 
  dplyr::pull(ave_life)
med_life <- prep_gpa %>% 
  dplyr::mutate(project_length = last_activity_at - created_at) %>% 
  dplyr::summarise(med_life = median(project_length)) %>% 
  dplyr::pull(med_life)

# activity through time
prep_activity <- prep_gpa %>% 
  dplyr::mutate(
    day = lubridate::round_date(
      x = created_at,
      unit =  "month")) %>% 
  dplyr::group_by(day) %>% 
  dplyr::summarise(visits = n()) %>% 
  dplyr::arrange(dplyr::desc(visits))

plot_activity <- prep_activity %>% 
  ggplot2::ggplot(ggplot2::aes(x = day, y = visits)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(se = FALSE, method = "loess") +
  ggplot2::labs(
    title = "DevForce GitLab Project Creation", 
    subtitle = "Dates are group by month", 
    x = "Year", 
    y = "Created Projects") +
  ggplot2::theme_bw()

plotly::ggplotly(plot_activity) %>% 
  plotly::layout(
    title = list(
      text = paste0('DevForce GitLab Project Creation',
                    '<br>',
                    '<sup>',
                    'Dates are group by month',
                    '</sup>')))

# top 15 contributing organizations
prep_orgs <- prep_gpa %>% 
  dplyr::group_by(L1) %>% 
  dplyr::count() %>% 
  dplyr::arrange(dplyr::desc(n)) %>% 
  dplyr::rename("count" = "n") %>% 
  utils::head(15)

plot_orgs <- prep_orgs %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(
        stringr::str_wrap(L1, 30), 
        count), 
      y = count)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Top Contributiong Organizational Group", 
    x = "Organizational Group", 
    y = "Number of Projects") +
  ggplot2::coord_flip()
  
plotly::ggplotly(plot_orgs)

# Organization ------------------------------------------------------------

# filter data frame by selected group (aka L1)
prep_org <- prep_gpa %>% 
  dplyr::filter(L1 == "netc-dsd" & L2 == "pmo")

# develop metrics data frame by organization
#   total projects
#   number of internal projects
#   number of private projects
#   average time a project is active
tot_projects_org <- nrow(prep_org)
num_internal_org <- prep_org %>% 
  dplyr::count(visibility) %>% 
  dplyr::filter(visibility == "internal") %>% 
  dplyr::pull(n)
num_private_org <- prep_org %>% 
  dplyr::count(visibility) %>% 
  dplyr::filter(visibility == "private") %>% 
  dplyr::pull(n)
ave_life_org <- prep_org %>% 
  dplyr::mutate(project_length = last_activity_at - created_at) %>% 
  dplyr::summarise(ave_life = mean(project_length)) %>% 
  dplyr::pull(ave_life)
med_life_org <- prep_org %>% 
  dplyr::mutate(project_length = last_activity_at - created_at) %>% 
  dplyr::summarise(med_life = median(project_length)) %>% 
  dplyr::pull(med_life)

tot_projects_org
num_internal_org
num_private_org
ave_life_org
med_life_org


# Experiment with Dot-plots -----------------------------------------------

dot_prep <- all_commits %>% 
  dplyr::filter()






# view code repos through time by group (aka L1)
prep_projects <- prep_org %>% 
  dplyr::arrange(-dplyr::desc(created_at)) %>% 
  dplyr::mutate(
    proj_ord = dplyr::row_number()) %>% 
  dplyr::relocate(proj_ord, .before = id) %>% 
  dplyr::mutate(
    name = forcats::fct_reorder(name, proj_ord),
    name = forcats::fct_rev(name)) 

plot_projects <- prep_projects %>% 
  ggplot2::ggplot(
    ggplot2::aes(y = name)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = created_at,
                 xend = last_activity_at,
                 y = name, yend = name),
    color = "royalblue1", 
    lineend = "round",
    size = 2) +
  ggplot2::geom_vline(
    ggplot2::aes(
      xintercept = as.numeric(Sys.Date())), 
    color = "red", linetype = "dashed") +
  ggplot2::scale_x_date() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "DSD GitLab Code Repositories", 
    subtitle = "Red dashed line is today's date",
    x = "Year", 
    y = "Project")

plotly::ggplotly(plot_projects) %>% 
  plotly::layout(
    title = list(
      text = paste0('GitLab Code Repositories',
                    '<br>',
                    '<sup>',
                    'Red dashed line is current the date',
                    '</sup>')))

# view sub group (aka L2) repo content given a group (aka L1)
prep_levels <- prep_org %>% 
  dplyr::group_by(L2) %>% 
  dplyr::count() %>% 
  dplyr::arrange(dplyr::desc(n))

plot_levels <- prep_levels %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(
        stringr::str_wrap(L2, 30), 
        n), 
      y = n)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Top Contributiong Organizational Group", 
    x = "Organizational Group", 
    y = "Number of Projects") +
  ggplot2::coord_flip()

plotly::ggplotly(plot_levels)



# # Select DSD Projects -----------------------------------------------------

# Select Project (using Fever as example)
project_select <- 4097

# Determine number of commits
project_commits <- gitlabr::gl_get_commits(project = project_select)
nrow(project_commits)

# Determine number of branches
project_branches <- gitlabr::gl_list_branches(project = project_select)
nrow(project_branches)

# Mean time between commits (commit_diffs has been converted to days)
project_timedif <- project_commits %>% 
  dplyr::select(committed_date) %>% 
  dplyr::mutate(
    committed_date = lubridate::ymd_hms(
      committed_date, 
      tz = "US/Arizona")) %>% 
  dplyr::arrange(dplyr::desc(committed_date)) %>% 
  dplyr::mutate(
    commit_diff = committed_date - dplyr::lead(committed_date),
    commit_diff = round(
      x = as.numeric(commit_diff) / 86400, 
      digits = 3))
  
mean(project_timedif$commit_diff, na.rm = TRUE)

# Average commits per day of project
nrow(project_commits) / as.numeric(head(project_timedif$committed_date, n = 1) - tail(project_timedif$committed_date, n = 1))

# Determine project duration
as.numeric(head(project_timedif$committed_date, n = 1) - tail(project_timedif$committed_date, n = 1))

# show project activity over time
project_activity <- project_timedif %>% 
  dplyr::mutate(
    day = lubridate::round_date(
      x = committed_date,
      unit =  "day")) %>% 
  dplyr::group_by(day) %>%
  dplyr::summarise(commits = n())

plot_activity <- project_activity %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = day, 
      y = commits)) +
  ggplot2::geom_point() +
  ggplot2::geom_line(color = "#e24329") +
  # ggplot2::geom_smooth(
  #   se = FALSE,
  #   method = "loess",
  #   color = "#e24329") +
  ggplot2::labs(
    x = "Year", 
    y = "Created Projects") +
  ggplot2::theme_bw()

plotly::ggplotly(plot_activity)

  

# show the number of commits by author(s)
project_committers <- project_commits %>% 
  dplyr::group_by(author_name) %>% 
  dplyr::count() %>% 
  dplyr::arrange(dplyr::desc(n))

committers <- project_committers %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(
        stringr::str_wrap(author_name, 30), 
        n), 
      y = n)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Top Contributiong Organizational Group", 
    x = "Organizational Group", 
    y = "Number of Projects") +
  ggplot2::coord_flip()

plotly::ggplotly(committers)

# Project branches


# GPA project
my_project <- 5977

# note about main vs master branch
gitlabr::gitlabr_options_set(key = "GPA.main", value = "master")
gl_list_files(project = my_project, ref = "master")
gl_list_issues(project = my_project)

# # Personal Projects
elm_projects <- gitlabr::gl_list_user_projects(user_id = "1850", max_page = 2)

# Explore individual projects
#   GPA project
my_project <- 5977
# gitlabr::gitlabr_options_set(key = "GPA.main", value = "master")
gitlabr::gl_list_files(project = my_project, ref = "master")





# Scraps ------------------------------------------------------------------



# # select DSD projects
# dsd_projects <- gl_projects %>% 
#   dplyr::filter(namespace_id == 1932 |
#                   namespace_id == 4209 |
#                   namespace_id == 3957 |
#                   namespace_id == 2746 |
#                   namespace_id == 2456 |
#                   namespace_id == 3294)
# 
# breakdowns <- dsd_projects %>% 
#   dplyr::group_by(namespace_full_path) %>% 
#   dplyr::count()
# 
# sep <- dsd_projects %>% 
#   tidyr::separate(
#     col = namespace_full_path, 
#     sep = "/", 
#     into = c("L1", "L2", "L3"), 
#     remove = FALSE, 
#     convert = TRUE) %>% 
#   dplyr::mutate(
#     L2 = dplyr::if_else(
#       condition = is.na(L2), 
#       true = L1, 
#       false = L2),
#     L3 = dplyr::if_else(
#       condition = is.na(L3), 
#       true = L2, 
#       false = L3))
# 
# sep_break <- sep %>% 
#   dplyr::group_by(L2) %>% 
#   dplyr::count()
# 
# # Project Chart -----------------------------------------------------------
# 
# # DSD GitLab Repos
# dsd_gl_projects <- dsd_projects %>% 
#   dplyr::mutate(
#     last_activity_at = lubridate::as_date(last_activity_at),
#     created_at = lubridate::as_date(created_at)) %>% 
#   dplyr::arrange(-dplyr::desc(created_at)) %>% 
#   dplyr::mutate(
#     proj_ord = dplyr::row_number()) %>% 
#   dplyr::relocate(proj_ord, .before = id) %>% 
#   dplyr::mutate(
#     name = forcats::fct_reorder(name, proj_ord),
#     name = forcats::fct_rev(name)) %>%  
#   ggplot2::ggplot(ggplot2::aes(y = name)) +
#   ggplot2::geom_segment(
#     ggplot2::aes(x = created_at,
#                  xend = last_activity_at,
#                  y = name, yend = name),
#     color = "royalblue1", 
#     lineend = "round",
#     size = 2) +
#   ggplot2::geom_vline(
#     ggplot2::aes(
#       xintercept = as.numeric(Sys.Date())), 
#     color = "red", linetype = "dashed") +
#   ggplot2::annotate(
#     geom = "text", 
#     x = Sys.Date() - 20,
#     y = nrow(dsd_projects) - 3, 
#     label = "Today", 
#     color = "red") +
#   ggplot2::scale_x_date() +
#   ggplot2::theme_bw() +
#   ggplot2::labs(
#     title = "DSD GitLab Code Repositories", 
#     x = "Year", 
#     y = "Project")
# plotly::ggplotly(dsd_gl_projects)
# 
# 
# 
# 
# # check ARC
# 
# arc <- dsd %>% 
#   dplyr::filter(namespace.id == 3568 |
#                   namespace.id == 3813 |
#                   namespace.id == 3664 |
#                   namespace.id == 3829 |
#                   namespace.id == 3792 |
#                   namespace.id == 3592 |
#                   namespace.id == 3581 |
#                   namespace.id == 3574) %>% 
#   dplyr::select(id:created_at,
#                 last_activity_at:namespace.web_url)
# 
# 


# Personnel Tab -----------------------------------------------------------

# waffle
# https://github.com/hrbrmstr/waffle/blob/master/DESCRIPTION
# https://r-charts.com/part-whole/waffle-chart-ggplot2/
# https://www.geeksforgeeks.org/r-waffle-chart/

# User projects
elm <- gitlabr::gl_list_user_projects(user_id = "1850", max_page = 1)

