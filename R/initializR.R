#' Initialize new project
#'
#' @param name optional name of script file, default = "MAIN_SCRIPT"
#' @param path optional name of path. If null a pop-up window for directory selection will pop-up
#' @param proj whether a project file should be created (default = T)
#' @param script type of initial script, choose between "standard" and "quarto" (default = "standard")
#' @param repo name of github repo if the project should be pushed to github
#' @param open whether the project should be opened after initialization (default = T)
#'
#' @returns an initialized project
#' @export
#'
#'
#
# initializR(name = "test",
#            #path = "P:/RH/HOC/Lukkede Mapper/GH Group/HUDKRÆFT/Angiosarcoma/git_test",
#            proj = T,
#            repo = "my_repo")


initializR <- function(name = NULL,
                       path = NULL,
                       proj = TRUE,
                       script = "standard",
                       repo = NULL,
                       open = T) {

  if (is.null(path)) path <- rstudioapi::selectDirectory()
  if (is.null(name)) name <- "MAIN_SCRIPT"

  created <- character()

  # Project file
  if (proj) {
    writeLines(c("Version: 1.0", "RestoreWorkspace: No", "SaveWorkspace: No"),
               file.path(path, "PROJECT.Rproj"))

    created <- c(created, "Project file")
  }

  script <- defusR(script)

  if(!is.null(script)) {
    #Add script
    template <- switch(script,
                       standard = "script_template.R",
                       quarto   = "quarto_template.qmd")

    from <- system.file("templates", template, package = "cancR", mustWork = TRUE)
    to   <- file.path(path, paste0(name, ".", tools::file_ext(template)))

    if(file.exists(to)) cli::cli_abort("Error: File \'{name}\' already exists")
    file.copy(from, to)
    created <- c(created, "Initial script")
  }

  # Folders (always)
  for (d in c("data", "Tables and Figures")) {
    dir.create(file.path(path, d), showWarnings = FALSE)
  }
  created <- c(created, "Folders: Data and Tables & Figures")

  if (!is.null(repo)) {

    cli::cli_alert_info("Creating git repository")

    # .gitignore
    gi <- file.path(path, ".gitignore")
    if (!file.exists(gi)) {
      writeLines(c(".Rproj.user/", ".Rhistory", ".RData", ".httr-oauth", ".DS_Store",
                   ".quarto/", "", "data/", "", "*.html", "*_files/", "*_cache/"), gi)
    }

    #Make repo
    gert::git_init(path)
    gert::git_add(".", repo = path)
    gert::git_commit("Initial commit", repo = path)

    res <- gh::gh("POST /user/repos", name = repo, private = T)
    gert::git_remote_add(res$clone_url, name = "origin", repo = path)
    gert::git_push(remote = "origin", set_upstream = TRUE, repo = path)
    created <- c(created, paste0("GitHub repo: ", res$html_url))
  }

  cli::cli_alert_success("Project initialized")
  cli::cli_text("Path: {path}")
  cli::cli_text("Created files:")
  cli::cli_ul(created)

  if (open) {
    if (proj) {
      rstudioapi::openProject(file.path(path, "PROJECT.Rproj"), newSession = TRUE)
    } else if (!is.null(script)) {
      rstudioapi::navigateToFile(to)
    }
  }

  invisible(NULL)

}
