#' Create Task Graph
#'
#' @param df data.frame listing
#' @param repos repositories which will be used to identify dependencies chain
#' to run R CMD checks
#' @return A dependency graph with vertex attributes "root" (a logical value
#'   indicating whether the package as one of the roots used to create the
#'   graph), "status" (installation status) and "order" (installation order).
#'
#' @keywords internal
#' @importFrom igraph V
task_graph_create <- function(df, repos = getOption("repos")) {
  edges <- task_edges_df(df, repos)
  vertices <- task_vertices_df(df, edges, repos)

  g <- igraph::graph_from_data_frame(edges, vertices = vertices)
  igraph::V(g)$status <- STATUS$pending  # nolint object_name_linter
  igraph::V(g)$process <- rep_len(list(), length(g))
  task_graph_sort(g)
}

task_edges_df <- function(df, repos) {

  if (NROW(df) == 0) {
    return(empty_edge)
  }

  db <- utils::available.packages(repos = repos)[, DB_COLNAMES]

  # For checks alias has to have different name than package name

  # Add custom packages to db
  custom_aliases_idx <- which(vlapply(df$custom, function(x) !is.null(x$alias)))
  custom_aliases <- vcapply(df$custom[custom_aliases_idx], `[[`, "alias")
  custom_aliases_map <- unique(data.frame(
    value = custom_aliases,
    hash = vcapply(custom_aliases, hash_alias)
  ))

  desc <- drlapply(df$custom, function(x) {
    row <- get_package_spec_dependencies(x$package_spec)
    hash <- custom_aliases_map[custom_aliases_map$value == x$alias, ]$hash
    row[, "Package"] <- hash
    row
  })
  # Drop potential duplicates
  desc <- unique(desc)

  # Adding checks to db and custom packages as Depends link
  checks <- drlapply(df$package, function(x) {
    p <- df[df$alias == x$alias, ]
    row <- get_package_spec_dependencies(x$package_spec)
    row[, "Package"] <- x$alias
    if (!is.null(p$custom[[1]]$alias)) {
      row_idx <- custom_aliases_map$value == p$custom[[1]]$alias
      hash <- custom_aliases_map[row_idx, ]$hash
      row[, "Depends"] <- ifelse(
        is.na(row[, "Depends"]),
        hash,
        paste0(row[, "Depends"], ", ", hash)
      )
    }
    row
  })

  db <- rbind(db, desc, checks)

  # Get suggests end enhances dependencies first so we can derive hard
  # dependencies for them as well
  suggests_dependencies <- uulist(package_deps(
    df$alias,
    db = db,
    which = c("Suggests", "Enhances"),
    recursive = FALSE
  ))

  # Get recursively strong dependencies for all packages
  core_dependencies <- package_deps(
    c(df$alias, custom_aliases_map$hash, suggests_dependencies),
    db = db,
    which = "strong",
    recursive = TRUE
  )

  dependencies <- uulist(c(
    # tools::package_dependencies do not include package itself.
    # we add it at this stage
    df$alias,
    custom_aliases_map$hash,
    suggests_dependencies,
    core_dependencies
  ))

  dependencies <- dependencies[!dependencies %in% base_pkgs()]

  edges <- drlapply(dependencies, function(p) {
    edges_per_type <- drlapply(uulist(DEP), function(type) {
      deps <- try(db[db[, "Package"] == p, type], silent = TRUE)
      if (inherits(deps, "try-error") || length(deps) == 0) {
        empty_edge
      } else {
        deps <- split_packages_names(deps)
        # Filter out base packages
        deps <- deps[deps$dep %in% dependencies, ]
        cbind(
          deps,
          root = rep(p, times = NROW(deps)),
          type = rep(type, times = NROW(deps))
        )
      }
    })
  })

  edges$dep <- replace_with_map(edges$dep, custom_aliases_map$hash, custom_aliases_map$value)
  edges$root <- replace_with_map(edges$root, custom_aliases_map$hash, custom_aliases_map$value)
  # reorder columns to the igraph format
  edges[, c("dep", "root", "type", "op", "version")]
}

task_vertices_df <- function(df, edges, repos) {
  vertices <- unique(c(edges$dep, edges$root))
  custom_pkgs_aliases <- uulist(lapply(df$custom, `[[`, "alias"))
  task_type <- ifelse(vertices %in% df$alias, "check", "install")

  spec <- lapply(vertices, function(v) {
    if (v %in% df$alias) {
      df$package[[which(df$alias == v)]]
    } else if (v %in% custom_pkgs_aliases) {
      df$custom[[utils::head(which(as.character(lapply(df$custom, `[[`, "alias")) == v), 1)]]
    } else {
      e <- edges[edges$dep == v, ]
      ver_order <- order(
        e$version,
        # In case of multiple requirements with the same version
        # prioritize those using ">" operator
        e$op,
        na.last = TRUE,
        decreasing = c(TRUE, FALSE),
        method = "radix"
      )
      install_task_spec(
        alias = v,
        package_spec = package_spec(
          name = v,
          repos = repos,
          # Specify version requirements for dependencies
          op = e$op[[ver_order[[1]]]],
          version = e$version[[ver_order[[1]]]]
        )
      )
    }
  })

  out <- data.frame(
    name = vertices,
    type = task_type,
    custom = vertices %in% custom_pkgs_aliases
  )

  out$spec <- spec
  out
}

#' Find Task Neighborhood
#'
#' @param g A task graph, as produced with [task_graph_create()]
#' @param nodes Names or nodes objects of packages whose neighborhoods
#' should be calculated.
#'
#' @importFrom igraph neighborhood
#' @keywords internal
task_graph_neighborhoods <- function(g, nodes) {
  igraph::neighborhood(
    g,
    order = length(g),
    nodes = nodes,
    mode = "in"
  )
}

#' Sort Task Graph by Strong Dependency Order
#'
#' @note
#' Cyclic dependencies are possible. Cyclic dependencies are disallowed for all
#' hard dependencies on CRAN today, though there have been historical instances
#' where they appeared on CRAN.
#'
#' Installation priority is based on:
#'   1. Total dependency footprint (low to high)
#'   2. Topology (leaf nodes first)
#'
#' @param g A [igraph::graph], expected to contain node attribute `type`.
#' @return The [igraph::graph] `g`, with vertices sorted in preferred
#'   installation order.
#'
#' @importFrom igraph vertex_attr neighborhood subgraph.edges permute topo_sort E V E<- V<-
#' @keywords internal
task_graph_sort <- function(g) {
  roots <- which(igraph::vertex_attr(g, "type") == "check")

  # split into neighborhoods by root (revdep)
  nhood <- task_graph_neighborhoods(g, roots)

  # prioritize by neighborhood size (small to large)
  priority <- length(nhood)
  priority_footprint <- integer(length(g))
  for (i in order(-vapply(nhood, length, integer(1L)))) {
    priority_footprint[nhood[[i]]] <- priority
    priority <- priority - 1
  }

  # use only strong dependencies to prioritize by topology (leafs first)
  strong_edges <- igraph::E(g)[igraph::E(g)$type %in% DEP_STRONG]
  g_strong <- igraph_subgraph_from_edges(g, strong_edges, delete.vertices = FALSE)
  topo <- igraph::topo_sort(g_strong, mode = "in")
  priority_topo <- integer(length(g))
  priority_topo[match(topo$name, igraph::V(g)$name)] <- rev(seq_along(topo))

  # combine priorities, prioritize first by total, footprint then topology
  priorities <- rbind(priority_footprint, priority_topo)
  order <- rank(length(igraph::V(g))^seq(nrow(priorities) - 1, 0) %*% priorities)
  g <- igraph::permute(g, order)

  g
}

#' Find the Next Packages Not Dependent on an Unavailable Package
#'
#' While other packages are in progress, identify tasks with all the
#' dependencies done and mark them as \code{ready} already has its dependencies
#' done.
#' 
#' @details
#' There are helpers defined for particular use cases that strictly rely on the
#' [`task_graph_update_ready()`], they are:
#'
#' * `task_graph_update_ready_strong()` - List vertices whose strong
#'   dependencies are satisfied.
#' * `task_graph_update_check_ready()` - List root vertices whose all
#'   dependencies are satisfied.
#' * `task_graph_update_install_ready()` - List install vertices whose
#'   dependencies are all satisfied
#'
#' @param g A dependency graph, as produced with [task_graph_create()].
#' @param v Names or nodes objects of packages whose readiness should be
#' checked.
#' @param dependencies Which dependencies types should be met for a node to be
#' considered satisfied.
#' @param status status name. Nodes in v fill be filtered to consists only nodes
#' with that status.
#'
#' @return The name of the next package to prioritize
#'
#' @importFrom igraph incident_edges tail_of
#' @keywords internal
task_graph_update_ready <- function(
    g,
    v = igraph::V(g),
    dependencies = TRUE,
    status = STATUS$pending) {
  if (is.character(status)) status <- STATUS[[status]]
  dependencies <- check_dependencies(dependencies)
  if (length(status) > 0) {
    idx <- v$status %in% status
    v <- v[idx]
  }

  deps_met <- vlapply(
    igraph::incident_edges(g, v, mode = "in"),
    function(edges) {
      edges <- edges[edges$type %in% dependencies]
      all(igraph::tail_of(g, edges)$status == STATUS$done)
    }
  )

  task_graph_set_package_status(
    g,
    names(deps_met[deps_met]),
    STATUS$ready
  )
}

task_graph_update_ready_strong <- function(..., dependencies = "strong") { # nolint
  task_graph_update_ready(..., dependencies = dependencies)
}


task_graph_update_check_ready <- function(
    g,
    ...,
    dependencies = "all",
    status = STATUS$pending) {
  task_graph_update_ready(
    g,
    igraph::V(g)[igraph::V(g)$type == "check"],
    ...,
    dependencies = dependencies,
    status = status
  )
}

task_graph_update_install_ready <- function(
    g,
    ...,
    dependencies = "strong",
    status = STATUS$pending) {
  task_graph_update_ready(
    g,
    igraph::V(g)[igraph::V(g)$type == "install"],
    ...,
    dependencies = dependencies,
    status = status
  )
}


#' Find task with ready state
#'
#' List tasks which have ready state prioritizing check tasks over
#' install tasks.
#'
#' @param g A dependency graph, as produced with [task_graph_create()].
#'
#' @return The names of packages with ready state.
#'
#' @importFrom igraph incident_edges tail_of
#' @keywords internal
task_graph_which_ready <- function(g) {
  ready_checks <- task_graph_get_package_with_status(
    g,
    igraph::V(g)[igraph::V(g)$type == "check"],
    "ready"
  )
  ready_installs <- task_graph_get_package_with_status(
    g,
    igraph::V(g)[igraph::V(g)$type == "install"],
    "ready"
  )

  c(ready_checks, ready_installs)
}

empty_edge <- data.frame(
  dep = character(0),
  root = character(0),
  type = character(0)
)

task_graph_set_package_status <- function(g, v, status) {
  if (is.character(status)) status <- STATUS[[status]]
  igraph::set_vertex_attr(g, "status", v, status)
}

task_graph_package_status <- function(g, v) {
  igraph::vertex.attributes(g, v)$status
}

`task_graph_package_status<-` <- function(x, v, value) {
  task_graph_set_package_status(x, v, value)
}

`task_graph_package_status<-` <- function(x, v, value) {
  task_graph_set_package_status(x, v, value)
}

task_graph_get_package_with_status <- function(g, v, status) {
  if (is.character(status)) status <- STATUS[[status]]
  statuses <- igraph::vertex.attributes(g, v)$status
  
  v[statuses == .env$status]
}

`task_graph_task_process<-` <- function(x, v, value) {
  task_graph_set_task_process(x, v, value)
}


task_graph_task_spec <- function(g, v) {
  igraph::vertex_attr(g, "spec", v)[[1]]
}

task_graph_task_name <- function(g, v) {
  igraph::vertex_attr(g, "name", v)[[1]]
}

task_graph_task_process <- function(g, v) {
  igraph::vertex_attr(g, "process", v)[[1]]
}

task_graph_set_task_process <- function(g, v, process) {
  igraph::set_vertex_attr(g, "process", v, list(process))
}

task_graph_update_done <- function(g, lib.loc) {
  custom_installs <- vlapply(
    igraph::V(g)$spec,
    inherits,
    "custom_install_task_spec"
  )
  installs <- igraph::V(g)$type == "install" 
  # custom install cannot be satisfied
  v <- igraph::V(g)[installs & !custom_installs]
  which_done <- which(vlapply(v$spec, is_package_satisfied, lib.loc = lib.loc))
  task_graph_set_package_status(g, v[which_done], STATUS$done)
}

is_package_satisfied <- function(v, lib.loc) {  # nolint object_name_linter
  if (!is.null(v$package_spec$version)) {
    installed_version <- tryCatch(
      utils::packageVersion(v$package_spec$name, lib.loc = lib.loc),
      error = function(e) {
        numeric_version("0")
      }
    )
    get(v$package_spec$op)(installed_version, v$package_spec$version)
  } else {
    FALSE
  }
}

igraph_subgraph_from_edges <- function(...) {
  if (utils::packageVersion("igraph") < "2.1.0") {
    igraph::subgraph.edges(...)
  } else {
    igraph::subgraph_from_edges(...)
  }
}
