options(warn = 1)

fmt_ts <- function(t = Sys.time()) format(t, "%Y-%m-%d %H:%M:%OS3")

emit <- function(kind, pkg, demo, status = "", elapsed = NA_real_) {
  fields <- c(kind, fmt_ts(), pkg, demo, status, sprintf("%.6f", elapsed))
  cat(paste(fields, collapse = "\t"), "\n", sep = "")
  flush.console()
}

run_demo <- function(pkg, demo) {
  t0 <- Sys.time()
  emit("DEMO_START", pkg, demo)
  status <- "ok"
  tryCatch(
    {
      demo(demo, package = pkg, character.only = TRUE)
    },
    error = function(e) {
      status <<- paste0("error:", conditionMessage(e))
    }
  )
  emit("DEMO_END", pkg, demo, status = status, elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

set.seed(123)
cat("TRACE_RUN_START\t", fmt_ts(), "\n", sep = "")
flush.console()

# Small startup delay so DTrace attaches before first demo.
Sys.sleep(1)

dont <- list(
  graphics = c("Hershey", "Japanese", "plotmath"),
  stats = c("lm.glm", "nlm")
)

for (pkg in c("base", "graphics", "stats")) {
  demo_dir <- file.path(system.file(package = pkg), "demo")
  demos <- list.files(demo_dir, pattern = "\\.R$")
  excluded <- paste(dont[[pkg]], "R", sep = ".")
  demos <- demos[is.na(match(demos, excluded))]
  demos <- sub("\\.R$", "", demos)

  need_attach <- pkg != "base"
  if (need_attach) {
    library(pkg, character.only = TRUE)
  }

  for (demo_name in demos) {
    run_demo(pkg, demo_name)
  }

  if (need_attach) {
    fpkg <- paste("package", pkg, sep = ":")
    if (fpkg %in% search()) {
      detach(pos = which(fpkg == search()))
    }
  }
}

for (demo_name in c("Hershey", "Japanese", "lm.glm", "nlm", "plotmath")) {
  run_demo("base", demo_name)
}

cat("TRACE_RUN_END\t", fmt_ts(), "\n", sep = "")
flush.console()
