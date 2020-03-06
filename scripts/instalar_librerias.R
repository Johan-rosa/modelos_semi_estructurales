# Dependencias
install.packages("Rcpp")
install.packages("nleqslv")
install.packages("KFAS")
install.packages(c("coda", "doParallel", "FKF", "forech", "numDeriv"))

# Descargando source del paquetes para linux 
download.file(
  "http://gecon.r-forge.r-project.org/files/gEcon_1.2.0.tar.gz",
  "package_files/gEcon_1.2.0.tar.gz")

download.file(
  "http://gecon.r-forge.r-project.org/files/gEcon.estimation_0.1.0.tar.gz",
  "package_files/gEcon.estimation_0.1.0.tar.gz")

download.file(
  "http://gecon.r-forge.r-project.org/files/gEcon.iosam_0.2.0.tar.gz",
  "package_files/gEcon.iosam_0.2.0.tar.gz")

# Instalando los paquetes

# gEcon
install.packages(
  "/cloud/project/package_files/gEcon_1.2.0.tar.gz",
  repos = NULL,
  type = "source",
  dependencies = TRUE)

# gEcon.estimation
install.packages(
  "/cloud/project/package_files/gEcon.estimation_0.1.0.tar.gz",
  repos = NULL,
  type = "source",
  dependencies = TRUE)

#gEcon.iosam
install.packages(
  "/cloud/project/package_files/gEcon.iosam_0.2.0.tar.gz",
  repos = NULL,
  type = "source",
  dependencies = TRUE)
