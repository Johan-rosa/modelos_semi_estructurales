# Generated on 2020-03-06 00:49:08 by gEcon ver. 1.2.0 (2019-09-08)
# http://gecon.r-forge.r-project.org/

# Model name: econ_cerrada

# info
info__ <- c("econ_cerrada", "/cloud/project/gcn_file/econ_cerrada.gcn", "2020-03-06 00:49:08", "false")

# index sets
index_sets__ <- list()

# variables
variables__ <- c("gybar",
                 "gy",
                 "pic",
                 "picA2",
                 "picA",
                 "picA1",
                 "pic__lag_1",
                 "pic__lag_2",
                 "r",
                 "rgap",
                 "tpm",
                 "ygap")

variables_tex__ <- c("{g\\!y\\!b\\!a\\!r}",
                     "{g\\!y}",
                     "{p\\!i\\!c}",
                     "{p\\!i\\!c\\!A\\!2}",
                     "{p\\!i\\!c\\!A}",
                     "{p\\!i\\!c\\!A\\!1}",
                     "{p\\!i\\!c}^{\\mathrm{lag}^{\\mathrm{1}}}",
                     "{p\\!i\\!c}^{\\mathrm{lag}^{\\mathrm{2}}}",
                     "r",
                     "{r\\!g\\!a\\!p}",
                     "{t\\!p\\!m}",
                     "{y\\!g\\!a\\!p}")

# shocks
shocks__ <- c("eta_ygap",
              "eta_pic",
              "eta_tpm",
              "eta_ybar")

shocks_tex__ <- c("\\eta^{\\mathrm{ygap}}",
                  "\\eta^{\\mathrm{pic}}",
                  "\\eta^{\\mathrm{tpm}}",
                  "\\eta^{\\mathrm{ybar}}")

# parameters
parameters__ <- c("a1",
                  "a2",
                  "a3",
                  "b1",
                  "b2",
                  "b3",
                  "c1",
                  "gss",
                  "ibar",
                  "meta",
                  "phi_tpm",
                  "phi_pic",
                  "phi_ygap",
                  "rbar")

parameters_tex__ <- c("{a\\!1}",
                     "{a\\!2}",
                     "{a\\!3}",
                     "{b\\!1}",
                     "{b\\!2}",
                     "{b\\!3}",
                     "{c\\!1}",
                     "{g\\!s\\!s}",
                     "{i\\!b\\!a\\!r}",
                     "{m\\!e\\!t\\!a}",
                     "\\phi^{\\mathrm{tpm}}",
                     "\\phi^{\\mathrm{pic}}",
                     "\\phi^{\\mathrm{ygap}}",
                     "{r\\!b\\!a\\!r}")

# free parameters
parameters_free__ <- c("a1",
                       "a2",
                       "a3",
                       "b1",
                       "b2",
                       "b3",
                       "c1",
                       "gss",
                       "ibar",
                       "meta",
                       "phi_tpm",
                       "phi_pic",
                       "phi_ygap",
                       "rbar")

# free parameters' values
parameters_free_val__ <- c(0.4,
                           0.6,
                           0.05,
                           0.5,
                           0.05,
                           0.15,
                           0.5,
                           0,
                           0,
                           0,
                           0.7,
                           1.5,
                           0.4,
                           0)

# equations
equations__ <- c("pic[-1] - pic__lag_1[] = 0",
                 "pic__lag_1[-1] - pic__lag_2[] = 0",
                 "-picA2[] + E[][picA1[1]] = 0",
                 "-picA1[] + E[][picA[1]] = 0",
                 "-rbar + r[] - rgap[] = 0",
                 "-r[] + tpm[] - E[][pic[1]] = 0",
                 "-ygap[-1] + gybar[] - gy[] + ygap[] = 0",
                 "eta_tpm[] - tpm[] + phi_tpm * tpm[-1] + (1 - phi_tpm) * (ibar + phi_pic * (-meta + E[][picA2[1]]) + phi_ygap * ygap[]) = 0",
                 "eta_ybar[] - gybar[] + c1 * gybar[-1] + gss * (1 - c1) = 0",
                 "0.25 * pic[-1] + 0.25 * pic__lag_1[-1] + 0.25 * pic__lag_2[-1] + 0.25 * pic[] - picA[] = 0",
                 "eta_ygap[] - ygap[] + b1 * ygap[-1] + b2 * E[][ygap[1]] - b3 * rgap[] = 0",
                 "eta_pic[] - pic[] + a1 * pic[-1] + a2 * E[][pic[1]] + a3 * ygap[] = 0")

# calibrating equations
calibr_equations__ <- character(0)

# variables / equations map
vareqmap__ <- sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                                 6, 6, 6, 7, 7, 7, 8, 8, 8, 9,
                                 10, 10, 10, 10, 11, 11, 12, 12),
                           j = c(3, 7, 7, 8, 4, 6, 5, 6, 9, 10,
                                 3, 9, 11, 1, 2, 12, 4, 11, 12, 1,
                                 3, 5, 7, 8, 10, 12, 3, 12),
                           x = c(1, 2, 1, 2, 2, 4, 4, 2, 2, 2,
                                 4, 2, 2, 2, 2, 3, 4, 3, 2, 3,
                                 3, 2, 1, 1, 2, 7, 7, 2),
                           dims = c(12, 12))

# variables / calibrating equations map
varcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 12))

# calibrated parameters / equations map
calibrpareqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(12, 0))

# calibrated parameters / calibrating equations map
calibrparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 0))

# free parameters / equations map
freepareqmap__ <- sparseMatrix(i = c(5, 8, 8, 8, 8, 8, 9, 9, 11, 11,
                                     11, 12, 12, 12),
                               j = c(14, 9, 10, 11, 12, 13, 7, 8, 4, 5,
                                     6, 1, 2, 3),
                               x = rep(1, 14), dims = c(12, 14))

# free parameters / calibrating equations map
freeparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 14))

# shocks / equations map
shockeqmap__ <- sparseMatrix(i = c(8, 9, 11, 12),
                             j = c(3, 4, 1, 2),
                             x = rep(1, 4), dims = c(12, 4))

# steady state equations
ss_eq__ <- function(v, pc, pf)
{
    r <- numeric(12)
    r[1] = v[3] - v[7]
    r[2] = v[7] - v[8]
    r[3] = -v[4] + v[6]
    r[4] = v[5] - v[6]
    r[5] = -pf[14] + v[9] - v[10]
    r[6] = -v[3] - v[9] + v[11]
    r[7] = v[1] - v[2]
    r[8] = -v[11] + pf[11] * v[11] + (1 - pf[11]) * (pf[9] + pf[12] * (-pf[10] + v[4]) + pf[13] * v[12])
    r[9] = -v[1] + pf[7] * v[1] + pf[8] * (1 - pf[7])
    r[10] = 0.5 * v[3] - v[5] + 0.25 * v[7] + 0.25 * v[8]
    r[11] = -v[12] + pf[4] * v[12] + pf[5] * v[12] - pf[6] * v[10]
    r[12] = -v[3] + pf[1] * v[3] + pf[2] * v[3] + pf[3] * v[12]

    return(r)
}

# calibrating equations
calibr_eq__ <- function(v, pc, pf)
{
    r <- numeric(0)

    return(r)
}

# steady state and calibrating equations Jacobian
ss_calibr_eq_jacob__ <- function(v, pc, pf)
{
    r <- numeric(0)
    jac <- numeric(27)
    jac[1] = 1
    jac[2] = -1
    jac[3] = 1
    jac[4] = -1
    jac[5] = -1
    jac[6] = 1
    jac[7] = 1
    jac[8] = -1
    jac[9] = 1
    jac[10] = -1
    jac[11] = -1
    jac[12] = -1
    jac[13] = 1
    jac[14] = 1
    jac[15] = -1
    jac[16] = pf[12] * (1 - pf[11])
    jac[17] = -1 + pf[11]
    jac[18] = pf[13] * (1 - pf[11])
    jac[19] = -1 + pf[7]
    jac[20] = 0.5
    jac[21] = -1
    jac[22] = 0.25
    jac[23] = 0.25
    jac[24] = -pf[6]
    jac[25] = -1 + pf[4] + pf[5]
    jac[26] = -1 + pf[1] + pf[2]
    jac[27] = pf[3]
    jacob <- sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                                6, 6, 6, 7, 7, 8, 8, 8, 9, 10,
                                10, 10, 10, 11, 11, 12, 12),
                          j = c(3, 7, 7, 8, 4, 6, 5, 6, 9, 10,
                                3, 9, 11, 1, 2, 4, 11, 12, 1, 3,
                                5, 7, 8, 10, 12, 3, 12),
                          x = jac, dims = c(12, 12))

    return(jacob)
}

# 1st order perturbation
pert1__ <- function(v, pc, pf)
{
    Atm1x <- numeric(10)
    Atm1x[1] = 1
    Atm1x[2] = 1
    Atm1x[3] = -1
    Atm1x[4] = pf[11]
    Atm1x[5] = pf[7]
    Atm1x[6] = 0.25
    Atm1x[7] = 0.25
    Atm1x[8] = 0.25
    Atm1x[9] = pf[4]
    Atm1x[10] = pf[1]
    Atm1 <- sparseMatrix(i = c(1, 2, 7, 8, 9, 10, 10, 10, 11, 12),
                         j = c(3, 7, 12, 11, 1, 3, 7, 8, 12, 3),
                         x = Atm1x, dims = c(12, 12))

    Atx <- numeric(20)
    Atx[1] = -1
    Atx[2] = -1
    Atx[3] = -1
    Atx[4] = -1
    Atx[5] = 1
    Atx[6] = -1
    Atx[7] = -1
    Atx[8] = 1
    Atx[9] = 1
    Atx[10] = -1
    Atx[11] = 1
    Atx[12] = -1
    Atx[13] = pf[13] * (1 - pf[11])
    Atx[14] = -1
    Atx[15] = 0.25
    Atx[16] = -1
    Atx[17] = -pf[6]
    Atx[18] = -1
    Atx[19] = -1
    Atx[20] = pf[3]
    At <- sparseMatrix(i = c(1, 2, 3, 4, 5, 5, 6, 6, 7, 7,
                             7, 8, 8, 9, 10, 10, 11, 11, 12, 12),
                       j = c(7, 8, 4, 6, 9, 10, 9, 11, 1, 2,
                             12, 11, 12, 1, 3, 5, 10, 12, 3, 12),
                         x = Atx, dims = c(12, 12))

    Atp1x <- numeric(6)
    Atp1x[1] = 1
    Atp1x[2] = 1
    Atp1x[3] = -1
    Atp1x[4] = pf[12] * (1 - pf[11])
    Atp1x[5] = pf[5]
    Atp1x[6] = pf[2]
    Atp1 <- sparseMatrix(i = c(3, 4, 6, 8, 11, 12),
                         j = c(6, 5, 3, 4, 12, 3),
                         x = Atp1x, dims = c(12, 12))

    Aepsx <- numeric(4)
    Aepsx[1] = 1
    Aepsx[2] = 1
    Aepsx[3] = 1
    Aepsx[4] = 1
    Aeps <- sparseMatrix(i = c(8, 9, 11, 12),
                         j = c(3, 4, 1, 2),
                         x = Aepsx, dims = c(12, 4))

    return(list(Atm1, At, Atp1, Aeps))
}

ext__ <- list()

# create model object
gecon_model(model_info = info__,
            index_sets = index_sets__,
            variables = variables__,
            variables_tex = variables_tex__,
            shocks = shocks__,
            shocks_tex = shocks_tex__,
            parameters = parameters__,
            parameters_tex = parameters_tex__,
            parameters_free = parameters_free__,
            parameters_free_val = parameters_free_val__,
            equations = equations__,
            calibr_equations = calibr_equations__,
            var_eq_map = vareqmap__,
            shock_eq_map = shockeqmap__,
            var_ceq_map = varcalibreqmap__,
            cpar_eq_map = calibrpareqmap__,
            cpar_ceq_map = calibrparcalibreqmap__,
            fpar_eq_map = freepareqmap__,
            fpar_ceq_map = freeparcalibreqmap__,
            ss_function = ss_eq__,
            calibr_function = calibr_eq__,
            ss_calibr_jac_function = ss_calibr_eq_jacob__,
            pert = pert1__,
            ext = ext__)
