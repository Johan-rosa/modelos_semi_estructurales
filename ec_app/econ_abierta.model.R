# Generated on 2020-03-06 00:59:16 by gEcon ver. 1.2.0 (2019-09-08)
# http://gecon.r-forge.r-project.org/

# Model name: econ_abierta

# info
info__ <- c("econ_abierta", "/cloud/project/ec_app/econ_abierta.gcn", "2020-03-06 00:59:16", "false")

# index sets
index_sets__ <- list()

# variables
variables__ <- c("ds",
                 "dsA",
                 "ds__lag_1",
                 "ds__lag_2",
                 "dz",
                 "gybar",
                 "gy",
                 "istar",
                 "pic",
                 "picA2",
                 "picA",
                 "picA1",
                 "pic__lag_1",
                 "pic__lag_2",
                 "pistar",
                 "r",
                 "rgap",
                 "risk",
                 "tpm",
                 "ygap",
                 "ystar",
                 "zgap")

variables_tex__ <- c("{d\\!s}",
                     "{d\\!s\\!A}",
                     "{d\\!s}^{\\mathrm{lag}^{\\mathrm{1}}}",
                     "{d\\!s}^{\\mathrm{lag}^{\\mathrm{2}}}",
                     "{d\\!z}",
                     "{g\\!y\\!b\\!a\\!r}",
                     "{g\\!y}",
                     "{i\\!s\\!t\\!a\\!r}",
                     "{p\\!i\\!c}",
                     "{p\\!i\\!c\\!A\\!2}",
                     "{p\\!i\\!c\\!A}",
                     "{p\\!i\\!c\\!A\\!1}",
                     "{p\\!i\\!c}^{\\mathrm{lag}^{\\mathrm{1}}}",
                     "{p\\!i\\!c}^{\\mathrm{lag}^{\\mathrm{2}}}",
                     "{p\\!i\\!s\\!t\\!a\\!r}",
                     "r",
                     "{r\\!g\\!a\\!p}",
                     "{r\\!i\\!s\\!k}",
                     "{t\\!p\\!m}",
                     "{y\\!g\\!a\\!p}",
                     "{y\\!s\\!t\\!a\\!r}",
                     "{z\\!g\\!a\\!p}")

# shocks
shocks__ <- c("eta_ygap",
              "eta_pic",
              "eta_tpm",
              "eta_ybar",
              "eta_UIP",
              "eta_pistar",
              "eta_istar",
              "eta_ystar",
              "eta_risk")

shocks_tex__ <- c("\\eta^{\\mathrm{ygap}}",
                  "\\eta^{\\mathrm{pic}}",
                  "\\eta^{\\mathrm{tpm}}",
                  "\\eta^{\\mathrm{ybar}}",
                  "\\eta^{\\mathrm{UIP}}",
                  "\\eta^{\\mathrm{pistar}}",
                  "\\eta^{\\mathrm{istar}}",
                  "\\eta^{\\mathrm{ystar}}",
                  "\\eta^{\\mathrm{risk}}")

# parameters
parameters__ <- c("a1",
                  "a2",
                  "a3",
                  "a4",
                  "b1",
                  "b2",
                  "b3",
                  "b4",
                  "b5",
                  "c1",
                  "c2",
                  "c3",
                  "c4",
                  "c5",
                  "dzbar",
                  "gss",
                  "ibar",
                  "istar_bar",
                  "meta",
                  "phi_pic",
                  "phi_ygap",
                  "phi_in",
                  "pistar_bar",
                  "rbar",
                  "risk_bar")

parameters_tex__ <- c("{a\\!1}",
                     "{a\\!2}",
                     "{a\\!3}",
                     "{a\\!4}",
                     "{b\\!1}",
                     "{b\\!2}",
                     "{b\\!3}",
                     "{b\\!4}",
                     "{b\\!5}",
                     "{c\\!1}",
                     "{c\\!2}",
                     "{c\\!3}",
                     "{c\\!4}",
                     "{c\\!5}",
                     "{d\\!z\\!b\\!a\\!r}",
                     "{g\\!s\\!s}",
                     "{i\\!b\\!a\\!r}",
                     "{i\\!s\\!t\\!a\\!r}^{\\mathrm{bar}}",
                     "{m\\!e\\!t\\!a}",
                     "\\phi^{\\mathrm{pic}}",
                     "\\phi^{\\mathrm{ygap}}",
                     "\\phi^{\\in}",
                     "{p\\!i\\!s\\!t\\!a\\!r}^{\\mathrm{bar}}",
                     "{r\\!b\\!a\\!r}",
                     "{r\\!i\\!s\\!k}^{\\mathrm{bar}}")

# free parameters
parameters_free__ <- c("a1",
                       "a2",
                       "a3",
                       "a4",
                       "b1",
                       "b2",
                       "b3",
                       "b4",
                       "b5",
                       "c1",
                       "c2",
                       "c3",
                       "c4",
                       "c5",
                       "dzbar",
                       "gss",
                       "ibar",
                       "istar_bar",
                       "meta",
                       "phi_pic",
                       "phi_ygap",
                       "phi_in",
                       "pistar_bar",
                       "rbar",
                       "risk_bar")

# free parameters' values
parameters_free_val__ <- c(0.4,
                           0.6,
                           0.05,
                           0.1,
                           0.5,
                           0.05,
                           0.15,
                           0.05,
                           0.1,
                           0.5,
                           0.5,
                           0.5,
                           0.5,
                           0.5,
                           0,
                           5,
                           7,
                           2,
                           4,
                           1.5,
                           0.4,
                           0.7,
                           2,
                           3,
                           3)

# equations
equations__ <- c("ds[-1] - ds__lag_1[] = 0",
                 "ds__lag_1[-1] - ds__lag_2[] = 0",
                 "pic[-1] - pic__lag_1[] = 0",
                 "pic__lag_1[-1] - pic__lag_2[] = 0",
                 "-picA2[] + E[][picA1[1]] = 0",
                 "-picA1[] + E[][picA[1]] = 0",
                 "-rbar + r[] - rgap[] = 0",
                 "eta_ystar[] - ystar[] + c4 * ystar[-1] = 0",
                 "-r[] + tpm[] - E[][pic[1]] = 0",
                 "-0.25 * dzbar + zgap[-1] + 0.25 * dz[] - zgap[] = 0",
                 "-ygap[-1] + gybar[] - gy[] + ygap[] = 0",
                 "ds[] - dz[] - pic[] + pistar[] = 0",
                 "eta_tpm[] - tpm[] + phi_in * tpm[-1] + (1 - phi_in) * (ibar + phi_pic * (-meta + E[][picA[1]]) + phi_ygap * ygap[]) = 0",
                 "eta_ybar[] - gybar[] + c1 * gybar[-1] + gss * (1 - c1) = 0",
                 "eta_pistar[] - pistar[] + c2 * pistar[-1] + pistar_bar * (1 - c2) = 0",
                 "eta_istar[] - istar[] + c3 * istar[-1] + istar_bar * (1 - c3) = 0",
                 "eta_risk[] - risk[] + c5 * risk[-1] + risk_bar * (1 - c5) = 0",
                 "0.25 * ds[-1] + 0.25 * ds__lag_1[-1] + 0.25 * ds__lag_2[-1] + 0.25 * ds[] - dsA[] = 0",
                 "0.25 * pic[-1] + 0.25 * pic__lag_1[-1] + 0.25 * pic__lag_2[-1] + 0.25 * pic[] - picA[] = 0",
                 "eta_UIP[] + istar[] + risk[] - tpm[] + E[][ds[1]] = 0",
                 "eta_pic[] - pic[] + a1 * pic[-1] + a2 * E[][pic[1]] + a3 * ygap[] + a4 * zgap[] = 0",
                 "eta_ygap[] - ygap[] + b1 * ygap[-1] + b2 * E[][ygap[1]] - b3 * rgap[] + b4 * zgap[] + b5 * ystar[] = 0")

# calibrating equations
calibr_equations__ <- character(0)

# variables / equations map
vareqmap__ <- sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                                 6, 6, 7, 7, 8, 9, 9, 9, 10, 10,
                                 11, 11, 11, 12, 12, 12, 12, 13, 13, 13,
                                 14, 15, 16, 17, 18, 18, 18, 18, 19, 19,
                                 19, 19, 20, 20, 20, 20, 21, 21, 21, 22,
                                 22, 22, 22),
                           j = c(1, 3, 3, 4, 9, 13, 13, 14, 10, 12,
                                 11, 12, 16, 17, 21, 9, 16, 19, 5, 22,
                                 6, 7, 20, 1, 5, 9, 15, 11, 19, 20,
                                 6, 15, 8, 18, 1, 2, 3, 4, 9, 11,
                                 13, 14, 1, 8, 18, 19, 9, 20, 22, 17,
                                 20, 21, 22),
                           x = c(1, 2, 1, 2, 1, 2, 1, 2, 2, 4,
                                 4, 2, 2, 2, 3, 4, 2, 2, 2, 3,
                                 2, 2, 3, 2, 2, 2, 2, 4, 3, 2,
                                 3, 3, 3, 3, 3, 2, 1, 1, 3, 2,
                                 1, 1, 4, 2, 2, 2, 7, 2, 2, 2,
                                 7, 2, 2),
                           dims = c(22, 22))

# variables / calibrating equations map
varcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 22))

# calibrated parameters / equations map
calibrpareqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(22, 0))

# calibrated parameters / calibrating equations map
calibrparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 0))

# free parameters / equations map
freepareqmap__ <- sparseMatrix(i = c(7, 8, 10, 13, 13, 13, 13, 13, 14, 14,
                                     15, 15, 16, 16, 17, 17, 21, 21, 21, 21,
                                     22, 22, 22, 22, 22),
                               j = c(24, 13, 15, 17, 19, 20, 21, 22, 10, 16,
                                     11, 23, 12, 18, 14, 25, 1, 2, 3, 4,
                                     5, 6, 7, 8, 9),
                               x = rep(1, 25), dims = c(22, 25))

# free parameters / calibrating equations map
freeparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 25))

# shocks / equations map
shockeqmap__ <- sparseMatrix(i = c(8, 13, 14, 15, 16, 17, 20, 21, 22),
                             j = c(8, 3, 4, 6, 7, 9, 5, 2, 1),
                             x = rep(1, 9), dims = c(22, 9))

# steady state equations
ss_eq__ <- function(v, pc, pf)
{
    r <- numeric(22)
    r[1] = v[1] - v[3]
    r[2] = v[3] - v[4]
    r[3] = v[9] - v[13]
    r[4] = v[13] - v[14]
    r[5] = -v[10] + v[12]
    r[6] = v[11] - v[12]
    r[7] = -pf[24] + v[16] - v[17]
    r[8] = -v[21] + pf[13] * v[21]
    r[9] = -v[9] - v[16] + v[19]
    r[10] = -0.25 * pf[15] + 0.25 * v[5]
    r[11] = v[6] - v[7]
    r[12] = v[1] - v[5] - v[9] + v[15]
    r[13] = -v[19] + pf[22] * v[19] + (1 - pf[22]) * (pf[17] + pf[20] * (-pf[19] + v[11]) + pf[21] * v[20])
    r[14] = -v[6] + pf[10] * v[6] + pf[16] * (1 - pf[10])
    r[15] = -v[15] + pf[11] * v[15] + pf[23] * (1 - pf[11])
    r[16] = -v[8] + pf[12] * v[8] + pf[18] * (1 - pf[12])
    r[17] = -v[18] + pf[14] * v[18] + pf[25] * (1 - pf[14])
    r[18] = 0.5 * v[1] - v[2] + 0.25 * v[3] + 0.25 * v[4]
    r[19] = 0.5 * v[9] - v[11] + 0.25 * v[13] + 0.25 * v[14]
    r[20] = v[1] + v[8] + v[18] - v[19]
    r[21] = -v[9] + pf[1] * v[9] + pf[2] * v[9] + pf[3] * v[20] + pf[4] * v[22]
    r[22] = -v[20] + pf[5] * v[20] + pf[6] * v[20] - pf[7] * v[17] + pf[8] * v[22] + pf[9] * v[21]

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
    jac <- numeric(51)
    jac[1] = 1
    jac[2] = -1
    jac[3] = 1
    jac[4] = -1
    jac[5] = 1
    jac[6] = -1
    jac[7] = 1
    jac[8] = -1
    jac[9] = -1
    jac[10] = 1
    jac[11] = 1
    jac[12] = -1
    jac[13] = 1
    jac[14] = -1
    jac[15] = -1 + pf[13]
    jac[16] = -1
    jac[17] = -1
    jac[18] = 1
    jac[19] = 0.25
    jac[20] = 1
    jac[21] = -1
    jac[22] = 1
    jac[23] = -1
    jac[24] = -1
    jac[25] = 1
    jac[26] = pf[20] * (1 - pf[22])
    jac[27] = -1 + pf[22]
    jac[28] = pf[21] * (1 - pf[22])
    jac[29] = -1 + pf[10]
    jac[30] = -1 + pf[11]
    jac[31] = -1 + pf[12]
    jac[32] = -1 + pf[14]
    jac[33] = 0.5
    jac[34] = -1
    jac[35] = 0.25
    jac[36] = 0.25
    jac[37] = 0.5
    jac[38] = -1
    jac[39] = 0.25
    jac[40] = 0.25
    jac[41] = 1
    jac[42] = 1
    jac[43] = 1
    jac[44] = -1
    jac[45] = -1 + pf[1] + pf[2]
    jac[46] = pf[3]
    jac[47] = pf[4]
    jac[48] = -pf[7]
    jac[49] = -1 + pf[5] + pf[6]
    jac[50] = pf[9]
    jac[51] = pf[8]
    jacob <- sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                                6, 6, 7, 7, 8, 9, 9, 9, 10, 11,
                                11, 12, 12, 12, 12, 13, 13, 13, 14, 15,
                                16, 17, 18, 18, 18, 18, 19, 19, 19, 19,
                                20, 20, 20, 20, 21, 21, 21, 22, 22, 22,
                                22),
                          j = c(1, 3, 3, 4, 9, 13, 13, 14, 10, 12,
                                11, 12, 16, 17, 21, 9, 16, 19, 5, 6,
                                7, 1, 5, 9, 15, 11, 19, 20, 6, 15,
                                8, 18, 1, 2, 3, 4, 9, 11, 13, 14,
                                1, 8, 18, 19, 9, 20, 22, 17, 20, 21,
                                22),
                          x = jac, dims = c(22, 22))

    return(jacob)
}

# 1st order perturbation
pert1__ <- function(v, pc, pf)
{
    Atm1x <- numeric(20)
    Atm1x[1] = 1
    Atm1x[2] = 1
    Atm1x[3] = 1
    Atm1x[4] = 1
    Atm1x[5] = pf[13]
    Atm1x[6] = 1
    Atm1x[7] = -1
    Atm1x[8] = pf[22]
    Atm1x[9] = pf[10]
    Atm1x[10] = pf[11]
    Atm1x[11] = pf[12]
    Atm1x[12] = pf[14]
    Atm1x[13] = 0.25
    Atm1x[14] = 0.25
    Atm1x[15] = 0.25
    Atm1x[16] = 0.25
    Atm1x[17] = 0.25
    Atm1x[18] = 0.25
    Atm1x[19] = pf[1]
    Atm1x[20] = pf[5]
    Atm1 <- sparseMatrix(i = c(1, 2, 3, 4, 8, 10, 11, 13, 14, 15,
                               16, 17, 18, 18, 18, 19, 19, 19, 21, 22),
                         j = c(1, 3, 9, 13, 21, 22, 20, 19, 6, 15,
                               8, 18, 1, 3, 4, 9, 13, 14, 9, 20),
                         x = Atm1x, dims = c(22, 22))

    Atx <- numeric(40)
    Atx[1] = -1
    Atx[2] = -1
    Atx[3] = -1
    Atx[4] = -1
    Atx[5] = -1
    Atx[6] = -1
    Atx[7] = 1
    Atx[8] = -1
    Atx[9] = -1
    Atx[10] = -1
    Atx[11] = 1
    Atx[12] = 0.25
    Atx[13] = -1
    Atx[14] = 1
    Atx[15] = -1
    Atx[16] = 1
    Atx[17] = 1
    Atx[18] = -1
    Atx[19] = -1
    Atx[20] = 1
    Atx[21] = -1
    Atx[22] = pf[21] * (1 - pf[22])
    Atx[23] = -1
    Atx[24] = -1
    Atx[25] = -1
    Atx[26] = -1
    Atx[27] = 0.25
    Atx[28] = -1
    Atx[29] = 0.25
    Atx[30] = -1
    Atx[31] = 1
    Atx[32] = 1
    Atx[33] = -1
    Atx[34] = -1
    Atx[35] = pf[3]
    Atx[36] = pf[4]
    Atx[37] = -pf[7]
    Atx[38] = -1
    Atx[39] = pf[9]
    Atx[40] = pf[8]
    At <- sparseMatrix(i = c(1, 2, 3, 4, 5, 6, 7, 7, 8, 9,
                             9, 10, 10, 11, 11, 11, 12, 12, 12, 12,
                             13, 13, 14, 15, 16, 17, 18, 18, 19, 19,
                             20, 20, 20, 21, 21, 21, 22, 22, 22, 22),
                       j = c(3, 4, 13, 14, 10, 12, 16, 17, 21, 16,
                             19, 5, 22, 6, 7, 20, 1, 5, 9, 15,
                             19, 20, 6, 15, 8, 18, 1, 2, 9, 11,
                             8, 18, 19, 9, 20, 22, 17, 20, 21, 22),
                         x = Atx, dims = c(22, 22))

    Atp1x <- numeric(7)
    Atp1x[1] = 1
    Atp1x[2] = 1
    Atp1x[3] = -1
    Atp1x[4] = pf[20] * (1 - pf[22])
    Atp1x[5] = 1
    Atp1x[6] = pf[2]
    Atp1x[7] = pf[6]
    Atp1 <- sparseMatrix(i = c(5, 6, 9, 13, 20, 21, 22),
                         j = c(12, 11, 9, 11, 1, 9, 20),
                         x = Atp1x, dims = c(22, 22))

    Aepsx <- numeric(9)
    Aepsx[1] = 1
    Aepsx[2] = 1
    Aepsx[3] = 1
    Aepsx[4] = 1
    Aepsx[5] = 1
    Aepsx[6] = 1
    Aepsx[7] = 1
    Aepsx[8] = 1
    Aepsx[9] = 1
    Aeps <- sparseMatrix(i = c(8, 13, 14, 15, 16, 17, 20, 21, 22),
                         j = c(8, 3, 4, 6, 7, 9, 5, 2, 1),
                         x = Aepsx, dims = c(22, 9))

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
