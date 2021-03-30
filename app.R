# License and Copyright Information
#
# This Premorbid IQ Simulator Shiny App by Brandon Gavett is licensed under a 
# Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# Based on work at https://github.com/begavett/simulate_pIQ
# Copyright 2021 Brandon Gavett
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(dplyr)
library(data.table)
library(xtable)
library(mirt)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Simulate premorbid IQ and cognitive test data"),

    # Sidebar with numeric inputs 
    sidebarLayout(
        sidebarPanel(
            numericInput("theta_pIQ_SS",
                        "Population mean for IQ (Standard Score units):",
                        min = 70,
                        max = 130,
                        value = 100),
            numericInput("rho_piq_cog",
                         "Population correlation between pIQ and cognitive test:",
                         min = 0,
                         max = 1,
                         value = .44),
            numericInput("z_cutoff",
                         "Cutoff for classifying a score as abnormal (number of standard deviations from the reference):",
                         min = -3,
                         max = -.01,
                         value = -1),
            numericInput("br_cd",
                         "Base rate of cognitive disorder in the population:",
                         min = .05,
                         max = .95,
                         value = .4),
            numericInput("delta_cd",
                         "Population effect size of cognitive disorder on cognitive test (number of standard deviations from the mean):",
                         min = -3,
                         max = -.01,
                         value = -.70),
            numericInput("sim_n",
                         "Number of cases to simulate (higher = more precise estimates):",
                         min = 100,
                         max = 1e07,
                         value = 1e06),
            numericInput("edu",
                         "Years of education:",
                         min = 0,
                         max = 20,
                         value = 12),
            numericInput("icr_Cog",
                         "Internal consistency reliability of cognitive test:",
                         min = .01,
                         max = .99,
                         value = .90),
            numericInput("truncateCogLo",
                         "Minimum possible score on cognitive test (Standard Score units):",
                         min = 10,
                         max = 85,
                         value = 40),
            numericInput("truncateCogHi",
                         "Maximum possible score on cognitive test (Standard Score units):",
                         min = 115,
                         max = 190,
                         value = 160),
            actionButton("runSim", "Run Simulation")
            
        ),

        # Show the results
        mainPanel(
            h1("Results"),
            htmlOutput("resultsSummary")
        )
    )
)

# Define server logic required to simulate the data
server <- function(input, output, session){
    
    sim_piq_cog <- function(cor_pIQ_Cog = input$rho_piq_cog, 
                            icr_Cog = input$icr_Cog, 
                            Theta_pIQ = (input$theta_pIQ_SS - 100)/15, 
                            br = input$br_cd,
                            Cut = input$z_cutoff, 
                            edu = input$edu, 
                            cdES = input$delta_cd, 
                            cdThetaCog = (input$theta_pIQ_SS - 100)/15 + input$delta_cd, 
                            truncateCogLo = (input$truncateCogLo - 100)/15, 
                            truncateCogHi = (input$truncateCogHi - 100)/15,
                            sim_n = input$sim_n){
        
        ncd <- as.integer(sim_n*br)
        ncn <- as.integer(sim_n - ncd)
        
        cnThetaCog <- Theta_pIQ
        cdThetaIQ <- Theta_pIQ
        
        covMat_pIQ_Cog <- matrix(c(1, cor_pIQ_Cog, cor_pIQ_Cog, 
                                       cor_pIQ_Cog, 1, icr_Cog,
                                       cor_pIQ_Cog, icr_Cog, 1), nrow = 3, byrow = TRUE)
        
        set.seed(8675309)
        cn <- data.frame(mvrnorm(ncn, mu = c(Theta_pIQ, cnThetaCog, cnThetaCog), Sigma = covMat_pIQ_Cog))
        names(cn) <- c("Theta_IQ_i", "Theta_pCog_i", "Obs_Cog_z")
        cn$Edu_i <- edu
        cn$Group <- "CN"
        
        set.seed(90210)
        cd <- data.frame(mvrnorm(ncd, mu = c(Theta_pIQ, cnThetaCog, cdThetaCog), Sigma = covMat_pIQ_Cog))
        names(cd) <- c("Theta_IQ_i", "Theta_pCog_i", "Obs_Cog_z")
        cd$Edu_i <- edu
        cd$Group <- "CD"
        
        both <- bind_rows(cn, cd)
        both$Theta <- Theta_pIQ
        both$rho_piq_cog <- cor_pIQ_Cog
        both$cd_br <- br
        both$Cut <- Cut
        both$delta_cog_cd <- cdES
        
        dat_ind <- both
        rm(both)
        
        amnart_coef <- structure(list(a1 = c(1.91340390332552, 1.67282788157484, 1.5418612330138, 
                                             2.53618638667661, 1.53016479233213, 1.87969776036209, 0.98432461118343, 
                                             2.14131063453984, 2.70485666635488, 2.83665369994675, 2.35280574705403, 
                                             2.25676781378809, 2.81230449504513, 1.28689002541582, 2.19991500282535, 
                                             2.41651480971578, 0.986398549133714, 3.04225139396231, 1.30167459986889, 
                                             1.67801857916187, 1.51427669207732, 2.7135258993079, 2.50602723853578, 
                                             2.81919094261147, 1.70665115227321, 1.82346149607869, 0.972263976161445, 
                                             2.31160509852141, 2.56380212140471, 2.1781354431772, 2.17858455496791, 
                                             1.39823261798244, 1.72530595846228, 1.0808453375695, 1.7028413877837, 
                                             1.45261440186138, 1.11043429665533, 1.00272654199587, 1.1450902127568, 
                                             2.35620728301628, 1.31869508785511, 1.02204977096453, 2.68930780880669, 
                                             1.30481771746215, 1.05189341606903), d = c(3.6581491959349, 2.34510956641479, 
                                                                                        2.62458629121586, 2.9469657293081, 1.74547949250453, 2.23377477346602, 
                                                                                        0.264814499649228, 1.86366081869775, 3.20798630239192, 0.476723166549478, 
                                                                                        3.49181990063508, 3.3175660304417, 2.22694378117984, -0.0996594037305196, 
                                                                                        1.52540974139438, 1.92889757039686, -1.78939406753103, -2.39228208167587, 
                                                                                        -1.39294614739552, -0.831413956936372, -0.575850338683235, 0.00115913024127471, 
                                                                                        -0.374209147025594, 0.236807530162989, -1.06256832530666, 0.504036839298113, 
                                                                                        -0.721459068880437, 1.08454386790892, 1.03742776964994, -1.70855869424773, 
                                                                                        -1.30872899749246, -0.0507183090671781, 0.418586162041944, -1.24482835871387, 
                                                                                        0.150889592410968, -0.60906497517091, -1.63705098949122, -1.33426102762371, 
                                                                                        -0.896194214276525, -3.13372389998924, 1.35837023711015, -1.51665616039459, 
                                                                                        -2.81886568792082, -2.31077448843953, -0.0285736735492737), g = c(0, 
                                                                                                                                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                          0, 0), u = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                       1, 1, 1, 1, 1, 1, 1, 1)), class = "data.frame", row.names = c("Ache", 
                                                                                                                                                                                                                                     "Debt", "Depot", "Chord", "Bouquet", "Deny", "Capon", "Heir", 
                                                                                                                                                                                                                                     "Aisle", "Subtle", "Nausea", "Gauge", "Naive", "Thyme", "Algae", 
                                                                                                                                                                                                                                     "Fetal", "Quadruped", "Epitome", "Superfluous", "Chamois", "Papyrus", 
                                                                                                                                                                                                                                     "Hiatus", "Simile", "Blatant", "Cellist", "Zealot", "Abstemious", 
                                                                                                                                                                                                                                     "Meringue", "Placebo", "Facade", "Pugilist", "Virulent", "Worsted", 
                                                                                                                                                                                                                                     "Detente", "Sieve", "Chassis", "Beatify", "Scion", "Cabal", "Apropos", 
                                                                                                                                                                                                                                     "Caprice", "Imbroglio", "Hyperbole", "Syncope", "Prelate"))
        set.seed(1234567)
        amnart_sim <- as.data.table(data.frame(simdata(
            a = amnart_coef$a1,
            d = amnart_coef$d,
            N = nrow(dat_ind),
            itemtype = "2PL",
            Theta = matrix(dat_ind$Theta_IQ_i, ncol = 1))))
        amnart_sim <- setnames(amnart_sim, names(amnart_sim), rownames(amnart_coef))
        amnart_sim[, errors := rowSums(.SD == 0)]
        amnart_sim$Edu <- dat_ind$Edu
        amnart_sim[, Obs_pIQ := 118.56 - .88*errors + .56*Edu]
        
        dat_ind <- as.data.table(dat_ind)
        
        dat_ind$Obs_pIQ_SS <- amnart_sim$Obs_pIQ
        dat_ind[, Obs_pIQ_z := (Obs_pIQ_SS-100)/15]
        
        dat_ind[Obs_Cog_z > truncateCogHi, Obs_Cog_z := truncateCogHi]  # Truncate distribution like most tests do (e.g., WMS-IV has range of +/- 4 SD)
        dat_ind[Obs_Cog_z < truncateCogLo, Obs_Cog_z := truncateCogLo] # Truncate distribution like most tests do  (e.g., WMS-IV has range of +/- 4 SD)
        dat_ind[, Obs_Cog_SS  := Obs_Cog_z*15+100]
        dat_ind[, Theta_pCog_SS  := Theta_pCog_i*15+100]
        dat_ind[, Obs_pIQ_i  := round(Obs_pIQ_SS, 0)]
        dat_ind[, Theta_pCog_SS  := round(Theta_pCog_SS, 0)]
        dat_ind[, Obs_Cog_i  := round(Obs_Cog_SS, 0)]
        
        dat_ind[, Diff_i := Obs_Cog_i - Obs_pIQ_i]
        dat_ind[, CogChg_SS_i := Obs_Cog_i - Theta_pCog_SS]
        dat_ind[, CogChg_z_i := CogChg_SS_i/15]
        
        
        dat_ind[, below_pIQ := FALSE][Diff_i < Cut*15, below_pIQ := TRUE][, below_pIQ := factor(below_pIQ, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        dat_ind[, belowFixedCut := FALSE][Obs_Cog_i-100 < Cut*15, belowFixedCut := TRUE][, belowFixedCut := factor(belowFixedCut, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        dat_ind[, bothAgree := 0][below_pIQ == belowFixedCut, bothAgree := 1]
        dat_ind[, belowBoth := NA][bothAgree == 1 & below_pIQ == "Negative" & belowFixedCut == "Negative", belowBoth := FALSE][bothAgree == 1 & below_pIQ == "Positive" & belowFixedCut == "Positive", belowBoth := TRUE]
        dat_ind[, belowBoth := factor(belowBoth, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        dat_ind[, below_pIQOnly := NA][bothAgree == 0 & belowFixedCut == "Positive", below_pIQOnly := FALSE][bothAgree == 0 & below_pIQ == "Positive", below_pIQOnly := TRUE]
        dat_ind[, below_pIQOnly := factor(below_pIQOnly, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        dat_ind[, belowFixedCutOnly := NA][bothAgree == 0 & below_pIQ == "Positive", belowFixedCutOnly := FALSE][bothAgree == 0 & belowFixedCut == "Positive", belowFixedCutOnly := TRUE]
        dat_ind[, belowFixedCutOnly := factor(belowFixedCutOnly, levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))]
        
        dat_ind[, Cog_est_norm_i := Obs_Cog_z - 0]
        dat_ind[, Cog_est_pIQ_i := Obs_Cog_z - Obs_pIQ_z]
        dat_ind[, Cog_resid_norm_i := Cog_est_norm_i - CogChg_z_i]
        dat_ind[, Cog_resid_pIQ_i := Cog_est_pIQ_i - CogChg_z_i]
        
        dat_flow <- dat_ind %>% 
            group_by(rho_piq_cog) %>%
            summarise(tp_piq = sum(Group == "CD" & below_pIQOnly == "Positive", na.rm = TRUE),
                      fp_piq = sum(Group == "CN" & below_pIQOnly == "Positive", na.rm = TRUE),
                      fn_piq = sum(Group == "CD" & below_pIQOnly == "Negative", na.rm = TRUE),
                      tn_piq = sum(Group == "CN" & below_pIQOnly == "Negative", na.rm = TRUE),
                      odds_cd_piq = tp_piq / fn_piq,
                      odds_cd_nm = fp_piq / tn_piq,
                      or_piq = odds_cd_piq / odds_cd_nm,
                      log_or_piq = log(or_piq),
                      se_log_or = sqrt(1/tp_piq + 1/fp_piq + 1/fn_piq + 1/tn_piq),
                      ci95_l = exp(log_or_piq - qnorm(.975)*se_log_or),
                      ci95_u = exp(log_or_piq + qnorm(.975)*se_log_or))
        
        out <- dat_flow
        
        return(out)
        
    }

    output$resultsSummary <- eventReactive(input$runSim, {
        withProgress(message = "Running simulation...", value = 0, {
            dat_flow <- sim_piq_cog()
            
            out1 <- paste0("Among the ", format(input$sim_n, big.mark = ",", scientific = FALSE), " cases simulated, there were ", 
                           format(rowSums(dat_flow[c("tp_piq", "fp_piq", "fn_piq", "tn_piq")], na.rm = TRUE), big.mark = ",", scientific = FALSE),
                           " disagreements. ",
                           "Given these inputs, when a disagreement does occur, the ratio of the odds in favor of the pIQ-based comparison being correct, relative to the odds in favor of the norm-based comparison being correct, is OR = ", 
                          round(dat_flow$or_piq, 2),
                   ", 95% CI [", 
                   round(dat_flow$ci95_l, 2), 
                   ", ",
                   round(dat_flow$ci95_u, 2),
                   "].")
            
            out2 <- data.frame(Cognitively_Impaired_Population = c(dat_flow$tp_piq, dat_flow$fn_piq),
                               Cognitively_Normal_Population = c(dat_flow$fp_piq, dat_flow$tn_piq),
                               Total = c(dat_flow$tp_piq + dat_flow$fp_piq, dat_flow$fn_piq + dat_flow$tn_piq))
            rownames(out2) <- c("pIQ Abnormal / norm-based Normal",
                                "pIQ Normal / norm-based Abnormal")

            out <- print(xtable(out2, caption = out1, align = c("l", "c", "c", "c")), type = "html")
            return(out)
        })
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
