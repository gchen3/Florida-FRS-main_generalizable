
# Read FRS liability tables
db_dc_legacy_table <- read_excel(here::here(sddir, "FRS_liability_table.xlsx"), sheet = "db_dc_legacy") 
db_dc_new_table <- readxl::read_excel(file.path(sddir, "FRS_liability_table.xlsx"), sheet = "db_dc_new") 

# Read current year table to get current benefit payments, current retiree population, current normal cost, and current pvfb
current_year_table <- readxl::read_excel(file.path(sddir, "FRS_liability_table.xlsx"), sheet = "current_year")



#.. FRS Retirement benefit assumptions
pension_payment_ <- 11944986866         #Pension Payments in Reconciliation of Market Value of Assets Used for Valuation table
contribution_refunds_ <- 28343757       #Contribution Refunds in Reconciliation of Market Value of Assets Used for Valuation table
disbursement_to_ip_ <- 768106850        #Disbursements to Investment Plan in Reconciliation of Market Value of Assets Used for Valuation table
admin_expense_ <- 22494571              #Administrative Expenses in Reconciliation of Market Value of Assets Used for Valuation table

ben_payment_ratio_ <- pension_payment_ /
  (pension_payment_ + contribution_refunds_ + disbursement_to_ip_ + admin_expense_)

#Below are the cash outflows for each membership class. These numbers are Benefit Payments and other Disbursements in the Development of Actuarial Value of Assets table
regular_outflow_ <- 8967096000
special_outflow_ <- 2423470000
admin_outflow_ <- 8090000
judges_outflow_ <- 105844000
eso_outflow_ <- 53526000
eco_outflow_ <- 9442000
senior_management_outflow_ <- 338664000

#Below are the estimated benefit payments for each membership class
regular_ben_payment_current_ <- regular_outflow_ * ben_payment_ratio_
special_ben_payment_current_ <- special_outflow_ * ben_payment_ratio_
admin_ben_payment_current_ <- admin_outflow_ * ben_payment_ratio_
judges_ben_payment_current_ <- judges_outflow_ * ben_payment_ratio_
eso_ben_payment_current_ <- eso_outflow_ * ben_payment_ratio_
eco_ben_payment_current_ <- eco_outflow_ * ben_payment_ratio_
senior_management_ben_payment_current_ <- senior_management_outflow_ * ben_payment_ratio_

#Below are the numbers of annuitants for each membership class, representing the retiree populations
regular_retiree_pop_current_ <- 393308
special_retiree_pop_current_ <- 41696
admin_retiree_pop_current_ <- 160
judges_retiree_pop_current_ <- 989
eso_retiree_pop_current_ <- 1446
eco_retiree_pop_current_ <- 227
senior_management_retiree_pop_current_ <- 5828


#Adjust this calibration factor after getting preliminary results from the benefit model.

regular_val_norm_cost_ <- 0.0896               #Normal cost for Regular members from the val report
special_val_norm_cost_ <- 0.2013               #Normal cost for Special Risk members from the val report
admin_val_norm_cost_ <- 0.1457                 #Normal cost for Special Risk Admin members from the val report
judges_val_norm_cost_ <- 0.1777                #Normal cost for Judicial members from the val report
eso_val_norm_cost_ <- 0.1463                   #Normal cost for Local members from the val report
eco_val_norm_cost_ <- 0.1254                   #Normal cost for Leg-Atty-Cab members from the val report
senior_management_val_norm_cost_ <- 0.1086     #Normal cost for Senior Management members from the val report

#Below are calibration factors to further adjust the normal costs to match the numbers from the val report.These are calculated by dividing the normal costs from the val report by the normal costs from the benefit model.
regular_nc_cal_ <- regular_val_norm_cost_ / 0.09096784
special_nc_cal_ <- special_val_norm_cost_ / 0.2044051
admin_nc_cal_ <- admin_val_norm_cost_ / 0.10436284
judges_nc_cal_ <- judges_val_norm_cost_ / 0.1937982
eso_nc_cal_ <- eso_val_norm_cost_ / 0.1557111
eco_nc_cal_ <- eco_val_norm_cost_ / 0.1513904
senior_management_nc_cal_ <- senior_management_val_norm_cost_ / 0.11295223


#Assumptions about the "remaining" accrued liability (which is not accounted for by the PVFB calculations).
#This is lumped together with the accrued liability for current term vested members.
#The value is calculated by subtracting the preliminary actuarial accrued liability produced by the liability model from the accrued liability from the val report.
#The preliminary accrued liability is the liability calculated by the liability model when these values below are set to 0.
regular_pvfb_term_current_ <- 145585523000 - 138993598036
special_pvfb_term_current_ <- 45070773000 - 41833009006
admin_pvfb_term_current_ <- 90337000 - 92432291
eco_pvfb_term_current_ <- 138008000 - 110403603
eso_pvfb_term_current_ <- 751363000 - 720397602
judges_pvfb_term_current_ <- 1545348000 - 1444240024
senior_management_pvfb_term_current_ <- 6039701000 - 5404229360

current_year_table 


