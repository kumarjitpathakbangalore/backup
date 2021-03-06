rm(list = ls())

setwd("E:\\Hayatt\\Research on KPI\\HYATT ACTUAL DATA")

jan_data<-read.csv('jan_data.del',sep=",")
feb_data<-read.csv('feb_data.del',sep=",")
march_data<-read.csv('march_data.del',sep=",")
april_data<-read.csv('april_data.del',sep=",")
may_data<-read.csv('may_data.del',sep=",")
june_data<-read.csv('june_data.del',sep=",")
july_data<-read.csv('july_data.del',sep=",")
aug_data<-read.csv('aug_dat1.txt',sep=",")
sept_14_data<-read.csv('sept_14_data.del',sep=",")
oct_14_data<-read.csv('oct_14_data.del',sep=",")
nov_14_data<-read.csv('nov_14_data.del',sep=",")
dec_14_data<-read.csv('dec_14_data.del',sep=",")

names(aug_data)<-c("CG_CONS_GUEST_ID","CG_LAST_NAME","CG_FIRST_NAME","CG_GENDER","CG_HOME_ADDR_LN1","CG_HOME_ADDR_LN2","CG_HOME_ADDR_CITY","CG_HOME_ADDR_STATE","CG_HOME_ADDR_ZIP_CD","CG_HOME_ADDR_COUNTRY_CD","CG_HOME_PHONE_NUM","CG_BUS_ADDR_LN1","CG_BUS_ADDR_LN2","CG_BUS_ADDR_CITY","CG_BUS_ADDR_STATE","CG_BUS_ADDR_ZIP_CD","CG_BUS_ADDR_COUNTRY_CD","CG_BUS_PHONE_NUM","CG_HOME_EMAIL","CG_BUS_EMAIL","CG_GP_NUM","SRC_SYS_ID","CHECK_IN_DATE","CHECK_OUT_DATE","ALT_CONF_NO","SETTLEMENT_CODE","RESERVATION_DATE","DIRECT_POV_CODE","INDIRECT_POV_CODE","FINAL_POV_CODE","POV_FAMILY","GUARANTEED_CC","CANCELLATION_DATE","PMS_OTHER_REV","PMS_OTHER_REV_USD","PMS_TOTAL_REV","PMS_TOTAL_REV_USD","PR_ROOM_REV","PR_ROOM_REV_USD","NUMBER_OF_ROOMS","ROOM_NUM","ADULT_NUM","CHILDREN_NUM","PMS_ROOM_REV","PMS_ROOM_REV_USD","PMS_FOOD_BEVERAGE_REV","PR_FOOD_BEVERAGE_REV","PR_FOOD_BEVERAGE_REV_USD","PR_OTHER_REV","PR_OTHER_REV_USD","PR_TOTAL_REV","PR_TOTAL_REV_USD","STAY_DATE","PMS_FOOD_BEVERAGE_REV_USD","OFFER_FLG","OFFER_CODE_FIRST_LETTER","PRIVATE_LINE_FLG","ITIN","MAJOR_MARKET_CODE","PMS_RATE_CATEGORY_CODE","GH_NUM","TOUR_CODE","NT_RATE","REC_LOC","REQ_CD1","ETA","TRAVEL_AGENT_NAME","CONFIRMATION_REQUEST","EXT_RMK_FLG","AIR_RATE_CATEGROY_CODE","RP_CD","FORCE_SELL_FLG","GOLD_PASSPORT_NUM","GUEST_CODE","DAY_CODE","POV_CODE","LENGTH_OF_STAY","LENGTH_OF_STAY_CATEGORY","HTL_TYP","GOLDPASSPORT_FLG","BKNG_SRC_CD","CHANNEL_CODE","LANG_PREF","PIK_FLAG","HOTEL_ABBR","SHR_IDX","GDS_NPR","TITLE","VIP_STATUS","USR_SRC_CD","ENTRY_HTL_CD","STATUS_CALCULATION","GROUPS_VS_FIT","MEMBER_STATUS","PACE_CATEGORY","PACE","HTL_CD","CONFIRMATION_NUM","CONFIRMATION_PAGE","GDS_NPR","ENTRY_HOTEL_CODE","RP_KEY","REPORT_FLG","DIAM_CC","ACCOUNT_CODE","DEFAULT_MARKET_CODE","DEFAULT_RATE_CATEGORY_CODE","ROH_CODE","ROH_ID","DEFAULT_AREA_CODE","DEPOSIT_FLG","LOC_CODE","HOTEL_CLASS","SPIRIT_DIVISION","LAST_INVOICE_DATE","PAPER_SIZE","Q_CB","Q_FB","Q_MESSAGE","Q_CONFIRMATION","Q_PRT","PRICE_ORD","TM_DIFF","SEND_WLIST","RMS_CODE","ETA_REQUIRED","ALL_POINTS","GROUP_FLG","Q_EXT_RMK","AUTO_CXL_PRE_FLG","Q1_ARRIVAL","FROM_REQ","WARN_MESSAGE","CREATE_GUEST_REC","OTHER_CHARGE","CM_FLG","NEW_RATES_FLG","SIM_FLG","WRAP_UP","BK_LMT_PCT","AUTO_RTCHG_FLG","IGNORE_CUTOFF","BRAND_NAME","OWNERSHIP_TYPE","SEGMENT","PROPERTY_SHORT_NAME","HOTEL_NAME","CY_COMPARABLE_MANAGED","CY_COMPARABLE_OWNED","CY_COMPARABLE_STR","OWNER","OPERATOR","STR_NUMBER","PROPERTY_SUB_TYPE","MEETING_SPACE","MEETING_ROOM_LARGEST","PLANNING_GEO2","PLANNING_GEO3","OPS_REGION","OPS_VP","JOINT_VENTURE","JOINT_VENTURE_PCT","GP_CLASS","BRAND_CHANGE_DATE","PREVIOUS_BRAND","PREVIOUS_OWNERSHIP_TYPE","STR_MARKET","STR_LOCATION_TYPE","NUM_SUITES","OWNERSHIP_TYPE_CHANGE_DATE","PREVIOUS_OWNER","OWNER_CHANGE_DATE","PREVIOUS_OPERATOR","OPERATOR_CHANGE_DATE","HOTEL_WEIGHT","OPERA_CONVERSION_DATE","ENVISION_CONVERSION_DATE","CATEGORY","SELECT_CS_GOAL","BI_CATEGORY","BI_SUPPORT_REGION","SOURCE_SYSTEM_CODE","PROPERTY_CODE","PROPERTY_NAME","PROPERTY_TYPE","MTC_FLG","INF_TYPE","CONFIRMATION_PRT_FLG","CURRENCY_CODE","EFFECTIVE_DATE","EXPIRATION_DATE","DEFAULT_RATE_PLAN_ID","DPS_FROM_RMS","DOMESTIC_ZIP_FLG","BASE_THRESHOLD","BOOKING_SOURCE_REQUIRED","RMS_TYPE","ALT_HTL_INFO","ALLOW_MAIL_CONFIRMATION","BRAND_CD","HOTEL_MAX_STAY","DEPOSIT_THRESHOLD","PROPERTY_STATUS","PROPERTY_ABBRV","OPERATING_PROPERTY_TYPE","TIME_ZONE","HOTEL_EMAIL","LAST_RENV_END_DATE","TIME_DIFF_CST","TIME_DIFF_GMT","LAST_RENOVATION_START_DATE","ENVISION_PROPERTY_ID","OPEN_DATE","TEMP_CLOSURE_END_DATE","SINGLE_IMAGE_YN","BENEFITS_NUMBER","HOTEL_URL","USAGE_TYPE","CLOSE_DATE","DIGITAL_DIVISION","DIGITAL_REGION","DIGITAL_MARKET","PROPERTY_MANAGEMENT_SYSTEM","SALES_SYSTEM","MSI_PROPERTY_CODE","TEMP_CLOSURE_START_DATE","DELPHI_PROPERTY_ID","OPERA_SYSCODE","STAY_CURRENCY_CODE","BOOKING_CURRENCY_CODE","CATERING_CURRENCY_CODE","SPA_FLG","CR_PROPERTY_LONG_NAME","CR_REGION","CR_GSS_AVP","CR_GSS_INTERVIEW","CR_MP_INTERVIEW","CR_GSS_GUEST_NPS_GOAL","CR_MP_NPS_GOAL","CAMP_HYATT","CONCIERGE","BUSINESS_CENTER","FITNESS_CENTER","POOL","GOLF","LEADERSHIP_GROUP","BUCKET","HOTEL_NAME_INVITE"
                   ,"CLUB_TYPE","VACATION_PROPERTY","MINIBAR","LAUNDRY","BELLSTAFF","TRANSPORTATION","VALET","AV_COMPANY","SPA_SERVICESFC","SPA_NAME","GOLF_COURSE_NAME","REGION_NAME","DIVISION_NAME","TRAVEL_CLICK_FLG","UPSELL_AGENT_PROCESS_IND","DIRECT_BILL_FLG","REGION","OPS_REGION1","OPS_REGION2","RESERVATION_ID","PMS_STAY_ID","RESERVATION_STATUS_ID")

names(jan_data)<-names(aug_data)
names(feb_data)<-names(aug_data)
names(march_data)<-names(aug_data)
names(april_data)<-names(aug_data)
names(may_data)<-names(aug_data)
names(june_data)<-names(aug_data)
names(july_data)<-names(aug_data)
names(sept_14_data)<-names(aug_data)
names(oct_14_data)<-names(aug_data)
names(nov_14_data)<-names(aug_data)
names(dec_14_data)<-names(aug_data)

Final_Data<-rbind(jan_data,feb_data,march_data,april_data,may_data,june_data,july_data,aug_data,sept_14_data,oct_14_data,nov_14_data,dec_14_data)


library(sqldf)
sqldf('select count(reservation_id) from Final_Data')
#1123412

sqldf('select count(reservation_id) from Final_Data')

Final_Data$CHECK_OUT_DATE1<-substr(Final_Data$CHECK_OUT_DATE,6,7)

monthly_revenue<-sqldf('select CHECK_OUT_DATE1 as month,count(reservation_id), sum(PMS_OTHER_REV_USD), sum(PMS_TOTAL_REV_USD),sum(PR_ROOM_REV_USD), sum(PMS_ROOM_REV_USD), sum(PMS_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV_USD), sum(PR_OTHER_REV), sum(PR_OTHER_REV_USD), sum(PR_TOTAL_REV), sum(PR_TOTAL_REV_USD), sum(PMS_FOOD_BEVERAGE_REV_USD) from Final_Data group by 1')
monthly_offers<-sqldf('select CHECK_OUT_DATE1 as month, OFFER_CODE_FIRST_LETTER,count(reservation_id), sum(PMS_OTHER_REV_USD), sum(PMS_TOTAL_REV_USD),sum(PR_ROOM_REV_USD), sum(PMS_ROOM_REV_USD), sum(PMS_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV_USD), sum(PR_OTHER_REV), sum(PR_OTHER_REV_USD), sum(PR_TOTAL_REV), sum(PR_TOTAL_REV_USD), sum(PMS_FOOD_BEVERAGE_REV_USD) from Final_Data group by 1,2')
monthly_GPs<-sqldf('select CHECK_OUT_DATE1 as month, count(GOLD_PASSPORT_NUM),count(reservation_id), sum(PMS_OTHER_REV_USD), sum(PMS_TOTAL_REV_USD),sum(PR_ROOM_REV_USD), sum(PMS_ROOM_REV_USD), sum(PMS_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV_USD), sum(PR_OTHER_REV), sum(PR_OTHER_REV_USD), sum(PR_TOTAL_REV), sum(PR_TOTAL_REV_USD), sum(PMS_FOOD_BEVERAGE_REV_USD) from Final_Data group by 1')
monthly_member_status<-sqldf('select CHECK_OUT_DATE1 as month, MEMBER_STATUS,count(reservation_id), sum(PMS_OTHER_REV_USD), sum(PMS_TOTAL_REV_USD),sum(PR_ROOM_REV_USD), sum(PMS_ROOM_REV_USD), sum(PMS_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV_USD), sum(PR_OTHER_REV), sum(PR_OTHER_REV_USD), sum(PR_TOTAL_REV), sum(PR_TOTAL_REV_USD), sum(PMS_FOOD_BEVERAGE_REV_USD) from Final_Data group by 1,2')
monthly_vip_status<-sqldf('select CHECK_OUT_DATE1 as month, VIP_STATUS,count(reservation_id), sum(PMS_OTHER_REV_USD), sum(PMS_TOTAL_REV_USD),sum(PR_ROOM_REV_USD), sum(PMS_ROOM_REV_USD), sum(PMS_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV_USD), sum(PR_OTHER_REV), sum(PR_OTHER_REV_USD), sum(PR_TOTAL_REV), sum(PR_TOTAL_REV_USD), sum(PMS_FOOD_BEVERAGE_REV_USD) from Final_Data group by 1,2')
monthly_status_calculation<-sqldf('select CHECK_OUT_DATE1 as month, STATUS_CALCULATION,count(reservation_id), sum(PMS_OTHER_REV_USD), sum(PMS_TOTAL_REV_USD),sum(PR_ROOM_REV_USD), sum(PMS_ROOM_REV_USD), sum(PMS_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV), sum(PR_FOOD_BEVERAGE_REV_USD), sum(PR_OTHER_REV), sum(PR_OTHER_REV_USD), sum(PR_TOTAL_REV), sum(PR_TOTAL_REV_USD), sum(PMS_FOOD_BEVERAGE_REV_USD) from Final_Data group by 1,2')

View(monthly_revenue)

View(Final_Data$CHECK_OUT_DATE1)
write.csv(monthly_revenue,'monthly_revenue.csv')
write.csv(monthly_offers,'monthly_offers.csv')
write.csv(monthly_GPs,'monthly_GPs.csv')
write.csv(monthly_member_status,'monthly_member_status.csv')
write.csv(monthly_vip_status,'monthly_vip_status.csv')
write.csv(monthly_status_calculation,'monthly_status_calculation.csv')
