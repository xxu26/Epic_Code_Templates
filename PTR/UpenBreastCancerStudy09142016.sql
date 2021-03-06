/*
SUTTER HEALTH WITH UNIVERSITY OF PENNSILVNIA 
INVESTIGATOR: SHERRY YAN
AUTHOR: LYDIA XU
PROJECT: BREAST CANCER STUDY
CRITERIA: THERE ARE TWO SCENARIOS, EACH SCENARIO HAS DIFFERENT CRITERIA. 

--SCENARIO 1: (09/2014 - 10/2015) 
FEMALE PATIENTS DIAGNOSED WITH AJCC STAGE 0 TO 2 BREAST CANCERS IN MOST RECENT ANNUAL PERIOD WHO RECEIVED DEFINITIVE CANCER-DIRECTED SURGERY. 
EXCLUDE PATIENTS WITH A HISTORY OF ANY PRIOR CANCER DIAGNOSIS, EXCLUDING NON-MELANOMA SKIN CANCERS. 
THEN ALSO PULL OUT NUMBER OF PATIENTS WHO HAVE THEIR FIRST BREAST CANCER DIAGNOSIS BEFORE AND AFTER 30 DAYS, DOING CT, PET AND PET-CT, AND BONE SCANS. 
ALSO PROVIDER CORRESPONDING ONCOLOGISTS, AND SURGEONS' DEPARTMENT, HOSPITAL, AND SERVICE AREAS, INCLUDING ]), 
TREATMENT TYPE AND DATES (LUMPECTOMY/MASTECTOMY, CHEMOTHERAPY OR RADIATION). BREAST CANCER SURGEON ONLY 
OUTCOME: 
NUMBER of patients who received at least one imaging study in window. imaging studies include ct, pet, and bone scans (see example cpt codes below). 
exclude imaging services performed in the emergency department or inpatient setting 
(services in these settings are unlikely to be related to staging for clinical early-stage breast cancer, per hahn, 2015).  

--SCENARIO 2:(FIRST DIAGNOSIS IN 2014, AND WITHIN 12 MONTHS OF FIRST DIAGNOSIS DOING EITHER IMAGING ORDERS OR BIOMARKER TEST)
FEMALE PATIENTS DIAGNOSED WITH AJCC STAGE 0 TO 3A BREAST CANCER AT LEAST 12 MONTHS AFTER DIAGNOSIS.
EXCLUDE PATIENTS WITH A HISTORY OF ANY PRIOR CANCER DIAGNOSIS, CANCER RECURRENCE, OR NEW PRIMARY EXCLUDING NON-MELANOMA SKIN CANCERS.
OUTCOME:
NUMBER of patients who received at least 1 imaging test (per hahn, 2016)
defined as chest and abdominal ct, chest, breast and abdominal magnetic resonance imaging [mri], abdominal ultrasound, chest x-ray, radionuclide bone scan, pet scan)
number of patients who received at least 1 biomarker test (cea, ca27.29, ca125,ca15-3).
*/



USE CLARITY_RPT_CRYSTAL;


--------Step 0: SELECT ENCOUNTERS WITH ERRONEOUS REASON

IF OBJECT_ID('TEMPDB..#ERR_ENC_RSN') IS NOT NULL DROP TABLE #ERR_ENC_RSN;
SELECT DISTINCT PAT_ENC_CSN_ID 
INTO #ERR_ENC_RSN
FROM DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC_RSN_VISIT 
WHERE ENC_REASON_ID=10000

--------SELECT ENCOUNTERS WITH ERRONEOUS DIAGNOSIS

IF OBJECT_ID('TEMPDB..#ERR_CSN_DX') IS NOT NULL DROP TABLE #ERR_CSN_DX;
SELECT DISTINCT PAT_ENC_CSN_ID 
INTO #ERR_CSN_DX
FROM DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC_DX
WHERE DX_ID IN (90010000, 15077)






-----------Step1: **********PULL OUT PT FROM ALL OUT-PT ENCOUNTERS---------------------------

IF OBJECT_ID('TEMPDB..#TOTAL_PAT_ENCOUNTER') IS NOT NULL DROP TABLE #TOTAL_PAT_ENCOUNTER;

DECLARE @BEG AS DATE
DECLARE @FINISH AS DATE
SET @BEG='09/01/2012'     --research time frame
SET @FINISH='9/30/2015'



SELECT DISTINCT P.PAT_ID
	   ,CAST(E.CONTACT_DATE AS DATE) AS CONTACT_DATE
	   ,E.PAT_ENC_CSN_ID 

INTO    #TOTAL_PAT_ENCOUNTER
FROM 
		DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT            AS P 
		INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT_3 AS P_3 ON P.PAT_ID=P_3.PAT_ID
		LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.ZC_SEX   AS ZCS ON ZCS.RCPT_MEM_SEX_C = P.SEX_C
		INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC AS E ON P.PAT_ID=E.PAT_ID
		LEFT JOIN #ERR_ENC_RSN                                AS A1 ON A1.PAT_ENC_CSN_ID = E.PAT_ENC_CSN_ID
		LEFT JOIN #ERR_CSN_DX                                 AS A2 ON A2.PAT_ENC_CSN_ID = E.PAT_ENC_CSN_ID
WHERE 	CAST(E.CONTACT_DATE AS DATE) BETWEEN @BEG AND @FINISH                     --select time period
		AND P.PAT_NAME IS NOT NULL
		AND P.PAT_NAME NOT LIKE 'ZZ%' 
		AND P.PAT_NAME NOT LIKE 'XX%' 
		AND P_3.IS_TEST_PAT_YN='N'
		AND ZCS.RCPT_MEM_SEX_C = '1'                                              --1 female, 2 male, 3 unknown, 4 other
		AND E.SERV_AREA_ID IN (7,22,26,27,28, 11000, 12000, 13000, 14000, 15000)  --sutter medical foundation
		AND E.ENC_TYPE_C NOT IN ('201','249', '5')                                --no erroeous encounter
		AND (E.APPT_STATUS_C IN (2,6) OR E.APPT_STATUS_C IS NULL)                 --completed encounter
		AND E.ENC_CLOSED_YN='Y'
		AND E.ENC_TYPE_C = '101'                                                  --office encounter    
		AND A1.PAT_ENC_CSN_ID IS NULL 
		AND A2.PAT_ENC_CSN_ID IS NULL



--select count(distinct pat_id) from #TOTAL_PAT_ENCOUNTER  --9/13 1,191,543 --9/14  1,191,542, 11/1 1,191,541
--select count(*) from #TOTAL_PAT_ENCOUNTER  --9/13 (9,431,842 row(s) affected)  9/14 (9,431,835 row(s) affected)
--select top 100* from #TOTAL_PAT_ENCOUNTER








------Step2: **********PULL OUT OUTPT WHO ARE DIAGNOSED WITH ALL CANCERS EXCEPT BREAST CANCER WITH THE SAME DIAGNOSIS MORE THAN ONE ENCOUNTERS
IF OBJECT_ID('TEMPDB..#OUTPT_DX') IS NOT NULL DROP TABLE #OUTPT_DX;

SELECT DISTINCT TOTAL_ENC.PAT_ID
	  ,MIN(TOTAL_ENC.CONTACT_DATE) AS EARLY_DIAGNOSIS
      ,EDG.REF_BILL_CODE 
INTO #OUTPT_DX
FROM #TOTAL_PAT_ENCOUNTER AS TOTAL_ENC
     INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC_DX  AS ENCDX ON TOTAL_ENC.PAT_ENC_CSN_ID = ENCDX.PAT_ENC_CSN_ID
	 INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG AS EDG   ON ENCDX.DX_ID = EDG.DX_ID
WHERE EDG.REF_BILL_CODE IN ('140',  '140.0', 	'140.1',	'140.3',	'140.4',	'140.5',	'140.6',	'140.8',	'140.9',	
        '141',	'141.0',	'141.1',    '141.2',	'141.3',     '141.4',	'141.5',	'141.6',	'141.8',	'141.9',	'142',	    
	   '142.0',	'142.1',	'142.2',	'142.8',	'142.9',	'143',	    '143.0',	'143.1',	'143.8',	'143.9',	
	   '144',   '144.0',	'144.1',	'144.8',	'144.9',	'145',	    '145.0',	'145.1',	'145.2',	'145.3',	
	   '145.4',	'145.5',	'145.6',	'145.8',	'145.9',	'146',	    '146.0',	'146.1',	'146.2',	'146.3',	
	   '146.4',	'146.5',	'146.6',	'146.7',	'146.8',	'146.9',	'147',	    '147.0',	'147.1',	'147.2',	
	   '147.3',	'147.8',	'147.9',	'148',	    '148.0',	'148.1',	'148.2',	'148.3',	'148.8',	'148.9',	
	   '149',	'149.0',	'149.1',	'149.8',	'149.9',	'150',	    '150.0',	'150.1',	'150.2',	'150.3',	
	   '150.4',	'150.5',	'150.8',	'150.9',	'151',	    '151.0',	'151.1',	'151.2',	'151.3',	'151.4',
	   '151.5',	'151.6',	'151.8',	'151.9',	'152',	    '152.0',	'152.1',	'152.2',	'152.3',	'152.8',	
	   '152.9',	'153',	     '153.0',	'153.1',	'153.2',	'153.3',	'153.4',	'153.5',	'153.6',	'153.7',	
	   '153.8',	'153.9',	'154',	    '154.0',	'154.1',	'154.2',    '154.3',	'154.8',	'155',	    '155.0',	
	   '155.1',	'155.2',	'156',	    '156.0',	'156.1',	'156.2',	'156.8',	'156.9',	'157',      '157.0',	
	   '157.1',	'157.2',	'157.3',	'157.4',	'157.8',	'157.9',	'158',	    '158.0',	'158.8',	'158.9',	
	   '159',   '159.0',    '159.1',	'159.8',	'159.9',	'160',	    '160.0',	'160.1',	'160.2',	'160.3',	
	   '160.4',	'160.5',	'160.8',	'160.9',	'161',	    '161.0',	'161.1',	'161.2',	'161.3',	'161.8',	
	   '161.9',	'162',	    '162.0',	'162.2',	'162.3',	'162.4',	
'162.5',	'162.8',	'162.9',	'163',	    '163.0',	'163.1',	'163.8',	'163.9',	'164',   	'164.0',	'164.1',	'164.2',	
'164.3',	'164.8',	'164.9',	'165',	    '165.0',	'165.8',	'165.9',	'170',	    '170.0',	'170.1',	'170.2',	'170.3',	
'170.4',	'170.5',	'170.6',	'170.7',	'170.8',	'170.9',	'171',	    '171.0',	'171.2',	'171.3',	'171.4',	'171.5',	
'171.6',	'171.7',	'171.8',	'171.9',	'172',	    '172.0',	'172.1',	'172.2',	'172.3',	'172.4',	'172.5',	'172.6',	
'172.7',	'172.8',	'172.9',	'175',	    '175.0',	'175.9',	'176',	    '176.0',	'176.1',	'176.2',	'176.3',	'176.4',	'176.5',	'176.8',	
'176.9',	'179',	     '180',	    '180.0',	'180.1',	'180.8',	'180.9',	'181',	    '182',	    '182.0',	'182.1',	'182.8',	'183',	
'183.0',	'183.2',    '183.3',	'183.4',	'183.5',	'183.8',	'183.9',	'184',	    '184.0',	'184.1',	'184.2',	'184.3',	'184.4',	'184.8',	
'184.9',	'185',	      '186',	'186.0',	'186.9',	'187',	    '187.1',	'187.2',	'187.3',	'187.4',	'187.5',	'187.6',	'187.7',	
'187.8',	'187.9',	  '188',	'188.0',	'188.1',	'188.2',	'188.3',	'188.4',	'188.5',	'188.6',	'188.7',	'188.8',	'188.9',	
'189',	    '189.0',	'189.1',	'189.2',	'189.3',	'189.4',	'189.8',	'189.9',	'190',	    '190.0',	'190.1',	'190.2',	'190.3',	
'190.4',	'190.5',	'190.6',	'190.7',	'190.8',	'190.9',	'191',	    '191.0',	'191.1',	'191.2',	'191.3',	'191.4',	'191.5',
'191.6',	'191.7',	'191.8',	'191.9',	'192',	    '192.0',	'192.1',	'192.2',	'192.3',	'192.8',	'192.9',	'193',	    '194',	
'194.0',	'194.1',	'194.3',	'194.4',	'194.5',	'194.6',	'194.8',	'194.9',	'195',	    '195.0',	'195.1',	'195.2',	'195.3',
'195.4',	'195.5',	'195.8',	'200',	    '200.0',	'200.00',	'200.01',	'200.02',	'200.03',	'200.04',	'200.05',	'200.06',	
'200.07',	'200.08',	'200.1',	'200.10',	'200.11',	'200.12',	'200.13',	'200.14',	'200.15',	'200.16',	'200.17',	'200.18',
'200.2',	'200.20',	'200.21',	'200.22',	'200.23',	'200.24',	'200.25',	'200.26',	'200.27',	'200.28',	'200.3',	'200.30',	
'200.31',	'200.32',	'200.33',	'200.34',	'200.35',	'200.36',	'200.37',	'200.38',	'200.4',	'200.40',	'200.41',	'200.42',	
'200.43',	'200.44',	'200.45',	'200.46',	'200.47',	'200.48',	'200.5',	'200.50',	'200.51',	'200.52',	'200.53',	'200.54',	
'200.55',	'200.56',	'200.57',	'200.58',	'200.6',	'200.60',	'200.61',	'200.62',	'200.63',	'200.64',	'200.65',	'200.66',	
'200.67',	'200.68',	'200.7',	'200.70',	'200.71',	'200.72',	'200.73',	'200.74',	'200.75',	'200.76',	'200.77',	'200.78',	
'200.8',	'200.80',	'200.81',	'200.82',	'200.83',	'200.84',	'200.85',	'200.86',	'200.87',	'200.88',	'201',	    '201.0',	'201.00',
'201.01',	'201.02',	'201.03',	'201.04',	'201.05',	'201.06',	'201.07',	'201.08',	'201.1',	'201.10',	'201.11',	'201.12',	'201.13',	
'201.14',	'201.15',	'201.16',	'201.17',	'201.18',	'201.2',	'201.20',	'201.21',	'201.22',	'201.23',	'201.24',	'201.25',	'201.26',	
'201.27',	'201.28',	'201.4',	'201.40',	'201.41',	'201.42',	'201.43',	'201.44',	'201.45',	'201.46',	'201.47',	'201.48',	'201.5',	
'201.50',	'201.51',	'201.52',	'201.53',	'201.54',	'201.55',	'201.56',	'201.57',	'201.58',	'201.6',	'201.60',	'201.61',	'201.62',	
'201.63',	'201.64',	'201.65',	'201.66',	'201.67',	'201.68',	'201.7',	'201.70',	'201.71',	'201.72',	'201.73',	'201.74',	'201.75',	
'201.76',	'201.77',	'201.78',	'201.9',	'201.90',	'201.91',	'201.92',	'201.93',	'201.94',	'201.95',	'201.96',	'201.97',	'201.98',	
'202',	    '202.0',	'202.00',	'202.01',	'202.02',	'202.03',	'202.04',	'202.05',	'202.06',	'202.07',	'202.08',	'202.1',	'202.10',	
'202.11',	'202.12',	'202.13',	'202.14',	'202.15',	'202.16',	'202.17',	'202.18',	'202.2',	'202.20',	'202.21',	'202.22',	'202.23',	
'202.24',	'202.25',	'202.26',	'202.27',	'202.28',	'202.3',	'202.30',	'202.31',	'202.32',	'202.33',	'202.34',	'202.35',	'202.36',	
'202.37',	'202.38',	'202.4',	'202.40',	'202.41',	'202.42',	'202.43',	'202.44',	'202.45',	'202.46',	'202.47',	'202.48',	'202.5',	
'202.50',	'202.51',	'202.52',	'202.53',	'202.54',	'202.55',	'202.56',	'202.57',	'202.58',	'202.6',	'202.60',	'202.61',	'202.62',	
'202.63',	'202.64',	'202.65',	'202.66',	'202.67',	'202.68',	'202.7',	'202.70',	'202.71',	'202.72',	'202.73',	'202.74',	'202.75',
'202.76',	'202.77',	'202.78',	'202.8',	'202.80',	'202.81',	'202.82',	'202.83',	'202.84',	'202.85',	'202.86',	'202.87',	'202.88',	
'202.9',	'202.90',	'202.91',	'202.92',	'202.93',	'202.94',	'202.95',	'202.96',	'202.97',	'202.98',	'203',	    '203.0',	'203.00',	
'203.01',	'203.02',	'203.1',	'203.10',	'203.11',	'203.12',	'203.8',	'203.80',	'203.81',	'203.82',	'204',	    '204.0',	'204.00',	
'204.01',	'204.02',	'204.1',	'204.10',	'204.11',	'204.12',	'204.2',	'204.20',	'204.21',	'204.22',	'204.8',	'204.80',	'204.81',
'204.82',	'204.9',	'204.90',	'204.91',	'204.92',	'205',   	'205.0',	'205.00',	'205.01',	'205.02',	'205.1',	'205.10',	'205.11',	
'205.12',	'205.2',	'205.20',	'205.21',	'205.22',	'205.3',	'205.30',	'205.31',	'205.32',	'205.8',	'205.80',	'205.81',	'205.82',
'205.9',	'205.90',	'205.91',	'205.92',	'206',	    '206.0',	'206.00',	'206.01',	'206.02',	'206.1',	'206.10',	'206.11',	'206.12',	'206.2',
'206.20',	'206.21',	'206.22',	'206.8',	'206.80',	'206.81',	'206.82',	'206.9',	'206.90',	'206.91',	'206.92',	'207',	    '207.0',	'207.00',	
'207.01',	'207.02',	'207.1',	'207.10',	'207.11',	'207.12',	'207.2',	'207.20',	'207.21',	'207.22',	'207.8',	'207.80',	'207.81',	'207.82',
'208',   	'208.0',	'208.00',	'208.01',	'208.02',	'208.1',	'208.10',	'208.11',	'208.12',	'208.2',	'208.20',	'208.21',	'208.22',	'208.8',	
'208.80',	'208.81',	'208.82',	'208.9',	'208.90',	'208.91',	'208.92',	'238.6',    '196',	    '196.0',	'196.1',	'196.2',	'196.3',	'196.5',	'196.6',	'196.8',	'196.9',	'197',	    '197.0',	'197.1',	'197.2',	
'197.3',	'197.4',	'197.5',	'197.6',	'197.7',	'197.8',	'198',	    '198.0',	'198.1',	'198.2',	'198.3',	'198.4',	'198.5',
'198.6',	'198.7',	'198.8',	'198.81',	'198.82',	'198.89',	'199',	    '199.0',	'199.1',	'199.2')
GROUP BY TOTAL_ENC.PAT_ID
        ,EDG.REF_BILL_CODE                            --*******group by means the same pt, same dx_code, have at least two encounters
HAVING COUNT(DISTINCT TOTAL_ENC.PAT_ENC_CSN_ID) > 1   --****pat_enc_csn_id shouldn't appear in the variables of the table


--select * from #OUTPT_DX where PAT_ID = '10005412'                                                                 --9/14 20734   
--select count(distinct pat_id) from #OUTPT_DX                                             --9/14 17344
--select distinct pat_id, min(early_diagnosis) as early_dx from #outpt_dx group by pat_id  --9/14 17344






----********PULL OUT INPT WITH ONE CANCER DIAGNOSIS ******************************************
IF OBJECT_ID('TEMPDB..#INPT_DX') IS NOT NULL DROP TABLE #INPT_DX;

DECLARE @BEG2 AS DATE
DECLARE @FINISH2 AS DATE
SET @BEG2='09/01/2012'     
SET @FINISH2='9/30/2015'


SELECT DISTINCT P.PAT_ID       -- ,CAST(P.BIRTH_DATE AS DATE) AS BIRTH_DATE
	   ,MIN(CAST(HSP.ADM_DATE_TIME AS DATE))AS EARLY_DIAGNOSIS
INTO    #INPT_DX
FROM 
		DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT                     AS P 
		INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT_3        AS P_3 ON P.PAT_ID=P_3.PAT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCOUNT      AS HSP ON HSP.PAT_ID = P.PAT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCT_DX_LIST AS DX ON DX.HSP_ACCOUNT_ID = HSP.HSP_ACCOUNT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG      AS EDG   ON EDG.DX_ID = DX.DX_ID		
		LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.ZC_SEX            AS ZCS ON ZCS.RCPT_MEM_SEX_C = P.SEX_C
		
WHERE 	CAST(HSP.ADM_DATE_TIME AS DATE) BETWEEN @BEG2 AND @FINISH2                     --select time period
		AND P.PAT_NAME IS NOT NULL
		AND P.PAT_NAME NOT LIKE 'ZZ%' 
		AND P.PAT_NAME NOT LIKE 'XX%' 
		AND P_3.IS_TEST_PAT_YN='N'
		AND ZCS.RCPT_MEM_SEX_C = '1'                                              --1 female, 2 male, 3 unknown, 4 other
        AND EDG.REF_BILL_CODE IN ('140',  '140.0', 	'140.1',	'140.3',	'140.4',	'140.5',	'140.6',	'140.8',	'140.9',	
        '141',	'141.0',	'141.1',    '141.2',	'141.3',     '141.4',	'141.5',	'141.6',	'141.8',	'141.9',	'142',	    
	   '142.0',	'142.1',	'142.2',	'142.8',	'142.9',	'143',	    '143.0',	'143.1',	'143.8',	'143.9',	
	   '144',   '144.0',	'144.1',	'144.8',	'144.9',	'145',	    '145.0',	'145.1',	'145.2',	'145.3',	
	   '145.4',	'145.5',	'145.6',	'145.8',	'145.9',	'146',	    '146.0',	'146.1',	'146.2',	'146.3',	
	   '146.4',	'146.5',	'146.6',	'146.7',	'146.8',	'146.9',	'147',	    '147.0',	'147.1',	'147.2',	
	   '147.3',	'147.8',	'147.9',	'148',	    '148.0',	'148.1',	'148.2',	'148.3',	'148.8',	'148.9',	
	   '149',	'149.0',	'149.1',	'149.8',	'149.9',	'150',	    '150.0',	'150.1',	'150.2',	'150.3',	
	   '150.4',	'150.5',	'150.8',	'150.9',	'151',	    '151.0',	'151.1',	'151.2',	'151.3',	'151.4',
	   '151.5',	'151.6',	'151.8',	'151.9',	'152',	    '152.0',	'152.1',	'152.2',	'152.3',	'152.8',	
	   '152.9',	'153',	     '153.0',	'153.1',	'153.2',	'153.3',	'153.4',	'153.5',	'153.6',	'153.7',	
	   '153.8',	'153.9',	'154',	    '154.0',	'154.1',	'154.2',    '154.3',	'154.8',	'155',	    '155.0',	
	   '155.1',	'155.2',	'156',	    '156.0',	'156.1',	'156.2',	'156.8',	'156.9',	'157',      '157.0',	
	   '157.1',	'157.2',	'157.3',	'157.4',	'157.8',	'157.9',	'158',	    '158.0',	'158.8',	'158.9',	
	   '159',   '159.0',    '159.1',	'159.8',	'159.9',	'160',	    '160.0',	'160.1',	'160.2',	'160.3',	
	   '160.4',	'160.5',	'160.8',	'160.9',	'161',	    '161.0',	'161.1',	'161.2',	'161.3',	'161.8',	
	   '161.9',	'162',	    '162.0',	'162.2',	'162.3',	'162.4',	
'162.5',	'162.8',	'162.9',	'163',	    '163.0',	'163.1',	'163.8',	'163.9',	'164',   	'164.0',	'164.1',	'164.2',	
'164.3',	'164.8',	'164.9',	'165',	    '165.0',	'165.8',	'165.9',	'170',	    '170.0',	'170.1',	'170.2',	'170.3',	
'170.4',	'170.5',	'170.6',	'170.7',	'170.8',	'170.9',	'171',	    '171.0',	'171.2',	'171.3',	'171.4',	'171.5',	
'171.6',	'171.7',	'171.8',	'171.9',	'172',	    '172.0',	'172.1',	'172.2',	'172.3',	'172.4',	'172.5',	'172.6',	
'172.7',	'172.8',	'172.9',	'175',	    '175.0',	'175.9',	'176',	    '176.0',	'176.1',	'176.2',	'176.3',	'176.4',	'176.5',	'176.8',	
'176.9',	'179',	     '180',	    '180.0',	'180.1',	'180.8',	'180.9',	'181',	    '182',	    '182.0',	'182.1',	'182.8',	'183',	
'183.0',	'183.2',    '183.3',	'183.4',	'183.5',	'183.8',	'183.9',	'184',	    '184.0',	'184.1',	'184.2',	'184.3',	'184.4',	'184.8',	
'184.9',	'185',	      '186',	'186.0',	'186.9',	'187',	    '187.1',	'187.2',	'187.3',	'187.4',	'187.5',	'187.6',	'187.7',	
'187.8',	'187.9',	  '188',	'188.0',	'188.1',	'188.2',	'188.3',	'188.4',	'188.5',	'188.6',	'188.7',	'188.8',	'188.9',	
'189',	    '189.0',	'189.1',	'189.2',	'189.3',	'189.4',	'189.8',	'189.9',	'190',	    '190.0',	'190.1',	'190.2',	'190.3',	
'190.4',	'190.5',	'190.6',	'190.7',	'190.8',	'190.9',	'191',	    '191.0',	'191.1',	'191.2',	'191.3',	'191.4',	'191.5',
'191.6',	'191.7',	'191.8',	'191.9',	'192',	    '192.0',	'192.1',	'192.2',	'192.3',	'192.8',	'192.9',	'193',	    '194',	
'194.0',	'194.1',	'194.3',	'194.4',	'194.5',	'194.6',	'194.8',	'194.9',	'195',	    '195.0',	'195.1',	'195.2',	'195.3',
'195.4',	'195.5',	'195.8',	'200',	    '200.0',	'200.00',	'200.01',	'200.02',	'200.03',	'200.04',	'200.05',	'200.06',	
'200.07',	'200.08',	'200.1',	'200.10',	'200.11',	'200.12',	'200.13',	'200.14',	'200.15',	'200.16',	'200.17',	'200.18',
'200.2',	'200.20',	'200.21',	'200.22',	'200.23',	'200.24',	'200.25',	'200.26',	'200.27',	'200.28',	'200.3',	'200.30',	
'200.31',	'200.32',	'200.33',	'200.34',	'200.35',	'200.36',	'200.37',	'200.38',	'200.4',	'200.40',	'200.41',	'200.42',	
'200.43',	'200.44',	'200.45',	'200.46',	'200.47',	'200.48',	'200.5',	'200.50',	'200.51',	'200.52',	'200.53',	'200.54',	
'200.55',	'200.56',	'200.57',	'200.58',	'200.6',	'200.60',	'200.61',	'200.62',	'200.63',	'200.64',	'200.65',	'200.66',	
'200.67',	'200.68',	'200.7',	'200.70',	'200.71',	'200.72',	'200.73',	'200.74',	'200.75',	'200.76',	'200.77',	'200.78',	
'200.8',	'200.80',	'200.81',	'200.82',	'200.83',	'200.84',	'200.85',	'200.86',	'200.87',	'200.88',	'201',	    '201.0',	'201.00',
'201.01',	'201.02',	'201.03',	'201.04',	'201.05',	'201.06',	'201.07',	'201.08',	'201.1',	'201.10',	'201.11',	'201.12',	'201.13',	
'201.14',	'201.15',	'201.16',	'201.17',	'201.18',	'201.2',	'201.20',	'201.21',	'201.22',	'201.23',	'201.24',	'201.25',	'201.26',	
'201.27',	'201.28',	'201.4',	'201.40',	'201.41',	'201.42',	'201.43',	'201.44',	'201.45',	'201.46',	'201.47',	'201.48',	'201.5',	
'201.50',	'201.51',	'201.52',	'201.53',	'201.54',	'201.55',	'201.56',	'201.57',	'201.58',	'201.6',	'201.60',	'201.61',	'201.62',	
'201.63',	'201.64',	'201.65',	'201.66',	'201.67',	'201.68',	'201.7',	'201.70',	'201.71',	'201.72',	'201.73',	'201.74',	'201.75',	
'201.76',	'201.77',	'201.78',	'201.9',	'201.90',	'201.91',	'201.92',	'201.93',	'201.94',	'201.95',	'201.96',	'201.97',	'201.98',	
'202',	    '202.0',	'202.00',	'202.01',	'202.02',	'202.03',	'202.04',	'202.05',	'202.06',	'202.07',	'202.08',	'202.1',	'202.10',	
'202.11',	'202.12',	'202.13',	'202.14',	'202.15',	'202.16',	'202.17',	'202.18',	'202.2',	'202.20',	'202.21',	'202.22',	'202.23',	
'202.24',	'202.25',	'202.26',	'202.27',	'202.28',	'202.3',	'202.30',	'202.31',	'202.32',	'202.33',	'202.34',	'202.35',	'202.36',	
'202.37',	'202.38',	'202.4',	'202.40',	'202.41',	'202.42',	'202.43',	'202.44',	'202.45',	'202.46',	'202.47',	'202.48',	'202.5',	
'202.50',	'202.51',	'202.52',	'202.53',	'202.54',	'202.55',	'202.56',	'202.57',	'202.58',	'202.6',	'202.60',	'202.61',	'202.62',	
'202.63',	'202.64',	'202.65',	'202.66',	'202.67',	'202.68',	'202.7',	'202.70',	'202.71',	'202.72',	'202.73',	'202.74',	'202.75',
'202.76',	'202.77',	'202.78',	'202.8',	'202.80',	'202.81',	'202.82',	'202.83',	'202.84',	'202.85',	'202.86',	'202.87',	'202.88',	
'202.9',	'202.90',	'202.91',	'202.92',	'202.93',	'202.94',	'202.95',	'202.96',	'202.97',	'202.98',	'203',	    '203.0',	'203.00',	
'203.01',	'203.02',	'203.1',	'203.10',	'203.11',	'203.12',	'203.8',	'203.80',	'203.81',	'203.82',	'204',	    '204.0',	'204.00',	
'204.01',	'204.02',	'204.1',	'204.10',	'204.11',	'204.12',	'204.2',	'204.20',	'204.21',	'204.22',	'204.8',	'204.80',	'204.81',
'204.82',	'204.9',	'204.90',	'204.91',	'204.92',	'205',   	'205.0',	'205.00',	'205.01',	'205.02',	'205.1',	'205.10',	'205.11',	
'205.12',	'205.2',	'205.20',	'205.21',	'205.22',	'205.3',	'205.30',	'205.31',	'205.32',	'205.8',	'205.80',	'205.81',	'205.82',
'205.9',	'205.90',	'205.91',	'205.92',	'206',	    '206.0',	'206.00',	'206.01',	'206.02',	'206.1',	'206.10',	'206.11',	'206.12',	'206.2',
'206.20',	'206.21',	'206.22',	'206.8',	'206.80',	'206.81',	'206.82',	'206.9',	'206.90',	'206.91',	'206.92',	'207',	    '207.0',	'207.00',	
'207.01',	'207.02',	'207.1',	'207.10',	'207.11',	'207.12',	'207.2',	'207.20',	'207.21',	'207.22',	'207.8',	'207.80',	'207.81',	'207.82',
'208',   	'208.0',	'208.00',	'208.01',	'208.02',	'208.1',	'208.10',	'208.11',	'208.12',	'208.2',	'208.20',	'208.21',	'208.22',	'208.8',	
'208.80',	'208.81',	'208.82',	'208.9',	'208.90',	'208.91',	'208.92',	'238.6',    '196',	    '196.0',	'196.1',	'196.2',	'196.3',	'196.5',	'196.6',	'196.8',	'196.9',	'197',	    '197.0',	'197.1',	'197.2',	
'197.3',	'197.4',	'197.5',	'197.6',	'197.7',	'197.8',	'198',	    '198.0',	'198.1',	'198.2',	'198.3',	'198.4',	'198.5',
'198.6',	'198.7',	'198.8',	'198.81',	'198.82',	'198.89',	'199',	    '199.0',	'199.1',	'199.2')
GROUP BY P.PAT_ID        


--select * from #INPT_DX where pat_id = '10005412'      --9/14 19497 
--select count(distinct pat_id) from #INPT_DX   --9/14 19497
--select top 10* from #INPT_DX order by pat_id 






------Step 3: ***********COMBINE OUTPT & INPT WITH ALL THE OTHER CANCER DIAGNOSIS, 2 ENCOUTNERS FOR OUTPT, 1 ENCOUNTER FOR INPT  #######TABLE TO USE 
IF OBJECT_ID('TEMPDB..#TOTAL_CANCER_PT') IS NOT NULL DROP TABLE #TOTAL_CANCER_PT;
SELECT PAT_ID, MIN(EARLY_DX) AS EARLIEST_DX
INTO #TOTAL_CANCER_PT
FROM (
      SELECT PAT_ID, MIN(EARLY_DIAGNOSIS) AS EARLY_DX FROM #OUTPT_DX GROUP BY PAT_ID
      UNION
      SELECT PAT_ID, MIN(EARLY_DIAGNOSIS) AS EARLY_DX FROM #INPT_DX GROUP BY PAT_ID
     ) AS F
GROUP BY PAT_ID

--select * from #TOTAL_CANCER_PT where pat_id = '10005412'        --9/14 31,789
--select count(distinct pat_id) from #TOTAL_CANCER_PT   --9/14 31789




----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------Step 4: -----------------------------------------------------------------------------------------------------------------------------------------------------------

----**************PULL OUT PT WITH BREAST CANCER ONLY 
----**************PULL OUT PT FROM ALL OUT-PT ENCOUNTERS---------------------------

------**********PULL OUT OUTPT WHO ARE DIAGNOSED WITH ALL CANCERS EXCEPT BREAST CANCER WITH THE SAME DIAGNOSIS MORE THAN ONE ENCOUNTERS
IF OBJECT_ID('TEMPDB..#OUTPT_BREAST_DX') IS NOT NULL DROP TABLE #OUTPT_BREAST_DX;

SELECT DISTINCT TOTAL_ENC.PAT_ID
	  ,MIN(TOTAL_ENC.CONTACT_DATE) AS EARLY_DIAGNOSIS
      ,EDG.REF_BILL_CODE 
INTO #OUTPT_BREAST_DX
FROM #TOTAL_PAT_ENCOUNTER AS TOTAL_ENC                 --use the previous all-encounter pt. table #TOTAL_PAT_ENCOUNTER
     INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC_DX  AS ENCDX ON TOTAL_ENC.PAT_ENC_CSN_ID = ENCDX.PAT_ENC_CSN_ID
	 INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG AS EDG   ON ENCDX.DX_ID = EDG.DX_ID
WHERE EDG.REF_BILL_CODE LIKE '%174%'
GROUP BY TOTAL_ENC.PAT_ID
        ,EDG.REF_BILL_CODE                            --*******group by means the same pt, same dx_code, have at least two encounters
HAVING COUNT(DISTINCT TOTAL_ENC.PAT_ENC_CSN_ID) > 1   --****pat_enc_csn_id shouldn't appear in the variables of the table


--select * from #OUTPT_BREAST_DX where pat_id = '10005412'                                                               --9/14 12,380 
--select count(distinct pat_id) from #OUTPT_BREAST_DX                                         --9/14  12,040
--select distinct pat_id, min(early_diagnosis) as early_dx from #OUTPT_BREAST_DX group by pat_id  --9/14 12,040




----********PULL OUT INPT WITH ONE CANCER DIAGNOSIS******************************* 
IF OBJECT_ID('TEMPDB..#INPT_BREAST_DX') IS NOT NULL DROP TABLE #INPT_BREAST_DX;

DECLARE @BEG3 AS DATE
DECLARE @FINISH3 AS DATE
SET @BEG3='09/01/2012'     
SET @FINISH3='9/30/2015'


SELECT DISTINCT P.PAT_ID       -- ,CAST(P.BIRTH_DATE AS DATE) AS BIRTH_DATE
	   ,MIN(CAST(HSP.ADM_DATE_TIME AS DATE))AS EARLY_DIAGNOSIS
INTO    #INPT_BREAST_DX
FROM 
		DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT                     AS P 
		INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT_3        AS P_3 ON P.PAT_ID=P_3.PAT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCOUNT      AS HSP ON HSP.PAT_ID = P.PAT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCT_DX_LIST AS DX ON DX.HSP_ACCOUNT_ID = HSP.HSP_ACCOUNT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG      AS EDG   ON EDG.DX_ID = DX.DX_ID		
		LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.ZC_SEX            AS ZCS ON ZCS.RCPT_MEM_SEX_C = P.SEX_C
		
WHERE 	CAST(HSP.ADM_DATE_TIME AS DATE) BETWEEN @BEG3 AND @FINISH3                     --select time period
		AND P.PAT_NAME IS NOT NULL
		AND P.PAT_NAME NOT LIKE 'ZZ%' 
		AND P.PAT_NAME NOT LIKE 'XX%' 
		AND P_3.IS_TEST_PAT_YN='N'
		AND ZCS.RCPT_MEM_SEX_C = '1'                                              --1 female, 2 male, 3 unknown, 4 other
        AND EDG.REF_BILL_CODE LIKE '%174%'
GROUP BY P.PAT_ID        

--select * from #INPT_BREAST_DX where pat_id = '10005412'       --9/14 11,711
--select count(distinct pat_id) from #INPT_BREAST_DX   --9/14 11,711
--select top 10* from #INPT_BREAST_DX order by pat_id 






------******************COMBINE OUTPT & INPT WITH ALL THE OTHER CANCER DIAGNOSIS, 2 ENCOUTNERS FOR OUTPT, 1 ENCOUNTER FOR INPT  
IF OBJECT_ID('TEMPDB..#TOTAL_BREAST_PT') IS NOT NULL DROP TABLE #TOTAL_BREAST_PT;
SELECT PAT_ID, MIN(EARLY_DX) AS EARLIEST_DX
INTO #TOTAL_BREAST_PT
FROM (
      SELECT PAT_ID, MIN(EARLY_DIAGNOSIS) AS EARLY_DX FROM #OUTPT_BREAST_DX GROUP BY PAT_ID
      UNION
      SELECT PAT_ID, MIN(EARLY_DIAGNOSIS) AS EARLY_DX FROM #INPT_BREAST_DX GROUP BY PAT_ID
     ) AS Fi
GROUP BY PAT_ID

--select * from #TOTAL_BREAST_PT where pat_id = '10005412'         --9/14 20,030
--select count(distinct pat_id) from #TOTAL_BREAST_PT   --9/14 20,030 






------*******************Baseline data (after excluding pt. who has other cancer diagnosis early than breast cancer): USE this table for analysis STAGE 1 to 4
IF OBJECT_ID('TEMPDB..#BASE_BREASTCANCER_PT') IS NOT NULL DROP TABLE #BASE_BREASTCANCER_PT;
SELECT DISTINCT B.PAT_ID
      ,B.EARLIEST_DX AS EARLIEST_BREAST_CANCER_DX
INTO #BASE_BREASTCANCER_PT
FROM #TOTAL_BREAST_PT AS B
     LEFT JOIN #TOTAL_CANCER_PT AS T ON B.PAT_ID = T.PAT_ID
WHERE (T.EARLIEST_DX IS NULL 
      OR T.EARLIEST_DX > B.EARLIEST_DX)
	  AND B.EARLIEST_DX BETWEEN '09/01/2013' AND '09/30/2015'  --exlude pt who has other cancer and breast cancer diagnosis on the same day
ORDER BY B.PAT_ID ASC

--select * from #BASE_BREASTCANCER_PT --9/14  9211



--------_____________________________________________________________________________________________________________________________________________
------*****START PULLING DATA FOR USING THE LOGISTIC REGRESSION MODEL



----**********PULLING OUT PT WITH METASTATIC DISEASE   Stage IV 4 cancer status
IF OBJECT_ID('TEMPDB..#OUTPT_META_DX') IS NOT NULL DROP TABLE #OUTPT_META_DX; --pull out out pat data

SELECT DISTINCT TOTAL_ENC.PAT_ID
      ,EDG.REF_BILL_CODE 
INTO #OUTPT_META_DX
FROM #BASE_BREASTCANCER_PT AS TOTAL_ENC                 --use the previous all-encounter pt. table #TOTAL_PAT_ENCOUNTER
     INNER JOIN PAT_ENC AS ENC ON ENC.PAT_ID = TOTAL_ENC.PAT_ID
     INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC_DX  AS ENCDX ON ENC.PAT_ENC_CSN_ID = ENCDX.PAT_ENC_CSN_ID
	 INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG AS EDG   ON ENCDX.DX_ID = EDG.DX_ID
WHERE EDG.REF_BILL_CODE IN ('196.2', '196.5', '196.6', '197', '197.0', '197.1', '197.2', '197.3', '197.4', '197.5', '197.6', '197.7', '197.8', 
                            '198', '198.0', '198.1', '198.2', '198.3', '198.4', '198.5', '198.6', '198.7','198.8', '198.81', '198.82', '198.89')  --140
      AND CAST(ENC.CONTACT_DATE AS DATE) BETWEEN DATEADD(mm, -3,  TOTAL_ENC.EARLIEST_BREAST_CANCER_DX) AND DATEADD(mm, 3,  TOTAL_ENC.EARLIEST_BREAST_CANCER_DX)


--select * from #outpt_meta_dx  --140
--select count(distinct pat_id) from #outpt_meta_dx  --119
--select * from #OUTPT_META_DX WHERE REF_BILL_CODE LIKE '%233%'  --stage0 937

 

IF OBJECT_ID('TEMPDB..#INPT_META_DX') IS NOT NULL DROP TABLE #INPT_META_DX;  --pull out inpt data
SELECT DISTINCT P.PAT_ID 
       , EDG.REF_BILL_CODE      
INTO    #INPT_META_DX
FROM 
		#BASE_BREASTCANCER_PT                                          AS P 
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCOUNT      AS HSP ON HSP.PAT_ID = P.PAT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCT_DX_LIST AS DX ON DX.HSP_ACCOUNT_ID = HSP.HSP_ACCOUNT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG      AS EDG   ON EDG.DX_ID = DX.DX_ID		
WHERE EDG.REF_BILL_CODE IN ('196.2', '196.5', '196.6', '197', '197.0', '197.1', '197.2', '197.3', '197.4', '197.5', '197.6', '197.7', '197.8', 
                            '198', '198.0', '198.1', '198.2', '198.3', '198.4', '198.5', '198.6', '198.7','198.8', '198.81', '198.82', '198.89')  
      AND CAST(HSP.ADM_DATE_TIME AS DATE) BETWEEN DATEADD(mm, -3,  P.EARLIEST_BREAST_CANCER_DX) AND DATEADD(mm, 3,  P.EARLIEST_BREAST_CANCER_DX) 

	  
--select * from #inpt_meta_dx --9/15  156
--select count(distinct pat_id) from #inpt_meta_dx  --103
--select * from #inpt_meta_dx WHERE REF_BILL_CODE LIKE '%233%'  --stage 0, 1268

------******COMBINE OUTPT & INPT WITH STAGE 4 BREAST CANCER
IF OBJECT_ID('TEMPDB..#STAGE4_PAT_LIST') IS NOT NULL DROP TABLE #STAGE4_PAT_LIST;
SELECT DISTINCT PAT_ID 
INTO #STAGE4_PAT_LIST
FROM (
      SELECT DISTINCT PAT_ID FROM #OUTPT_META_DX 
      UNION
      SELECT DISTINCT PAT_ID FROM #INPT_META_DX 
     ) AS T

--select * from #STAGE4_PAT_LIST  --185
--select count(distinct pat_id) from #STAGE4_PAT_LIST  --185



----*****EXCLUDE STAGE 4 CANCER PT FROM MASTER BREAST CANCER TABLE #BASE_BREASTCANCER_PT    STAGE 1-3
IF OBJECT_ID('TEMPDB..#STAGE0to3_PAT_LIST') IS NOT NULL DROP TABLE #STAGE0to3_PAT_LIST;
SELECT DISTINCT PAT_ID, EARLIEST_BREAST_CANCER_DX
INTO #STAGE0to3_PAT_LIST 
FROM #BASE_BREASTCANCER_PT
WHERE PAT_ID NOT IN (SELECT DISTINCT PAT_ID FROM #STAGE4_PAT_LIST) 


--select * from #STAGE0to3_PAT_LIST  --9/26  9026
--select * from #BASE_BREASTCANCER_PT 

/*
select * from (
select * from #OUTPT_META_DX WHERE REF_BILL_CODE LIKE '%233%'  
UNION
select * from #inpt_meta_dx WHERE REF_BILL_CODE LIKE '%233%'
) AS T
*/


---_____________________________________________________________________________________________________________________________________________________
---------**************************START PULLING DATA WITH STAGE 0 ICD9 CODE 233        ********************Stage 0 ------
IF OBJECT_ID('TEMPDB..#OUTPT_BREAST_S0_DX') IS NOT NULL DROP TABLE #OUTPT_BREAST_S0_DX;

SELECT DISTINCT TOTAL_ENC.PAT_ID
	  ,MIN(TOTAL_ENC.CONTACT_DATE) AS EARLY_DIAGNOSIS
      ,EDG.REF_BILL_CODE 
INTO #OUTPT_BREAST_S0_DX
FROM #TOTAL_PAT_ENCOUNTER AS TOTAL_ENC                 --use the previous all-encounter pt. table #TOTAL_PAT_ENCOUNTER
     INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC_DX  AS ENCDX ON TOTAL_ENC.PAT_ENC_CSN_ID = ENCDX.PAT_ENC_CSN_ID
	 INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG AS EDG   ON ENCDX.DX_ID = EDG.DX_ID
WHERE EDG.REF_BILL_CODE LIKE '%233%'
GROUP BY TOTAL_ENC.PAT_ID
        ,EDG.REF_BILL_CODE                            --*******group by means the same pt, same dx_code, have at least two encounters
HAVING COUNT(DISTINCT TOTAL_ENC.PAT_ENC_CSN_ID) > 1   --****pat_enc_csn_id shouldn't appear in the variables of the table


--select * from #OUTPT_BREAST_S0_DX                                                               --9/15   2803
--select count(distinct pat_id) from #OUTPT_BREAST_S0_DX                                         --9/15     2796
--select distinct pat_id, min(early_diagnosis) as early_dx from #OUTPT_BREAST_S0_DX group by pat_id  --9/15 2796


----********PULL OUT INPT WITH ONE CANCER DIAGNOSIS******************************* 
IF OBJECT_ID('TEMPDB..#INPT_BREAST_S0_DX') IS NOT NULL DROP TABLE #INPT_BREAST_S0_DX;

DECLARE @BEG0 AS DATE
DECLARE @FINISH0 AS DATE
SET @BEG0='09/01/2012'     
SET @FINISH0='9/30/2015'


SELECT  DISTINCT P.PAT_ID       
	   ,MIN(CAST(HSP.ADM_DATE_TIME AS DATE))AS EARLY_DIAGNOSIS
INTO    #INPT_BREAST_S0_DX
FROM 
		DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT                     AS P 
		INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT_3        AS P_3 ON P.PAT_ID=P_3.PAT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCOUNT      AS HSP ON HSP.PAT_ID = P.PAT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCT_DX_LIST AS DX ON DX.HSP_ACCOUNT_ID = HSP.HSP_ACCOUNT_ID
	    INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_EDG      AS EDG   ON EDG.DX_ID = DX.DX_ID		
		LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.ZC_SEX            AS ZCS ON ZCS.RCPT_MEM_SEX_C = P.SEX_C
		
WHERE 	CAST(HSP.ADM_DATE_TIME AS DATE) BETWEEN @BEG0 AND @FINISH0                   
		AND P.PAT_NAME IS NOT NULL
		AND P.PAT_NAME NOT LIKE 'ZZ%' 
		AND P.PAT_NAME NOT LIKE 'XX%' 
		AND P_3.IS_TEST_PAT_YN='N'
		AND ZCS.RCPT_MEM_SEX_C = '1'                                         
        AND EDG.REF_BILL_CODE LIKE '%233%'
GROUP BY P.PAT_ID        

--select * from #INPT_BREAST_S0_DX                        --9/15 3480
--select count(distinct pat_id) from #INPT_BREAST_S0_DX   --9/15   3480   



------******************COMBINE OUTPT & INPT WITH ALL THE OTHER CANCER DIAGNOSIS, 2 ENCOUTNERS FOR OUTPT, 1 ENCOUNTER FOR INPT  
IF OBJECT_ID('TEMPDB..#TOTAL_BREAST_S0_PT') IS NOT NULL DROP TABLE #TOTAL_BREAST_S0_PT;
SELECT DISTINCT PAT_ID, MIN(EARLY_DX) AS EARLIEST_DX
INTO #TOTAL_BREAST_S0_PT
FROM (
      SELECT PAT_ID, MIN(EARLY_DIAGNOSIS) AS EARLY_DX FROM #OUTPT_BREAST_S0_DX GROUP BY PAT_ID
      UNION
      SELECT PAT_ID, MIN(EARLY_DIAGNOSIS) AS EARLY_DX FROM #INPT_BREAST_S0_DX GROUP BY PAT_ID
     ) AS F
GROUP BY PAT_ID

--select * from #TOTAL_BREAST_S0_PT                        --9/15 5721
--select count(distinct pat_id) from #TOTAL_BREAST_S0_PT   --9/15 5721






------*******************Baseline data for stage 0
IF OBJECT_ID('TEMPDB..#BASE_BREASTCANCER_S0_PT') IS NOT NULL DROP TABLE #BASE_BREASTCANCER_S0_PT;
SELECT DISTINCT B.PAT_ID
      ,B.EARLIEST_DX AS EARLIEST_BREAST_CANCER_DX
INTO #BASE_BREASTCANCER_S0_PT
FROM #TOTAL_BREAST_S0_PT AS B
     LEFT JOIN #TOTAL_CANCER_PT AS T ON B.PAT_ID = T.PAT_ID
WHERE (T.EARLIEST_DX IS NULL 
      OR T.EARLIEST_DX > B.EARLIEST_DX)
	  AND B.EARLIEST_DX BETWEEN '09/01/2013' AND '09/30/2015'  
ORDER BY B.PAT_ID ASC

--select * from #BASE_BREASTCANCER_S0_PT --9/14  3230
--select count(distinct pat_id) from #BASE_BREASTCANCER_S0_PT


/*
select * from (
              select  * from #STAGE0to3_PAT_LIST 
			  UNION 
			  select * from #BASE_BREASTCANCER_S0_PT
              ) AS A
  

select count(distinct pat_id) from #STAGE0to3_PAT_LIST where pat_id not in (select distinct pat_id from #BASE_BREASTCANCER_S0_PT)  --7852
select count(distinct pat_id) from #STAGE0to3_PAT_LIST where pat_id in (select distinct pat_id from #BASE_BREASTCANCER_S0_PT)      --1174

*/


--------------------------------------------------------------------------------------------------------------------------------------------
----Breast cancer pt Stage real 1 to 3 after eliminate Stage 0 pt. 
IF OBJECT_ID('TEMPDB..#STAGE1to3_PAT_LIST') IS NOT NULL DROP TABLE #STAGE1to3_PAT_LIST;
SELECT * 
INTO #STAGE1to3_PAT_LIST 
FROM #STAGE0to3_PAT_LIST 
WHERE PAT_ID NOT IN (select distinct pat_id from #BASE_BREASTCANCER_S0_PT) 


--select * from #STAGE1to3_PAT_LIST --7852
--_____________________________________________________________________________________________________________________________________________________
--------PULL OUT PROVIDERS' INFORMATION


--select * from #STAGE0to3_PAT_LIST_Prov

--final list of pt. Stage 0 to Stage 3  will use this table to pull out providers' information
IF OBJECT_ID('TEMPDB..#STAGE0TO3_PAT_LIST_PROV') IS NOT NULL DROP TABLE #STAGE0TO3_PAT_LIST_PROV;     --9/16 11082   
SELECT PAT_ID, MIN(EARLIEST_BREAST_CANCER_DX) AS FIRST_DIAGNOSIS
INTO #STAGE0TO3_PAT_LIST_PROV
FROM (
      SELECT DISTINCT PAT_ID, EARLIEST_BREAST_CANCER_DX
	  FROM #STAGE0TO3_PAT_LIST 
      UNION 
      SELECT DISTINCT PAT_ID, EARLIEST_BREAST_CANCER_DX 
	  FROM #BASE_BREASTCANCER_S0_PT
	  ) AS P  
GROUP BY PAT_ID

	  
--select top 100* from #STAGE0TO3_PAT_LIST_PROV  --11082
--select * from #STAGE0TO3_PAT_LIST_PROV
/*
select distinct f.pat_id
from #STAGE0TO3_PAT_LIST_PROV as f
     inner join pat_enc as e on e.pat_id = f.pat_id
where e.contact_date between '01/01/2015' and '09/30/2015'
      and e.serv_area_id = '26'


select distinct f.pat_id
from #STAGE0TO3_PAT_LIST_PROV as f
     inner join hsp_account as hsp on hsp.pat_id = f.pat_id
where hsp.adm_date_time between '01/01/2015' and '09/30/2015'
      and hsp.ACCT_BASECLS_HA_C != 3
	  and hsp.serv_area_id = '26'

	   
select top 100* from hsp_account
where serv_area_id = '26'
*/

---#####################################   PULL OUT PT WITH SUGERY CODES    ##################################
DECLARE @ADMIT AS DATE
DECLARE @DSCH AS DATE
SET @ADMIT='09/01/2013'     
SET @DSCH='9/30/2015'   


IF OBJECT_ID('TEMPDB..#STAGE0TO3_PAT_SURGERY') IS NOT NULL DROP TABLE #STAGE0TO3_PAT_SURGERY; 

SELECT DISTINCT P.PAT_ID
       ,P.FIRST_DIAGNOSIS AS FIRST_DIAGNOSIS_DATE
	   ,CAST(PX.PROC_DATE AS DATE) AS FIRST_SURGERY_DATE
	   ,PX.PROC_PERF_PROV_ID AS PERFORM_PROVIDER
    -- ,CAST(H.ADM_DATE_TIME AS DATE) AS ADM_DATE
	-- ,CPT.CPT_PERF_PROV_ID AS CPT_PROVIDER
	--,SER.PROV_NAME
	-- ,ICDPX.PROCEDURE_NAME
	-- ,ICDPX.REF_BILL_CODE
	-- ,CPT.CPT_CODE
	-- ,CPT.CPT_CODE_DESC
	--,ZSPEC.NAME AS SPECIALTY_NAME
	-- ,PX.FINAL_ICD_PX_ID
	-- ,ICDPX.ICD_PX_NAME
INTO #STAGE0TO3_PAT_SURGERY 
FROM 
	#STAGE0TO3_PAT_LIST_PROV    AS P 
	LEFT JOIN HSP_ACCOUNT      AS H              ON P.PAT_ID=H.PAT_ID
	LEFT JOIN HSP_ACCT_PX_LIST AS PX             ON PX.HSP_ACCOUNT_ID = H.HSP_ACCOUNT_ID
	LEFT JOIN CL_ICD_PX        AS ICDPX          ON ICDPX.ICD_PX_ID = PX.FINAL_ICD_PX_ID
    LEFT JOIN HSP_ACCT_CPT_CODES AS CPT          ON CPT.HSP_ACCOUNT_ID = H.HSP_ACCOUNT_ID
 --   LEFT JOIN HSP_TRANSACTIONS AS CPT          ON CPT.HSP_ACCOUNT_ID = H.HSP_ACCOUNT_ID
	LEFT JOIN CLARITY_SER                                          AS SER            ON PX.PROC_PERF_PROV_ID =  SER.PROV_ID
	LEFT JOIN CLARITY_SER_SPEC                                    AS SPEC           ON SER.PROV_ID = SPEC.PROV_ID
	LEFT JOIN  ZC_SPECIALTY                                        AS ZSPEC          ON ZSPEC.SPECIALTY_C = SPEC.SPECIALTY_C
WHERE 	CAST(H.ADM_DATE_TIME AS DATE) BETWEEN @ADMIT AND @DSCH 
        AND CAST(PX.PROC_DATE AS DATE) BETWEEN @ADMIT AND @DSCH    	  
		AND SER.ACTIVE_STATUS='ACTIVE'
		AND SER.CLINICIAN_TITLE = 'MD'
	    AND H.ACCT_BASECLS_HA_C != 3
	    AND ICDPX.REF_BILL_CODE LIKE '85.2%'
		AND ICDPX.REF_BILL_CODE NOT LIKE '85.24'
		OR ICDPX.REF_BILL_CODE LIKE '85.4%'
		OR CPT.CPT_CODE IN ('19110', '19120', '19125', '19160', '19162', '19180', '19182', '19200', '19220', '19240')
		AND ZSPEC.NAME IN ('Hospital Surgical', 'General Surgery','Surgical Oncology', 'Thoracic Surgery')
	    
  
                                                                                         
--select * from #STAGE0TO3_PAT_SURGERY --9/30 order by first_surgery_date where pat_id in ('3842930', '4246044') --2240   
--select count(distinct pat_id) from #STAGE0TO3_PAT_SURGERY       --9/28 2822,  9/30  2810  
--select count(distinct PERFORM_PROVIDER) from #STAGE0TO3_PAT_SURGERY  --9/28 114  9/30  140
--select distinct ref_bill_code from #STAGE0TO3_PAT_SURGERY 
--select distinct cpt_code from #STAGE0TO3_PAT_SURGERY order by cpt_code
--select distinct specialty_name from #STAGE0TO3_PAT_SURGERY











------#################################### breast cancer pt with surgeries #############################################

IF OBJECT_ID('TEMPDB..#TOTAL_COHORT') IS NOT NULL DROP TABLE #TOTAL_COHORT; 
 SELECT DISTINCT SURGERY.*
 INTO #TOTAL_COHORT
 FROM #STAGE0TO3_PAT_SURGERY AS SURGERY INNER JOIN 
    (SELECT DISTINCT PAT_ID, MIN(FIRST_SURGERY_DATE) AS FIRST_SURG_DATE
	 FROM #STAGE0TO3_PAT_SURGERY 
	 GROUP BY PAT_ID
	) T ON SURGERY.PAT_ID = T.PAT_ID
	   AND SURGERY.FIRST_SURGERY_DATE = T.FIRST_SURG_DATE  
WHERE FIRST_SURGERY_DATE BETWEEN '09/01/2014' AND '09/30/2015'  --scenario 1   1613 unique pt  --1611, 11/1 1617

--select count(distinct pat_id) from #total_cohort  --9/26 2812 --2798 -- 9/27 2812/2822
--select * from #total_cohort
--#################change where conditions and the cohort number changes



------****######adding surgeon's information to surgery patients
--select count(distinct pat_id) from #TOTAL_COHORT  --851
--select * from #TOTAL_COHORT  --WHERE PERFORM_PROVIDER NOT IN ('11000080', '30026399') --853
IF OBJECT_ID('TEMPDB..#TOTAL_COHORT_Prov') IS NOT NULL DROP TABLE #TOTAL_COHORT_Prov;
SELECT DISTINCT CO.*
     --  , DEP.DEPARTMENT_NAME AS SURGEON_DEPT
    --  , LOC.LOC_NAME         AS SURGEON_LOCATION
      , SA.SERV_AREA_NAME    AS SURGEON_SERV_AREA
	  , SA.SERV_AREA_ID
INTO #TOTAL_COHORT_Prov
FROM #TOTAL_COHORT AS CO
    /* LEFT JOIN HSP_TRANSACTIONS AS TRANS     ON TRANS.PERFORMING_PROV_ID = CO.PERFORM_PROVIDER
     left JOIN CLARITY_SA       AS SA       ON TRANS.SERV_AREA_ID = SA.SERV_AREA_ID
     left JOIN CLARITY_LOC      AS LOC       ON LOC.serv_area_id = trans.REVENUE_LOC_ID
     */
     LEFT JOIN CLARITY_SER      AS SER       ON SER.PROV_ID = CO.PERFORM_PROVIDER
     left JOIN CLARITY_SER_DEPT AS SDEPT	 ON SDEPT.PROV_ID = SER.PROV_ID 
	 left JOIN CLARITY_DEP      AS DEP	     ON DEP.DEPARTMENT_ID = SDEPT.DEPARTMENT_ID   
	 left JOIN CLARITY_SA       AS SA        ON SA.SERV_AREA_ID = DEP.SERV_AREA_ID
	 left JOIN CLARITY_LOC      AS LOC       ON LOC.serv_area_id = SA.SERV_AREA_ID


--select count(*) from #TOTAL_COHORT_Prov   --9/30 39383
--select count(distinct pat_id) from #TOTAL_COHORT_Prov  --1613  --9/30 1611
--select * from #TOTAL_COHORT_Prov  




---*******see cohort have imaging order or not************

--&&&&&&try 2 use general procedure type to pull out pt imaging orders

IF OBJECT_ID('TEMPDB..#COHORT_SCENARIO1') IS NOT NULL DROP TABLE #COHORT_SCENARIO1;

SELECT DISTINCT CO1.PAT_ID 
      ,CO1.FIRST_SURGERY_DATE 
      ,CAST(ORPROC.ORDERING_DATE AS DATE) AS ORDER_DATE 
    --  ,ORPROC.DESCRIPTION       
    --  ,ORPROC.DISPLAY_NAME
       ,ORPROC.AUTHRZING_PROV_ID AS IMG_PROV
    -- ,DEP.DEPARTMENT_NAME  AS IMG_PROV_DEPT
    --  ,LOC.LOC_NAME         AS IMG_PROV_LOCATION
	  ,SA.SERV_AREA_NAME     AS IMG_PROV_SERV_AREA
    --  ,ORPROC.CPT_CODE
    --  ,ORPROC.PROC_CODE
    -- ,ORPROC.ORDER_TYPE_C
      , CASE WHEN ORPROC.DESCRIPTION LIKE 'CT %' THEN 1 ELSE 0 END AS CT
      , CASE WHEN ORPROC.DESCRIPTION LIKE 'PET%' THEN 1 ELSE 0 END AS PET
      , CASE WHEN ORPROC.DESCRIPTION LIKE '%BONE SCAN%' THEN 1 ELSE 0 END AS BoneScan
INTO #COHORT_SCENARIO1
FROM #TOTAL_COHORT              AS CO1 
     INNER JOIN ORDER_PROC       AS ORPROC    ON CO1.PAT_ID = ORPROC.PAT_ID
     LEFT JOIN CLARITY_SER      AS SER       ON SER.PROV_ID = ORPROC.AUTHRZING_PROV_ID
	 left JOIN CLARITY_SER_DEPT AS SDEPT	 ON SDEPT.PROV_ID = SER.PROV_ID 
	 left JOIN CLARITY_DEP      AS DEP	     ON DEP.DEPARTMENT_ID = SDEPT.DEPARTMENT_ID   
	 left JOIN CLARITY_SA       AS SA        ON SA.SERV_AREA_ID = DEP.SERV_AREA_ID
	 left JOIN CLARITY_LOC      AS LOC       ON LOC.SERV_AREA_ID = SA.SERV_AREA_ID
WHERE 
      CO1.FIRST_SURGERY_DATE BETWEEN '09/01/2014' AND '09/30/2015' AND --!!!!!!!!!!!!! either 60 or 90 days' range  -60  or -30
      CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(dy, -60,  CO1.FIRST_SURGERY_DATE) AND DATEADD(dy, 30,  CO1.FIRST_SURGERY_DATE)  
	  AND ORPROC.ORDER_STATUS_C NOT IN (4, 9, 7)
	  AND (ORPROC.DESCRIPTION NOT LIKE '%FACIAL SINUSES WO CONTRAST%'
	        OR ORPROC.DESCRIPTION NOT LIKE '%GUIDANCE NEEDLE PLACEMENT%' )
	  OR
        (ORPROC.CPT_CODE IN ('78300','78305','78306','78315','78399')
         OR ORPROC.CPT_CODE IN ('78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254')
	     OR ORPROC.CPT_CODE IN ('70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	                              '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	                              '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	                              '74176','74177','74178','76497'))
	   OR (
	       ORPROC.PROC_CODE IN ('EA78815', '78815','EA78815','78815','RAD0348', '30500001','78816','EA78999' /*pet*/)
	       OR ORPROC.PROC_CODE IN ('78300','78306','EA78306','EA063195','78315','EA78315'/*bone*/)
	       OR ORPROC.PROC_CODE IN ('EA70450','EA064155','70450','RAD0076','70460','EA70460','EA064160','70470','EA064150',
'70480','EA064625','EA064415','EA064450','EA064455','EA064410','EA064386','70486',
'EA70486','EA064385','70487','70488','70490','70491','EA70491','EA71250',
'71250','EA064355','RAD0071','71260','EA71260','EA064360',
'RAD0069','71270','EA71270','EA064350','72125','EA064640','EA72125','EA72126',
'EA72128','EA064670','72131','EA064655','EA72131','EA72132','EA72192','EA064465','72192',
'RAD0088','EA72193','EA064470','EA064460','RAD0089','EA72194','EA73200','EA064292',
'EA73202','RAD0081','73700','EA064282','30400060','74150','RAD0062','EA064615',
'EA74150','EA74160','EA064620','74160','RAD0060','74170','EA74170','EA064610','RAD0061','74176','74177','74178'/*CT*/)
           AND ORPROC.PROC_CODE NOT IN ('RAD0083', 'RAD0057'))
	

--select count(distinct pat_id)from #COHORT_SCENARIO1   -- 1613(Sutter Clarity use internal imaging codes)
--select * from #COHORT_SCENARIO1 order by description where description like '%Bone scan%' '%CT %'  order_type_c = '5502'
--select distinct description from #COHORT_SCENARIO1 

/*
select distinct pat_id from (
--select distinct description from (
select  * from #COHORT_SCENARIO1 
--select  * from #SCENARIO1 
where BoneScan !=0
     OR CT !=0
     OR PET !=0) #w
*/

IF OBJECT_ID('TEMPDB..#SCENARIO1') IS NOT NULL DROP TABLE #SCENARIO1;
SELECT Prov.*
       ,CO1.ORDER_DATE
       ,CO1.IMG_PROV
   --    , IMG_PROV_LOCATION
       , IMG_PROV_SERV_AREA
       ,CO1.CT
       ,CO1.PET
       ,CO1.BoneScan
INTO #SCENARIO1
FROM #TOTAL_COHORT_Prov AS Prov 
     LEFT JOIN #COHORT_SCENARIO1 AS CO1 ON Prov.PAT_ID = Co1.PAT_ID



--select count(distinct pat_id) from #scenario1  --1613 , 9/30 1611
--select top 100* from #Scenario1 
--select count(distinct pat_id) from #scenario1 where CT = 1  --444  --9/30 444
--select count(distinct pat_id) from #scenario1 where PET = 1  --86, 9/30 86
--select count(distinct pat_id) from #scenario1 where BoneScan = 1  --33, 9/30 33
--select * from #SCENARIO1


-------summary data pullouts
select COUNT(distinct perform_provider) from #SCENARIO1 --114
--select surgeon_location, COUNT(distinct perform_provider) as num_of_surgeons from #SCENARIO1 group by SURGEON_LOCATION order by SURGEON_LOCATION
select surgeon_serv_area, COUNT(distinct perform_provider) as num_of_surgeons from #SCENARIO1 group by surgeon_serv_area order by surgeon_serv_area


--select surgeon_location, COUNT(distinct pat_id) as num_of_pt from #SCENARIO1 group by SURGEON_LOCATION order by SURGEON_LOCATION
select surgeon_serv_area, COUNT(distinct pat_id) as num_of_pt from #SCENARIO1 group by surgeon_serv_area order by surgeon_serv_area

/*
select  COUNT(distinct pat_id) as num_of_pt_with_imaging 
from #SCENARIO1 
where (CT = 1 or PET=1 or BoneScan = 1)
group by SURGEON_LOCATION 
order by SURGEON_LOCATION 
*/

select surgeon_serv_area, COUNT(distinct pat_id) as num_of_pt_with_imaging 
from #SCENARIO1 
where (CT = 1 or PET=1 or BoneScan = 1)
group by surgeon_serv_area
order by surgeon_serv_area


select distinct perform_provider as surgeon, count(distinct pat_id) as count_of_pt from #scenario1 group by perform_provider 
select distinct perform_provider as surgeon, count(distinct pat_id) as count_of_pt 
from #scenario1
where (CT = 1 or PET=1 or BoneScan = 1)
group by perform_provider 


select distinct img_prov as img_provider, count(distinct pat_id) as num_of_pt 
from #scenario1
where img_prov not in (select distinct PERFORM_PROVIDER from #scenario1)
group by img_prov
 

select distinct img_prov as img_provider, count(distinct pat_id) as num_of_pt 
from #scenario1
where img_prov not in (select distinct PERFORM_PROVIDER from #scenario1)
      and (CT = 1 or PET=1 or BoneScan = 1)
group by img_prov

 






--select * from #TOTAL_COHORT_Prov --9/27 3558  S1 2026
--select count(distinct pat_id) from #TOTAL_COHORT_Prov  --9/27 2812   S1 1613  S2 1052, 9/30 S1 1611

/*
----*************************************************************************************************************
----******10/3 New critieria pull out Sutter Medical Foundation patients only and find out all of their providers
----*************************************************************************************************************
*/
IF OBJECT_ID('TEMPDB..#PATList_C1') IS NOT NULL DROP TABLE #PATList_C1;
SELECT DISTINCT PAT_ID
       , FIRST_SURGERY_DATE
	   , SERV_AREA_ID
INTO #PATList_C1
FROM #SCENARIO1
WHERE SERV_AREA_ID IN (7, 22, 26, 27, 28)

--select * from #PATList_C1
--select distinct pat_id from #PATList_C1  --1162 compared to 1613



---#######LIST OF PT WITH ENCOUNTERS AND VISITING PROVIDERS
IF OBJECT_ID('TEMPDB..#COHORT1_ALL') IS NOT NULL DROP TABLE #COHORT1_ALL;

SELECT DISTINCT PATList.PAT_ID
       , PATList.FIRST_SURGERY_DATE
	--   , CAST(ENC.CONTACT_DATE AS DATE) AS ENCOUNTER_DATE
	   , ENC.VISIT_PROV_ID
	--   , ENC.VISIT_PROV_TITLE AS PROV_TITLE_C
	--   , TIT.NAME AS PROVIDER_TITLE
	--   , ENC.DEPARTMENT_ID
	   , DEP.DEPARTMENT_NAME
	   , LOC.LOC_NAME AS LOC_NAME
	--   , SA. SERV_AREA_NAME
	--   , EDG.REF_BILL_CODE
	--   , EDG.CURRENT_ICD9_LIST
	    , ROW_NUMBER () OVER (PARTITION BY  ENC.VISIT_PROV_ID, PATList.PAT_ID
	                                     ORDER BY ENC.VISIT_PROV_ID) AS DuplicateCount
INTO #COHORT1_ALL
FROM #PATList_C1                AS PATList 
     LEFT JOIN PAT_ENC          AS ENC ON ENC.PAT_ID = PATList.PAT_ID
	 INNER JOIN PAT_ENC_DX      AS DX ON DX.PAT_ENC_CSN_ID = ENC.PAT_ENC_CSN_ID
	 INNER JOIN CLARITY_EDG     AS EDG ON EDG.DX_ID = DX.DX_ID
	 LEFT JOIN #ERR_ENC_RSN     AS A1 ON A1.PAT_ENC_CSN_ID = ENC.PAT_ENC_CSN_ID
	 LEFT JOIN #ERR_CSN_DX      AS A2 ON A2.PAT_ENC_CSN_ID = ENC.PAT_ENC_CSN_ID
	 LEFT JOIN CLARITY_SER      AS SER ON SER.PROV_ID = ENC.VISIT_PROV_ID
	 LEFT JOIN ZC_ALT_ORD_PRV_TIT AS TIT ON TIT.ALT_ORD_PRV_TIT_C = ENC.VISIT_PROV_TITLE 
	 LEFT JOIN CLARITY_DEP AS DEP ON DEP.DEPARTMENT_ID = ENC.DEPARTMENT_ID
	 LEFT JOIN CLARITY_LOC AS LOC ON LOC.LOC_ID = DEP.REV_LOC_ID
	-- LEFT JOIN CLARITY_SA AS SA   ON SA.SERV_AREA_ID = LOC.SERV_AREA_ID
WHERE 
     CAST(ENC.CONTACT_DATE AS DATE) BETWEEN DATEADD(m, -6,  PATList.FIRST_SURGERY_DATE) AND DATEADD(m, 6,  PATList.FIRST_SURGERY_DATE) 
	 AND ENC.SERV_AREA_ID IN (7,22,26,27,28)
	 AND ENC.ENC_TYPE_C NOT IN ('201','249')
	 AND (ENC.APPT_STATUS_C IN (2,6) OR ENC.APPT_STATUS_C IS NULL)
	 AND ENC.ENC_CLOSED_YN='Y'
	 AND A1.PAT_ENC_CSN_ID IS NULL 
	 AND A2.PAT_ENC_CSN_ID IS NULL
	-- AND (EDG.REF_BILL_CODE LIKE '174%' OR  EDG.REF_BILL_CODE LIKE '233%')
	-- AND (EDG.CURRENT_ICD9_LIST LIKE '174%' OR EDG.CURRENT_ICD9_LIST LIKE '233%')
	 AND SER.ACTIVE_STATUS NOT LIKE 'Inactive%'
	 AND (TIT.NAME LIKE 'MD' OR TIT.NAME LIKE 'DO' OR TIT.NAME LIKE NULL)
ORDER BY PAT_ID

DELETE FROM #COHORT1_ALL
WHERE DuplicateCount >1




--select * from #COHORT1_ALL
 select distinct visit_prov_id from (select distinct visit_prov_id, count(distinct pat_id) as countNo from #COHORT1_ALL 
 group by visit_prov_id) m where countNo >=10   --10/3  682 , 752
--select count(distinct visit_prov_id) from #COHORT1_ALL -- 10/5   897
--select count(distinct pat_id) from #COHORT1_ALL  --10/5  1092






IF OBJECT_ID('TEMPDB..#COHORT1_ORDERS') IS NOT NULL DROP TABLE #COHORT1_ORDERS;
SELECT DISTINCT AL.PAT_ID
       , AL.FIRST_SURGERY_DATE
	   , AL.VISIT_PROV_ID
	   , AL.DEPARTMENT_NAME
	   , AL.LOC_NAME
	--   ,ORPROC.CPT_CODE
    --   ,ORPROC.PROC_CODE
	--   , ORPROC.DESCRIPTION 
	  , CASE WHEN ORPROC.DESCRIPTION LIKE 'CT %' THEN 1 ELSE 0 END AS CT
       , CASE WHEN ORPROC.DESCRIPTION LIKE 'PET%' THEN 1 ELSE 0 END AS PET
      , CASE WHEN ORPROC.DESCRIPTION LIKE '%BONE SCAN%' THEN 1 ELSE 0 END AS BoneScan
INTO #COHORT1_ORDERS
FROM #COHORT1_ALL AS AL
     LEFT JOIN ORDER_PROC AS ORPROC  ON (AL.PAT_ID = ORPROC.PAT_ID AND AL.VISIT_PROV_ID = ORPROC.AUTHRZING_PROV_ID)
WHERE 
       AL.FIRST_SURGERY_DATE BETWEEN '09/01/2014' AND '09/30/2015'
	   /*
	   AND AL.VISIT_PROV_ID IN (select distinct visit_prov_id 
	                            from (select distinct visit_prov_id, count(distinct pat_id) as countNo 
								from #COHORT1_ALL 
                                group by visit_prov_id) m where countNo >=10)
       */
	   AND
       CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(m, -6,  AL.FIRST_SURGERY_DATE) AND DATEADD(m, +6,  AL.FIRST_SURGERY_DATE)
      OR (
         ORPROC.CPT_CODE IN ('78300','78305','78306','78315','78399'/*bone*/)
         OR ORPROC.CPT_CODE IN ('78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254'/*pet*/)
	     OR ORPROC.CPT_CODE IN ('70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	                              '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	                              '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	                              '74176','74177','74178','76497'/*CT*/)	   
	       OR ORPROC.PROC_CODE IN ('EA78815', '78815','EA78815','78815','RAD0348', '30500001','78816','EA78999' /*pet*/)
	       OR ORPROC.PROC_CODE IN ('78300','78306','EA78306','EA063195','78315','EA78315'/*bone*/)
	       OR ORPROC.PROC_CODE IN ('EA70450','EA064155','70450','RAD0076','70460','EA70460','EA064160','70470','EA064150',
'70480','EA064625','EA064415','EA064450','EA064455','EA064410','EA064386','70486',
'EA70486','EA064385','70487','70488','70490','70491','EA70491','EA71250',
'71250','EA064355','RAD0071','71260','EA71260','EA064360',
'RAD0069','71270','EA71270','EA064350','72125','EA064640','EA72125','EA72126',
'EA72128','EA064670','72131','EA064655','EA72131','EA72132','EA72192','EA064465','72192',
'RAD0088','EA72193','EA064470','EA064460','RAD0089','EA72194','EA73200','EA064292',
'EA73202','RAD0081','73700','EA064282','30400060','74150','RAD0062','EA064615',
'EA74150','EA74160','EA064620','74160','RAD0060','74170','EA74170','EA064610','RAD0061','74176','74177','74178'/*CT*/)
              )


--select * from #COHORT1_ORDERS --1749
--select count(distinct pat_id) from #COHORT1_ORDERS where (ct=1 or pet=1 or bonescan=1)  --140         199
--select count(distinct visit_prov_id) from #COHORT1_ORDERS where (ct=1 or pet=1 or bonescan=1)   --  105
--select visit_prov_id, department_name, loc_name, count (distinct pat_id) as countOfPt from #COHORT1_ORDERS where (ct=1 or pet=1 or bonescan=1) group by visit_prov_id, department_name, loc_name

     select t.* 
            ,ROW_NUMBER () OVER (PARTITION BY t.PAT_ID, t.VISIT_PROV_ID
	                                     ORDER BY t.PAT_ID) AS CountProv
            ,ROW_NUMBER () OVER (PARTITION BY PAT_ID ORDER BY VISIT_PROV_ID) AS RowNumber
     from (select * from #COHORT1_ORDERS where (ct=1 or pet=1 or bonescan=1)) t  
	 order by pat_id




IF OBJECT_ID('TEMPDB..#C1') IS NOT NULL DROP TABLE #C1;
select distinct visit_prov_id 
INTO #c1
from (select distinct visit_prov_id, count(distinct pat_id) as countNo 
		from #COHORT1_ALL 
        group by visit_prov_id) p where countNo >=10 





--select * from #c1
select distinct or1.visit_prov_id, zcspec.name as specialty_name, count(distinct pat_id) as count_of_total_pt, department_name, loc_name
from #c1  as or1
     LEFT JOIN #COHORT1_ORDERS on #COHORT1_ORDERS.visit_prov_id = or1.visit_prov_id
	 LEFT JOIN CLARITY_SER_SPEC AS PROV_SPEC ON prov_spec.prov_id = or1.visit_prov_id
	 LEFT JOIN ZC_SPECIALTY AS ZCSPEC ON ZCSPEC.SPECIALTY_C = PROV_SPEC.SPECIALTY_C
group by or1.visit_prov_id, zcspec.name, department_name, loc_name





select visit_prov_id, specialty_name, count(distinct pat_id), department_name, loc_name
from
(select #COHORT1_ORDERS.*, zcspec.name as specialty_name
from #c1  as or1
     LEFT JOIN #COHORT1_ORDERS on #COHORT1_ORDERS.visit_prov_id = or1.visit_prov_id
	 LEFT JOIN CLARITY_SER_SPEC AS PROV_SPEC ON prov_spec.prov_id = or1.visit_prov_id
	 LEFT JOIN ZC_SPECIALTY AS ZCSPEC ON ZCSPEC.SPECIALTY_C = PROV_SPEC.SPECIALTY_C
where #COHORT1_ORDERS.visit_prov_id is not null
      AND (CT=1 OR PET=1 OR BoneScan=1))as q
group by visit_prov_id, specialty_name, department_name, loc_name


























----******************Total cohort time range for scenario2
IF OBJECT_ID('TEMPDB..#TOTAL_COHORT2') IS NOT NULL DROP TABLE #TOTAL_COHORT2; 
SELECT DISTINCT SURGERY.*
INTO #TOTAL_COHORT2
FROM #STAGE0TO3_PAT_SURGERY AS SURGERY INNER JOIN 
    (SELECT DISTINCT PAT_ID, MIN(FIRST_SURGERY_DATE) AS FIRST_SURG_DATE
	 FROM #STAGE0TO3_PAT_SURGERY 
	 GROUP BY PAT_ID
	) T ON SURGERY.PAT_ID = T.PAT_ID
	   AND SURGERY.FIRST_SURGERY_DATE = T.FIRST_SURG_DATE  
WHERE FIRST_SURGERY_DATE BETWEEN '09/01/2013' AND '08/31/2014'  --scenario 2     1056 pt  1052  

--select count(distinct pat_id) from #TOTAL_COHORT2 --9/30  1052
--select * from #TOTAL_COHORT2
--select count(distinct perform_provider) from #TOTAL_COHORT2 --83

---*********providers from scenario 2


IF OBJECT_ID('TEMPDB..#TOTAL_COHORT_Prov2') IS NOT NULL DROP TABLE #TOTAL_COHORT_Prov2;
SELECT DISTINCT CO.*
     --  , DEP.DEPARTMENT_NAME AS SURGEON_DEPT
    --  , LOC.LOC_NAME         AS SURGEON_LOCATION
      , SA.SERV_AREA_NAME    AS SURGEON_SERV_AREA
	  , SA.SERV_AREA_ID
INTO #TOTAL_COHORT_Prov2
FROM #TOTAL_COHORT2 AS CO
     LEFT JOIN CLARITY_SER      AS SER       ON SER.PROV_ID = CO.PERFORM_PROVIDER
     left JOIN CLARITY_SER_DEPT AS SDEPT	 ON SDEPT.PROV_ID = SER.PROV_ID 
	 left JOIN CLARITY_DEP      AS DEP	     ON DEP.DEPARTMENT_ID = SDEPT.DEPARTMENT_ID   
	 left JOIN CLARITY_SA       AS SA        ON SA.SERV_AREA_ID = DEP.SERV_AREA_ID
	 left JOIN CLARITY_LOC      AS LOC       ON LOC.serv_area_id = SA.SERV_AREA_ID



--select count (distinct pat_id) from #TOTAL_COHORT_Prov2    9/30  1052
--select top 100* from #TOTAL_COHORT_Prov2
--select count (distinct perform_provider) from #TOTAL_COHORT_Prov2 --9/30 83



---pull out data for scenario 2 imaging orders, increase flag variables 
IF OBJECT_ID('TEMPDB..#COHORT_SCENARIO2') IS NOT NULL DROP TABLE #COHORT_SCENARIO2;

SELECT DISTINCT CO2.PAT_ID 
      ,CO2.FIRST_SURGERY_DATE 
      ,CAST(ORPROC.ORDERING_DATE AS DATE) AS ORDER_DATE 
      ,ORPROC.DESCRIPTION       
    --  ,ORPROC.DISPLAY_NAME
       ,ORPROC.AUTHRZING_PROV_ID AS IMG_PROV
    -- ,DEP.DEPARTMENT_NAME  AS IMG_PROV_DEPT
    --  ,LOC.LOC_NAME         AS IMG_PROV_LOCATION
	  ,SA.SERV_AREA_NAME     AS IMG_PROV_SERV_AREA
   --   ,ORPROC.CPT_CODE
  --    ,ORPROC.PROC_CODE
  --   ,ORPROC.ORDER_TYPE_C
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'CT %' THEN 1 ELSE 0 END AS CT
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'PET%' THEN 1 ELSE 0 END AS PET
   , CASE WHEN ORPROC.DESCRIPTION LIKE '%BONE SCAN%' THEN 1 ELSE 0 END AS BoneScan
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'US%' THEN 1 ELSE 0 END AS US
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'XR%' OR ORPROC.DESCRIPTION LIKE 'XRAY%' THEN 1 ELSE 0 END AS XRAY
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'MRI%' THEN 1 ELSE 0 END AS MRI
INTO #COHORT_SCENARIO2
FROM #TOTAL_COHORT2              AS CO2 
     INNER JOIN ORDER_PROC       AS ORPROC    ON CO2.PAT_ID = ORPROC.PAT_ID
     LEFT JOIN CLARITY_SER      AS SER       ON SER.PROV_ID = ORPROC.AUTHRZING_PROV_ID
	 left JOIN CLARITY_SER_DEPT AS SDEPT	 ON SDEPT.PROV_ID = SER.PROV_ID 
	 left JOIN CLARITY_DEP      AS DEP	     ON DEP.DEPARTMENT_ID = SDEPT.DEPARTMENT_ID   
	 left JOIN CLARITY_SA       AS SA        ON SA.SERV_AREA_ID = DEP.SERV_AREA_ID
	 left JOIN CLARITY_LOC      AS LOC       ON LOC.LOC_ID = SA.SERV_AREA_ID
WHERE 
      CO2.FIRST_SURGERY_DATE BETWEEN '09/01/2013' AND '08/31/2014' 
	  AND
      CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(m, 13,  CO2.FIRST_SURGERY_DATE) AND DATEADD(m, 24,  CO2.FIRST_SURGERY_DATE)
	  AND ORPROC.ORDER_STATUS_C NOT IN (130, 4, 9, 7)
	 AND  (ORPROC.DESCRIPTION NOT LIKE '%FACIAL SINUSES WO CONTRAST%'
	        OR ORPROC.DESCRIPTION NOT LIKE '%GUIDANCE NEEDLE PLACEMENT%' )
     OR (ORPROC.CPT_CODE IN ('76700','93975','76705','78399' /*US*/)
     OR ORPROC.PROC_CODE IN ('EA76700', 'EA76700BO', 'EA76705', '76700', '76705', '93975', 'EA067225', 'RAD0459', 'EA067236'/*US*/)
	 OR ORPROC.CPT_CODE IN ('71020','71023','71021','71022','71010', '71030', '71034', '71035'/*X-ray*/)
	 OR ORPROC.PROC_CODE IN ('EA062324','EA062326','EA062328','EA71010','EA71020','EA71020SF','71010','71020','RAD0576', 'RAD0575'/*x-ray*/)
	 OR ORPROC.CPT_CODE IN ('74181','74182','74183','77059','77058', '71550', '71551', '71552'/*MRI*/)
	 OR ORPROC.PROC_CODE IN ('30300075','RAD0185','RAD0190','EA77059','74181','74182','74183','77059','71552', '77058'/*MRI*/)
	 OR ORPROC.CPT_CODE IN ('78300','78305','78306','78315','78399')
     OR ORPROC.CPT_CODE IN ('78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254')
	 OR ORPROC.CPT_CODE IN ('70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	                              '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	                              '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	                              '74176','74177','74178','76497')
	  OR ORPROC.PROC_CODE IN ('EA78815', '78815','EA78815','78815','RAD0348', '30500001','78816','EA78999' /*pet*/)
	  OR ORPROC.PROC_CODE IN ('78300','78306','EA78306','EA063195','78315','EA78315'/*bone*/)
	  OR ORPROC.PROC_CODE IN ('EA70450','EA064155','70450','RAD0076','70460','EA70460','EA064160','70470','EA064150',
'70480','EA064625','EA064415','EA064450','EA064455','EA064410','EA064386','70486',
'EA70486','EA064385','70487','70488','70490','70491','EA70491','EA71250',
'71250','EA064355','RAD0071','71260','EA71260','EA064360',
'RAD0069','71270','EA71270','EA064350','72125','EA064640','EA72125','EA72126',
'EA72128','EA064670','72131','EA064655','EA72131','EA72132','EA72192','EA064465','72192',
'RAD0088','EA72193','EA064470','EA064460','RAD0089','EA72194','EA73200','EA064292',
'EA73202','RAD0081','73700','EA064282','30400060','74150','RAD0062','EA064615',
'EA74150','EA74160','EA064620','74160','RAD0060','74170','EA74170','EA064610','RAD0061','74176','74177','74178'/*CT*/)
           AND ORPROC.PROC_CODE NOT IN ('RAD0083', 'RAD0057'))
		  
--select count(distinct pat_id) from #COHORT_SCENARIO2 where US = 1   --366
--select count(distinct pat_id) from #COHORT_SCENARIO2 where XRAY = 1  --613
--select count(distinct pat_id) from #COHORT_SCENARIO2 where MRI = 1  --227
--select count(distinct pat_id) from #COHORT_SCENARIO2 where CT = 1  --294
--select count(distinct pat_id) from #COHORT_SCENARIO2 where PET = 1 --44
--select count(distinct pat_id) from #COHORT_SCENARIO2 where BoneScan = 1 --22
--select count(distinct pat_id) from #COHORT_SCENARIO2 where (US=1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1)  --782

--select distinct description, cpt_code, proc_code from #COHORT_SCENARIO2 order by description
--select count(distinct pat_id) from #COHORT_SCENARIO2   --960



----*******cohort 2 surgeon and imaging orders
IF OBJECT_ID('TEMPDB..#SCENARIO2') IS NOT NULL DROP TABLE #SCENARIO2;
SELECT DISTINCT Prov.*
       ,CO2.ORDER_DATE
       ,CO2.IMG_PROV
   --    , IMG_PROV_LOCATION
       , IMG_PROV_SERV_AREA
       ,CO2.CT
       ,CO2.PET
       ,CO2.BoneScan
	   ,CO2.MRI
	   ,CO2.XRAY
	   ,CO2.US
INTO #SCENARIO2
FROM #TOTAL_COHORT_Prov2 AS Prov 
     LEFT JOIN #COHORT_SCENARIO2 AS CO2 ON Prov.PAT_ID = Co2.PAT_ID

--select * from #Scenario2



select surgeon_serv_area, COUNT(distinct pat_id) as num_of_pt from #SCENARIO2 group by surgeon_serv_area order by surgeon_serv_area


select surgeon_serv_area, COUNT(distinct pat_id) as num_of_pt_with_imaging 
from #SCENARIO2
where (US=1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1)
group by surgeon_serv_area
order by surgeon_serv_area


select distinct perform_provider as surgeon, count(distinct pat_id) as count_of_pt from #scenario2 group by perform_provider 
select distinct perform_provider as surgeon, count(distinct pat_id) as count_of_pt 
from #scenario2
where (US=1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1)
group by perform_provider 


select distinct img_prov as img_provider, count(distinct pat_id) as num_of_pt 
from #scenario2
where img_prov not in (select distinct PERFORM_PROVIDER from #scenario1)
group by img_prov
 

select distinct img_prov as img_provider, count(distinct pat_id) as num_of_pt 
from #scenario2
where img_prov not in (select distinct PERFORM_PROVIDER from #scenario1)
      and (US=1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1)
group by img_prov





/*
----*************************************************************************************************************
----******10/4 New critieria pull out Sutter Medical Foundation patients only and find out all of their providers
----*************************************************************************************************************
*/
IF OBJECT_ID('TEMPDB..#PATList_C2') IS NOT NULL DROP TABLE #PATList_C2;
SELECT DISTINCT PAT_ID
       , FIRST_SURGERY_DATE
	   , SERV_AREA_ID
INTO #PATList_C2
FROM #SCENARIO2
WHERE SERV_AREA_ID IN (7, 22, 26, 27, 28)

--select * from #PATList_C2
--select distinct pat_id from #PATList_C2  --705 compared to 1052




---#######LIST OF PT WITH ENCOUNTERS AND VISITING PROVIDERS
IF OBJECT_ID('TEMPDB..#COHORT2_ALL') IS NOT NULL DROP TABLE #COHORT2_ALL;

SELECT DISTINCT PATList.PAT_ID
       , PATList.FIRST_SURGERY_DATE
	--   , CAST(ENC.CONTACT_DATE AS DATE) AS ENCOUNTER_DATE
	   , ENC.VISIT_PROV_ID
	--   , ENC.VISIT_PROV_TITLE AS PROV_TITLE_C
	--   , TIT.NAME AS PROVIDER_TITLE
	--   , ENC.DEPARTMENT_ID
	   , DEP.DEPARTMENT_NAME
	   , LOC.LOC_NAME AS LOC_NAME
	--   , SA. SERV_AREA_NAME
	--   , EDG.REF_BILL_CODE
	--   , EDG.CURRENT_ICD9_LIST
	    , ROW_NUMBER () OVER (PARTITION BY PATList.PAT_ID, ENC.VISIT_PROV_ID
	                                     ORDER BY PATList.PAT_ID) AS DuplicateCount
INTO #COHORT2_ALL
FROM #PATList_C2                AS PATList 
     LEFT JOIN PAT_ENC          AS ENC ON ENC.PAT_ID = PATList.PAT_ID
	 INNER JOIN PAT_ENC_DX      AS DX ON DX.PAT_ENC_CSN_ID = ENC.PAT_ENC_CSN_ID
	 INNER JOIN CLARITY_EDG     AS EDG ON EDG.DX_ID = DX.DX_ID
	 LEFT JOIN #ERR_ENC_RSN     AS A1 ON A1.PAT_ENC_CSN_ID = ENC.PAT_ENC_CSN_ID
	 LEFT JOIN #ERR_CSN_DX      AS A2 ON A2.PAT_ENC_CSN_ID = ENC.PAT_ENC_CSN_ID
	 LEFT JOIN CLARITY_SER      AS SER ON SER.PROV_ID = ENC.VISIT_PROV_ID
	 LEFT JOIN ZC_ALT_ORD_PRV_TIT AS TIT ON TIT.ALT_ORD_PRV_TIT_C = ENC.VISIT_PROV_TITLE 
	 LEFT JOIN CLARITY_DEP AS DEP ON DEP.DEPARTMENT_ID = ENC.DEPARTMENT_ID
	 LEFT JOIN CLARITY_LOC AS LOC ON LOC.LOC_ID = DEP.REV_LOC_ID
	-- LEFT JOIN CLARITY_SA AS SA   ON SA.SERV_AREA_ID = LOC.SERV_AREA_ID
WHERE 
     CAST(ENC.CONTACT_DATE AS DATE) BETWEEN DATEADD(m, 13 ,  PATList.FIRST_SURGERY_DATE) AND DATEADD(m, 24,  PATList.FIRST_SURGERY_DATE) 
	 AND ENC.SERV_AREA_ID IN (7,22,26,27,28)
	 AND ENC.ENC_TYPE_C NOT IN ('201','249')
	 AND (ENC.APPT_STATUS_C IN (2,6) OR ENC.APPT_STATUS_C IS NULL)
	 AND ENC.ENC_CLOSED_YN='Y'
	 AND A1.PAT_ENC_CSN_ID IS NULL 
	 AND A2.PAT_ENC_CSN_ID IS NULL
--	 AND (EDG.REF_BILL_CODE LIKE '174%' OR  EDG.REF_BILL_CODE LIKE '233%')
--	 AND (EDG.CURRENT_ICD9_LIST LIKE '174%' OR EDG.CURRENT_ICD9_LIST LIKE '233%')
	 AND SER.ACTIVE_STATUS NOT LIKE 'Inactive%'
	 AND (TIT.NAME LIKE 'MD' OR TIT.NAME LIKE 'DO' OR TIT.NAME LIKE NULL)
ORDER BY PAT_ID

DELETE FROM #COHORT2_ALL
WHERE DuplicateCount >1

--select * FROM #COHORT2_ALL
-- select count(distinct pat_id) FROM #COHORT2_ALL  --635   464
--select count(distinct visit_prov_id) FROM #COHORT2_ALL  --339  529





IF OBJECT_ID('TEMPDB..#COHORT2_ORDERS') IS NOT NULL DROP TABLE #COHORT2_ORDERS;
SELECT DISTINCT AL.PAT_ID
       , AL.FIRST_SURGERY_DATE
	   , AL.VISIT_PROV_ID
	--   , AL.DEPARTMENT_NAME
	   , AL.LOC_NAME
	--   ,ORPROC.CPT_CODE
    --   ,ORPROC.PROC_CODE
	--   , ORPROC.DESCRIPTION 
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'CT %' THEN 1 ELSE 0 END AS CT
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'PET%' THEN 1 ELSE 0 END AS PET
   , CASE WHEN ORPROC.DESCRIPTION LIKE '%BONE SCAN%' THEN 1 ELSE 0 END AS BoneScan
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'US%' THEN 1 ELSE 0 END AS US
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'XR%' OR ORPROC.DESCRIPTION LIKE 'XRAY%' THEN 1 ELSE 0 END AS XRAY
   , CASE WHEN ORPROC.DESCRIPTION LIKE 'MRI%' THEN 1 ELSE 0 END AS MRI
INTO #COHORT2_ORDERS
FROM #COHORT2_ALL AS AL
     LEFT JOIN ORDER_PROC AS ORPROC  ON (AL.PAT_ID = ORPROC.PAT_ID AND AL.VISIT_PROV_ID = ORPROC.AUTHRZING_PROV_ID)
WHERE 
       AL.FIRST_SURGERY_DATE BETWEEN '09/01/2013' AND '08/31/2014'
	   /*
	   AND AL.VISIT_PROV_ID IN (select distinct visit_prov_id 
	                            from (select distinct visit_prov_id, count(distinct pat_id) as countNo 
								from #COHORT2_ALL 
                                group by visit_prov_id) m where countNo >=10)
       */
	   AND
      CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(m, 13,  AL.FIRST_SURGERY_DATE) AND DATEADD(m, 24,  AL.FIRST_SURGERY_DATE)
	  AND ORPROC.ORDER_STATUS_C NOT IN (4, 9, 7)
      OR (ORPROC.CPT_CODE IN ('76700','93975','76705','78399' /*US*/)
         OR ORPROC.PROC_CODE IN ('EA76700', 'EA76700BO', 'EA76705', '76700', '76705', '93975', 'EA067225', 'RAD0459', 'EA067236'/*US*/)
	     OR ORPROC.CPT_CODE IN ('71020','71023','71021','71022','71010', '71030', '71034', '71035'/*X-ray*/)
	     OR ORPROC.PROC_CODE IN ('EA062324','EA062326','EA062328','EA71010','EA71020','EA71020SF','71010','71020','RAD0576', 'RAD0575'/*x-ray*/)
	     OR ORPROC.CPT_CODE IN ('74181','74182','74183','77059','77058', '71550', '71551', '71552'/*MRI*/)
	     OR ORPROC.PROC_CODE IN ('30300075','RAD0185','RAD0190','EA77059','74181','74182','74183','77059','71552', '77058'/*MRI*/)
	     OR ORPROC.CPT_CODE IN ('78300','78305','78306','78315','78399')
         OR ORPROC.CPT_CODE IN ('78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254')
	     OR ORPROC.CPT_CODE IN ('70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	                              '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	                              '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	                              '74176','74177','74178','76497')
	    OR ORPROC.PROC_CODE IN ('EA78815', '78815','EA78815','78815','RAD0348', '30500001','78816','EA78999' /*pet*/)
	    OR ORPROC.PROC_CODE IN ('78300','78306','EA78306','EA063195','78315','EA78315'/*bone*/)
	    OR ORPROC.PROC_CODE IN ('EA70450','EA064155','70450','RAD0076','70460','EA70460','EA064160','70470','EA064150',
                                '70480','EA064625','EA064415','EA064450','EA064455','EA064410','EA064386','70486', 'EA70486','EA064385',
								'70487','70488','70490', '70491','EA70491','EA71250', '71250','EA064355','RAD0071','71260','EA71260','EA064360',
								'RAD0069','71270','EA71270','EA064350','72125','EA064640','EA72125','EA72126',
								'EA72128','EA064670','72131','EA064655','EA72131','EA72132','EA72192','EA064465','72192',
								'RAD0088','EA72193','EA064470','EA064460','RAD0089','EA72194','EA73200','EA064292',
								'EA73202','RAD0081','73700','EA064282','30400060','74150','RAD0062','EA064615',
								'EA74150','EA74160','EA064620','74160','RAD0060','74170','EA74170','EA064610','RAD0061','74176','74177','74178'/*CT*/)
		   )

--select * from #COHORT2_ORDERS --665  905
--select count(distinct pat_id) from #COHORT2_ORDERS where  (US=1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1) --242  253  252
--select count(distinct visit_prov_id) from #COHORT2_ORDERS where  (US=1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1)  --91 131 212
select visit_prov_id, count (distinct pat_id) as countOfPt 
from #COHORT2_ORDERS 
where  (US=1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1)         
group by visit_prov_id




IF OBJECT_ID('TEMPDB..#C2') IS NOT NULL DROP TABLE #C2;
select distinct visit_prov_id 
INTO #c2
from (select distinct visit_prov_id, count(distinct pat_id) as countNo 
		from #COHORT2_ALL
        group by visit_prov_id) m where countNo >=10 





select distinct or2.visit_prov_id, zcspec.name as specialty_name, count(distinct pat_id) as count_of_total_pt, loc_name
from #c2  as or2
     LEFT JOIN #COHORT2_ORDERS on #COHORT2_ORDERS.visit_prov_id = or2.visit_prov_id
	 LEFT JOIN CLARITY_SER_SPEC AS PROV_SPEC ON prov_spec.prov_id = or2.visit_prov_id
	 LEFT JOIN ZC_SPECIALTY AS ZCSPEC ON ZCSPEC.SPECIALTY_C = PROV_SPEC.SPECIALTY_C
group by or2.visit_prov_id, zcspec.name, loc_name

--select * from ZC_FINANCIAL_CLASS


select * from #COHORT2_ORDERS



select visit_prov_id, specialty_name, count(distinct pat_id) as Pt_count, loc_name --department_name, 
from
(select #COHORT2_ORDERS.* , zcspec.name as specialty_name
from #c2  as or2
     LEFT JOIN #COHORT2_ORDERS on #COHORT2_ORDERS.visit_prov_id = or2.visit_prov_id
	 LEFT JOIN CLARITY_SER_SPEC AS PROV_SPEC ON prov_spec.prov_id = or2.visit_prov_id
	 LEFT JOIN ZC_SPECIALTY AS ZCSPEC ON ZCSPEC.SPECIALTY_C = PROV_SPEC.SPECIALTY_C
where #COHORT2_ORDERS.visit_prov_id is not null
      AND (BoneScan = 1 ))as n
group by visit_prov_id, specialty_name, loc_name

--US = 1 or XRAY=1 OR MRI=1 OR CT=1 OR PET=1 OR BoneScan=1






--##########cohort 2 biomarkers

--##########cohort 2 biomarkers
IF OBJECT_ID('TEMPDB..#COHORT2_LAB') IS NOT NULL DROP TABLE #COHORT2_LAB;
SELECT DISTINCT AL.PAT_ID
       , AL.FIRST_SURGERY_DATE
	   , AL.VISIT_PROV_ID
	   , AL.DEPARTMENT_NAME
	   , AL.LOC_NAME
	   ,ORPROC.CPT_CODE
       ,ORPROC.PROC_CODE
	   , ORPROC.DESCRIPTION 


INTO #COHORT2_LAB
FROM #COHORT2_ALL AS AL
     LEFT JOIN ORDER_PROC AS ORPROC  ON (AL.PAT_ID = ORPROC.PAT_ID AND AL.VISIT_PROV_ID = ORPROC.AUTHRZING_PROV_ID)
WHERE 
       AL.FIRST_SURGERY_DATE BETWEEN '09/01/2013' AND '08/31/2014'
	   AND
      CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(m, 13,  AL.FIRST_SURGERY_DATE) AND DATEADD(m, 24,  AL.FIRST_SURGERY_DATE)
	  AND ORPROC.ORDER_STATUS_C NOT IN (130, 4, 9, 7)
      OR (ORPROC.CPT_CODE IN ('82378','86300','86304')
         OR ORPROC.PROC_CODE IN ('82378','86300','86304', 'EA82378', 'EA86304', '45000094')
		   )



select * from #COHORT2_LAB 
where  CPT_CODE IN ('82378','86300','86304') OR PROC_CODE IN ('82378','86300','86304', 'EA82378', 'EA86304', '45000094')
		   		   

		   		   

--select top 100* from order_proc where order_status_c = 130





/*
SELECT DISTINCT C.PAT_ID
FROM #TOTAL_COHORT AS C
     INNER JOIN HSP_ACCOUNT AS HSP ON C.PAT_ID = HSP.PAT_ID 
	 --INNER JOIN HSP_ACCT_DX_LIST AS HSPDX ON HSP.HSP_ACCOUNT_ID = HSPDX.HSP_ACCOUNT_ID
	 --INNER JOIN CLARITY_EDG AS EDG ON EDG.DX_ID = HSPDX.DX_ID 
	 INNER JOIN HSP_ACCT_CPT_CODES AS CPT ON CPT.HSP_ACCOUNT_ID = HSP.HSP_ACCOUNT_ID
WHERE 
--HSP.ACCT_BASECLS_HA_C = 2 AND
       CPT.CPT_CODE IN ('70450', '70460', '70470',
	                       '70480', '70481', '70482', '70486', '70487', '70488',
						   '70490','70491', '70492', '71250', '71260', '71270', '72125', '72126','72127','72128','72129','72130','72131','72132','72133',
						   '72192', '72193', '72194', '73200', '73201', '73202',
						   '73700', '73701', '73702', '74150', '74160', '74170', '74176', '74177', '74178', '76497' )

SELECT DISTINCT C.PAT_ID
       ,OrTYPE.NAME AS ORDER_TYPE_NAME
	   ,ORDERPRO.DESCRIPTION
FROM #TOTAL_COHORT AS C
     INNER JOIN ORDERS     AS ORD            ON ORD.PAT_ID = C.PAT_ID
     INNER JOIN ORDER_PROC AS ORDERPRO       ON ORDERPRO.ORDER_PROC_ID = ORD.ORDER_ID
	 INNER JOIN ZC_DFLT_ORDER_TYPE AS OrTYPE ON OrType.DFLT_ORDER_TYPE_C = ORDERPRO.ORDER_TYPE_C
WHERE       
      CAST(ORDERPRO.ORDER_TIME AS DATE) BETWEEN '10/01/2014' AND '09/30/2015'
	  AND ORDERPRO.CPT_CODE IN ('72175', '71250', '71260') 


*/


/*[‎9/‎26/‎2016 5:00 PM] Mudiganti, Satish: 
select year(ordering_Date),count(distinct order_proc_id) as orders from order_proc
where cPT_code in ()
group by year(ordering_Date) 
select cpt_code,year(ordering_Date),count(distinct order_proc_id) as orders from order_proc
where cPT_code in ()
group by cpt_code,year(ordering_Date) order by cpt_code,year(ordering_Date) 
*/


/*AND ORPROC.ORDER_TYPE_C IN ('1', '5', '101', '107', '111', '113', '115', '122', '106','108', '109', '110', '112', '114')
	      AND (ORPROC.DESCRIPTION LIKE 'CT %'
	       OR ORPROC.DESCRIPTION NOT LIKE 'CTA%'  
	       OR ORPROC.DESCRIPTION LIKE 'PET%'
	       OR ORPROC.DESCRIPTION LIKE '%Bone SCAN%'
	       OR ORPROC.DESCRIPTION NOT LIKE 'CT colonograqphy diagnostic%'
	       OR ORPROC.DESCRIPTION NOT LIKE 'CT colonograqphy screening%'
	       OR ORPROC.DESCRIPTION NOT LIKE 'CT angiography chest%')
*/
	    
/*
----###############Stage 3 Axillary LN involvement ############
 --select * from #Total_Cohort

SELECT DISTINCT CO.PAT_ID
FROM #TOTAL_COHORT AS CO
     INNER JOIN PAT_ENC_DX AS DX ON DX.PAT_ID = CO.PAT_ID
	 INNER JOIN CLARITY_EDG AS EDG ON EDG.DX_ID = EDG.DX_ID
WHERE EDG.REF_BILL_CODE = '196.3' 
      AND DX.CONTACT_DATE between '09/01/2013' and '09/30/2015'   --based on 2356pt, 2340 
*/


/*
----pulling provider information

IF OBJECT_ID('TEMPDB..#PROVIDER_LIST') IS NOT NULL DROP TABLE #PROVIDER_LIST;

DECLARE @START AS DATE
DECLARE @END AS DATE
SET @START='09/01/2013'     
SET @END='9/30/2015'   


SELECT P.PAT_ID
	--   ,CAST(E.CONTACT_DATE AS DATE) AS CONTACT_DATE
	   ,E.PAT_ENC_CSN_ID 
	   ,E.VISIT_PROV_ID
	--   ,E.VISIT_PROV_TITLE
	   ,SER.PROV_NAME
	   ,ZSPEC.NAME AS SPECIALTY_NAME
	 --  ,ZSPEC.TITLE AS ZC_SPECIALTY_TITLE
INTO #PROVIDER_LIST

FROM 
		#STAGE0TO3_PAT_LIST_PROV                              AS P 
		INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PAT_ENC AS E              ON P.PAT_ID=E.PAT_ID
		LEFT JOIN CLARITY_SER                                 AS SER            ON E.VISIT_PROV_ID =  SER.PROV_ID
		INNER JOIN CLARITY_SER_SPEC                           AS SPEC           ON SER.PROV_ID = SPEC.PROV_ID
		INNER JOIN ZC_SPECIALTY                               AS ZSPEC          ON ZSPEC.SPECIALTY_C = SPEC.SPECIALTY_C
		LEFT JOIN #ERR_ENC_RSN                                AS A1             ON A1.PAT_ENC_CSN_ID = E.PAT_ENC_CSN_ID
		LEFT JOIN #ERR_CSN_DX                                 AS A2             ON A2.PAT_ENC_CSN_ID = E.PAT_ENC_CSN_ID
		LEFT JOIN CLARITY_LOC                                 AS LOC            ON LOC.LOC_ID = E.SERV_AREA_ID
		                                                                                                    
WHERE 	CAST(E.CONTACT_DATE AS DATE) BETWEEN @START AND @END                                   
		AND E.SERV_AREA_ID IN (7,22,26,27,28, 11000, 12000, 13000, 14000, 15000)  
		AND E.ENC_TYPE_C NOT IN ('201','249', '5')                                
		AND (E.APPT_STATUS_C IN (2,6) OR E.APPT_STATUS_C IS NULL)                 
		AND E.ENC_CLOSED_YN='Y'
		AND E.ENC_TYPE_C = '101'                                                   
		AND A1.PAT_ENC_CSN_ID IS NULL 
		AND A2.PAT_ENC_CSN_ID IS NULL
		AND SER.ACTIVE_STATUS='Active'
		AND SER.CLINICIAN_TITLE = 'MD'
		AND SER.PROV_NAME NOT LIKE '%DO NOT USE%' 
		AND (ZSPEC.NAME LIKE '%General Surgery%'
		OR ZSPEC.NAME LIKE'%Hematology/Oncology%'
        OR ZSPEC.NAME LIKE '%Medical Oncology%'
		OR ZSPEC.NAME LIKE '%Nuclear Medicine%'
		OR ZSPEC.NAME LIKE '%Oncology%'
		OR ZSPEC.NAME LIKE '%Surgery Center%'
		OR ZSPEC.NAME LIKE '%Surgical Oncology%')


--select count(distinct pat_id) from #PROVIDER_LIST  --9/20 5537  9/21 
--select count(distinct visit_prov_id) from #PROVIDER_LIST  --9/20 157  9/21
--select count(distinct PAT_ENC_CSN_ID ) from #PROVIDER_LIST  --9/21 36,779
--select distinct visit_prov_id from #PROVIDER_LIST
--select * from #PROVIDER_LIST    --9/20 56,698  9/21 56698
--select distinct specialty_name from #PROVIDER_LIST 




--********************************************************
--IF OBJECT_ID('TEMPDB..#tpt') IS NOT NULL DROP TABLE #tpt;
select pat_id, max(count_encounter) as max_count
--into #tpt
from ( 
      select pat_id, visit_prov_id,specialty_name, count(distinct pat_enc_csn_id) as count_encounter 
      from #provider_list 
      group by pat_id, visit_prov_id, specialty_name
     ) as outpt
group by pat_id order by pat_id


--IF OBJECT_ID('TEMPDB..#tptM') IS NOT NULL DROP TABLE #tptM;
select pat_id, visit_prov_id,specialty_name, count(distinct pat_enc_csn_id) as count_encounter 
--into #tptM
from #provider_list 
group by pat_id, visit_prov_id, specialty_name




select pat_id, visit_prov_id, specialty_name, max(count_encounter) over (partition by pat_id, visit_prov_id, specialty_name) as max_count 
from ( 
      select pat_id, visit_prov_id,specialty_name, count(distinct pat_enc_csn_id) as count_encounter 
      from #provider_list 
      group by pat_id, visit_prov_id, specialty_name
     ) as outpt
--group by pat_id, visit_prov_id, specialty_name

select distinct specialty_name from #provider_list



----inpatient providers

IF OBJECT_ID('TEMPDB..#PROVIDER_LIST2') IS NOT NULL DROP TABLE #PROVIDER_LIST2;

DECLARE @START2 AS DATE
DECLARE @END2 AS DATE
SET @START2='09/01/2013'     
SET @END2='9/30/2015'	   

SELECT P.PAT_ID
	--   ,CAST(E.ADM_DATE_TIME AS DATE) AS CONTACT_DATE
	   ,E.hsp_account_ID 
	   ,E.attending_PROV_ID
	   ,SER.PROV_NAME
	   ,ZSPEC.NAME AS SPECIALTY_NAME
	 --  ,ZSPEC.TITLE AS ZC_SPECIALTY_TITLE
INTO #PROVIDER_LIST2

FROM 
		#STAGE0TO3_PAT_LIST_PROV                              AS P 
		INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.hsp_account AS E              ON P.PAT_ID=E.PAT_ID
		LEFT JOIN CLARITY_SER                                 AS SER            ON E.ATTENDING_PROV_ID =  SER.PROV_ID
		INNER JOIN CLARITY_SER_SPEC                           AS SPEC           ON SER.PROV_ID = SPEC.PROV_ID
		INNER JOIN ZC_SPECIALTY                               AS ZSPEC          ON ZSPEC.SPECIALTY_C = SPEC.SPECIALTY_C
		LEFT JOIN CLARITY_LOC                                 AS LOC            ON LOC.LOC_ID = E.SERV_AREA_ID
		                                                                                                    
WHERE 	CAST(E.ADM_DATE_TIME AS DATE) BETWEEN @START2 AND @END2                                   
		AND SER.ACTIVE_STATUS='Active'
		AND SER.CLINICIAN_TITLE = 'MD'
		AND E.ACCT_BASECLS_HA_C != 3
		AND SER.PROV_NAME NOT LIKE '%DO NOT USE%' 
		AND (ZSPEC.NAME LIKE '%General Surgery%'
		OR ZSPEC.NAME LIKE'%Hematology/Oncology%'
        OR ZSPEC.NAME LIKE '%Medical Oncology%'
		OR ZSPEC.NAME LIKE '%Nuclear Medicine%'
		OR ZSPEC.NAME LIKE '%Oncology%'
		OR ZSPEC.NAME LIKE '%Surgery Center%'
		OR ZSPEC.NAME LIKE '%Surgical Oncology%')


--select count(distinct pat_id) from #PROVIDER_LIST2  -- 9/20 6134

--select count(distinct attending_prov_id) from #PROVIDER_LIST2  --345, 344
--select count(distinct hsp_account_ID ) from #PROVIDER_LIST2  --9/21 32,356, 32,348
--select * from #PROVIDER_LIST2    --9/20 45,746, 45736 (after eliminate ER)
--select distinct specialty_name from #PROVIDER_LIST2
*/


/*
--TEST CODES FOR PULLING OUT IMAGING ORDER INFORMATION
--&&&&&&try 1  use procedure table only to pull out cpt codes
IF OBJECT_ID('TEMPDB..#COHORT_SCENARIO1') IS NOT NULL DROP TABLE #COHORT_SCENARIO1;

SELECT CO1.PAT_ID 
       ,CO1.FIRST_SURGERY_DATE 
       ,CAST(ORPROC.ORDERING_DATE AS DATE) AS ORDER_DATE 
       ,ORPROC.ORDER_TYPE_C
       ,ORPROC.DESCRIPTION
       ,ORPROC.AUTHRZING_PROV_ID
       ,DEP.DEPARTMENT_NAME  AS IMG_PROV_DEPT
       ,LOC.LOC_NAME         AS IMG_PROV_LOCATION
	   ,SA.SERV_AREA_NAME    AS IMG_PROV_SERV_AREA
       ,ORPROC.CPT_CODE
INTO #COHORT_SCENARIO1
FROM #TOTAL_COHORT              AS CO1 
     LEFT JOIN ORDER_PROC       AS ORPROC    ON CO1.PAT_ID = ORPROC.PAT_ID
     LEFT JOIN CLARITY_SER      AS SER       ON SER.PROV_ID = ORPROC.AUTHRZING_PROV_ID
	 left JOIN CLARITY_SER_DEPT AS SDEPT	 ON SDEPT.PROV_ID = SER.PROV_ID 
	 left JOIN CLARITY_DEP      AS DEP	     ON DEP.DEPARTMENT_ID = SDEPT.DEPARTMENT_ID   
	 left JOIN CLARITY_SA       AS SA        ON SA.SERV_AREA_ID = DEP.SERV_AREA_ID
	 left JOIN CLARITY_LOC      AS LOC       ON LOC.LOC_ID = SA.SERV_AREA_ID
WHERE 
      CO1.FIRST_SURGERY_DATE BETWEEN '09/01/2014' AND '09/30/2015'
      --AND CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(mm, 13,  CO1.FIRST_SURGERY_DATE) AND DATEADD(mm, 24,  CO1.FIRST_SURGERY_DATE)
      AND CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(dy, -60,  CO1.FIRST_SURGERY_DATE) AND DATEADD(dy, 30,  CO1.FIRST_SURGERY_DATE)
	  --AND ORPROC.ORDER_STATUS_C NOT IN (4, 9, 7)
	  AND (ORPROC.CPT_CODE IN ('78300','78305','78306','78315','78399')
	       OR ORPROC.CPT_CODE IN ('78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254')
	       OR ORPROC.CPT_CODE IN ('70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	                              '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	                              '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	                              '74176','74177','74178','76497'))



[‎9/‎26/‎2016 5:00 PM] Mudiganti, Satish:   test codes:

select year(ORDERing_date),count(distinct order_proc_id) as num_of_orders, count(distinct pat_id) as count_pt from order_proc
where    CAST(ORDERing_date AS DATE) between '01/01/2012' and '01/01/2017'
        AND 
        (CPT_CODE IN ('78300','78305','78306','78315','78399')
	    OR CPT_CODE IN ('78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254')
	    OR CPT_CODE IN ('70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	                              '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	                              '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	                              '74176','74177','74178','76497'))
	  and pat_id in (select pat_id from #TOTAL_COHORT)
group by year(ORDERing_date) 



select cpt_code, year(ORDER_TIME),count(distinct order_proc_id) as num_of_orders, count(distinct pat_id) as num_pt from order_proc
where   CAST(ORDER_TIME AS DATE) between '01/01/2012' and '01/01/2017'
        AND
        cPT_code in ('78300','78305','78306','78315','78399','78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254',
	               '70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	               '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	               '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	               '74176','74177','74178','76497')
	   and pat_id in (select pat_id from #TOTAL_COHORT)
group by cpt_code,year(order_time) order by cpt_code, year(order_time) 



--&&&&&&try 3
IF OBJECT_ID('TEMPDB..#COHORT_SCENARIO1') IS NOT NULL DROP TABLE #COHORT_SCENARIO1;

SELECT CO1.PAT_ID 
       ,CO1.FIRST_SURGERY_DATE 
       ,eap.Proc_code
       ,enc.visit_prov_id
       ,DEP.DEPARTMENT_NAME  AS IMG_PROV_DEPT
       ,LOC.LOC_NAME         AS IMG_PROV_LOCATION
	   ,SA.SERV_AREA_NAME    AS IMG_PROV_SERV_AREA
INTO #COHORT_SCENARIO1
FROM #TOTAL_COHORT              AS CO1 
     LEFT JOIN PAT_ENC AS ENC ON ENC.PAT_ID = CO1.PAT_ID
     LEFT JOIN CLARITY_EAP AS EAP ON EAP.PROC_ID = ENC.LOS_PRIME_PROC_ID
     LEFT JOIN CLARITY_SER      AS SER       ON SER.PROV_ID = enc.visit_prov_id
	 left JOIN CLARITY_SER_DEPT AS SDEPT	 ON SDEPT.PROV_ID = SER.PROV_ID 
	 left JOIN CLARITY_DEP      AS DEP	     ON DEP.DEPARTMENT_ID = SDEPT.DEPARTMENT_ID   
	 left JOIN CLARITY_SA       AS SA        ON SA.SERV_AREA_ID = DEP.SERV_AREA_ID
	 left JOIN CLARITY_LOC      AS LOC       ON LOC.LOC_ID = SA.SERV_AREA_ID
WHERE 
      CO1.FIRST_SURGERY_DATE BETWEEN '09/01/2014' AND '09/30/2015'
      AND CAST(ENC.CONTACT_DATE AS DATE) BETWEEN DATEADD(dy, -60,  CO1.FIRST_SURGERY_DATE) AND DATEADD(dy, 30,  CO1.FIRST_SURGERY_DATE)
      --AND CAST(ORPROC.ORDERING_DATE AS DATE) BETWEEN DATEADD(dy, -60,  CO1.FIRST_SURGERY_DATE) AND DATEADD(dy, 30,  CO1.FIRST_SURGERY_DATE)
	  AND(eap.proc_code IN ('78300','78305','78306','78315','78399')
	       OR eap.proc_code IN ('78811','78812','78813','78814','78815','78816','78890','78891','78999','G0235','G0253','G0254')
	       OR eap.proc_code IN ('70450','70460','70470','70480','70481','70482','70486','70487','70488','70490','70491','70492',
	                              '71250','71260','71270','72125','72126','72127','72128','72129','72130','72131','72132','72133',
	                              '72192','72193','72194','73200','73201','73202','73700','73701','73702','74150','74160','74170',
	                              '74176','74177','74178','76497'))

--&&&&&try 4

IF OBJECT_ID('TEMPDB..#COHORT_SCENARIO1') IS NOT NULL DROP TABLE #COHORT_SCENARIO1;

SELECT CO1.PAT_ID 
       ,CO1.FIRST_SURGERY_DATE 
       ,enc.visit_prov_id
       , edg.REF_BILL_CODE
       ,DEP.DEPARTMENT_NAME  AS IMG_PROV_DEPT
       ,LOC.LOC_NAME         AS IMG_PROV_LOCATION
	   ,SA.SERV_AREA_NAME    AS IMG_PROV_SERV_AREA
INTO #COHORT_SCENARIO1
FROM #TOTAL_COHORT              AS CO1 
     LEFT JOIN PAT_ENC AS ENC ON ENC.PAT_ID = CO1.PAT_ID
     LEFT JOIN PAT_ENC_DX AS DX ON DX.PAT_ENC_CSN_ID = ENC.PAT_ENC_CSN_ID
     LEFT JOIN CLARITY_EDG AS EDG ON EDG.DX_ID = DX.DX_ID
     LEFT JOIN CLARITY_SER      AS SER       ON SER.PROV_ID = enc.visit_prov_id
	 left JOIN CLARITY_SER_DEPT AS SDEPT	 ON SDEPT.PROV_ID = SER.PROV_ID 
	 left JOIN CLARITY_DEP      AS DEP	     ON DEP.DEPARTMENT_ID = SDEPT.DEPARTMENT_ID   
	 left JOIN CLARITY_SA       AS SA        ON SA.SERV_AREA_ID = DEP.SERV_AREA_ID
	 left JOIN CLARITY_LOC      AS LOC       ON LOC.LOC_ID = SA.SERV_AREA_ID
WHERE 
      CO1.FIRST_SURGERY_DATE BETWEEN '09/01/2014' AND '09/30/2015'
      AND CAST(ENC.CONTACT_DATE AS DATE) BETWEEN DATEADD(dy, -60,  CO1.FIRST_SURGERY_DATE) AND DATEADD(dy, 30,  CO1.FIRST_SURGERY_DATE)
      AND EDG.REF_BILL_CODE IN ('87.03','87.41','87.71','88.01','88.38','92.11','92.12','92.18','92.19','92.14','88.91')



--&&&&try 5
IF OBJECT_ID('TEMPDB..#COHORT_SCENARIO1') IS NOT NULL DROP TABLE #COHORT_SCENARIO1;
SELECT  CO1.PAT_ID 
       ,CO1.FIRST_SURGERY_DATE 

INTO #COHORT_SCENARIO1
FROM 
	#TOTAL_COHORT              AS CO1   
	LEFT JOIN HSP_ACCOUNT      AS H              ON CO1.PAT_ID=H.PAT_ID
	LEFT JOIN HSP_ACCT_PX_LIST AS PX             ON PX.HSP_ACCOUNT_ID = H.HSP_ACCOUNT_ID
	LEFT JOIN CL_ICD_PX        AS ICDPX          ON ICDPX.ICD_PX_ID = PX.FINAL_ICD_PX_ID
	LEFT JOIN HSP_ACCT_CPT_CODES AS CPT          ON CPT.HSP_ACCOUNT_ID = H.HSP_ACCOUNT_ID
WHERE 	CO1.FIRST_SURGERY_DATE BETWEEN '09/01/2014' AND '09/30/2015'
       AND CAST(PX.PROC_DATE AS DATE) BETWEEN DATEADD(dy, -60,  CO1.FIRST_SURGERY_DATE) AND DATEADD(dy, 30,  CO1.FIRST_SURGERY_DATE) 	  
	    --AND H.ACCT_BASECLS_HA_C =3
		AND ICDPX.REF_BILL_CODE IN ('87.03','87.41','87.71','88.01','88.38','92.11','92.12','92.18','92.19','92.14','88.91')
*/


