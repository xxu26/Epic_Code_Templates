/********************************************************************************
Program name:	UTIL_TYPOL_DATA_EXTRACT.sql
Purpose:		Extract ED visits and inpatient admissions at ABSMC in 2014.
Project name:	GIS East Bay
Investigators:	Alice Pressman
Author:			Anjali Franco
Date created:	02-19-15
Input data:		DCPWDBS149.Clarity_Rpt_Crystal.dbo.HSP_ACCOUNT
				DCPWDBS149.Clarity_Rpt_Crystal.dbo.CLARITY_LOC	
Output data:	DCQWDBS088.GIS.dbo.UTIL_TYPOL_ED_VISITS_2014
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_ED_VISITS_2015Q1Q2
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_ED_VISITS_2015Q1Q2Q3
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_ED_VISITS_2015
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_IP_ADMITS_2014
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_IP_ADMITS_2015Q1Q2
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_IP_ADMITS_2015Q1Q2Q3
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_IP_ADMITS_2015
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_ASHBY_PATIENTS
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_SUMMIT_PATIENTS
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_ASHBY_PATIENTS_NAMES
				DCQWDBS088.GIS.dbo.UTIL_TYPOL_SUMMIT_PATIENTS_NAMES
Modifications:
Date		Description
--------	-----------
03-24-15	Extract patient information and demographics.  DID NOT RERUN CODE.
06-29-15	Run typology for Q1 and Q2 2015.
10-27-15	Run typology for Q1-Q3 2015.  Some hospital accounts could
			have been revised.
01-26-16	Run typology for 2015.  Some hospital accounts could
			have been revised.
********************************************************************************/

/*
--MAKE SURE YOU ARE IN THE GIS DATABASE ON THE DCQWDBS088 SERVER---
--EXTRACT ED VISITS--
*/
--drop table UTIL_TYPOL_ED_VISITS_2014
--drop table UTIL_TYPOL_ED_VISITS_2015Q1Q2
--drop table UTIL_TYPOL_ED_VISITS_2015Q1Q2Q3
--INTO UTIL_TYPOL_ED_VISITS_2014
--INTO UTIL_TYPOL_ED_VISITS_2015Q1Q2
--INTO UTIL_TYPOL_ED_VISITS_2015Q1Q2Q3

IF OBJECT_ID(N'GIS.dbo.UTIL_TYPOL_ED_VISITS_2015', N'U') IS NOT NULL
DROP TABLE GIS.dbo.UTIL_TYPOL_ED_VISITS_2015
SELECT DISTINCT A.PAT_ID,
				A.HSP_ACCOUNT_ID,
				A.ADM_DATE_TIME,
				A.DISCH_DATE_TIME,
				B.LOC_NAME,
				C.NAME AS ADMISSION_SOURCE_C

INTO UTIL_TYPOL_ED_VISITS_2015
FROM DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCOUNT AS A
     INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_LOC AS B ON A.LOC_ID = B.LOC_ID
     LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.ZC_ADMISSION_SRC AS C ON A.ADMISSION_SOURCE_C = C.ADMISSION_SRC_C
WHERE YEAR(A.ADM_DATE_TIME) = 2015
		--AND MONTH(A.ADM_DATE_TIME) IN (1, 2, 3, 4, 5, 6)   --by different quarters
		AND A.ACCT_BASECLS_HA_C = '3' --emergency admission
		AND B.LOC_NAME IN ('ALTA BATES SUMMIT - ALTA BATES', 'ALTA BATES SUMMIT - MERRITT')
		
--select * from UTIL_TYPOL_ED_VISITS_2014
--select * from UTIL_TYPOL_ED_VISITS_2015Q1Q2
--select * from UTIL_TYPOL_ED_VISITS_2015Q1Q2Q3
select * from UTIL_TYPOL_ED_VISITS_2015


--*******************************************************************************************************************
--EXTRACT INPATIENT ADMISSIONS--

--drop table UTIL_TYPOL_IP_ADMITS_2014
--drop table UTIL_TYPOL_IP_ADMITS_2015Q1Q2
--drop table UTIL_TYPOL_IP_ADMITS_2015Q1Q2Q3
--INTO UTIL_TYPOL_IP_ADMITS_2014
--INTO UTIL_TYPOL_IP_ADMITS_2015Q1Q2
--INTO UTIL_TYPOL_IP_ADMITS_2015Q1Q2Q3

IF OBJECT_ID(N'GIS.dbo.UTIL_TYPOL_IP_ADMITS_2015', N'U') IS NOT NULL
DROP TABLE GIS.dbo.UTIL_TYPOL_IP_ADMITS_2015

SELECT DISTINCT A.PAT_ID,
				A.HSP_ACCOUNT_ID,
				A.ADM_DATE_TIME,
				A.DISCH_DATE_TIME,
				B.LOC_NAME
INTO UTIL_TYPOL_IP_ADMITS_2015
FROM DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.HSP_ACCOUNT AS A
     INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.CLARITY_LOC AS B ON A.LOC_ID = B.LOC_ID
WHERE YEAR(A.ADM_DATE_TIME) = 2015 		--AND MONTH(A.ADM_DATE_TIME) IN (1, 2, 3, 4, 5, 6, 7, 8, 9)
		AND A.ACCT_BASECLS_HA_C = '1'  --hospital admission inpatients
		AND B.LOC_NAME IN ('ALTA BATES SUMMIT - ALTA BATES', 'ALTA BATES SUMMIT - MERRITT')
			
--select * from UTIL_TYPOL_IP_ADMITS_2014
--select * from UTIL_TYPOL_IP_ADMITS_2015Q1Q2
--select * from UTIL_TYPOL_IP_ADMITS_2015Q1Q2Q3
select * from UTIL_TYPOL_IP_ADMITS_2015



--***********************************************************************************************************
--ABSMC ASHBY CAMPUS TYPOLOGY
/*
select cnt_ed_ip_encs_grouped,
		COUNT(*) as cnt_patients_ashby
from GIS.DBO.UTIL_TYPOL_ASHBY_PATIENTS
group by cnt_ed_ip_encs_grouped
order by cnt_ed_ip_encs_grouped
(*/

IF OBJECT_ID(N'GIS.DBO.UTIL_TYPOL_ASHBY_PATIENTS', N'U') IS NOT NULL
DROP TABLE GIS.DBO.UTIL_TYPOL_ASHBY_PATIENTS

	SELECT PAT_ID, 
	   CASE WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 1'   AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 0')   THEN ' 1: 0'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 2-3' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 0')   THEN ' 2-3: 0'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 4-5' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 0')   THEN ' 4-5: 0'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 6-7' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 0')   THEN ' 6-7: 0'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 8-9' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 0')   THEN ' 8-9: 0'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = '10+'  AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 0')   THEN '10+: 0'	
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 0'   AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 1') THEN ' 0: 1'	
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 1'   AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 1') THEN ' 1: 1'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 2-3' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 1') THEN ' 2-3: 1'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 4-5' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 1') THEN ' 4-5: 1'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 6-7' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 1') THEN ' 6-7: 1'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 8-9' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 1') THEN ' 8-9: 1'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = '10+'  AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 1') THEN '10+: 1'	
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 0'   AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 2') THEN ' 0: 2'	
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 1'   AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 2') THEN ' 1: 2'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 2-3' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 2') THEN ' 2-3: 2'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 4-5' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 2') THEN ' 4-5: 2'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 6-7' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 2') THEN ' 6-7: 2'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 8-9' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 2') THEN ' 8-9: 2'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = '10+'  AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 2') THEN '10+: 2'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 0'   AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 3-4') THEN ' 0: 3-4'	
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 1'   AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 3-4') THEN ' 1: 3-4'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 2-3' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 3-4') THEN ' 2-3: 3-4'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 4-5' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 3-4') THEN ' 4-5: 3-4'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 6-7' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 3-4') THEN ' 6-7: 3-4'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 8-9' AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 3-4') THEN ' 8-9: 3-4'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = '10+'  AND CNT_IP_ADMITS_ASHBY_GROUPED = ' 3-4') THEN '10+: 3-4'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 0'   AND CNT_IP_ADMITS_ASHBY_GROUPED = '5+') THEN ' 0:5+'	
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 1'   AND CNT_IP_ADMITS_ASHBY_GROUPED = '5+') THEN ' 1:5+'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 2-3' AND CNT_IP_ADMITS_ASHBY_GROUPED = '5+') THEN ' 2-3:5+'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 4-5' AND CNT_IP_ADMITS_ASHBY_GROUPED = '5+') THEN ' 4-5:5+'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 6-7' AND CNT_IP_ADMITS_ASHBY_GROUPED = '5+') THEN ' 6-7:5+'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = ' 8-9' AND CNT_IP_ADMITS_ASHBY_GROUPED = '5+') THEN ' 8-9:5+'
			WHEN (CNT_ED_VISITS_ASHBY_GROUPED = '10+'  AND CNT_IP_ADMITS_ASHBY_GROUPED = '5+') THEN '10+:5+'				
		END AS CNT_ED_IP_ENCS_GROUPED
	INTO UTIL_TYPOL_ASHBY_PATIENTS
	FROM (
		SELECT PAT_ID, 
		   CASE WHEN CNT_ED_VISITS_ASHBY = 0 THEN ' 0'
				WHEN CNT_ED_VISITS_ASHBY IS NULL THEN ' 0'
				WHEN CNT_ED_VISITS_ASHBY = 1 THEN ' 1'
				WHEN CNT_ED_VISITS_ASHBY BETWEEN 2 AND 3 THEN ' 2-3'
				WHEN CNT_ED_VISITS_ASHBY BETWEEN 4 AND 5 THEN ' 4-5'
				WHEN CNT_ED_VISITS_ASHBY BETWEEN 6 AND 7 THEN ' 6-7'
				WHEN CNT_ED_VISITS_ASHBY BETWEEN 8 AND 9 THEN ' 8-9'
				WHEN CNT_ED_VISITS_ASHBY >= 10 THEN '10+'
			END AS CNT_ED_VISITS_ASHBY_GROUPED,
			CASE WHEN CNT_IP_ADMITS_ASHBY = 0 THEN ' 0'
				WHEN CNT_IP_ADMITS_ASHBY IS NULL THEN ' 0'
				WHEN CNT_IP_ADMITS_ASHBY = 1 THEN ' 1'
				WHEN CNT_IP_ADMITS_ASHBY = 2 THEN ' 2'
				WHEN CNT_IP_ADMITS_ASHBY BETWEEN 3 AND 4 THEN ' 3-4'
				WHEN CNT_IP_ADMITS_ASHBY >= 5 THEN '5+'
			END AS CNT_IP_ADMITS_ASHBY_GROUPED
		FROM (
			SELECT CASE WHEN B.PAT_ID IS NULL THEN C.PAT_ID
						ELSE B.PAT_ID
					END AS PAT_ID,
					B.CNT_ED_VISITS_ASHBY,
					C.CNT_IP_ADMITS_ASHBY
			FROM (
					SELECT PAT_ID,
							COUNT(*) AS CNT_ED_VISITS_ASHBY
					FROM (
						SELECT * 
						--FROM  UTIL_TYPOL_ED_VISITS_2014
						--FROM UTIL_TYPOL_ED_VISITS_2015Q1Q2
						--FROM UTIL_TYPOL_ED_VISITS_2015Q1Q2Q3
						FROM UTIL_TYPOL_ED_VISITS_2015
						WHERE LOC_NAME = 'ALTA BATES SUMMIT - ALTA BATES'
					) AS A
					GROUP BY A.PAT_ID
			) AS B
			FULL JOIN 
			(
					SELECT PAT_ID,
							COUNT(*) AS CNT_IP_ADMITS_ASHBY
					FROM (
						SELECT * 
						--FROM  UTIL_TYPOL_IP_ADMITS_2014
						--FROM UTIL_TYPOL_IP_ADMITS_2015Q1Q2
						--FROM UTIL_TYPOL_IP_ADMITS_2015Q1Q2Q3
						FROM UTIL_TYPOL_IP_ADMITS_2015
						WHERE LOC_NAME = 'ALTA BATES SUMMIT - ALTA BATES'
					) AS A
					GROUP BY A.PAT_ID
			) AS C
			ON B.PAT_ID = C.PAT_ID
		) AS D
	) AS E
/*) as f
group by f.cnt_ed_ip_encs_grouped*/




--**********************************************************************************************************************
--ABSMC SUMMIT CAMPUS TYPOLOGY
/*
select cnt_ed_ip_encs_grouped,
		COUNT(*) as cnt_patients_summit
from UTIL_TYPOL_SUMMIT_PATIENTS
group by cnt_ed_ip_encs_grouped
ORDER BY cnt_ed_ip_encs_grouped
 (*/

IF OBJECT_ID(N'GIS.DBO.UTIL_TYPOL_SUMMIT_PATIENTS', N'U') IS NOT NULL
DROP TABLE GIS.DBO.UTIL_TYPOL_SUMMIT_PATIENTS

	SELECT PAT_ID, 
	   CASE WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 1'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 0') THEN ' 1: 0'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 2-3' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 0') THEN ' 2-3: 0'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 4-5' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 0') THEN ' 4-5: 0'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 6-7' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 0') THEN ' 6-7: 0'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 8-9' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 0') THEN ' 8-9: 0'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = '10+'  AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 0') THEN '10+: 0'	
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 0'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 1') THEN ' 0: 1'	
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 1'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 1') THEN ' 1: 1'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 2-3' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 1') THEN ' 2-3: 1'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 4-5' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 1') THEN ' 4-5: 1'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 6-7' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 1') THEN ' 6-7: 1'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 8-9' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 1') THEN ' 8-9: 1'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = '10+'  AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 1') THEN '10+: 1'	
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 0'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 2') THEN ' 0: 2'	
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 1'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 2') THEN ' 1: 2'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 2-3' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 2') THEN ' 2-3: 2'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 4-5' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 2') THEN ' 4-5: 2'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 6-7' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 2') THEN ' 6-7: 2'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 8-9' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 2') THEN ' 8-9: 2'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = '10+'  AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 2') THEN '10+: 2'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 0'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 3-4') THEN ' 0: 3-4'	
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 1'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 3-4') THEN ' 1: 3-4'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 2-3' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 3-4') THEN ' 2-3: 3-4'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 4-5' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 3-4') THEN ' 4-5: 3-4'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 6-7' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 3-4') THEN ' 6-7: 3-4'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 8-9' AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 3-4') THEN ' 8-9: 3-4'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = '10+'  AND CNT_IP_ADMITS_SUMMIT_GROUPED = ' 3-4') THEN '10+: 3-4'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 0'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = '5+') THEN ' 0:5+'	
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 1'   AND CNT_IP_ADMITS_SUMMIT_GROUPED = '5+') THEN ' 1:5+'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 2-3' AND CNT_IP_ADMITS_SUMMIT_GROUPED = '5+') THEN ' 2-3:5+'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 4-5' AND CNT_IP_ADMITS_SUMMIT_GROUPED = '5+') THEN ' 4-5:5+'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 6-7' AND CNT_IP_ADMITS_SUMMIT_GROUPED = '5+') THEN ' 6-7:5+'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = ' 8-9' AND CNT_IP_ADMITS_SUMMIT_GROUPED = '5+') THEN ' 8-9:5+'
			WHEN (CNT_ED_VISITS_SUMMIT_GROUPED = '10+'  AND CNT_IP_ADMITS_SUMMIT_GROUPED = '5+') THEN '10+:5+'				
		END AS CNT_ED_IP_ENCS_GROUPED
	INTO UTIL_TYPOL_SUMMIT_PATIENTS
	FROM (
		SELECT PAT_ID, 
		   CASE WHEN CNT_ED_VISITS_SUMMIT = 0 THEN ' 0'
				WHEN CNT_ED_VISITS_SUMMIT IS NULL THEN ' 0'
				WHEN CNT_ED_VISITS_SUMMIT = 1 THEN ' 1'
				WHEN CNT_ED_VISITS_SUMMIT BETWEEN 2 AND 3 THEN ' 2-3'
				WHEN CNT_ED_VISITS_SUMMIT BETWEEN 4 AND 5 THEN ' 4-5'
				WHEN CNT_ED_VISITS_SUMMIT BETWEEN 6 AND 7 THEN ' 6-7'
				WHEN CNT_ED_VISITS_SUMMIT BETWEEN 8 AND 9 THEN ' 8-9'
				WHEN CNT_ED_VISITS_SUMMIT >= 10 THEN '10+'
			END AS CNT_ED_VISITS_SUMMIT_GROUPED,
			CASE WHEN CNT_IP_ADMITS_SUMMIT = 0 THEN ' 0'
				WHEN CNT_IP_ADMITS_SUMMIT IS NULL THEN ' 0'
				WHEN CNT_IP_ADMITS_SUMMIT = 1 THEN ' 1'
				WHEN CNT_IP_ADMITS_SUMMIT = 2 THEN ' 2'
				WHEN CNT_IP_ADMITS_SUMMIT BETWEEN 3 AND 4 THEN ' 3-4'
				WHEN CNT_IP_ADMITS_SUMMIT >= 5 THEN '5+'
			END AS CNT_IP_ADMITS_SUMMIT_GROUPED
		FROM (
			SELECT 
			   CASE WHEN B.PAT_ID IS NULL THEN C.PAT_ID
					ELSE B.PAT_ID
					END AS PAT_ID,
				B.CNT_ED_VISITS_SUMMIT,
				C.CNT_IP_ADMITS_SUMMIT
			FROM (
					SELECT PAT_ID,
						   COUNT(*) AS CNT_ED_VISITS_SUMMIT
					FROM (
						SELECT * 
						--FROM  UTIL_TYPOL_ED_VISITS_2014
						--FROM  UTIL_TYPOL_ED_VISITS_2015Q1Q2
						--FROM  UTIL_TYPOL_ED_VISITS_2015Q1Q2Q3
						FROM  UTIL_TYPOL_ED_VISITS_2015
						WHERE LOC_NAME = 'ALTA BATES SUMMIT - MERRITT'
					) AS A
					GROUP BY A.PAT_ID
			) AS B

			FULL JOIN 

			(
					SELECT PAT_ID,
							COUNT(*) AS CNT_IP_ADMITS_SUMMIT
					FROM (
						SELECT * 
						--FROM UTIL_TYPOL_IP_ADMITS_2014
						--FROM UTIL_TYPOL_IP_ADMITS_2015Q1Q2
						--FROM UTIL_TYPOL_IP_ADMITS_2015Q1Q2Q3
						FROM UTIL_TYPOL_IP_ADMITS_2015
						WHERE LOC_NAME = 'ALTA BATES SUMMIT - MERRITT'
					) AS A
					GROUP BY A.PAT_ID
			) AS C
			ON B.PAT_ID = C.PAT_ID
		) AS D
	) AS E
/*) as f
group by f.cnt_ed_ip_encs_grouped*/



































--********************************************************************************************----
--********************************************************************************************----
--PULL NAMES AND DEMOGRAPHICS
IF OBJECT_ID('tempdb..#ashby_names') IS NOT NULL DROP Table #ashby_names;
SELECT A.PAT_ID,
		B.PAT_MRN_ID,
		B.PAT_FIRST_NAME,
		B.PAT_MIDDLE_NAME,
		B.PAT_LAST_NAME,
		B.BIRTH_DATE,
		CASE WHEN B.SEX_C = '1' THEN 'FEMALE'
			WHEN B.SEX_C = '2' THEN 'MALE'
			WHEN B.SEX_C = '3' THEN 'UNKNOWN'
			ELSE 'UNKNOWN'
		END AS GENDER,
		D.NAME AS RACE,
		A.CNT_ED_IP_ENCS_GROUPED,
		ROW_NUMBER() OVER(PARTITION BY A.PAT_ID ORDER BY A.PAT_ID, C.PATIENT_RACE_C) AS RACE_I
INTO #ASHBY_NAMES		
FROM UTIL_TYPOL_ASHBY_PATIENTS AS A
INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT AS B
ON A.PAT_ID = B.PAT_ID
LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT_RACE AS C
ON A.PAT_ID = C.PAT_ID
LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.ZC_PATIENT_RACE AS D
ON C.PATIENT_RACE_C = D.PATIENT_RACE_C
WHERE A.CNT_ED_IP_ENCS_GROUPED IN (' 4-5: 0', ' 6-7: 0', ' 8-9: 0', '10+: 0', '10+:5+')







IF OBJECT_ID('tempdb..#ashby_names') IS NOT NULL DROP Table #ashby_names;
DROP TABLE UTIL_TYPOL_ASHBY_PATIENTS_NAMES
DECLARE @COLS AS NVARCHAR(MAX), @QUERY AS NVARCHAR(MAX)
SELECT @COLS = STUFF((SELECT ',' + QUOTENAME(RACE_I) 
                    FROM #ASHBY_NAMES
                    GROUP BY RACE_I
                    ORDER BY RACE_I 
                    FOR XML PATH(''), TYPE).VALUE('.', 'NVARCHAR(MAX)'),1,1,'')  
SET @QUERY = 'SELECT PAT_ID, PAT_MRN_ID, PAT_FIRST_NAME, PAT_MIDDLE_NAME, PAT_LAST_NAME, BIRTH_DATE, GENDER, CNT_ED_IP_ENCS_GROUPED, ' + @COLS + '
			INTO UTIL_TYPOL_ASHBY_PATIENTS_NAMES
			FROM (
			SELECT *
			FROM #ASHBY_NAMES
            ) X
			PIVOT 
            (
            MAX(RACE)
			FOR RACE_I IN (' + @COLS + ')
            ) P'
EXECUTE(@QUERY);

SELECT * FROM UTIL_TYPOL_ASHBY_PATIENTS_NAMES








IF OBJECT_ID('tempdb..#SUMMIT_NAMES') IS NOT NULL DROP Table #SUMMIT_NAMES;
SELECT A.PAT_ID,
		B.PAT_MRN_ID,
		B.PAT_FIRST_NAME,
		B.PAT_MIDDLE_NAME,
		B.PAT_LAST_NAME,
		B.BIRTH_DATE,
		CASE WHEN B.SEX_C = '1' THEN 'FEMALE'
			WHEN B.SEX_C = '2' THEN 'MALE'
			WHEN B.SEX_C = '3' THEN 'UNKNOWN'
			ELSE 'UNKNOWN'
		END AS GENDER,
		D.NAME AS RACE,
		A.CNT_ED_IP_ENCS_GROUPED,
		ROW_NUMBER() OVER(PARTITION BY A.PAT_ID ORDER BY A.PAT_ID, C.PATIENT_RACE_C) AS RACE_I
INTO #SUMMIT_NAMES	
FROM UTIL_TYPOL_SUMMIT_PATIENTS AS A
     INNER JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT AS B ON A.PAT_ID = B.PAT_ID
     LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.PATIENT_RACE AS C ON A.PAT_ID = C.PAT_ID
     LEFT JOIN DCPWDBS149.CLARITY_RPT_CRYSTAL.DBO.ZC_PATIENT_RACE AS D ON C.PATIENT_RACE_C = D.PATIENT_RACE_C
WHERE A.CNT_ED_IP_ENCS_GROUPED IN (' 4-5: 0', ' 6-7: 0', ' 8-9: 0', '10+: 0', '10+:5+')









IF OBJECT_ID(N'GIS.DBO.UTIL_TYPOL_SUMMIT_PATIENTS_NAMESS', N'U') IS NOT NULL
DROP TABLE UTIL_TYPOL_SUMMIT_PATIENTS_NAMES
DECLARE @COLS AS NVARCHAR(MAX), @QUERY AS NVARCHAR(MAX)
SELECT @COLS = STUFF((SELECT ',' + QUOTENAME(RACE_I) 
                    FROM #SUMMIT_NAMES
                    GROUP BY RACE_I
                    ORDER BY RACE_I 
                    FOR XML PATH(''), TYPE).VALUE('.', 'NVARCHAR(MAX)'),1,1,'')  
SET @QUERY = 'SELECT PAT_ID, PAT_MRN_ID, PAT_FIRST_NAME, PAT_MIDDLE_NAME, PAT_LAST_NAME, BIRTH_DATE, GENDER, CNT_ED_IP_ENCS_GROUPED, ' + @COLS + '
			INTO UTIL_TYPOL_SUMMIT_PATIENTS_NAMES
			FROM (
			SELECT *
			FROM #SUMMIT_NAMES
            ) X
			PIVOT 
            (
            MAX(RACE)
			FOR RACE_I IN (' + @COLS + ')
            ) P'
EXECUTE(@QUERY);
							
SELECT * FROM UTIL_TYPOL_SUMMIT_PATIENTS_NAMES