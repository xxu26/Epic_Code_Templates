
--Epic continuously merge pt id, so this codes deal with that situation.

IF OBJECT_ID('TEMPDB..#BIOBANK_BASE') IS NOT NULL DROP TABLE #BIOBANK_BASE;
SELECT DISTINCT 
       CASE WHEN B.PAT_ID IS NULL THEN A.PAT_ID 
	   ELSE B.PAT_ID 
	   END AS PAT_ID  
	   ,A.PAT_ID AS OLD_PAT_ID
INTO #BIOBANK_BASE
FROM DCPWDBS149.RDD_MRT.DBO.RDD_ENC_CSN A 
     LEFT JOIN DCPWDBS149.Clarity_Rpt_Crystal.dbo.PAT_MERGE_HISTORY B ON A.PAT_ID=B.PATIENT_MRG_HIST
WHERE A.PROJECT_ID='AE_BIOBANK_MODEL' 


--alternatives methods
Method1:
SELECT DISTINCT CASE WHEN B.PAT_ID IS NULL THEN A.PAT_ID ELSE B.PAT_ID END AS PAT_ID ,
A.PAT_ID AS OLD_PAT_ID
FROM DCPWDBS149.RDD_MRT.DBO.RDD_ENC_CSN A LEFT JOIN DCPWDBS149.Clarity_Rpt_Crystal.dbo.PAT_MERGE_HISTORY B ON A.PAT_ID=B.PATIENT_MRG_HIST
            

Method2:
select distinct pat_id, pat_id as old_pat_id into #az_pcp_pats 
from dcpwdbs149.rdd_mrt.dbo.RDD_ENC_CSN
where PROJECT_ID='SM_AZ_PCP'

select distinct a.pat_id as old_pat_id, b.pat_id as new_pat_id 
into #pat_v2
from #az_pcp_pats a, dcpwdbs149.clarity_rpt_crystal.dbo.pat_merge_history b
where a.pat_id=b.patient_mrg_hist

select * into #pat_list_FINAL from 
(select * from #az_pcp_pats where pat_id not in (select old_pat_id from #pat_v2)
union 
select distinct new_pat_id as pat_id, old_pat_id from #pat_v2) as a


----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------


/*
now we have a setup that automatically upload data to a database RDD_mrt on DCPWDBS149 from  a database RDD_to_149 on DCQWDBS088 daily at 6am. 
Only our team (with CLARITY access) has read only permission to the database RDD_mrt. 
Similarly, Ken and Clarity_system has read only access to our database RDD_to_149 .

Append your data to a database RDD_to_149 on our server DCQWDBS088.
Right now there are two tables RDD_ENC_CSN and RDD_HSP_ACCT. 
If we decide to add new tables/ variables, we should work with DBA to update the setup.
You can use the code below to append data to our database(RDD_to_149). 

we can only upload pat_id (plus encounter id and hospital id), nothing else.
*/
insert into RDD_ENC_CSN (pat_id, PAT_ENC_CSN_ID, PROJECT_DATE, PROJECT_ID)
select  pat_id,pat_enc_csn_id, 'upload date', 'your unique project id' as project_id from 'Your_Table_name'

insert into RDD_HSP_ACCT (pat_id, HSP_ACCOUNT_ID, PROJECT_DATE, PROJECT_ID)
select  pat_id,hsp_account_id, 'upload date', 'your unique project id' as project_id from 'Your_Table_name'
