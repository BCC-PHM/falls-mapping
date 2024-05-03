/*
 ---- Falls A&E Admissions 22/23 ----

Defined by ONS as:
ICD10 codes "S00 to T98" in the primary diagnosis and
"W00-W19" in any of the other diagnosis fields

https://fingertips.phe.org.uk/search/falls#page/6/gid/1/pat/159/par/K02000001/ati/15/are/E92000001/iid/22401/age/27/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1
*/

WITH injuries AS (
SELECT DISTINCT
	[NHSNumber],
	[AdmissionDate],
	[AgeOnAdmission]
  FROM 
	[EAT_Reporting_BSOL].[SUS].[VwInpatientEpisodesDiagnosisRelational] AS A
  LEFT JOIN 
	[EAT_Reporting_BSOL].[SUS].[VwInpatientEpisodes] AS B
  ON 
	A.[EpisodeId] = B.[EpisodeId]
  WHERE 
    -- Physical injury codes S00 to T98
    ([DiagnosisCode] LIKE '[S][0-9][0-9]%' OR
	[DiagnosisCode] LIKE '[T][0-9][0-8]%') AND
	-- First reason for admission
	[DiagnosisOrder] = 1 AND
    -- In year 22/23
	[AdmissionDate] >= '2022-04-01' AND [AdmissionDate] < '2023-04-01' 
),

falls AS (
SELECT DISTINCT
	[NHSNumber],
	[AdmissionDate]
  FROM 
	[EAT_Reporting_BSOL].[SUS].[VwInpatientEpisodesDiagnosisRelational] AS A
  LEFT JOIN 
	[EAT_Reporting_BSOL].[SUS].[VwInpatientEpisodes] AS B
  ON 
	A.[EpisodeId] = B.[EpisodeId]
  WHERE 
    -- Fall codes W00 to W19
    [DiagnosisCode] LIKE '[W][0-1][0-9]%' AND
	-- First reason for admission
	[DiagnosisOrder] > 1 AND
    -- In year 22/23
	[AdmissionDate] >= '2022-04-01' AND [AdmissionDate] < '2023-04-01' AND
	-- Only include those aged 65 +
	AgeOnAdmission >= 65
),

injuries_from_falls_65 AS (
	-- get all admissions with "S00 to T98" in the primary diagnosis and
    -- "W00-W19" in any of the other diagnosis fields
	SELECT 
		I.[NHSNumber]
	FROM 
		injuries AS I
	INNER JOIN 
		falls as F
	ON
		I.[NHSNumber] = F.[NHSNumber] AND
		I.[AdmissionDate] = F.[AdmissionDate]
	WHERE
		-- Only include those aged 65 +
		AgeOnAdmission >= 65
),

injuries_from_falls_80 AS (
	-- get all admissions with "S00 to T98" in the primary diagnosis and
    -- "W00-W19" in any of the other diagnosis fields
	SELECT 
		I.[NHSNumber]
	FROM 
		injuries AS I
	INNER JOIN 
		falls as F
	ON
		I.[NHSNumber] = F.[NHSNumber] AND
		I.[AdmissionDate] = F.[AdmissionDate]
	WHERE
		-- Only include those aged 65 +
		AgeOnAdmission >= 80
),

latest_arrivals AS (
	-- Calculate the most recent admission for each NHS Number
	SELECT
		[NHSNumber],
		MAX(ArrivalDateTime) AS LatestArrival
	FROM 
		[EAT_Reporting_BSOL].[SUS].[VwAEPatientGeography]
	WHERE 
		[ArrivalDateTime] >= '2022-04-01' AND [ArrivalDateTime] < '2023-04-01'
	GROUP BY [NHSNumber]
),

patient_LSOAs_all AS (
	-- Get latest ward for each NHS number
	SELECT
		DISTINCT
		L.[NHSNumber],
		W.[LowerLayerSuperOutputArea]
	FROM latest_arrivals AS L
	LEFT JOIN
		[EAT_Reporting_BSOL].[SUS].[VwAEPatientGeography] AS W
	ON 
		L.[NHSNumber] = W.[NHSNumber] AND 
		L.[LatestArrival] = W.[ArrivalDateTime]
	WHERE 
		W.[ArrivalDateTime] >= '2022-04-01' AND W.[ArrivalDateTime] < '2023-04-01'
),

problem_IDs AS (
	-- Get NHS numbers that still have multiple wards
	SELECT
		[NHSNumber],
		COUNT(*) AS N
	FROM patient_LSOAs_all
	GROUP BY [NHSNumber]
	HAVING COUNT(*) > 1
),

patient_LSOAs AS (
	-- Remove problem NHS IDs
	SELECT
		WA.*
	FROM patient_LSOAs_all as WA
	LEFT JOIN problem_IDs AS P
	ON WA.[NHSNumber] = P.[NHSNumber]
	WHERE P.[NHSNumber] IS NULL
)


SELECT 
	GEO.[LowerLayerSuperOutputArea],
	COUNT(*) AS number_of_falls
FROM 
	injuries_from_falls_80 AS IFF
LEFT JOIN patient_LSOAs AS GEO
	ON IFF.[NHSNumber] = GEO.[NHSNumber]
GROUP BY GEO.[LowerLayerSuperOutputArea]
ORDER BY COUNT(*) DESC


