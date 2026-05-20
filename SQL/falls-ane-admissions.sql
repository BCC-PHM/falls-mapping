/*
 ---- Falls A&E Admissions 23/24 ----

Defined by ONS as:
ICD10 codes "S00 to T98" in the primary diagnosis and
"W00-W19" in any of the other diagnosis fields

https://fingertips.phe.org.uk/search/falls#page/6/gid/1/pat/159/par/K02000001/ati/15/are/E92000001/iid/22401/age/27/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1
*/

-- In 20/21 to 25/26
DECLARE @MinDate DATETIME = '2020-04-01';
DECLARE @MaxDate DATETIME = '2026-04-01';

WITH injuries AS (
SELECT DISTINCT
	NHSNumber,
	AdmissionDate,
	AgeOnAdmission,
	LowerlayerSuperOutputArea2021 AS LSOA21,
	ElectoralWardDivision AS WardCode,
	-- Calculate Financial Year
    CASE 
        WHEN MONTH(AdmissionDate) >= 4 
            THEN CONCAT(YEAR(AdmissionDate), '/', RIGHT(YEAR(AdmissionDate) + 1, 2))
        ELSE 
            CONCAT(YEAR(AdmissionDate) - 1, '/', RIGHT(YEAR(AdmissionDate), 2))
    END AS FinancialYear

	FROM 
		EAT_Reporting_BSOL.SUS.VwInpatientEpisodesDiagnosisRelational AS A
	LEFT JOIN 
		EAT_Reporting_BSOL.SUS.VwInpatientEpisodes AS B
		ON A.EpisodeId = B.EpisodeId
	LEFT JOIN
		EAT_Reporting_BSOL.SUS.VwInpatientEpisodesPatientGeography AS D
		ON A.EpisodeId = D.EpisodeId

  WHERE 
    -- Physical injury codes S00 to T98
    (DiagnosisCode LIKE '[S][0-9][0-9]%' OR
	DiagnosisCode LIKE '[T][0-9][0-8]%') AND
	-- First reason for admission
	DiagnosisOrder = 1 AND
    -- In defined date range
	AdmissionDate >= @MinDate AND AdmissionDate < @MaxDate
),

falls AS (
SELECT DISTINCT
	NHSNumber,
	AdmissionDate
  FROM 
	EAT_Reporting_BSOL.SUS.VwInpatientEpisodesDiagnosisRelational AS A
  LEFT JOIN 
	EAT_Reporting_BSOL.SUS.VwInpatientEpisodes AS B
  ON 
	A.EpisodeId = B.EpisodeId
  WHERE 
    -- Fall codes W00 to W19
    DiagnosisCode LIKE '[W][0-1][0-9]%' AND
	-- First reason for admission
	DiagnosisOrder > 1 AND
    -- In defined date range
	AdmissionDate >= @MinDate AND AdmissionDate < @MaxDate
),

injuries_from_falls AS (
	-- get all admissions with "S00 to T98" in the primary diagnosis and
    -- "W00-W19" in any of the other diagnosis fields
	SELECT 
		DISTINCT -- Only count one per day
		I.NHSNumber,
		CASE
			WHEN I.AgeOnAdmission < 65 THEN 'Under 65'
			WHEN I.AgeOnAdmission >= 65 AND I.AgeOnAdmission <= 79 THEN '65-79'
			WHEN I.AgeOnAdmission >= 80 THEN '80+'
			ELSE 'Unexpected age'
		END AS AgeGroup,
		I.FinancialYear,
		I.LSOA21,
		I.WardCode
	FROM 
		injuries AS I
	INNER JOIN 
		falls as F
	ON
		I.NHSNumber = F.NHSNumber AND
		I.AdmissionDate = F.AdmissionDate
)

/*

-- Extract falls counts by year, age group and LSOA21
SELECT 
	FinancialYear, 
	LEFT(FinancialYear, 4) AS FinancialYearSortable,
	LSOA21, 
	AgeGroup, 
	COUNT(*) AS N
FROM injuries_from_falls
WHERE LSOA21 IS NOT NULL
GROUP BY 
	FinancialYear, 
	LSOA21, 
	AgeGroup
ORDER BY 
	FinancialYear, 
	AgeGroup 
	ASC

*/


-- Extract falls counts by year, age group and ward
SELECT 
	FinancialYear, 
	LEFT(FinancialYear, 4) AS FinancialYearSortable,
	WardCode, 
	AgeGroup, 
	COUNT(*) AS N
FROM injuries_from_falls
WHERE LSOA21 IS NOT NULL
GROUP BY 
	FinancialYear, 
	WardCode, 
	AgeGroup
ORDER BY 
	FinancialYear, 
	AgeGroup 
	ASC
