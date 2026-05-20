/*
 ---- Hip Fractures A&E Admissions ----

Defined by ONS as:
 - episode order number equals 1
 - admission method starts with 2, 
 - diagnosis code ICD10 S72.0, S72.1, or S72.2 

*/

-- In 20/21 to 25/26
DECLARE @MinDate DATETIME = '2020-04-01';
DECLARE @MaxDate DATETIME = '2026-04-01';

WITH hip_fractures AS (

SELECT DISTINCT 
	NHSNumber,
	AdmissionDate,
	-- Calculate age group
	CASE
			WHEN AgeOnAdmission < 65 THEN 'Under 65'
			WHEN AgeOnAdmission >= 65 AND AgeOnAdmission <= 84 THEN '65-84'
			WHEN AgeOnAdmission >= 85 THEN '85+'
			ELSE 'Unexpected age'
		END AS AgeGroup,
	LowerlayerSuperOutputArea2021 AS LSOA21,
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
    DiagnosisCode LIKE '[S72][0-2]%' AND
	-- First reason for admission
	DiagnosisOrder = 1 AND
	-- Episode order 1
	OrderInSpell = 1 AND
    -- In defined date range
	AdmissionDate >= @MinDate AND AdmissionDate < @MaxDate
	)

-- Extract falls counts by year, age group and LSOA21
SELECT 
	FinancialYear, 
	LEFT(FinancialYear, 4) AS FinancialYearSortable,
	LSOA21, 
	AgeGroup, 
	COUNT(*) AS N
FROM hip_fractures
WHERE LSOA21 IS NOT NULL
GROUP BY 
	FinancialYear, 
	LSOA21, 
	AgeGroup
ORDER BY 
	FinancialYear, 
	AgeGroup 
	ASC
