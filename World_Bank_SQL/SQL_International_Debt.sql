-- Datos del Banco Mundial sobre la deuda internacional 2024
SELECT *
FROM international_debt_clean
-- Limpiando data solo paises
DELETE FROM international_debt_clean
WHERE country_name IN (
    'Europe & Central Asia (excluding high income)', 
    'Middle East & North Africa (excluding high income)', 
    'South Asia', 
    'Low & middle income', 
    'Low income', 
    'Lower middle income', 
    'Upper middle income', 
    'Middle income', 
    'IDA only', 
    'IDA total'
);
-- ANALISIS
-- 1. Hallando el número de países distintos
SELECT 
    COUNT(DISTINCT "country_name") AS total_distinct_countries
FROM international_debt_clean;
-- 2. averiguando los distintos indicadores de deuda
SELECT DISTINCT indicator_code AS distinct_debt_indicators
FROM international_debt_clean
ORDER BY distinct_debt_indicators
-- 3. totalizando el importe de la deuda de los países
SELECT 
    ROUND(SUM (debt) / 1000000, 2) AS total_debt
FROM international_debt_clean;
-- 4. País con mayor deuda
SELECT TOP 1
    country_name, 
    SUM(debt) AS total_debt
FROM international_debt_clean
GROUP BY country_name
ORDER BY total_debt DESC;
-- 5. Importe medio de la deuda según los indicadores
SELECT TOP 10
    indicator_code AS debt_indicator,
    indicator_name,
    AVG(debt) AS average_debt
FROM international_debt_clean
GROUP BY indicator_code, indicator_name
ORDER BY average_debt DESC;
-- 6. El importe más elevado de los reembolsos del principal
SELECT 
    country_name, 
    indicator_name
FROM international_debt_clean
WHERE debt = (SELECT 
                 MAX(debt)
             FROM international_debt_clean
             WHERE indicator_code = 'DT.AMT.DLXF.CD');
-- 7. El indicador de deuda más común
SELECT TOP 20
    indicator_code, 
    COUNT(indicator_code) AS indicator_count
FROM international_debt_clean
GROUP BY indicator_code
ORDER BY indicator_count DESC, indicator_code DESC;
-- 8. Otras emisiones de deuda viables y conclusión
SELECT TOP 10
    country_name,
    MAX(debt) AS maximum_debt
FROM international_debt_clean
GROUP BY country_name
ORDER BY maximum_debt DESC;
