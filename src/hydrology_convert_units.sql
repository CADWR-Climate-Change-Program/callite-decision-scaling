create table sim_obs11stations_pSJRTmonoNoAdap_maf as
select `callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`dt`                                                AS `dt`,
       `callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`dp`                                                AS `dp`,
       `callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`Year`                                              AS `Year`,
       `callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`Month`                                             AS `Month`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`AMF` * 0.001) * 3.28084) * 1192084.247) / 1e6) AS `AMF`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`BLB` * 0.001) * 3.28084) * 476088.972) / 1e6)  AS `BLB`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`BND` * 0.001) * 3.28084) * 6386051.927) / 1e6) AS `BND`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`FTO` * 0.001) * 3.28084) * 2330562.077) / 1e6) AS `FTO`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`MRC` * 0.001) * 3.28084) * 673997.868) / 1e6)  AS `MRC`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`SIS` * 0.001) * 3.28084) * 4780655.140) / 1e6) AS `SIS`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`SJF` * 0.001) * 3.28084) * 1072680.817) / 1e6) AS `SJF`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`SNS` * 0.001) * 3.28084) * 627490.889) / 1e6)  AS `SNS`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`TLG` * 0.001) * 3.28084) * 983437.363) / 1e6)  AS `TLG`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`TNL` * 0.001) * 3.28084) * 459906.679) / 1e6)  AS `TNL`,
       ((((`callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`.`YRS` * 0.001) * 3.28084) * 710451.826) / 1e6)  AS `YRS`
from `callite-ds`.`sim_obs11stations_pSJRTmonoNoAdap`;

create table sim_add9stations_pSJRTmonoNoAdap_taf as
select `callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`dt`                                                              AS `dt`,
       `callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`dp`                                                              AS `dp`,
       `callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`Year`                                                            AS `Year`,
       `callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`Month`                                                           AS `Month`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`BearRiver` * 0.001) * 730771760) * 0.000810714) / 1e3)       AS `BearRiver`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`CacheCreek` * 0.001) * 2455821437) * 0.000810714) /
        1e3)                                                                                             AS `CacheCreek`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`CalaverasRiver` * 0.001) * 927537363) * 0.000810714) /
        1e3)                                                                                             AS `CalaverasRiver`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`ChowchillaRiver` * 0.001) * 685393248) * 0.000810714) /
        1e3)                                                                                             AS `ChowchillaRiver`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`CosumnesRiver` * 0.001) * 1384751552) * 0.000810714) /
        1e3)                                                                                             AS `CosumnesRiver`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`FresnoRiver` * 0.001) * 717246675) * 0.000810714) /
        1e3)                                                                                             AS `FresnoRiver`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`MokelumneRiver` * 0.001) * 1408736800) * 0.000810714) /
        1e3)                                                                                             AS `MokelumneRiver`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`PutahCreek` * 0.001) * 1477609964) * 0.000810714) /
        1e3)                                                                                             AS `PutahCreek`,
       ((((`callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`.`StonyCreek` * 0.001) * 1926664012) * 0.000810714) / 1e3)     AS `StonyCreek`
from `callite-ds`.`sim_add9stations_pSJRTmonoNoAdap`;

