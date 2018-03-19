CREATE TABLE #Codesets (
codeset_id int NOT NULL,
concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (40228152)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (40228152)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (313217)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (313217)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (314665)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (314665)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 3 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (40241331)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (40241331)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 4 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (43013024)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (43013024)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 5 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (42898160)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (42898160)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 6 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (40483762,4123927,4086294,4137269,4137272,4062333,40481548,4109386,4301458,4301459,2720815,2720814,2720817,2720816,2720812,2720813,2617270,2721445,2721700,2721702,40664432,2721701,2721703,2720811,38003372,2721699,38003368,38003366,38003370,38003369,38003373,38003371,38003367,38003131,38003066,38003036,38003046,38003076,4082084,4140947,38003056,915618,915614,915615,915616,915619,915620,915617,4062044,2514512,4086777)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (40483762,4123927,4086294,4137269,4137272,4062333,40481548,4109386,4301458,4301459,2720815,2720814,2720817,2720816,2720812,2720813,2617270,2721445,2721700,2721702,40664432,2721701,2721703,2720811,38003372,2721699,38003368,38003366,38003370,38003369,38003373,38003371,38003367,38003131,38003066,38003036,38003046,38003076,4082084,4140947,38003056,915618,915614,915615,915616,915619,915620,915617,4062044,2514512,4086777)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 7 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4060089,4195003,44782431,4013355,4165384,2617335,43020459,312773,4020159,44783274,315273,4110937,2001447,2001448,4119522,4145884,2617334,4339971,4121484,4013356,4181749,4304541)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (4060089,4195003,44782431,4013355,4165384,2617335,43020459,312773,4020159,44783274,315273,4110937,2001447,2001448,4119522,4145884,2617334,4339971,4121484,4013356,4181749,4304541)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 8 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (1310149)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (1310149)
                                             and c.invalid_reason is null

                                           ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 9 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (2101660,2101635,2101634,2104836,2103931,2105103,2104837,2104835,2000075,2000076,2000074,2000073,4001859,4134857,4207955,2005902,4162099,2000085,2000084,2000083,4010119,2000070,2000072,2000069,2000071,2000080,2000081,2000079,2000078,45887894,2104839,2104838,2104840,4266062,2105128,2105129,2000082,2005891,2005904,4203771,2005903)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (2101660,2101635,2101634,2104836,2103931,2105103,2104837,2104835,2000075,2000076,2000074,2000073,4001859,4134857,4207955,2005902,4162099,2000085,2000084,2000083,4010119,2000070,2000072,2000069,2000071,2000080,2000081,2000079,2000078,45887894,2104839,2104838,2104840,4266062,2105128,2105129,2000082,2005891,2005904,4203771,2005903)
                                             and c.invalid_reason is null

                                           ) I
                                           LEFT JOIN
                                           (
                                             select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (2104914)and invalid_reason is null
                                             UNION  select c.concept_id
                                             from @vocabulary_database_schema.CONCEPT c
                                             join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                             and ca.ancestor_concept_id in (2104914)
                                             and c.invalid_reason is null

                                           ) E ON I.concept_id = E.concept_id
                                           WHERE E.concept_id is null
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 10 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                            (
                                              select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4126124,4092504,435649,40480136,4181476,44786469,4120120,2101833,4137616,313232,4300099,4297919,4297658,4099603,44782924,44786470,44786471,43533281,4324124,2003564,4300106,4046829,2109584,4021107,4197300,4324754,4002215,4022805,2003626,40664909,2109586,2109589,4163566,37521745,4346636,4346505,4347789,2721092,4322471,4343000)and invalid_reason is null
                                              UNION  select c.concept_id
                                              from @vocabulary_database_schema.CONCEPT c
                                              join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                              and ca.ancestor_concept_id in (4126124,4092504,435649,40480136,4181476,44786469,4120120,2101833,4137616,313232,4300099,4297919,4297658,4099603,44782924,44786470,44786471,43533281,4324124,2003564,4300106,4046829,2109584,4021107,4197300,4324754,4002215,4022805,2003626,40664909,2109586,2109589,4163566,37521745,4346636,4346505,4347789,2721092,4322471,4343000)
                                              and c.invalid_reason is null

                                            ) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 11 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                            (
                                              select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (318775,444247)and invalid_reason is null
                                              UNION  select c.concept_id
                                              from @vocabulary_database_schema.CONCEPT c
                                              join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                              and ca.ancestor_concept_id in (318775,444247)
                                              and c.invalid_reason is null

                                            ) I
                                            LEFT JOIN
                                            (
                                              select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (435887,195562,4179912,318137,199837,438820,4235812,4187790)and invalid_reason is null
                                              UNION  select c.concept_id
                                              from @vocabulary_database_schema.CONCEPT c
                                              join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                              and ca.ancestor_concept_id in (435887,195562,4179912,318137,199837,438820,4235812,4187790)
                                              and c.invalid_reason is null

                                            ) E ON I.concept_id = E.concept_id
                                            WHERE E.concept_id is null
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 12 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
                                            (
                                              select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (40480461,440417,40479606)and invalid_reason is null
                                              UNION  select c.concept_id
                                              from @vocabulary_database_schema.CONCEPT c
                                              join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                              and ca.ancestor_concept_id in (40480461,440417,40479606)
                                              and c.invalid_reason is null

                                            ) I
                                            LEFT JOIN
                                            (
                                              select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (435026)and invalid_reason is null
                                              UNION  select c.concept_id
                                              from @vocabulary_database_schema.CONCEPT c
                                              join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
                                              and ca.ancestor_concept_id in (435026)
                                              and c.invalid_reason is null

                                            ) E ON I.concept_id = E.concept_id
                                            WHERE E.concept_id is null
) C;


with primary_events (event_id, person_id, start_date, end_date, op_start_date, op_end_date, visit_occurrence_id) as
(
  -- Begin Primary Events
  select row_number() over (PARTITION BY P.person_id order by P.start_date) as event_id, P.person_id, P.start_date, P.end_date, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date, cast(P.visit_occurrence_id as bigint) as visit_occurrence_id
  FROM
  (
    select P.person_id, P.start_date, P.end_date, row_number() OVER (PARTITION BY person_id ORDER BY start_date ASC) ordinal, cast(P.visit_occurrence_id as bigint) as visit_occurrence_id
    FROM
    (
      -- Begin Drug Era Criteria
      select C.person_id, C.drug_era_id as event_id, C.drug_era_start_date as start_date, C.drug_era_end_date as end_date, C.drug_concept_id as TARGET_CONCEPT_ID, NULL as visit_occurrence_id
      from
      (
        select de.*, row_number() over (PARTITION BY de.person_id ORDER BY de.drug_era_start_date, de.drug_era_id) as ordinal
        FROM @cdm_database_schema.DRUG_ERA de
        where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 8)
        ) C
        JOIN @cdm_database_schema.PERSON P on C.person_id = P.person_id
        WHERE C.ordinal = 1
        AND C.drug_era_start_date >= DATEFROMPARTS(2010, 10, 19)
        AND YEAR(C.drug_era_start_date) - P.year_of_birth >= 65
        -- End Drug Era Criteria

      ) P
    ) P
    JOIN @cdm_database_schema.observation_period OP on P.person_id = OP.person_id and P.start_date >=  OP.observation_period_start_date and P.start_date <= op.observation_period_end_date
    WHERE DATEADD(day,183,OP.OBSERVATION_PERIOD_START_DATE) <= P.START_DATE AND DATEADD(day,0,P.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE AND P.ordinal = 1
    -- End Primary Events

  )
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, visit_occurrence_id
  INTO #qualified_events
  FROM
  (
    select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal, cast(pe.visit_occurrence_id as bigint) as visit_occurrence_id
    FROM primary_events pe

  ) QE

  ;

  --- Inclusion Rule Inserts

  select 0 as inclusion_rule_id, person_id, event_id
  INTO #Inclusion_0
  FROM
  (
    select pe.person_id, pe.event_id
    FROM #qualified_events pe

    JOIN (
      -- Begin Criteria Group
      select 0 as index_id, person_id, event_id
      FROM
      (
        select E.person_id, E.event_id
        FROM #qualified_events E
        LEFT JOIN
        (
          -- Begin Correlated Criteria
          SELECT 0 as index_id, p.person_id, p.event_id
          FROM #qualified_events P
          LEFT JOIN
          (
            -- Begin Condition Occurrence Criteria
            SELECT C.person_id, C.condition_occurrence_id as event_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID, C.visit_occurrence_id
            FROM
            (
              SELECT co.*, row_number() over (PARTITION BY co.person_id ORDER BY co.condition_start_date, co.condition_occurrence_id) as ordinal
              FROM @cdm_database_schema.CONDITION_OCCURRENCE co
              where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 1)
              ) C


              -- End Condition Occurrence Criteria

            ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
            GROUP BY p.person_id, p.event_id
            HAVING COUNT(A.TARGET_CONCEPT_ID) >= 1
            -- End Correlated Criteria

            UNION ALL
            -- Begin Correlated Criteria
            SELECT 1 as index_id, p.person_id, p.event_id
            FROM #qualified_events P
            LEFT JOIN
            (
              -- Begin Condition Occurrence Criteria
              SELECT C.person_id, C.condition_occurrence_id as event_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID, C.visit_occurrence_id
              FROM
              (
                SELECT co.*, row_number() over (PARTITION BY co.person_id ORDER BY co.condition_start_date, co.condition_occurrence_id) as ordinal
                FROM @cdm_database_schema.CONDITION_OCCURRENCE co
                where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 2)
                ) C


                -- End Condition Occurrence Criteria

              ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
              GROUP BY p.person_id, p.event_id
              HAVING COUNT(A.TARGET_CONCEPT_ID) >= 1
              -- End Correlated Criteria

            ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
            GROUP BY E.person_id, E.event_id
            HAVING COUNT(index_id) > 0
          ) G
          -- End Criteria Group
        ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
      ) Results
      ;

      select 1 as inclusion_rule_id, person_id, event_id
      INTO #Inclusion_1
      FROM
      (
        select pe.person_id, pe.event_id
        FROM #qualified_events pe

        JOIN (
          -- Begin Criteria Group
          select 0 as index_id, person_id, event_id
          FROM
          (
            select E.person_id, E.event_id
            FROM #qualified_events E
            LEFT JOIN
            (
              -- Begin Correlated Criteria
              SELECT 0 as index_id, p.person_id, p.event_id
              FROM #qualified_events P
              LEFT JOIN
              (
                -- Begin Drug Exposure Criteria
                select C.person_id, C.drug_exposure_id as event_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                from
                (
                  select de.*, row_number() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date, de.drug_exposure_id) as ordinal
                  FROM @cdm_database_schema.DRUG_EXPOSURE de
                  where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 0)
                  ) C


                  -- End Drug Exposure Criteria

                ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                GROUP BY p.person_id, p.event_id
                HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                -- End Correlated Criteria

              ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
              GROUP BY E.person_id, E.event_id
              HAVING COUNT(index_id) = 1
            ) G
            -- End Criteria Group
          ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
        ) Results
        ;

        select 2 as inclusion_rule_id, person_id, event_id
        INTO #Inclusion_2
        FROM
        (
          select pe.person_id, pe.event_id
          FROM #qualified_events pe

          JOIN (
            -- Begin Criteria Group
            select 0 as index_id, person_id, event_id
            FROM
            (
              select E.person_id, E.event_id
              FROM #qualified_events E
              LEFT JOIN
              (
                -- Begin Correlated Criteria
                SELECT 0 as index_id, p.person_id, p.event_id
                FROM #qualified_events P
                LEFT JOIN
                (
                  -- Begin Drug Exposure Criteria
                  select C.person_id, C.drug_exposure_id as event_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                  from
                  (
                    select de.*, row_number() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date, de.drug_exposure_id) as ordinal
                    FROM @cdm_database_schema.DRUG_EXPOSURE de
                    where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 3)
                    ) C


                    -- End Drug Exposure Criteria

                  ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                  GROUP BY p.person_id, p.event_id
                  HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                  -- End Correlated Criteria

                  UNION ALL
                  -- Begin Correlated Criteria
                  SELECT 1 as index_id, p.person_id, p.event_id
                  FROM #qualified_events P
                  LEFT JOIN
                  (
                    -- Begin Drug Exposure Criteria
                    select C.person_id, C.drug_exposure_id as event_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                    from
                    (
                      select de.*, row_number() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date, de.drug_exposure_id) as ordinal
                      FROM @cdm_database_schema.DRUG_EXPOSURE de
                      where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 4)
                      ) C


                      -- End Drug Exposure Criteria

                    ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                    GROUP BY p.person_id, p.event_id
                    HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                    -- End Correlated Criteria

                  ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
                  GROUP BY E.person_id, E.event_id
                  HAVING COUNT(index_id) = 2
                ) G
                -- End Criteria Group
              ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
            ) Results
            ;

            select 3 as inclusion_rule_id, person_id, event_id
            INTO #Inclusion_3
            FROM
            (
              select pe.person_id, pe.event_id
              FROM #qualified_events pe

              JOIN (
                -- Begin Criteria Group
                select 0 as index_id, person_id, event_id
                FROM
                (
                  select E.person_id, E.event_id
                  FROM #qualified_events E
                  LEFT JOIN
                  (
                    -- Begin Correlated Criteria
                    SELECT 0 as index_id, p.person_id, p.event_id
                    FROM #qualified_events P
                    LEFT JOIN
                    (
                      -- Begin Visit Occurrence Criteria
                      select C.person_id, C.visit_occurrence_id as event_id, C.visit_start_date as start_date, C.visit_end_date as end_date, C.visit_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                      from
                      (
                        select vo.*, row_number() over (PARTITION BY vo.person_id ORDER BY vo.visit_start_date, vo.visit_occurrence_id) as ordinal
                        FROM @cdm_database_schema.VISIT_OCCURRENCE vo
                        where vo.visit_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 5)
                        ) C


                        -- End Visit Occurrence Criteria

                      ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,0,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                      GROUP BY p.person_id, p.event_id
                      HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                      -- End Correlated Criteria

                      UNION ALL
                      -- Begin Correlated Criteria
                      SELECT 1 as index_id, p.person_id, p.event_id
                      FROM #qualified_events P
                      LEFT JOIN
                      (
                        -- Begin Procedure Occurrence Criteria
                        select C.person_id, C.procedure_occurrence_id as event_id, C.procedure_date as start_date, DATEADD(d,1,C.procedure_date) as END_DATE, C.procedure_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                        from
                        (
                          select po.*, row_number() over (PARTITION BY po.person_id ORDER BY po.procedure_date, po.procedure_occurrence_id) as ordinal
                          FROM @cdm_database_schema.PROCEDURE_OCCURRENCE po
                          where po.procedure_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 6)
                          ) C


                          -- End Procedure Occurrence Criteria

                        ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                        GROUP BY p.person_id, p.event_id
                        HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                        -- End Correlated Criteria

                        UNION ALL
                        -- Begin Correlated Criteria
                        SELECT 2 as index_id, p.person_id, p.event_id
                        FROM #qualified_events P
                        LEFT JOIN
                        (
                          -- Begin Observation Criteria
                          select C.person_id, C.observation_id as event_id, C.observation_date as start_date, DATEADD(d,1,C.observation_date) as END_DATE, C.observation_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                          from
                          (
                            select o.*, row_number() over (PARTITION BY o.person_id ORDER BY o.observation_date, o.observation_id) as ordinal
                            FROM @cdm_database_schema.OBSERVATION o
                            where o.observation_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 6)
                            ) C


                            -- End Observation Criteria
                          ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                          GROUP BY p.person_id, p.event_id
                          HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                          -- End Correlated Criteria

                        ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
                        GROUP BY E.person_id, E.event_id
                        HAVING COUNT(index_id) = 3
                      ) G
                      -- End Criteria Group
                    ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
                  ) Results
                  ;

                  select 4 as inclusion_rule_id, person_id, event_id
                  INTO #Inclusion_4
                  FROM
                  (
                    select pe.person_id, pe.event_id
                    FROM #qualified_events pe

                    JOIN (
                      -- Begin Criteria Group
                      select 0 as index_id, person_id, event_id
                      FROM
                      (
                        select E.person_id, E.event_id
                        FROM #qualified_events E
                        LEFT JOIN
                        (
                          -- Begin Correlated Criteria
                          SELECT 0 as index_id, p.person_id, p.event_id
                          FROM #qualified_events P
                          LEFT JOIN
                          (
                            -- Begin Condition Occurrence Criteria
                            SELECT C.person_id, C.condition_occurrence_id as event_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID, C.visit_occurrence_id
                            FROM
                            (
                              SELECT co.*, row_number() over (PARTITION BY co.person_id ORDER BY co.condition_start_date, co.condition_occurrence_id) as ordinal
                              FROM @cdm_database_schema.CONDITION_OCCURRENCE co
                              where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 10)
                              ) C


                              -- End Condition Occurrence Criteria

                            ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                            GROUP BY p.person_id, p.event_id
                            HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                            -- End Correlated Criteria

                            UNION ALL
                            -- Begin Correlated Criteria
                            SELECT 1 as index_id, p.person_id, p.event_id
                            FROM #qualified_events P
                            LEFT JOIN
                            (
                              -- Begin Procedure Occurrence Criteria
                              select C.person_id, C.procedure_occurrence_id as event_id, C.procedure_date as start_date, DATEADD(d,1,C.procedure_date) as END_DATE, C.procedure_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                              from
                              (
                                select po.*, row_number() over (PARTITION BY po.person_id ORDER BY po.procedure_date, po.procedure_occurrence_id) as ordinal
                                FROM @cdm_database_schema.PROCEDURE_OCCURRENCE po
                                where po.procedure_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 10)
                                ) C


                                -- End Procedure Occurrence Criteria

                              ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                              GROUP BY p.person_id, p.event_id
                              HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                              -- End Correlated Criteria

                              UNION ALL
                              -- Begin Correlated Criteria
                              SELECT 2 as index_id, p.person_id, p.event_id
                              FROM #qualified_events P
                              LEFT JOIN
                              (
                                -- Begin Observation Criteria
                                select C.person_id, C.observation_id as event_id, C.observation_date as start_date, DATEADD(d,1,C.observation_date) as END_DATE, C.observation_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                                from
                                (
                                  select o.*, row_number() over (PARTITION BY o.person_id ORDER BY o.observation_date, o.observation_id) as ordinal
                                  FROM @cdm_database_schema.OBSERVATION o
                                  where o.observation_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 10)
                                  ) C


                                  -- End Observation Criteria
                                ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                                GROUP BY p.person_id, p.event_id
                                HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                                -- End Correlated Criteria

                              ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
                              GROUP BY E.person_id, E.event_id
                              HAVING COUNT(index_id) = 3
                            ) G
                            -- End Criteria Group
                          ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
                        ) Results
                        ;

                        select 5 as inclusion_rule_id, person_id, event_id
                        INTO #Inclusion_5
                        FROM
                        (
                          select pe.person_id, pe.event_id
                          FROM #qualified_events pe

                          JOIN (
                            -- Begin Criteria Group
                            select 0 as index_id, person_id, event_id
                            FROM
                            (
                              select E.person_id, E.event_id
                              FROM #qualified_events E
                              LEFT JOIN
                              (
                                -- Begin Correlated Criteria
                                SELECT 0 as index_id, p.person_id, p.event_id
                                FROM #qualified_events P
                                LEFT JOIN
                                (
                                  -- Begin Condition Occurrence Criteria
                                  SELECT C.person_id, C.condition_occurrence_id as event_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID, C.visit_occurrence_id
                                  FROM
                                  (
                                    SELECT co.*, row_number() over (PARTITION BY co.person_id ORDER BY co.condition_start_date, co.condition_occurrence_id) as ordinal
                                    FROM @cdm_database_schema.CONDITION_OCCURRENCE co
                                    where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 7)
                                    ) C


                                    -- End Condition Occurrence Criteria

                                  ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                                  GROUP BY p.person_id, p.event_id
                                  HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                                  -- End Correlated Criteria

                                  UNION ALL
                                  -- Begin Correlated Criteria
                                  SELECT 1 as index_id, p.person_id, p.event_id
                                  FROM #qualified_events P
                                  LEFT JOIN
                                  (
                                    -- Begin Procedure Occurrence Criteria
                                    select C.person_id, C.procedure_occurrence_id as event_id, C.procedure_date as start_date, DATEADD(d,1,C.procedure_date) as END_DATE, C.procedure_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                                    from
                                    (
                                      select po.*, row_number() over (PARTITION BY po.person_id ORDER BY po.procedure_date, po.procedure_occurrence_id) as ordinal
                                      FROM @cdm_database_schema.PROCEDURE_OCCURRENCE po
                                      where po.procedure_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 7)
                                      ) C


                                      -- End Procedure Occurrence Criteria

                                    ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                                    GROUP BY p.person_id, p.event_id
                                    HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                                    -- End Correlated Criteria

                                    UNION ALL
                                    -- Begin Correlated Criteria
                                    SELECT 2 as index_id, p.person_id, p.event_id
                                    FROM #qualified_events P
                                    LEFT JOIN
                                    (
                                      -- Begin Observation Criteria
                                      select C.person_id, C.observation_id as event_id, C.observation_date as start_date, DATEADD(d,1,C.observation_date) as END_DATE, C.observation_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                                      from
                                      (
                                        select o.*, row_number() over (PARTITION BY o.person_id ORDER BY o.observation_date, o.observation_id) as ordinal
                                        FROM @cdm_database_schema.OBSERVATION o
                                        where o.observation_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 7)
                                        ) C


                                        -- End Observation Criteria
                                      ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                                      GROUP BY p.person_id, p.event_id
                                      HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                                      -- End Correlated Criteria

                                    ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
                                    GROUP BY E.person_id, E.event_id
                                    HAVING COUNT(index_id) = 3
                                  ) G
                                  -- End Criteria Group
                                ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
                              ) Results
                              ;

                              select 6 as inclusion_rule_id, person_id, event_id
                              INTO #Inclusion_6
                              FROM
                              (
                                select pe.person_id, pe.event_id
                                FROM #qualified_events pe

                                JOIN (
                                  -- Begin Criteria Group
                                  select 0 as index_id, person_id, event_id
                                  FROM
                                  (
                                    select E.person_id, E.event_id
                                    FROM #qualified_events E
                                    LEFT JOIN
                                    (
                                      -- Begin Correlated Criteria
                                      SELECT 0 as index_id, p.person_id, p.event_id
                                      FROM #qualified_events P
                                      LEFT JOIN
                                      (
                                        -- Begin Condition Occurrence Criteria
                                        SELECT C.person_id, C.condition_occurrence_id as event_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID, C.visit_occurrence_id
                                        FROM
                                        (
                                          SELECT co.*, row_number() over (PARTITION BY co.person_id ORDER BY co.condition_start_date, co.condition_occurrence_id) as ordinal
                                          FROM @cdm_database_schema.CONDITION_OCCURRENCE co
                                          where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 11)
                                          ) C


                                          -- End Condition Occurrence Criteria

                                        ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                                        GROUP BY p.person_id, p.event_id
                                        HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                                        -- End Correlated Criteria

                                        UNION ALL
                                        -- Begin Correlated Criteria
                                        SELECT 1 as index_id, p.person_id, p.event_id
                                        FROM #qualified_events P
                                        LEFT JOIN
                                        (
                                          -- Begin Condition Occurrence Criteria
                                          SELECT C.person_id, C.condition_occurrence_id as event_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID, C.visit_occurrence_id
                                          FROM
                                          (
                                            SELECT co.*, row_number() over (PARTITION BY co.person_id ORDER BY co.condition_start_date, co.condition_occurrence_id) as ordinal
                                            FROM @cdm_database_schema.CONDITION_OCCURRENCE co
                                            where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 12)
                                            ) C


                                            -- End Condition Occurrence Criteria

                                          ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                                          GROUP BY p.person_id, p.event_id
                                          HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                                          -- End Correlated Criteria

                                        ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
                                        GROUP BY E.person_id, E.event_id
                                        HAVING COUNT(index_id) = 2
                                      ) G
                                      -- End Criteria Group
                                    ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
                                  ) Results
                                  ;

                                  select 7 as inclusion_rule_id, person_id, event_id
                                  INTO #Inclusion_7
                                  FROM
                                  (
                                    select pe.person_id, pe.event_id
                                    FROM #qualified_events pe

                                    JOIN (
                                      -- Begin Criteria Group
                                      select 0 as index_id, person_id, event_id
                                      FROM
                                      (
                                        select E.person_id, E.event_id
                                        FROM #qualified_events E
                                        LEFT JOIN
                                        (
                                          -- Begin Correlated Criteria
                                          SELECT 0 as index_id, p.person_id, p.event_id
                                          FROM #qualified_events P
                                          LEFT JOIN
                                          (
                                            -- Begin Procedure Occurrence Criteria
                                            select C.person_id, C.procedure_occurrence_id as event_id, C.procedure_date as start_date, DATEADD(d,1,C.procedure_date) as END_DATE, C.procedure_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id
                                            from
                                            (
                                              select po.*, row_number() over (PARTITION BY po.person_id ORDER BY po.procedure_date, po.procedure_occurrence_id) as ordinal
                                              FROM @cdm_database_schema.PROCEDURE_OCCURRENCE po
                                              where po.procedure_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 9)
                                              ) C


                                              -- End Procedure Occurrence Criteria

                                            ) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,-183,P.START_DATE) and A.START_DATE <= DATEADD(day,0,P.START_DATE)
                                            GROUP BY p.person_id, p.event_id
                                            HAVING COUNT(A.TARGET_CONCEPT_ID) = 0
                                            -- End Correlated Criteria

                                          ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
                                          GROUP BY E.person_id, E.event_id
                                          HAVING COUNT(index_id) = 1
                                        ) G
                                        -- End Criteria Group
                                      ) AC on AC.person_id = pe.person_id AND AC.event_id = pe.event_id
                                    ) Results
                                    ;

                                    SELECT inclusion_rule_id, person_id, event_id
                                    INTO #inclusion_events
                                    FROM (select inclusion_rule_id, person_id, event_id from #Inclusion_0
                                          UNION ALL
                                          select inclusion_rule_id, person_id, event_id from #Inclusion_1
                                          UNION ALL
                                          select inclusion_rule_id, person_id, event_id from #Inclusion_2
                                          UNION ALL
                                          select inclusion_rule_id, person_id, event_id from #Inclusion_3
                                          UNION ALL
                                          select inclusion_rule_id, person_id, event_id from #Inclusion_4
                                          UNION ALL
                                          select inclusion_rule_id, person_id, event_id from #Inclusion_5
                                          UNION ALL
                                          select inclusion_rule_id, person_id, event_id from #Inclusion_6
                                          UNION ALL
                                          select inclusion_rule_id, person_id, event_id from #Inclusion_7) I;
                                          TRUNCATE TABLE #Inclusion_0;
                                          DROP TABLE #Inclusion_0;

                                          TRUNCATE TABLE #Inclusion_1;
                                          DROP TABLE #Inclusion_1;

                                          TRUNCATE TABLE #Inclusion_2;
                                          DROP TABLE #Inclusion_2;

                                          TRUNCATE TABLE #Inclusion_3;
                                          DROP TABLE #Inclusion_3;

                                          TRUNCATE TABLE #Inclusion_4;
                                          DROP TABLE #Inclusion_4;

                                          TRUNCATE TABLE #Inclusion_5;
                                          DROP TABLE #Inclusion_5;

                                          TRUNCATE TABLE #Inclusion_6;
                                          DROP TABLE #Inclusion_6;

                                          TRUNCATE TABLE #Inclusion_7;
                                          DROP TABLE #Inclusion_7;


                                          with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
                                          (
                                            SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
                                            from
                                            (
                                              select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
                                              from #qualified_events Q
                                              LEFT JOIN #inclusion_events I on I.person_id = Q.person_id and I.event_id = Q.event_id
                                              GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
                                            ) MG -- matching groups

                                            -- the matching group with all bits set ( POWER(2,# of inclusion rules) - 1 = inclusion_rule_mask
                                                                                            WHERE (MG.inclusion_rule_mask = POWER(cast(2 as bigint),8)-1)

                                            )
                                            select event_id, person_id, start_date, end_date, op_start_date, op_end_date
                                            into #included_events
                                            FROM cteIncludedEvents Results
                                            WHERE Results.ordinal = 1
                                            ;

                                            -- custom era strategy

                                            select de.PERSON_ID, DRUG_EXPOSURE_START_DATE,  COALESCE(DRUG_EXPOSURE_END_DATE, DATEADD(day,DAYS_SUPPLY,DRUG_EXPOSURE_START_DATE), DATEADD(day,1,DRUG_EXPOSURE_START_DATE)) as DRUG_EXPOSURE_END_DATE
                                            INTO #drugTarget
                                            FROM @cdm_database_schema.DRUG_EXPOSURE de
                                            WHERE de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 8)
                                                                         AND de.person_id in (select person_id from #included_events)
                                                                                              ;

                                                                                              select et.event_id, et.person_id, ERAS.era_end_date as end_date
                                                                                              INTO #strategy_ends
                                                                                              from #included_events et
                                                                                              JOIN
                                                                                              (
                                                                                                select ENDS.person_id, min(drug_exposure_start_date) as era_start_date, DATEADD(day,0, ENDS.era_end_date) as era_end_date
                                                                                                from
                                                                                                (
                                                                                                  select de.person_id, de.drug_exposure_start_date, MIN(e.END_DATE) as era_end_date
                                                                                                  FROM #drugTarget DE
                                                                                                  JOIN
                                                                                                  (
                                                                                                    --cteEndDates
                                                                                                    select PERSON_ID, DATEADD(day,-1 * 3,EVENT_DATE) as END_DATE -- unpad the end date by 3
                                                                                                    FROM
                                                                                                    (
                                                                                                      select PERSON_ID, EVENT_DATE, EVENT_TYPE,
                                                                                                      MAX(START_ORDINAL) OVER (PARTITION BY PERSON_ID ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal,
                                                                                                      ROW_NUMBER() OVER (PARTITION BY PERSON_ID ORDER BY EVENT_DATE, EVENT_TYPE) AS OVERALL_ORD -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
                                                                                                      from
                                                                                                      (
                                                                                                        -- select the start dates, assigning a row number to each
                                                                                                        Select PERSON_ID, DRUG_EXPOSURE_START_DATE AS EVENT_DATE, 0 as EVENT_TYPE, ROW_NUMBER() OVER (PARTITION BY PERSON_ID ORDER BY DRUG_EXPOSURE_START_DATE) as START_ORDINAL
                                                                                                        from #drugTarget D

                                                                                                        UNION ALL

                                                                                                        -- add the end dates with NULL as the row number, padding the end dates by 3 to allow a grace period for overlapping ranges.
                                                                                                        select PERSON_ID, DATEADD(day,3,DRUG_EXPOSURE_END_DATE), 1 as EVENT_TYPE, NULL
                                                                                                        FROM #drugTarget D
                                                                                                      ) RAWDATA
                                                                                                    ) E
                                                                                                    WHERE 2 * E.START_ORDINAL - E.OVERALL_ORD = 0
                                                                                                  ) E on DE.PERSON_ID = E.PERSON_ID and E.END_DATE >= DE.DRUG_EXPOSURE_START_DATE
                                                                                                  GROUP BY de.person_id, de.drug_exposure_start_date
                                                                                                ) ENDS
                                                                                                GROUP BY ENDS.person_id, ENDS.era_end_date
                                                                                              ) ERAS on ERAS.person_id = et.person_id
                                                                                              WHERE et.start_date between ERAS.era_start_date and ERAS.era_end_date;

                                                                                              TRUNCATE TABLE #drugTarget;
                                                                                              DROP TABLE #drugTarget;


                                                                                              -- generate cohort periods into #final_cohort
                                                                                              with cohort_ends (event_id, person_id, end_date) as
                                                                                              (
                                                                                                -- cohort exit dates
                                                                                                -- By default, cohort exit at the event's op end date
                                                                                                select event_id, person_id, op_end_date as end_date from #included_events
                                                                                                UNION ALL
                                                                                                -- End Date Strategy
                                                                                                SELECT event_id, person_id, end_date from #strategy_ends

                                                                                              ),
                                                                                                first_ends (person_id, start_date, end_date) as
                                                                                                (
                                                                                                select F.person_id, F.start_date, F.end_date
                                                                                                FROM (
                                                                                                select I.event_id, I.person_id, I.start_date, E.end_date, row_number() over (partition by I.person_id, I.event_id order by E.end_date) as ordinal
                                                                                                from #included_events I
                                                                                                join cohort_ends E on I.event_id = E.event_id and I.person_id = E.person_id and E.end_date >= I.start_date
                                                                                                ) F
                                                                                                WHERE F.ordinal = 1
                                                                                                )
                                                                                                select person_id, start_date, end_date
                                                                                                INTO #cohort_rows
                                                                                                from first_ends;

                                                                                                with cteEndDates (person_id, end_date) AS -- the magic
                                                                                                (
                                                                                                SELECT
                                                                                                person_id
                                                                                                , DATEADD(day,-1 * 0, event_date)  as end_date
                                                                                                FROM
                                                                                                (
                                                                                                SELECT
                                                                                                person_id
                                                                                                , event_date
                                                                                                , event_type
                                                                                                , MAX(start_ordinal) OVER (PARTITION BY person_id ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal
                                                                                                , ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY event_date, event_type) AS overall_ord
                                                                                                FROM
                                                                                                (
                                                                                                SELECT
                                                                                                person_id
                                                                                                , start_date AS event_date
                                                                                                , -1 AS event_type
                                                                                                , ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date) AS start_ordinal
                                                                                                FROM #cohort_rows

                                                                                                UNION ALL


                                                                                                SELECT
                                                                                                person_id
                                                                                                , DATEADD(day,0,end_date) as end_date
                                                                                                , 1 AS event_type
                                                                                                , NULL
                                                                                                FROM #cohort_rows
                                                                                                ) RAWDATA
                                                                                                ) e
                                                                                                WHERE (2 * e.start_ordinal) - e.overall_ord = 0
                                                                                                ),
                                                                                                cteEnds (person_id, start_date, end_date) AS
                                                                                                (
                                                                                                SELECT
                                                                                                c.person_id
                                                                                                , c.start_date
                                                                                                , MIN(e.end_date) AS era_end_date
                                                                                                FROM #cohort_rows c
                                                                                                JOIN cteEndDates e ON c.person_id = e.person_id AND e.end_date >= c.start_date
                                                                                                GROUP BY c.person_id, c.start_date
                                                                                                )
                                                                                                select person_id, min(start_date) as start_date, end_date
                                                                                                into #final_cohort
                                                                                                from cteEnds
                                                                                                group by person_id, end_date
                                                                                                ;

                                                                                                DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
                                                                                                INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
                                                                                                select @target_cohort_id as cohort_definition_id, person_id, start_date, end_date
                                                                                                FROM #final_cohort CO
                                                                                                ;



                                                                                                TRUNCATE TABLE #strategy_ends;
                                                                                                DROP TABLE #strategy_ends;


                                                                                                TRUNCATE TABLE #cohort_rows;
                                                                                                DROP TABLE #cohort_rows;

                                                                                                TRUNCATE TABLE #final_cohort;
                                                                                                DROP TABLE #final_cohort;

                                                                                                TRUNCATE TABLE #inclusion_events;
                                                                                                DROP TABLE #inclusion_events;

                                                                                                TRUNCATE TABLE #qualified_events;
                                                                                                DROP TABLE #qualified_events;

                                                                                                TRUNCATE TABLE #included_events;
                                                                                                DROP TABLE #included_events;

                                                                                                TRUNCATE TABLE #Codesets;
                                                                                                DROP TABLE #Codesets;
