WITH by_cp AS (
  SELECT
    clickability_test_id,
    click_package_id,
    SUM(left_quickly_30) as exited_quickly,
    SUM(visitor_count) as visitors
  FROM
    traffic
  WHERE
    len(click_package_id) = 24
  GROUP BY 1, 2),
involved_nuggets as (
  SELECT
    nugget_id,
    clickability_test_id
  FROM
    clickability_tests ct
    JOIN by_cp on ct._id = by_cp.clickability_test_id
  GROUP BY 1, 2),
by_nugget as (
  SELECT
    host_nugget_id as nugget_id,
    SUM(left_quickly_30) as exited_quickly,
    SUM(visitor_count) as visitors
  FROM
    traffic
    JOIN involved_nuggets inv on inv.nugget_id = host_nugget_id
  GROUP BY 1)
SELECT
  bn.nugget_id,
  bn.exited_quickly,
  bn.visitors,
  bc.clickability_test_id,
  bc.click_package_id,
  bc.exited_quickly,
  bc.visitors
FROM
  by_nugget bn
  JOIN involved_nuggets using (nugget_id)
  JOIN by_cp bc using (clickability_test_id);