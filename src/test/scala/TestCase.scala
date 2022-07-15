
//***************************************************************************************
//
// Title:   SQL Parser Test Cases
// Author:  Xiaomin Wang, Alex Schuler, Shue Kwan Ip, Ahmet Kösker
// Date:    January 25th, 2022
//
//***************************************************************************************

val sql_0 =
  """SELECT
    |    car/color
    |WHERE
    |    car/make = "Ford"
    |LIMIT 3""".stripMargin

/************************** Projection **************************/
//projectionType all
val sql_1 =
  """
    |SELECT
    |    ALL
    |    lineitem/shipmode
    |""".stripMargin

//projectionType distinct
val sql_2 =
  """
    |SELECT
    |    DISTINCT
    |    lineitem/shipmode
    |""".stripMargin

//projectionType none;
//result-column expression only
//result-column has column-alias(AS)
val sql_3 =
  """
    |SELECT
    |    lineitem/shipmode Shipmode,
    |    lineitem/name AS Name
    |""".stripMargin

//result-column *
val sql_4 =
  """
    |SELECT
    |    *
    |""".stripMargin

//cast_clause;
//literalValue(double)
val sql_5 =
"""
  |SELECT
  |    CAST(25.65 AS INT)
  |""".stripMargin

/************************** Selection **************************/
//where expression: binary_operator_clause(=);
//                  literalValue(string)
val sql_6 =
  """
    |SELECT
    |    part/partkey
    |WHERE
    |    part/partkey = "key"
    |""".stripMargin

//where expression: binary_operator_clause(!=,+,-);
//                  literalValue(int);
//                  unary_operator_clause(-);
val sql_7 =
  """
    |SELECT
    |    lineitem/quantity
    |WHERE
    |    -lineitem/quantity != -3
    |""".stripMargin

//where expression: expr_collate_clause;
val sql_8 =
"""
  |SELECT
  |    customer/name
  |WHERE
  |    customer/name COLLATE SQL_General_CP1_CS_AS
  |""".stripMargin

//where expression: not_clause((NOT)LIKE,GLOB,REGEXP,MATCH)
val sql_9 =
  """
    |SELECT
    |    part/type,
    |    customer/name,
    |    supplier/name,
    |    region/name
    |WHERE
    |    (part/type LIKE "%foo" ESCAPE "\")
    |    AND (customer/name NOT GLOB "Man*" )
    |    OR (supplier/name REGEXP "^[abcd]")
    |    AND (region/name MATCH "R?x[a-z]")
    |""".stripMargin

//where expression: expr_null_clause(ISNULL,NOTNULL,NOT NULL)
val sql_10 =
  """
    |SELECT
    |    customer/acctbal,
    |    customer/name,
    |    customer/address
    |WHERE
    |    (customer/acctbal ISNULL)
    |    AND (customer/name NOTNULL)
    |    AND (customer/address NOT NULL)
    |""".stripMargin

//where expression: is_not_clause(IS,IS NOT)
val sql_11 =
  """
    |SELECT
    |    customer/acctbal,
    |    customer/name
    |WHERE
    |    (customer/acctbal IS NULL)
    |    AND (customer/name IS NOT NULL)
    |""".stripMargin

//where expression: not_between_clause((NOT) BETWEEN AND)
val sql_12 =
  """
    |SELECT
    |    product/orderdate,
    |    lineitem/discount
    |WHERE
    |    (product/orderdate BETWEEN '1995-01-01' AND '1996-12-31')
    |    OR (lineitem/discount NOT BETWEEN (2 - 0.01) AND (2 + 0.01) )
    |""".stripMargin

//where expression: not_in_clause
val sql_13 =
  """
    |SELECT
    |    lineitem/shipmode,
    |    part/size,
    |    product/orderkey
    |WHERE
    |    (
    |    lineitem/shipmode IN ("mode0", "mode1")
    |    AND part/size NOT IN (3, 4, 5, 6)
    |    )
    |    OR
    |    (
    |    product/orderkey IN (
    |        SELECT
    |            lineitem/orderkey
    |        WHERE
    |            lineitem/orderkey = 'key'
    |        )
    |    AND part/size IN (part/size > 7, part/size < 10)
    |    )
    |""".stripMargin

//where expression: not_exists_clause
val sql_14 =
  """
    |SELECT
    |    supplier/name
    |WHERE
    |    EXISTS (
    |    SELECT
    |      *
    |    WHERE
    |      lineitem/orderkey = product/orderkey
    |    )
    |    AND NOT EXISTS (
    |    SELECT
    |      *
    |    WHERE
    |      lineitem/receiptdate <> nation/receiptdate
    |  )
    |""".stripMargin


/************************** GroupBy **************************/
val sql_15 =
  """
    |SELECT
    |    lineitem/returnflag,
    |    lineitem/linestatus
    |WHERE
    |    product/orderkey IN (
    |    SELECT
    |        product/orderkey
    |    GROUP BY
    |        product/orderkey HAVING SUM(lineitem/quantity) > 330
    |    )
    |GROUP BY
    |    l_returnflag,
    |    l_linestatus
    |""".stripMargin

/************************** OrderBy **************************/
//only with column_clause
val sql_16 =
  """
    |SELECT
    |    supplier/name,
    |    supplier/address
    |WHERE
    |    product/orderstatus = "F"
    |ORDER BY
    |  supplier/name
    |""".stripMargin

//COLLATE; ASC; NULLS FIRST
val sql_17 =
  """
    |SELECT
    |    supplier/name
    |WHERE
    |    product/orderstatus = "F"
    |ORDER BY
    |    supplier/name COLLATE Traditional_name ASC NULLS FIRST
    |""".stripMargin

//DESC; NULLS LAST
val sql_18 =
  """
    |SELECT
    |    SUM(customer/acctbal) AS total_acctbal
    |WHERE
    |    customer/phone IN (55, 66, 77)
    |ORDER BY
    |  customer/name COLLATE Traditional_name DESC NULLS LAST
    |""".stripMargin

/************************** Limit **************************/
val sql_19 =
  """
    |SELECT
    |    SUM(lineitem/extendedpricen * (1 - lineitem/discount)) AS revenue,
    |    customer/name
    |WHERE
    |    part/container IN ('MED BAG', 'MED BOX')
    |GROUP BY
    |    customer/name
    |ORDER BY
    |    customer/orderdate ASC
    |LIMIT 10
    |""".stripMargin

//with offset
val sql_20 =
  """
    |SELECT
    |    supplier/address,
    |    COUNT(*) AS numwait
    |WHERE
    |    (lineitem/quantity >= 5) 
    |    AND (lineitem/quantity <= 5 + 10)
    |GROUP BY
    |    supplier/address
    |ORDER BY
    |    numwait DESC
    |LIMIT 5 OFFSET 3
    |""".stripMargin

/************************** Window **************************/
//PARTITION BY
val sql_23 =
  """
    |SELECT
    |    AVG(part/size) OVER (
    |                          PARTITION BY part/type
    |                          ORDER BY part/name
    |                         ) AS part_avg
    |WHERE
    |    region/name = “somename”
    |""".stripMargin

//RANGE;ROWS;GROUPS
val sql_24 =
  """
    |SELECT
    |    product/date,
    |    AVG(product/amount) OVER (
    |                             PARTITION BY product/type
    |                             ORDER BY product/date ASC
    |                             RANGE BETWEEN INTERVAL "1" DAY PRECEDING AND CURRENT ROW
    |                            ) AS amount_avg,
    |    COUNT(*) OVER (
    |                   ORDER BY product/id
    |                   GROUPS BETWEEN 2 PRECEDING AND 2 FOLLOWING
    |                  )AS product_number,
    |    COUNT(*) OVER (
    |                   ORDER BY supplier/id
    |                   ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING
    |                   )AS supplier_number
    |""".stripMargin


/*
val sql_1 =
  """
    |SELECT
    |   lineitem/l_shipmode,
    |  sum(case
    |    when order/o_orderpriority = '1-URGENT'
    |      or order/o_orderpriority = '2-HIGH'
    |      then 1
    |    else 0
    |  end) as high_line_count,
    |  count(*) as custdist,
    |  0.5 * sum(lineitem/l_quantity)
    |WHERE
    |  lineitem/l_shipdate <= date '1998-12-01' - interval '5' day
    |  and part/p_partkey = partsupp/ps_partkey
    |  and part/p_size = 10
    |  and part/p_type like '%foo'
    |  and part/p_type not like 'bar%'
    |  and part/p_brand GLOB 'AAA*'
    |  and supplier/s_comment NOT REGEXP '^[A-Z0-9._%-]+@[A-Z0-9.-]+.[A-Z]{2,4}$'
    |  and supplier/s_comment Match '%Customer%Complaints%'
    |  and region/r_name = 'somename'
    |  and lineitem/l_discount BETWEEN 2 - 0.01 AND 2 + 0.01
    |  and lineitem/l_quantity < 3
    |  and partsupp/ps_supplycost = (
    |    select
    |      min(partsupp/ps_supplycost)
    |    where
    |      lineitem/l_commitdate < lineitem/l_receiptdate
    |      and (
    |        (nation/n_name = 'a' and region.n_name = 'b')
    |        or (supplier/s_nationkey = nation/n_nationkey and lineitem/l_shipdate between date '1995-01-01' and date '1996-12-31')
    |      )
    |      and lineitem/l_shipmode in ('mode0', 'mode1')
    |  )
    |  and exists (
    |    SELECT
    |      *
    |    where
    |      lineitem/l_orderkey = o_orderkey
    |      and lineitem/l_commitdate < lineitem/l_receiptdate
    |  )
    |GROUP BY
    |  part/p_brand,
    |  part/p_type,
    |  part/p_size
    |ORDER BY
    |  supplier/s_cnt desc
    |LIMIT 100
    """.stripMargin

  val sql_2 =
    """
      |SELECT
      |  lineitem/l_linestatus,
      |  sum(lineitem/l_quantity) as sum_qty,
      |  avg(lineitem/l_quantity) as avg_qty,
      |  sum(lineitem/l_extendedprice * (1 - lineitem/l_discount)) as revenue,
      |  supplier/s_name,
      |  supplier/s_address
      |WHERE
      |  part/p_partkey = l_partkey
      |  and part/p_brand = 'baz'
      |  and part/p_container not in ('LG CASE', 'LG BOX', 'LG PACK', 'LG PKG')
      |  and lineitem/l_quantity >= 6 and lineitem/l_quantity <= 6 + 10
      |  and part/p_size between 1 and 15
      |  and lineitem/l_shipmode in ('AIR', 'AIR REG')
      |  and lineitem/l_shipinstruct = 'DELIVER IN PERSON'
      |  and partsupp/ps_availqty > (
      |        SELECT
      |          0.5 * sum(lineitem/l_quantity)
      |        WHERE
      |          lineitem/l_partkey = partsupp/ps_partkey
      |          and lineitem/l_suppkey = partsupp/ps_suppkey
      |          and lineitem/l_shipdate >= date '1999-01-01'
      |          and lineitem/l_shipdate < date '1999-01-01' + interval '1' year
      |      )
      |   OR ps_partkey in (
      |        select
      |          part/p_partkey
      |        where
      |          p_name like 'foo%'
      |          and substring(customer/c_phone from 1 for 2) in ('11', '22', '33', '44', '55', '66', '77')
      |          and lineitem/l_shipdate NOT between date '1995-01-01' and date '1996-12-31'
      |      )
      |GROUP BY
      |  partsupp/ps_partkey having
      |    sum(partsupp/ps_supplycost * partsupp/ps_availqty) > (
      |      select
      |        sum(partsupp/ps_supplycost * partsupp/ps_availqty) * 300
      |      where
      |        partsupp/ps_suppkey = supplier/s_suppkey
      |        and supplier/s_nationkey = nation/n_nationkey
      |        and nation/n_name = 'name'
      |        AND oders/o_orderpriority <> '1-URGENT'
      |        AND oders/o_orderpriority <> '2-HIGH'
      |    )
      |ORDER BY
      |  sum_qty ASC NULLS FIRST
      |""".stripMargin
*/

//***************************************************************************************
// End of File
//***************************************************************************************