-- Get a list of everything downstream that depends on a system

create temp table xxx as
with recursive
depends_on(dependency, dependent) as (
  select dep_id, system_id
  from dep
  where dep_id = 2159
  union
  select dep_id, system_id
  from dep, depends_on
  where dep_id = dependent
),
dependents_per_system as (
  select
    (select name from system where id = dependency) as dependency,
    group_concat((select name from system where id = dependent)) as dependent,
    count(dependent) as dependent_count
  from depends_on
  group by dependency
) select dependency, group_concat(dependent) from dependents_per_system group by dependent;
