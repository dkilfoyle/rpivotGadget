tmplTableCount =
'# grouped by = {{groupby}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n())
'
tmplTableAgg =
'# grouped by  = {{groupby}}
# aggregated to = {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{vals}}.{{agg}}={{agg}}({{vals}}, na.rm=T)) {{#bar}} %>%
    ggplot2(aes(x={{groupby}}, y={{vals}}.{{agg}})) + geom_bar(stat="identity") {{/bar}}'

tmplBarCount =
'# grouped by {{groupby}}
ggplot({{df}}, aes(x={{groupby}})) +
    geom_bar()
'

tmplBarAgg1 =
'# grouped by  = {{groupby}}
# aggregated to = {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{vals}}.{{agg}}={{agg}}({{vals}}, na.rm=T)) %>%
    ggplot2(aes(x={{groupby}}, y={{vals}}.{{agg}})) + geom_bar(stat="identity")
'

tmplBarAgg2 =
  '# grouped by  = {{groupby}}
# aggregated to = {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{vals}}.{{agg}}={{agg}}({{vals}}, na.rm=T)) %>%
    ggplot(aes(x={{group1}}, fill={{group2}}, y={{vals}}.{{agg}})) + geom_bar(stat="identity", position="dodge")
'

tmplBarAgg3 =
  '# grouped by  = {{groupby}}
# aggregated to = {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{vals}}.{{agg}}={{agg}}({{vals}}, na.rm=T)) %>%
    ggplot(aes(x={{group1}}, fill={{group2}}, y={{vals}}.{{agg}})) + geom_bar(stat="identity", position="dodge") + facet_grid(. ~{{group3}})
'

tmplStackedAgg2 =
  '# grouped by  = {{groupby}}
# aggregated to = {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{vals}}.{{agg}}={{agg}}({{vals}}, na.rm=T)) %>%
  # mutate(group=paste0({{group1}}, "_", {{group2}})) %>%
    ggplot(aes(x={{group1}}, fill={{group2}}, y={{vals}}.{{agg}})) + geom_bar(stat="identity")
'
